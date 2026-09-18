from __future__ import annotations

import concurrent.futures
import os
import stat
import time
from pathlib import Path

import pytest
import zmq

import zara.server as server_module
from zara.principals import PrincipalContext
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.security import Capability, KeyNotActive
from zara.security_admin import SecurityAdminClient
from zara.security_state import PersistentSecurityState
from zara.security_transport import CurveClientConfig, configure_curve_client_socket
from zara.server import ServerLease, ServerState, ZaraServer, default_zmq_endpoint
from zara.zmq_transport import TransportConfig, apply_socket_options


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.NEW
        self._buses: dict[str, bridge.RuntimeEventBus] = {}

    def start(self, principal: PrincipalContext):
        self.state = ServerState.READY
        self._buses.setdefault(principal.principal_id, bridge.RuntimeEventBus())
        return object()

    def open_principal(self, principal: PrincipalContext):
        self._buses.setdefault(principal.principal_id, bridge.RuntimeEventBus())
        return object()

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        return self._buses.setdefault(
            principal.principal_id,
            bridge.RuntimeEventBus(),
        ).subscribe(maxsize=maxsize)

    def shutdown(self) -> bool:
        self.state = ServerState.STOPPED
        return True


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def receive(socket: zmq.Socket, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def test_server_rejects_tcp_without_explicit_security_state():
    with pytest.raises(ValueError, match="security"):
        ZaraServer(endpoint="tcp://127.0.0.1:5555")


def test_remote_endpoint_requires_tcp_and_security_state(tmp_path: Path):
    with pytest.raises(ValueError, match="security"):
        ZaraServer(remote_endpoint="tcp://127.0.0.1:5555")
    state = PersistentSecurityState(tmp_path / "security")
    with pytest.raises(ValueError, match="TCP"):
        ZaraServer(remote_endpoint="ipc:///tmp/other.sock", security_state=state)


def test_cli_requires_security_directory_for_remote_endpoint(capsys):
    assert server_module.main(["--remote-endpoint", "tcp://127.0.0.1:6060"]) == 2
    assert "remote endpoint requires --security-dir" in capsys.readouterr().err


def test_server_serves_local_ipc_and_authenticated_tcp_together(
    tmp_path: Path,
    zmq_context: zmq.Context,
):
    state = PersistentSecurityState(tmp_path / "security")
    server_curve = state.initialize()
    client_public, client_secret = zmq.curve_keypair()
    state.enroll_client(
        client_public,
        device_id="android-phone",
        principal=PrincipalContext.local_owner(),
        capabilities={Capability.SESSION_BASIC},
    )
    probe = zmq_context.socket(zmq.ROUTER)
    port = probe.bind_to_random_port("tcp://127.0.0.1")
    probe.close(0)
    remote_endpoint = f"tcp://127.0.0.1:{port}"
    runtime_dir = tmp_path / "runtime"
    supervisor = FakeSupervisor()
    server = ZaraServer(
        supervisor=supervisor,
        runtime_dir=runtime_dir,
        remote_endpoint=remote_endpoint,
        security_state=state,
        gateway_transport_config=TransportConfig(linger_ms=0, poll_interval_ms=5),
        shutdown_timeout=1.0,
    )
    assert server.start() is ServerState.READY
    assert server.start() is ServerState.READY

    local = zmq_context.socket(zmq.DEALER)
    local.connect(default_zmq_endpoint(runtime_dir))
    remote = zmq_context.socket(zmq.DEALER)
    configure_curve_client_socket(
        remote,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_curve.public_key,
        ),
    )
    remote.connect(remote_endpoint)
    try:
        for socket, request_id in ((local, "local-hello"), (remote, "remote-hello")):
            socket.send_multipart(
                encode_message(
                    ProtocolMessage(
                        type="hello",
                        id=request_id,
                        timestamp_ns=time.time_ns(),
                        payload_count=0,
                        body={"versions": [1]},
                    )
                )
            )
            response = receive(socket)
            assert response.type == "hello.ok"
            assert response.reply_to == request_id
            assert response.session_id
        assert server.supervisor is supervisor
    finally:
        local.close(0)
        remote.close(0)
        assert server.stop() is True


def test_remote_bind_failure_releases_local_listener_and_lease(
    tmp_path: Path,
    zmq_context: zmq.Context,
):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    blocker = zmq_context.socket(zmq.ROUTER)
    port = blocker.bind_to_random_port("tcp://127.0.0.1")
    runtime_dir = tmp_path / "runtime"
    supervisor = FakeSupervisor()
    server = ZaraServer(
        supervisor=supervisor,
        runtime_dir=runtime_dir,
        remote_endpoint=f"tcp://127.0.0.1:{port}",
        security_state=state,
        gateway_transport_config=TransportConfig(linger_ms=0, poll_interval_ms=5),
        shutdown_timeout=1.0,
    )
    try:
        with pytest.raises(Exception):
            server.start()
        assert server.state is ServerState.STOPPED
        assert supervisor.state is ServerState.STOPPED
        lease = ServerLease(runtime_dir)
        lease.acquire()
        lease.release()
    finally:
        blocker.close(0)


def test_security_state_initializes_stable_server_identity_with_private_permissions(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")

    first = state.initialize()
    second = PersistentSecurityState(tmp_path / "security").load_server_config()

    assert first.public_key == second.public_key
    assert first.secret_key == second.secret_key
    key_file = tmp_path / "security" / "server-curve.json"
    assert stat.S_IMODE(os.lstat(key_file).st_mode) == 0o600
    assert stat.S_IMODE(os.lstat(key_file.parent).st_mode) == 0o700
    assert state.server_public_key() == first.public_key.decode("ascii")


def test_security_admin_socket_remains_in_long_owner_private_security_directory(
    tmp_path: Path,
    zmq_context: zmq.Context,
):
    long_directory = tmp_path / ("security-" + "a" * 56) / ("state-" + "b" * 56)
    state = PersistentSecurityState(long_directory)
    state.initialize()
    assert len(os.fsencode(state.control_socket_path)) > 108

    probe = zmq_context.socket(zmq.ROUTER)
    port = probe.bind_to_random_port("tcp://127.0.0.1")
    probe.close(0)
    server = ZaraServer(
        supervisor=FakeSupervisor(),
        endpoint=f"tcp://127.0.0.1:{port}",
        runtime_dir=tmp_path / "runtime",
        security_state=state,
        gateway_transport_config=TransportConfig(linger_ms=0, poll_interval_ms=5),
        shutdown_timeout=1.0,
    )

    assert server.start() is ServerState.READY
    assert state.control_socket_path.parent == long_directory
    assert stat.S_ISSOCK(os.lstat(state.control_socket_path).st_mode)
    assert stat.S_IMODE(os.lstat(state.control_socket_path).st_mode) == 0o600
    assert stat.S_IMODE(os.lstat(long_directory).st_mode) == 0o700
    assert server.stop() is True
    assert not os.path.lexists(state.control_socket_path)


def test_security_registry_enrollment_and_revocation_survive_restart(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public, _secret = zmq.curve_keypair()
    principal = PrincipalContext.local_owner()

    enrolled = state.enroll_client(
        public,
        device_id="android-phone",
        principal=principal,
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    reloaded = PersistentSecurityState(tmp_path / "security").load_registry()

    assert reloaded.resolve_public_key(public).device_id == "android-phone"
    assert reloaded.resolve_public_key(public).principal == principal
    assert enrolled.public_key == public.decode("ascii")

    state.revoke_device("android-phone")
    after_revoke = PersistentSecurityState(tmp_path / "security").load_registry()
    with pytest.raises(KeyNotActive):
        after_revoke.resolve_public_key(public)


def test_owner_security_management_mutates_running_registry_without_reload(
    tmp_path: Path,
    zmq_context: zmq.Context,
    capsys,
):
    state = PersistentSecurityState(tmp_path / "security")
    server_curve = state.initialize()
    client_public, client_secret = zmq.curve_keypair()

    probe = zmq_context.socket(zmq.ROUTER)
    port = probe.bind_to_random_port("tcp://127.0.0.1")
    probe.close(0)
    endpoint = f"tcp://127.0.0.1:{port}"
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
    )
    server = ZaraServer(
        supervisor=FakeSupervisor(),
        endpoint=endpoint,
        runtime_dir=tmp_path / "runtime",
        security_state=state,
        gateway_transport_config=config,
        shutdown_timeout=1.0,
    )
    assert server.start() is ServerState.READY

    client = zmq_context.socket(zmq.DEALER)
    apply_socket_options(client, config, router=False)
    configure_curve_client_socket(
        client,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_curve.public_key,
        ),
    )
    try:
        enroll_args = server_module._parser().parse_args(
            [
                "--security-dir",
                str(state.directory),
                "--security-enroll-key",
                client_public.decode("ascii"),
                "--security-device-id",
                "android-live",
            ]
        )
        assert server_module._run_security_management(enroll_args) == 0
        capsys.readouterr()

        client.connect(endpoint)
        client.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="live-enroll-hello",
                    timestamp_ns=time.time_ns(),
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        response = receive(client)
        assert response.type == "hello.ok"
        assert response.reply_to == "live-enroll-hello"

        revoke_args = server_module._parser().parse_args(
            [
                "--security-dir",
                str(state.directory),
                "--security-revoke-device",
                "android-live",
            ]
        )
        assert server_module._run_security_management(revoke_args) == 0
        capsys.readouterr()

        client.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="revoked-ping",
                    timestamp_ns=time.time_ns(),
                    payload_count=0,
                    body={},
                )
            )
        )
        denied = receive(client)
        assert denied.type == "protocol.error"
        assert denied.reply_to is None
        assert denied.body["code"] == "authentication_required"
    finally:
        client.close(0)
        assert server.stop() is True


def test_production_zara_server_secure_tcp_accepts_only_enrolled_curve_client(
    tmp_path: Path,
    zmq_context: zmq.Context,
):
    state = PersistentSecurityState(tmp_path / "security")
    server_curve = state.initialize()
    client_public, client_secret = zmq.curve_keypair()
    state.enroll_client(
        client_public,
        device_id="android-phone",
        principal=PrincipalContext.local_owner(),
        capabilities={Capability.SESSION_BASIC},
    )

    probe = zmq_context.socket(zmq.ROUTER)
    port = probe.bind_to_random_port("tcp://127.0.0.1")
    probe.close(0)
    endpoint = f"tcp://127.0.0.1:{port}"
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
    )
    server = ZaraServer(
        supervisor=FakeSupervisor(),
        endpoint=endpoint,
        runtime_dir=tmp_path / "runtime",
        security_state=state,
        gateway_transport_config=config,
        shutdown_timeout=1.0,
    )
    assert server.start() is ServerState.READY

    enrolled = zmq_context.socket(zmq.DEALER)
    apply_socket_options(enrolled, config, router=False)
    configure_curve_client_socket(
        enrolled,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_curve.public_key,
        ),
    )
    enrolled.connect(endpoint)
    enrolled.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id="android-hello",
                timestamp_ns=time.time_ns(),
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    response = receive(enrolled)
    assert response.type == "hello.ok"
    assert response.reply_to == "android-hello"
    assert response.session_id

    unknown_public, unknown_secret = zmq.curve_keypair()
    unknown = zmq_context.socket(zmq.DEALER)
    apply_socket_options(unknown, config, router=False)
    configure_curve_client_socket(
        unknown,
        CurveClientConfig(
            public_key=unknown_public,
            secret_key=unknown_secret,
            server_public_key=server_curve.public_key,
        ),
    )
    unknown.connect(endpoint)
    unknown.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id="unknown-hello",
                timestamp_ns=time.time_ns(),
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    poller = zmq.Poller()
    poller.register(unknown, zmq.POLLIN)
    assert dict(poller.poll(250)).get(unknown) is None

    enrolled.close(0)
    unknown.close(0)
    assert server.stop() is True



def test_local_daemon_can_enable_and_restore_secure_remote_listener_without_restart(
    tmp_path: Path,
    zmq_context: zmq.Context,
):
    state = PersistentSecurityState(tmp_path / "security")
    runtime_dir = tmp_path / "runtime"

    probe = zmq_context.socket(zmq.ROUTER)
    port = probe.bind_to_random_port("tcp://127.0.0.1")
    probe.close(0)
    endpoint = f"tcp://127.0.0.1:{port}"

    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
    )
    server = ZaraServer(
        supervisor=FakeSupervisor(),
        runtime_dir=runtime_dir,
        security_state=state,
        gateway_transport_config=config,
        shutdown_timeout=1.0,
    )
    assert server.start() is ServerState.READY
    admin = SecurityAdminClient(state.control_socket_path)

    initial = admin.request("remote_listener.status")
    assert initial["active"] is False
    assert initial["endpoint"] is None

    ensured = admin.request("remote_listener.ensure", endpoint=endpoint)
    assert ensured["active"] is True
    assert ensured["endpoint"] == endpoint
    assert ensured["server_public_key"] == state.server_public_key()
    assert state.load_remote_endpoint() == endpoint

    client_public, client_secret = zmq.curve_keypair()
    admin.request(
        "enroll",
        public_key=client_public.decode("ascii"),
        device_id="android-live-pair",
    )

    remote = zmq_context.socket(zmq.DEALER)
    apply_socket_options(remote, config, router=False)
    configure_curve_client_socket(
        remote,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=state.load_server_config().public_key,
        ),
    )
    remote.connect(endpoint)
    remote.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id="dynamic-remote-hello",
                timestamp_ns=time.time_ns(),
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    response = receive(remote)
    assert response.type == "hello.ok"
    remote.close(0)
    assert server.stop() is True

    restarted = ZaraServer(
        supervisor=FakeSupervisor(),
        runtime_dir=runtime_dir,
        security_state=state,
        gateway_transport_config=config,
        shutdown_timeout=1.0,
    )
    assert restarted.start() is ServerState.READY
    try:
        restored = SecurityAdminClient(state.control_socket_path).request("remote_listener.status")
        assert restored["active"] is True
        assert restored["endpoint"] == endpoint
        assert restored["server_public_key"] == ensured["server_public_key"]
    finally:
        assert restarted.stop() is True
