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
from zara.security_state import PersistentSecurityState
from zara.security_transport import CurveClientConfig, configure_curve_client_socket
from zara.server import ServerState, ZaraServer
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
