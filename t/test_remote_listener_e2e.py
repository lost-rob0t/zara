from __future__ import annotations

import concurrent.futures
import socket as net_socket
from pathlib import Path

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime.host import RuntimeHostState
from zara.security_admin import SecurityAdminClient
from zara.security_transport import CurveClientConfig, configure_curve_client_socket
from zara.server import RuntimeSupervisor, ServerLease, ServerState, ZaraServer
from zara.zmq_transport import TransportConfig, apply_socket_options


def _completed(value=None):
    future: concurrent.futures.Future[object] = concurrent.futures.Future()
    future.set_result(value)
    return future


class _FakeHost:
    def __init__(self) -> None:
        self.state = RuntimeHostState.NEW
        self.is_alive = False

    def start(self):
        self.state = RuntimeHostState.RUNNING
        self.is_alive = True
        return _completed(None)

    def submit(self, command):
        return _completed(command)

    def shutdown(self, reason=""):
        self.state = RuntimeHostState.STOPPED
        self.is_alive = False
        return _completed(reason)

    def join(self, timeout=None):
        del timeout


class _LocalGateway:
    def __init__(self) -> None:
        self.started = False
        self.closed = False

    def start(self):
        self.started = True
        return _completed(True)

    def close(self, *, timeout: float) -> None:
        assert timeout > 0
        self.closed = True


def _local_gateway_factory(endpoint, *, supervisor, principal):
    assert endpoint.startswith("ipc://")
    assert supervisor is not None
    assert principal is not None
    return _LocalGateway()


def _ephemeral_tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = int(probe.getsockname()[1])
    return f"tcp://127.0.0.1:{port}"


def _server(runtime_dir: Path) -> ZaraServer:
    supervisor = RuntimeSupervisor(
        host_factory=lambda _principal, _bus: _FakeHost(),
        shutdown_timeout=1.0,
    )
    return ZaraServer(
        supervisor=supervisor,
        lease=ServerLease(runtime_dir),
        gateway_factory=_local_gateway_factory,
        shutdown_timeout=1.0,
    )


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _authenticated_hello(
    *,
    endpoint: str,
    client_public: str,
    client_secret: str,
    server_public: str,
) -> ProtocolMessage:
    context = zmq.Context()
    dealer = context.socket(zmq.DEALER)
    config = TransportConfig(request_timeout=1.5, poll_interval_ms=5)
    apply_socket_options(dealer, config, router=False)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
    )
    dealer.connect(endpoint)
    try:
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="bootstrap-e2e-hello",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        poller = zmq.Poller()
        poller.register(dealer, zmq.POLLIN)
        assert dict(poller.poll(1500)).get(dealer) == zmq.POLLIN
        return decode_message(dealer.recv_multipart()).message
    finally:
        dealer.close(0)
        context.term()


def test_running_server_bootstraps_authenticated_listener_and_preserves_identity_on_restart(
    monkeypatch,
    tmp_path: Path,
):
    runtime_dir = tmp_path / "runtime"
    state_home = tmp_path / "state"
    security_dir = state_home / "zarathushtra" / "security"
    monkeypatch.setenv("XDG_STATE_HOME", str(state_home))
    monkeypatch.delenv("ZARA_SECURITY_DIR", raising=False)

    first_endpoint = _ephemeral_tcp_endpoint()
    monkeypatch.setattr("zara.server._DEFAULT_REMOTE_ENDPOINT", first_endpoint)
    first = _server(runtime_dir)
    assert first.start() is ServerState.READY

    server_public = None
    client_public = None
    client_secret = None
    try:
        control_path = runtime_dir / "zara-control.sock"
        control = SecurityAdminClient(control_path)
        assert not security_dir.exists()
        assert control.request("remote_listener.status") == {
            "active": False,
            "endpoint": None,
            "server_public_key": None,
        }
        assert not security_dir.exists()

        first_metadata = control.request("remote_listener.ensure")
        assert security_dir.is_dir()
        assert first_metadata["active"] is True
        assert first_metadata["endpoint"] == first_endpoint
        server_public = first_metadata["server_public_key"]
        assert isinstance(server_public, str) and len(server_public) == 40
        assert control.request("remote_listener.ensure") == first_metadata

        client_public, client_secret = _keypair()
        enrolled = control.request(
            "enroll",
            public_key=client_public,
            device_id="bootstrap-e2e-phone",
        )
        assert enrolled["active"] is True
        assert enrolled["public_key"] == client_public

        hello = _authenticated_hello(
            endpoint=first_endpoint,
            client_public=client_public,
            client_secret=client_secret,
            server_public=server_public,
        )
        assert hello.type == "hello.ok"
        assert isinstance(hello.session_id, str) and hello.session_id
    finally:
        assert first.stop()

    assert isinstance(server_public, str)
    assert isinstance(client_public, str)
    assert isinstance(client_secret, str)

    second_endpoint = _ephemeral_tcp_endpoint()
    monkeypatch.setattr("zara.server._DEFAULT_REMOTE_ENDPOINT", second_endpoint)
    second = _server(runtime_dir)
    assert second.start() is ServerState.READY
    try:
        control = SecurityAdminClient(runtime_dir / "zara-control.sock")
        second_metadata = control.request("remote_listener.ensure")
        assert second_metadata["active"] is True
        assert second_metadata["endpoint"] == second_endpoint
        assert second_metadata["server_public_key"] == server_public

        clients = control.request("list")
        assert len(clients) == 1
        assert clients[0]["device_id"] == "bootstrap-e2e-phone"
        assert clients[0]["public_key"] == client_public
        assert clients[0]["active"] is True

        hello_after_restart = _authenticated_hello(
            endpoint=second_endpoint,
            client_public=client_public,
            client_secret=client_secret,
            server_public=server_public,
        )
        assert hello_after_restart.type == "hello.ok"
        assert isinstance(hello_after_restart.session_id, str) and hello_after_restart.session_id
    finally:
        assert second.stop()
