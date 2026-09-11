from __future__ import annotations

import concurrent.futures
import socket
import time

import pytest
import zmq

from zara import daemon_client
from zara.client import ZaraClientState
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import (
    ClientDisconnected,
    TransportConfig,
    ZaraZmqGateway,
    ZmqZaraClient,
)


class FakeConfig:
    def __init__(self, daemon: dict | None = None) -> None:
        self.daemon = daemon or {}

    def get_section(self, name: str):
        if name == "daemon":
            return dict(self.daemon)
        return {}


class StubSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        future.set_exception(AssertionError(f"unexpected command: {command!r}"))
        return future

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def clear_daemon_env(monkeypatch) -> None:
    for name in (
        daemon_client.DAEMON_ENDPOINT_ENV,
        daemon_client.CURVE_PUBLIC_KEY_ENV,
        daemon_client.CURVE_SECRET_KEY_ENV,
        daemon_client.CURVE_SERVER_PUBLIC_KEY_ENV,
    ):
        monkeypatch.delenv(name, raising=False)


def unique_inproc_endpoint(prefix: str) -> str:
    return f"inproc://{prefix}-{time.time_ns()}"


def closed_tcp_endpoint() -> str:
    probe = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    finally:
        probe.close()
    return f"tcp://127.0.0.1:{port}"


def test_resolved_endpoint_is_used_verbatim_when_client_starts(monkeypatch, zmq_context):
    clear_daemon_env(monkeypatch)
    monkeypatch.setenv(daemon_client.DAEMON_ENDPOINT_ENV, "tcp://10.0.0.8:7731")
    endpoint = unique_inproc_endpoint("resolution-verbatim")
    config = FakeConfig({"endpoint": "tcp://127.0.0.1:7731"})

    resolved = daemon_client.resolve_daemon_endpoint(config, explicit=endpoint)
    gateway = ZaraZmqGateway(
        resolved,
        supervisor=StubSupervisor(),
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
    )
    gateway.start().result(timeout=2.0)

    client = daemon_client.create_daemon_client(endpoint, config=config, context=zmq_context)
    try:
        assert client._endpoint == resolved
        assert resolved == endpoint
        client.start().result(timeout=2.0)
        assert client.state is ZaraClientState.READY
    finally:
        client.close()
        gateway.close(timeout=2.0)

    assert client._endpoint == resolved
    assert client._endpoint != "tcp://10.0.0.8:7731"


def test_environment_endpoint_overrides_config_file(monkeypatch, zmq_context):
    clear_daemon_env(monkeypatch)
    monkeypatch.setenv(daemon_client.DAEMON_ENDPOINT_ENV, "tcp://10.0.0.8:7731")
    config = FakeConfig({"endpoint": "tcp://127.0.0.1:7731"})

    client = daemon_client.create_daemon_client(config=config, context=zmq_context)

    assert client._endpoint == "tcp://10.0.0.8:7731"
    assert client._endpoint != "tcp://127.0.0.1:7731"


def test_missing_listener_fails_bounded(zmq_context):
    request_timeout = 1.0
    client = ZmqZaraClient(
        closed_tcp_endpoint(),
        context=zmq_context,
        config=TransportConfig(
            sndhwm=8,
            rcvhwm=8,
            request_timeout=request_timeout,
            poll_interval_ms=5,
        ),
    )
    started = time.monotonic()
    try:
        future = client.start()
        with pytest.raises(ClientDisconnected, match="handshake timed out"):
            future.result(timeout=request_timeout * 2 + 1.0)
        elapsed = time.monotonic() - started
    finally:
        client.close(timeout=request_timeout * 2 + 1.0)

    assert elapsed < request_timeout * 2 + 1.0
    assert elapsed >= request_timeout
    assert client.state is ZaraClientState.STOPPED
    assert not client.is_alive


def test_partial_curve_credentials_fail_closed_before_connecting(monkeypatch):
    clear_daemon_env(monkeypatch)
    monkeypatch.setenv(daemon_client.CURVE_SECRET_KEY_ENV, "env-secret-only")

    constructed = []

    class GuardClient:
        def __init__(self, endpoint, **kwargs) -> None:
            constructed.append(endpoint)

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", GuardClient)

    with pytest.raises(ValueError, match="requires public, secret, and server public keys"):
        daemon_client.curve_client_config(FakeConfig())

    with pytest.raises(ValueError, match="requires public, secret, and server public keys"):
        daemon_client.create_daemon_client("tcp://127.0.0.1:7731", config=FakeConfig())

    assert constructed == []


def test_daemon_client_uses_configured_endpoint_object_identity(monkeypatch):
    clear_daemon_env(monkeypatch)
    endpoint = unique_inproc_endpoint("daemon-client-identity")
    config = FakeConfig({"endpoint": "tcp://127.0.0.1:7731"})
    resolved = daemon_client.resolve_daemon_endpoint(config, explicit=endpoint)

    constructed = []

    class RecordingClient:
        def __init__(self, received_endpoint, **kwargs) -> None:
            constructed.append((received_endpoint, kwargs))

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", RecordingClient)

    client = daemon_client.create_daemon_client(endpoint, config=config)

    assert isinstance(client, RecordingClient)
    assert constructed == [(resolved, {})]
    assert constructed[0][0] == endpoint
