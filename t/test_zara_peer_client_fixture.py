from __future__ import annotations

import socket as net_socket
import subprocess
import sys
from pathlib import Path

import pytest
import zmq

from zara.protocol import ProtocolMessage, encode_message
from zara.runtime import bridge
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveServerConfig
from zara.server import PrincipalContext
from zara.zmq_transport import TransportConfig

FIXTURE = Path(__file__).resolve().parents[1] / "android" / "integration" / "zara_peer_client_fixture.py"


class _Supervisor:
    def __init__(self) -> None:
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, _principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


@pytest.fixture
def transport_config():
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
        event_queue_size=8,
        pending_request_limit=8,
    )


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    return f"tcp://127.0.0.1:{port}"


def _start_gateway(context: zmq.Context, transport_config: TransportConfig):
    endpoint = _tcp_endpoint()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        client_public,
        principal=PrincipalContext("user:fixture", kind="authenticated"),
        device_id="fixture-desktop",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=_Supervisor(),
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        context=context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    return gateway, endpoint, server_public, client_public, client_secret, enrolled


def _run_fixture(*extra: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            sys.executable,
            str(FIXTURE),
            *extra,
        ],
        capture_output=True,
        text=True,
        timeout=20,
        check=False,
    )


def test_fixture_client_completes_authenticated_hello_against_stock_gateway(
    zmq_context,
    transport_config,
):
    gateway, endpoint, server_public, client_public, client_secret, enrolled = _start_gateway(
        zmq_context,
        transport_config,
    )
    try:
        result = _run_fixture(
            "--endpoint",
            endpoint,
            "--server-key",
            server_public,
            "--client-key",
            client_public,
            "--client-secret",
            client_secret,
            "--node-id",
            enrolled.device_id,
            "--generation",
            str(enrolled.generation),
        )

        assert result.returncode == 0, result.stderr
        assert result.stdout.strip().startswith("RESULT type=hello.ok session_id=")
    finally:
        gateway.close(timeout=1.0)


def test_fixture_client_reports_node_authority_mismatch(
    zmq_context,
    transport_config,
):
    gateway, endpoint, server_public, client_public, client_secret, enrolled = _start_gateway(
        zmq_context,
        transport_config,
    )
    try:
        result = _run_fixture(
            "--endpoint",
            endpoint,
            "--server-key",
            server_public,
            "--client-key",
            client_public,
            "--client-secret",
            client_secret,
            "--node-id",
            "impostor-node",
            "--generation",
            str(enrolled.generation),
        )

        assert result.returncode == 0, result.stderr
        assert (
            "RESULT type=protocol.error code=node_authority_mismatch retryable=False"
            in result.stdout
        )
    finally:
        gateway.close(timeout=1.0)


def test_fixture_client_reports_timeout_for_unenrolled_key(
    zmq_context,
    transport_config,
):
    gateway, endpoint, server_public, _enrolled_public, _enrolled_secret, _enrolled = _start_gateway(
        zmq_context,
        transport_config,
    )
    stranger_public, stranger_secret = _keypair()
    try:
        result = _run_fixture(
            "--endpoint",
            endpoint,
            "--server-key",
            server_public,
            "--client-key",
            stranger_public,
            "--client-secret",
            stranger_secret,
            "--timeout-ms",
            "800",
        )

        assert result.returncode == 1, result.stdout
        assert "RESULT timeout" in result.stdout
    finally:
        gateway.close(timeout=1.0)
