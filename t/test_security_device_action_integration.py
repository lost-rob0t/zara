from __future__ import annotations

import socket as net_socket
import time

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig, configure_curve_client_socket
from zara.server import ServerState
from zara.zmq_transport import DeviceActionResult, TransportConfig, apply_socket_options


class _Supervisor:
    state = ServerState.READY

    def __init__(self) -> None:
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.destroy(linger=0)


def _endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _receive(dealer: zmq.Socket, *, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) == zmq.POLLIN
    return decode_message(dealer.recv_multipart()).message


def _send(dealer: zmq.Socket, message: ProtocolMessage) -> None:
    dealer.send_multipart(encode_message(message))


def test_authenticated_device_action_lifecycle_remains_session_basic_after_reconcile(zmq_context):
    endpoint = _endpoint()
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        heartbeat_interval_ms=50,
        heartbeat_timeout_ms=250,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
        event_queue_size=8,
        pending_request_limit=8,
    )
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    principal = PrincipalContext("user:android", kind="authenticated")
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=principal,
        device_id="android-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=_Supervisor(),
        context=zmq_context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(public_key=server_public, secret_key=server_secret),
    )
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
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
        _send(
            dealer,
            ProtocolMessage(
                type="hello",
                id="secure-device-hello",
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            ),
        )
        hello = _receive(dealer)
        assert hello.type == "hello.ok"
        assert hello.session_id

        _send(
            dealer,
            ProtocolMessage(
                type="capability.snapshot",
                id="secure-device-caps",
                session_id=hello.session_id,
                timestamp_ns=2,
                payload_count=0,
                body={"capabilities": [{"id": "open_uri", "version": 1}]},
            ),
        )
        advertised = _receive(dealer)
        assert advertised.type == "capability.snapshot.ok"
        assert gateway.capabilities_for(principal.principal_id, hello.session_id) == frozenset(
            {"open_uri"}
        )

        action = gateway.request_device_action(
            principal_id=principal.principal_id,
            session_id=hello.session_id,
            capability="open_uri",
            args={"uri": "https://example.com"},
            deadline_ns=time.time_ns() + 1_000_000_000,
            idempotency="at_most_once",
            trace_id="secure-device-trace",
        )
        request = _receive(dealer)
        assert request.type == "device.action.request"
        action_id = request.body["action_id"]

        _send(
            dealer,
            ProtocolMessage(
                type="device.action.accepted",
                id="secure-device-accepted",
                session_id=hello.session_id,
                timestamp_ns=3,
                payload_count=0,
                body={"action_id": action_id},
            ),
        )
        _send(
            dealer,
            ProtocolMessage(
                type="device.action.result",
                id="secure-device-result",
                session_id=hello.session_id,
                timestamp_ns=4,
                payload_count=0,
                body={"action_id": action_id, "outcome": "completed"},
            ),
        )
        assert action.result(timeout=1.0) == DeviceActionResult(
            action_id=action_id,
            capability="open_uri",
            outcome="completed",
        )
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
