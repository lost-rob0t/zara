from __future__ import annotations

import socket as net_socket

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig, configure_curve_client_socket
from zara.server import ServerState
from zara.zmq_transport import TransportConfig, apply_socket_options


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
        context.term()


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _receive(dealer: zmq.Socket):
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(1500)).get(dealer) == zmq.POLLIN
    return decode_message(dealer.recv_multipart()).message


def test_authenticated_recursive_json_attack_does_not_kill_gateway(zmq_context):
    endpoint = _endpoint()
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
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
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:recursive", kind="authenticated"),
        device_id="recursive-client",
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
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="recursive-hello",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = _receive(dealer)
        assert hello.type == "hello.ok"

        deep_envelope = ("{\"x\":" * 1500 + "0" + "}" * 1500).encode("ascii")
        assert len(deep_envelope) < 64 * 1024
        dealer.send_multipart([b"ZARA/1", deep_envelope])
        denied = _receive(dealer)
        assert denied.type == "protocol.error"
        assert denied.body["code"] == "invalid_message"

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="after-recursive-json",
                    session_id=hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                )
            )
        )
        pong = _receive(dealer)
        assert pong.type == "pong"
        assert gateway.is_alive
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
