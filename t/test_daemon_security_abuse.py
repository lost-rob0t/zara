from __future__ import annotations

import concurrent.futures
import socket as net_socket

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt
from zara.security import Capability, SecurityLimits, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import (
    CurveClientConfig,
    CurveServerConfig,
    configure_curve_client_socket,
)
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, apply_socket_options


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands: list[tuple[PrincipalContext, object]] = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(CommandReceipt(request_id=command.request_id, turn_id="turn-secure"))
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


def keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    return f"tcp://127.0.0.1:{port}"


def receive_message(socket: zmq.Socket, *, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def secure_dealer(
    context: zmq.Context,
    endpoint: str,
    config: TransportConfig,
    *,
    public_key: str,
    secret_key: str,
    server_public_key: str,
) -> zmq.Socket:
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=public_key,
            secret_key=secret_key,
            server_public_key=server_public_key,
        ),
    )
    dealer.connect(endpoint)
    return dealer


def make_gateway(
    context: zmq.Context,
    endpoint: str,
    config: TransportConfig,
    *,
    registry: SecurityRegistry,
    server_public: str,
    server_secret: str,
    limits: SecurityLimits,
) -> SecureZaraZmqGateway:
    return SecureZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        context=context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        security_limits=limits,
    )


def send_hello(dealer: zmq.Socket, message_id: str) -> ProtocolMessage:
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id=message_id,
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    return receive_message(dealer)


def test_authenticated_malformed_requests_consume_principal_rate_budget_without_starving_peer(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    alice_public, alice_secret = keypair()
    bob_public, bob_secret = keypair()
    registry = SecurityRegistry()
    for public_key, principal_id, device_id in (
        (alice_public, "user:alice", "alice-phone"),
        (bob_public, "user:bob", "bob-phone"),
    ):
        registry.enroll(
            public_key,
            principal=PrincipalContext(principal_id, kind="authenticated"),
            device_id=device_id,
            capabilities={Capability.SESSION_BASIC},
        )

    gateway = make_gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        limits=SecurityLimits(requests_per_window=2, request_window_seconds=60.0),
    )
    gateway.start().result(timeout=1.0)
    alice = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        public_key=alice_public,
        secret_key=alice_secret,
        server_public_key=server_public,
    )
    bob = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        public_key=bob_public,
        secret_key=bob_secret,
        server_public_key=server_public,
    )
    try:
        alice_hello = send_hello(alice, "alice-hello")
        assert alice_hello.type == "hello.ok"

        alice.send_multipart([b"not-zara-json"])
        first = receive_message(alice)
        assert first.type == "protocol.error"
        assert first.body["code"] == "invalid_message"

        alice.send_multipart([b"still-not-zara-json"])
        limited = receive_message(alice)
        assert limited.type == "protocol.error"
        assert limited.body["code"] == "quota_exceeded"

        bob_hello = send_hello(bob, "bob-hello")
        assert bob_hello.type == "hello.ok"
    finally:
        alice.close(0)
        bob.close(0)
        gateway.close(timeout=1.0)


def test_wrong_server_pin_cannot_complete_authenticated_handshake(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    wrong_server_public, _ = keypair()
    client_public, client_secret = keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway = make_gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        limits=SecurityLimits(),
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        public_key=client_public,
        secret_key=client_secret,
        server_public_key=wrong_server_public,
    )
    try:
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="wrong-pin",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        poller = zmq.Poller()
        poller.register(dealer, zmq.POLLIN)
        assert dict(poller.poll(300)).get(dealer) is None
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
