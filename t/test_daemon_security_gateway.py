from __future__ import annotations

import concurrent.futures
import socket as net_socket

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
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


def send_hello(socket: zmq.Socket, message_id: str = "hello-secure") -> ProtocolMessage:
    socket.send_multipart(
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
    return receive_message(socket)


def secure_dealer(
    context: zmq.Context,
    endpoint: str,
    transport_config: TransportConfig,
    *,
    client_public: str,
    client_secret: str,
    server_public: str,
) -> zmq.Socket:
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
    )
    dealer.connect(endpoint)
    return dealer


def make_secure_gateway(
    context: zmq.Context,
    endpoint: str,
    transport_config: TransportConfig,
    *,
    supervisor: FakeSupervisor,
    registry: SecurityRegistry,
    server_public: str,
    server_secret: str,
    limits: SecurityLimits | None = None,
) -> SecureZaraZmqGateway:
    return SecureZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        context=context,
        config=transport_config,
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        security_limits=limits or SecurityLimits(),
    )


def test_authenticated_gateway_dispatches_with_registry_principal_not_static_fallback(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=principal,
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello = send_hello(dealer)
        assert hello.type == "hello.ok"

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="turn.submit",
                    id="secure-submit",
                    session_id=hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "hello", "context_ids": []},
                )
            )
        )
        accepted = receive_message(dealer)
        assert accepted.type == "turn.accepted"
        assert supervisor.commands == [
            (principal, SubmitTurn(request_id="secure-submit", text="hello"))
        ]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_authenticated_gateway_denies_missing_capability_before_supervisor_dispatch(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:limited", kind="authenticated"),
        device_id="limited-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello = send_hello(dealer)
        assert hello.type == "hello.ok"
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="turn.submit",
                    id="denied-submit",
                    session_id=hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "nope", "context_ids": []},
                )
            )
        )
        denied = receive_message(dealer)
        assert denied.type == "protocol.error"
        assert denied.body == {
            "code": "authorization_denied",
            "message": "request is not authorized",
            "retryable": False,
        }
        assert supervisor.commands == []
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_live_revocation_denies_next_request_without_gateway_restart(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello = send_hello(dealer)
        assert hello.type == "hello.ok"
        registry.revoke("alice-phone")

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="post-revoke",
                    session_id=hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                )
            )
        )
        denied = receive_message(dealer)
        assert denied.type == "protocol.error"
        assert denied.body == {
            "code": "authentication_required",
            "message": "authentication required",
            "retryable": False,
        }
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_connection_quota_is_per_authenticated_principal_not_global(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    alice_one_public, alice_one_secret = keypair()
    alice_two_public, alice_two_secret = keypair()
    bob_public, bob_secret = keypair()
    registry = SecurityRegistry()
    alice = PrincipalContext("user:alice", kind="authenticated")
    bob = PrincipalContext("user:bob", kind="authenticated")
    for public, principal, device in (
        (alice_one_public, alice, "alice-one"),
        (alice_two_public, alice, "alice-two"),
        (bob_public, bob, "bob-one"),
    ):
        registry.enroll(
            public,
            principal=principal,
            device_id=device,
            capabilities={Capability.SESSION_BASIC},
        )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        limits=SecurityLimits(max_connections=1),
    )
    gateway.start().result(timeout=1.0)
    dealers = [
        secure_dealer(
            zmq_context,
            endpoint,
            transport_config,
            client_public=public,
            client_secret=secret,
            server_public=server_public,
        )
        for public, secret in (
            (alice_one_public, alice_one_secret),
            (alice_two_public, alice_two_secret),
            (bob_public, bob_secret),
        )
    ]
    try:
        assert send_hello(dealers[0], "alice-one-hello").type == "hello.ok"
        alice_two = send_hello(dealers[1], "alice-two-hello")
        assert alice_two.type == "protocol.error"
        assert alice_two.body["code"] == "quota_exceeded"
        assert send_hello(dealers[2], "bob-hello").type == "hello.ok"
    finally:
        for dealer in dealers:
            dealer.close(0)
        gateway.close(timeout=1.0)
