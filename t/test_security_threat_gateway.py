from __future__ import annotations

import concurrent.futures
import socket as net_socket

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt
from zara.security import Capability, SecurityLimits, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig, configure_curve_client_socket
from zara.server import ServerState
from zara.zmq_transport import TransportConfig, apply_socket_options


class _Supervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands: list[tuple[PrincipalContext, object]] = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(CommandReceipt(request_id=command.request_id, turn_id=f"turn-{command.request_id}"))
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
        sndhwm=16,
        rcvhwm=16,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=50,
        heartbeat_timeout_ms=250,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
        event_queue_size=16,
        pending_request_limit=16,
        idempotency_cache_size=32,
    )


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    return f"tcp://127.0.0.1:{port}"


def _receive(dealer: zmq.Socket, *, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) == zmq.POLLIN
    return decode_message(dealer.recv_multipart()).message


def _hello(dealer: zmq.Socket, message_id: str) -> ProtocolMessage:
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
    return _receive(dealer)


def _dealer(
    context: zmq.Context,
    endpoint: str,
    config: TransportConfig,
    *,
    public: str,
    secret: str,
    server_public: str,
    routing_id: bytes | None = None,
) -> zmq.Socket:
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    if routing_id is not None:
        dealer.setsockopt(zmq.ROUTING_ID, routing_id)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=public,
            secret_key=secret,
            server_public_key=server_public,
        ),
    )
    dealer.connect(endpoint)
    return dealer


def _gateway(
    context: zmq.Context,
    endpoint: str,
    config: TransportConfig,
    *,
    registry: SecurityRegistry,
    server_public: str,
    server_secret: str,
    supervisor: _Supervisor | None = None,
    limits: SecurityLimits | None = None,
) -> tuple[SecureZaraZmqGateway, _Supervisor]:
    actual_supervisor = supervisor or _Supervisor()
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=actual_supervisor,
        context=context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(public_key=server_public, secret_key=server_secret),
        security_limits=limits or SecurityLimits(),
    )
    gateway.start().result(timeout=1.0)
    return gateway, actual_supervisor


def _send_turn(dealer: zmq.Socket, *, session_id: str, request_id: str, text: str) -> ProtocolMessage:
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="turn.submit",
                id=request_id,
                session_id=session_id,
                timestamp_ns=2,
                payload_count=0,
                body={"text": text, "context_ids": []},
            )
        )
    )
    return _receive(dealer)


def test_wrong_server_pin_never_reaches_application_handshake(zmq_context, transport_config):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    wrong_server_public, _wrong_server_secret = _keypair()
    client_public, client_secret = _keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway, _supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    dealer = _dealer(
        zmq_context,
        endpoint,
        transport_config,
        public=client_public,
        secret=client_secret,
        server_public=wrong_server_public,
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


def test_unknown_curve_client_storm_does_not_starve_enrolled_client(zmq_context, transport_config):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    valid_public, valid_secret = _keypair()
    registry = SecurityRegistry()
    registry.enroll(
        valid_public,
        principal=PrincipalContext("user:valid", kind="authenticated"),
        device_id="valid-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway, _supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    attackers: list[zmq.Socket] = []
    valid = None
    try:
        for index in range(16):
            public, secret = _keypair()
            attacker = _dealer(
                zmq_context,
                endpoint,
                transport_config,
                public=public,
                secret=secret,
                server_public=server_public,
            )
            attackers.append(attacker)
            attacker.send_multipart(
                encode_message(
                    ProtocolMessage(
                        type="hello",
                        id=f"unknown-{index}",
                        timestamp_ns=1,
                        payload_count=0,
                        body={"versions": [1]},
                    )
                )
            )

        valid = _dealer(
            zmq_context,
            endpoint,
            transport_config,
            public=valid_public,
            secret=valid_secret,
            server_public=server_public,
        )
        assert _hello(valid, "valid-after-storm").type == "hello.ok"
    finally:
        if valid is not None:
            valid.close(0)
        for attacker in attackers:
            attacker.close(0)
        gateway.close(timeout=1.0)


def test_router_identity_handover_cannot_transfer_authenticated_session(zmq_context, transport_config):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    alice_public, alice_secret = _keypair()
    bob_public, bob_secret = _keypair()
    registry = SecurityRegistry()
    registry.enroll(
        alice_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    registry.enroll(
        bob_public,
        principal=PrincipalContext("user:bob", kind="authenticated"),
        device_id="bob-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway, _supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    routing_id = b"shared-route"
    alice = _dealer(
        zmq_context,
        endpoint,
        transport_config,
        public=alice_public,
        secret=alice_secret,
        server_public=server_public,
        routing_id=routing_id,
    )
    bob = None
    try:
        alice_hello = _hello(alice, "alice-hello")
        assert alice_hello.type == "hello.ok"

        bob = _dealer(
            zmq_context,
            endpoint,
            transport_config,
            public=bob_public,
            secret=bob_secret,
            server_public=server_public,
            routing_id=routing_id,
        )
        bob.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="bob-stolen-route-ping",
                    session_id=alice_hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                )
            )
        )
        denied = _receive(bob)
        assert denied.type == "protocol.error"
        assert denied.body["code"] == "authentication_required"

        bob_hello = _hello(bob, "bob-fresh-hello")
        assert bob_hello.type == "hello.ok"
        assert bob_hello.session_id != alice_hello.session_id
    finally:
        alice.close(0)
        if bob is not None:
            bob.close(0)
        gateway.close(timeout=1.0)


def test_same_request_id_is_isolated_by_authenticated_principal(zmq_context, transport_config):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    registry = SecurityRegistry()
    identities = []
    for name in ("alice", "bob"):
        public, secret = _keypair()
        principal = PrincipalContext(f"user:{name}", kind="authenticated")
        registry.enroll(
            public,
            principal=principal,
            device_id=f"{name}-phone",
            capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
        )
        identities.append((principal, public, secret))
    gateway, supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    dealers = [
        _dealer(
            zmq_context,
            endpoint,
            transport_config,
            public=public,
            secret=secret,
            server_public=server_public,
        )
        for _principal, public, secret in identities
    ]
    try:
        hellos = [_hello(dealer, f"hello-{index}") for index, dealer in enumerate(dealers)]
        responses = [
            _send_turn(
                dealer,
                session_id=hello.session_id,
                request_id="shared-request-id",
                text=f"payload-{index}",
            )
            for index, (dealer, hello) in enumerate(zip(dealers, hellos))
        ]
        assert all(response.type == "turn.accepted" for response in responses)
        assert [principal for principal, _command in supervisor.commands] == [
            identities[0][0],
            identities[1][0],
        ]
        assert [command.request_id for _principal, command in supervisor.commands] == [
            "shared-request-id",
            "shared-request-id",
        ]
    finally:
        for dealer in dealers:
            dealer.close(0)
        gateway.close(timeout=1.0)


def test_conflicting_replay_same_principal_request_id_fails_closed(zmq_context, transport_config):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    registry = SecurityRegistry()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry.enroll(
        client_public,
        principal=principal,
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    gateway, supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    dealer = _dealer(
        zmq_context,
        endpoint,
        transport_config,
        public=client_public,
        secret=client_secret,
        server_public=server_public,
    )
    try:
        hello = _hello(dealer, "replay-hello")
        first = _send_turn(
            dealer,
            session_id=hello.session_id,
            request_id="replay-id",
            text="first",
        )
        assert first.type == "turn.accepted"
        conflict = _send_turn(
            dealer,
            session_id=hello.session_id,
            request_id="replay-id",
            text="changed",
        )
        assert conflict.type == "protocol.error"
        assert conflict.body["code"] == "idempotency_conflict"
        assert len(supervisor.commands) == 1
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_malformed_authenticated_messages_consume_rate_budget_without_dispatch(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway, supervisor = _gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        limits=SecurityLimits(requests_per_window=2, request_window_seconds=60.0),
    )
    dealer = _dealer(
        zmq_context,
        endpoint,
        transport_config,
        public=client_public,
        secret=client_secret,
        server_public=server_public,
    )
    try:
        hello = _hello(dealer, "quota-hello")
        assert hello.type == "hello.ok"
        dealer.send_multipart([b"ZARA/1", b"{"])
        malformed = _receive(dealer)
        assert malformed.type == "protocol.error"
        assert malformed.body["code"] == "invalid_message"
        assert supervisor.commands == []

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="post-malformed-ping",
                    session_id=hello.session_id,
                    timestamp_ns=3,
                    payload_count=0,
                )
            )
        )
        limited = _receive(dealer)
        assert limited.type == "protocol.error"
        assert limited.body["code"] == "quota_exceeded"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
