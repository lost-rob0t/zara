from __future__ import annotations

import socket as net_socket

import pytest
import zmq

from zara.node import ZaraNode
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import (
    CurveClientConfig,
    CurveServerConfig,
    configure_curve_client_socket,
)
from zara.server import PrincipalContext
from zara.zmq_transport import TransportConfig, apply_socket_options


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


def _receive(socket: zmq.Socket, *, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def _send_hello(socket: zmq.Socket, body: dict[str, object], message_id: str) -> ProtocolMessage:
    socket.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id=message_id,
                timestamp_ns=1,
                payload_count=0,
                body=body,
            )
        )
    )
    return _receive(socket)


def _node_mapping(enrolled, *, capabilities: list[str] | None = None) -> dict[str, object]:
    return {
        "node_id": enrolled.device_id,
        "display_name": "Alice phone",
        "device_class": "android",
        "curve_public_key": enrolled.public_key,
        "endpoints": ["tcp://127.0.0.1:17865"],
        "capabilities": ["open_uri"] if capabilities is None else capabilities,
        "protocol_versions": ["ZARA/1"],
        "last_seen": 1,
        "enrollment_generation": enrolled.generation,
    }


def _start_peer(
    context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = _tcp_endpoint()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        client_public,
        principal=principal,
        device_id="alice-phone",
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
    return gateway, dealer, principal, enrolled, registry


def test_secure_hello_binds_node_to_authenticated_principal_and_session(
    zmq_context,
    transport_config,
):
    gateway, dealer, principal, enrolled, _registry = _start_peer(
        zmq_context,
        transport_config,
    )
    node_mapping = _node_mapping(enrolled)
    expected_node = ZaraNode.from_mapping(node_mapping)
    published_before_ack: list[ZaraNode | None] = []
    original_send = gateway._send

    def send_with_publication_assertion(
        socket,
        route,
        message,
        payloads=(),
        *,
        queue_on_again=True,
    ):
        if message.type == "hello.ok":
            published_before_ack.append(
                gateway.node_for_session(principal.principal_id, message.session_id)
            )
        return original_send(
            socket,
            route,
            message,
            payloads,
            queue_on_again=queue_on_again,
        )

    gateway._send = send_with_publication_assertion
    try:
        hello = _send_hello(
            dealer,
            {"versions": [1], "node": node_mapping},
            "peer-hello",
        )
        assert hello.type == "hello.ok"
        assert published_before_ack == [expected_node]
        assert gateway.node_for_session(
            principal.principal_id,
            hello.session_id,
        ) == expected_node

        legacy = _send_hello(dealer, {"versions": [1]}, "legacy-reset")
        assert legacy.type == "hello.ok"
        assert published_before_ack == [expected_node, None]
        assert gateway.node_for_session(principal.principal_id, hello.session_id) is None
        assert gateway.node_for_session(principal.principal_id, legacy.session_id) is None
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_secure_hello_rejects_node_identity_or_generation_mismatch(
    zmq_context,
    transport_config,
):
    gateway, dealer, _principal, enrolled, _registry = _start_peer(
        zmq_context,
        transport_config,
    )
    try:
        wrong_node = _node_mapping(enrolled)
        wrong_node["node_id"] = "different-device"
        denied = _send_hello(
            dealer,
            {"versions": [1], "node": wrong_node},
            "wrong-node",
        )
        assert denied.type == "protocol.error"
        assert denied.body == {
            "code": "node_authority_mismatch",
            "message": "peer node does not match authenticated identity",
            "retryable": False,
        }

        stale = _node_mapping(enrolled)
        stale["enrollment_generation"] = enrolled.generation + 1
        denied = _send_hello(
            dealer,
            {"versions": [1], "node": stale},
            "stale-node",
        )
        assert denied.type == "protocol.error"
        assert denied.body["code"] == "node_authority_mismatch"

        accepted = _send_hello(
            dealer,
            {"versions": [1], "node": _node_mapping(enrolled)},
            "good-node",
        )
        assert accepted.type == "hello.ok"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_secure_hello_rejects_authorization_capability_in_node_document(
    zmq_context,
    transport_config,
):
    gateway, dealer, _principal, enrolled, _registry = _start_peer(
        zmq_context,
        transport_config,
    )
    try:
        denied = _send_hello(
            dealer,
            {
                "versions": [1],
                "node": _node_mapping(enrolled, capabilities=["daemon.admin"]),
            },
            "authority-injection",
        )
        assert denied.type == "protocol.error"
        assert denied.body == {
            "code": "invalid_node",
            "message": "peer node descriptor is invalid",
            "retryable": False,
        }
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_node_lookup_fails_closed_after_registry_revocation(
    zmq_context,
    transport_config,
):
    gateway, dealer, principal, enrolled, registry = _start_peer(
        zmq_context,
        transport_config,
    )
    try:
        hello = _send_hello(
            dealer,
            {"versions": [1], "node": _node_mapping(enrolled)},
            "revocable-node",
        )
        assert hello.type == "hello.ok"
        assert gateway.node_for_session(principal.principal_id, hello.session_id) is not None

        registry.revoke(enrolled.device_id)
        assert gateway.node_for_session(principal.principal_id, hello.session_id) is None
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
