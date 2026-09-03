from __future__ import annotations

import time

import pytest
import zmq

from t.test_zara1_device_actions import (
    FakeSupervisor,
    advertise,
    hello,
    receive_message,
    send,
    unique_endpoint,
)
from zara.protocol import ProtocolMessage, ProtocolValidationError, encode_message
from zara.security import Capability
from zara.security_gateway import SecureZaraZmqGateway
from zara.server import PrincipalContext
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


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
        pending_request_limit=8,
    )


def connected_gateway(zmq_context, transport_config, prefix: str):
    endpoint = unique_endpoint(prefix)
    principal = PrincipalContext("owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        principal=principal,
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    dealer = zmq_context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(endpoint)
    session_id = hello(dealer)
    return gateway, dealer, principal, session_id


def test_capability_snapshot_count_is_bounded_before_entry_dispatch():
    with pytest.raises(ProtocolValidationError, match="capability count exceeds limit"):
        encode_message(
            ProtocolMessage(
                type="capability.snapshot",
                id="caps-too-many",
                session_id="session-1",
                timestamp_ns=1,
                payload_count=0,
                body={
                    "capabilities": [
                        {"id": "open_uri", "version": 1} for _ in range(33)
                    ]
                },
            )
        )


@pytest.mark.parametrize(
    "message_type",
    [
        "capability.snapshot",
        "device.action.accepted",
        "device.action.result",
        "device.action.error",
    ],
)
def test_device_advertisement_never_promotes_server_authority(message_type):
    assert SecureZaraZmqGateway._capability_for(message_type) is Capability.SESSION_BASIC


def test_request_handle_exposes_server_generated_action_id_for_cancellation(
    zmq_context,
    transport_config,
):
    gateway, dealer, principal, session_id = connected_gateway(
        zmq_context, transport_config, "device-handle"
    )
    advertise(dealer, session_id, "open_app")

    handle = gateway.request_device_action(
        principal_id=principal.principal_id,
        session_id=session_id,
        capability="open_app",
        args={"app": "browser"},
        deadline_ns=time.time_ns() + 1_000_000_000,
    )
    request = receive_message(dealer)
    assert handle.action_id == request.body["action_id"]
    assert gateway.cancel_device_action(handle.action_id, reason="caller_cancelled") is True
    cancelled = receive_message(dealer)
    assert cancelled.body["action_id"] == handle.action_id

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_expired_device_action_deadline_is_rejected_before_enqueue(
    zmq_context,
    transport_config,
):
    gateway, dealer, principal, session_id = connected_gateway(
        zmq_context, transport_config, "device-expired"
    )
    advertise(dealer, session_id, "open_uri")

    with pytest.raises(ValueError, match="deadline has expired"):
        gateway.request_device_action(
            principal_id=principal.principal_id,
            session_id=session_id,
            capability="open_uri",
            args={"uri": "https://example.com"},
            deadline_ns=time.time_ns() - 1,
        )

    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(50)).get(dealer) is None

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_terminal_result_before_accepted_fails_closed_without_completing_action(
    zmq_context,
    transport_config,
):
    gateway, dealer, principal, session_id = connected_gateway(
        zmq_context, transport_config, "device-order"
    )
    advertise(dealer, session_id, "open_app")

    handle = gateway.request_device_action(
        principal_id=principal.principal_id,
        session_id=session_id,
        capability="open_app",
        args={"app": "browser"},
        deadline_ns=time.time_ns() + 1_000_000_000,
    )
    request = receive_message(dealer)
    action_id = request.body["action_id"]

    send(
        dealer,
        ProtocolMessage(
            type="device.action.result",
            id="result-before-accepted",
            session_id=session_id,
            timestamp_ns=4,
            payload_count=0,
            body={"action_id": action_id, "outcome": "completed"},
        ),
    )
    error = receive_message(dealer)
    assert error.type == "protocol.error"
    assert error.body["code"] == "action_not_accepted"
    assert handle.done() is False

    send(
        dealer,
        ProtocolMessage(
            type="device.action.accepted",
            id="accepted-after-rejection",
            session_id=session_id,
            timestamp_ns=5,
            payload_count=0,
            body={"action_id": action_id},
        ),
    )
    send(
        dealer,
        ProtocolMessage(
            type="device.action.result",
            id="result-after-accepted",
            session_id=session_id,
            timestamp_ns=6,
            payload_count=0,
            body={"action_id": action_id, "outcome": "completed"},
        ),
    )
    assert handle.result(timeout=1.0).action_id == action_id

    dealer.close(0)
    gateway.close(timeout=1.0)
