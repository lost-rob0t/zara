from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, ProtocolValidationError, decode_message, encode_message
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        future.set_exception(AssertionError(f"unexpected runtime command: {command!r}"))
        return future


def _request_body(*, action_seq=None):
    body = {
        "action_id": "action-1",
        "capability": "open_app",
        "args": {"app": "browser"},
        "deadline_ns": 999,
        "idempotency": "at_most_once",
    }
    if action_seq is not None:
        body["action_seq"] = action_seq
    return body


def test_device_action_request_requires_positive_action_sequence():
    with pytest.raises(ProtocolValidationError):
        encode_message(
            ProtocolMessage(
                type="device.action.request",
                id="request-1",
                session_id="session-1",
                timestamp_ns=1,
                payload_count=0,
                body=_request_body(),
            )
        )

    for invalid in (0, -1, True, 1.5, "1"):
        with pytest.raises(ProtocolValidationError):
            encode_message(
                ProtocolMessage(
                    type="device.action.request",
                    id="request-1",
                    session_id="session-1",
                    timestamp_ns=1,
                    payload_count=0,
                    body=_request_body(action_seq=invalid),
                )
            )

    encoded = encode_message(
        ProtocolMessage(
            type="device.action.request",
            id="request-1",
            session_id="session-1",
            timestamp_ns=1,
            payload_count=0,
            body=_request_body(action_seq=1),
        )
    )
    assert decode_message(encoded).message.body["action_seq"] == 1


def test_gateway_issues_monotonic_action_sequence_per_authenticated_session():
    context = zmq.Context()
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        pending_request_limit=8,
    )
    endpoint = f"inproc://device-action-seq-{time.time_ns()}"
    principal = PrincipalContext("owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        principal=principal,
        context=context,
        config=config,
    )
    gateway.start().result(timeout=1.0)
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    dealer.connect(endpoint)

    def receive() -> ProtocolMessage:
        poller = zmq.Poller()
        poller.register(dealer, zmq.POLLIN)
        assert dict(poller.poll(1000)).get(dealer) == zmq.POLLIN
        return decode_message(dealer.recv_multipart()).message

    def send(message: ProtocolMessage) -> None:
        dealer.send_multipart(encode_message(message))

    def open_session(hello_id: str) -> str:
        send(
            ProtocolMessage(
                type="hello",
                id=hello_id,
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            )
        )
        response = receive()
        assert response.type == "hello.ok"
        session_id = response.session_id
        assert session_id
        send(
            ProtocolMessage(
                type="capability.snapshot",
                id=f"caps-{hello_id}",
                session_id=session_id,
                timestamp_ns=2,
                payload_count=0,
                body={"capabilities": [{"id": "open_app", "version": 1}]},
            )
        )
        assert receive().type == "capability.snapshot.ok"
        return session_id

    try:
        session_id = open_session("hello-1")
        for expected_seq in (1, 2):
            gateway.request_device_action(
                principal_id=principal.principal_id,
                session_id=session_id,
                capability="open_app",
                args={"app": "browser"},
                deadline_ns=time.time_ns() + 1_000_000_000,
            )
            request = receive()
            assert request.type == "device.action.request"
            assert request.body["action_seq"] == expected_seq

        new_session_id = open_session("hello-2")
        assert new_session_id != session_id
        gateway.request_device_action(
            principal_id=principal.principal_id,
            session_id=new_session_id,
            capability="open_app",
            args={"app": "browser"},
            deadline_ns=time.time_ns() + 1_000_000_000,
        )
        assert receive().body["action_seq"] == 1
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.destroy(linger=0)
