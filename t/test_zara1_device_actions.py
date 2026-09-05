from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.protocol import (
    DEVICE_CAPABILITIES,
    ProtocolMessage,
    ProtocolValidationError,
    decode_message,
    encode_message,
)
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import (
    DeviceActionCancelled,
    DeviceActionRemoteError,
    DeviceActionResult,
    DeviceCapabilityUnavailable,
    TransportConfig,
    ZaraZmqGateway,
    apply_socket_options,
)


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):  # pragma: no cover - device actions never use RuntimeHost
        future = concurrent.futures.Future()
        future.set_exception(AssertionError(f"unexpected runtime command: {command!r}"))
        return future


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.destroy(linger=0)


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


def unique_endpoint(prefix: str) -> str:
    return f"inproc://{prefix}-{time.time_ns()}"


def receive_message(socket: zmq.Socket, *, timeout_ms: int = 1000) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage) -> None:
    socket.send_multipart(encode_message(message))


def hello(socket: zmq.Socket) -> str:
    send(
        socket,
        ProtocolMessage(
            type="hello",
            id="hello-1",
            timestamp_ns=1,
            payload_count=0,
            body={"versions": [1]},
        ),
    )
    response = receive_message(socket)
    assert response.type == "hello.ok"
    assert response.session_id
    return response.session_id


def advertise(socket: zmq.Socket, session_id: str, *capabilities: str) -> ProtocolMessage:
    send(
        socket,
        ProtocolMessage(
            type="capability.snapshot",
            id=f"caps-{time.time_ns()}",
            session_id=session_id,
            timestamp_ns=2,
            payload_count=0,
            body={
                "capabilities": [
                    {"id": capability, "version": 1} for capability in capabilities
                ]
            },
        ),
    )
    return receive_message(socket)


def test_device_capability_v1_is_closed_and_golden_encoded():
    assert DEVICE_CAPABILITIES == frozenset({"open_app", "open_uri"})

    message = ProtocolMessage(
        type="capability.snapshot",
        id="caps-1",
        session_id="session-1",
        timestamp_ns=2,
        payload_count=0,
        body={
            "capabilities": [
                {"id": "open_app", "version": 1},
                {"id": "open_uri", "version": 1},
            ]
        },
    )
    assert encode_message(message) == [
        b"ZARA/1",
        b'{"body":{"capabilities":[{"id":"open_app","version":1},{"id":"open_uri","version":1}]},"id":"caps-1","payload_count":0,"session_id":"session-1","timestamp_ns":2,"type":"capability.snapshot"}',
    ]


@pytest.mark.parametrize(
    "body",
    [
        {"capabilities": [{"id": "admin", "version": 1}]},
        {"capabilities": [{"id": "open_uri", "version": 2}]},
        {"capabilities": [{"id": "open_uri", "version": 1, "authority": "admin"}]},
        {"capabilities": [{"id": "open_uri", "version": 1}] * 2},
        {"capabilities": "open_uri"},
    ],
)
def test_capability_snapshot_fails_closed(body):
    with pytest.raises(ProtocolValidationError):
        encode_message(
            ProtocolMessage(
                type="capability.snapshot",
                id="caps-bad",
                session_id="session-1",
                timestamp_ns=2,
                payload_count=0,
                body=body,
            )
        )


@pytest.mark.parametrize(
    ("capability", "args"),
    [
        ("open_uri", {"uri": "https://example.com", "shell": "rm -rf /"}),
        ("open_uri", {"uri": 7}),
        ("open_app", {"app": "browser", "class": "java.lang.Runtime"}),
        ("open_app", {"app": "x" * 129}),
        ("admin", {}),
    ],
)
def test_device_action_request_rejects_unknown_or_executable_shapes(capability, args):
    with pytest.raises(ProtocolValidationError):
        encode_message(
            ProtocolMessage(
                type="device.action.request",
                id="request-1",
                session_id="session-1",
                trace_id="trace-1",
                timestamp_ns=3,
                payload_count=0,
                body={
                    "action_id": "action-1",
                    "capability": capability,
                    "args": args,
                    "deadline_ns": 999,
                    "idempotency": "at_most_once",
                },
            )
        )


def test_gateway_replaces_capability_snapshot_and_completes_typed_action(
    zmq_context,
    transport_config,
):
    endpoint = unique_endpoint("device-action")
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
    ack = advertise(dealer, session_id, "open_uri", "open_app")
    assert ack.type == "capability.snapshot.ok"
    assert ack.session_id == session_id
    assert gateway.capabilities_for(principal.principal_id, session_id) == frozenset(
        {"open_uri", "open_app"}
    )

    future = gateway.request_device_action(
        principal_id=principal.principal_id,
        session_id=session_id,
        capability="open_uri",
        args={"uri": "https://example.com"},
        deadline_ns=time.time_ns() + 1_000_000_000,
        idempotency="at_most_once",
        trace_id="trace-1",
    )
    request = receive_message(dealer)
    assert request.type == "device.action.request"
    assert request.session_id == session_id
    assert request.body["capability"] == "open_uri"
    assert request.body["args"] == {"uri": "https://example.com"}
    action_id = request.body["action_id"]

    send(
        dealer,
        ProtocolMessage(
            type="device.action.accepted",
            id="accepted-1",
            session_id=session_id,
            timestamp_ns=4,
            payload_count=0,
            body={"action_id": action_id},
        ),
    )
    send(
        dealer,
        ProtocolMessage(
            type="device.action.result",
            id="result-1",
            session_id=session_id,
            timestamp_ns=5,
            payload_count=0,
            body={"action_id": action_id, "outcome": "completed"},
        ),
    )
    assert future.result(timeout=1.0) == DeviceActionResult(
        action_id=action_id,
        capability="open_uri",
        outcome="completed",
    )

    ack = advertise(dealer, session_id, "open_app")
    assert ack.type == "capability.snapshot.ok"
    assert gateway.capabilities_for(principal.principal_id, session_id) == frozenset({"open_app"})
    with pytest.raises(DeviceCapabilityUnavailable):
        gateway.request_device_action(
            principal_id=principal.principal_id,
            session_id=session_id,
            capability="open_uri",
            args={"uri": "https://example.com"},
            deadline_ns=time.time_ns() + 1_000_000_000,
        )

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_cancel_is_terminal_and_late_result_fails_closed(zmq_context, transport_config):
    endpoint = unique_endpoint("device-cancel")
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
    advertise(dealer, session_id, "open_app")
    future = gateway.request_device_action(
        principal_id=principal.principal_id,
        session_id=session_id,
        capability="open_app",
        args={"app": "browser"},
        deadline_ns=time.time_ns() + 1_000_000_000,
    )
    request = receive_message(dealer)
    action_id = request.body["action_id"]

    assert gateway.cancel_device_action(action_id, reason="superseded") is True
    cancelled = receive_message(dealer)
    assert cancelled.type == "device.action.cancel"
    assert cancelled.body == {"action_id": action_id, "reason": "superseded"}
    with pytest.raises(DeviceActionCancelled):
        future.result(timeout=1.0)

    send(
        dealer,
        ProtocolMessage(
            type="device.action.result",
            id="late-result",
            session_id=session_id,
            timestamp_ns=5,
            payload_count=0,
            body={"action_id": action_id, "outcome": "completed"},
        ),
    )
    error = receive_message(dealer)
    assert error.type == "protocol.error"
    assert error.body["code"] == "unknown_action"

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_error_and_reconnect_clear_route_local_action_authority(zmq_context, transport_config):
    endpoint = unique_endpoint("device-reconnect")
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
    advertise(dealer, session_id, "open_app")
    future = gateway.request_device_action(
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
            type="device.action.error",
            id="error-1",
            session_id=session_id,
            timestamp_ns=5,
            payload_count=0,
            body={"action_id": action_id, "code": "permission_denied"},
        ),
    )
    with pytest.raises(DeviceActionRemoteError) as error:
        future.result(timeout=1.0)
    assert error.value.code == "permission_denied"

    new_session = hello(dealer)
    assert new_session != session_id
    assert gateway.capabilities_for(principal.principal_id, new_session) == frozenset()
    with pytest.raises(DeviceCapabilityUnavailable):
        gateway.request_device_action(
            principal_id=principal.principal_id,
            session_id=new_session,
            capability="open_app",
            args={"app": "browser"},
            deadline_ns=time.time_ns() + 1_000_000_000,
        )

    dealer.close(0)
    gateway.close(timeout=1.0)
