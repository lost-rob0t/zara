from __future__ import annotations

import concurrent.futures
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
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

    def publish(self, principal, event):
        assert isinstance(principal, PrincipalContext)
        return self.bus.publish(event)

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        future.set_exception(AssertionError(f"unexpected runtime command: {command!r}"))
        return future


def config() -> TransportConfig:
    return TransportConfig(
        sndhwm=16,
        rcvhwm=16,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        pending_request_limit=16,
    )


def receive(socket: zmq.Socket, timeout_ms: int = 1000) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage) -> None:
    socket.send_multipart(encode_message(message))


def hello(socket: zmq.Socket, request_id: str) -> str:
    send(
        socket,
        ProtocolMessage(
            type="hello",
            id=request_id,
            timestamp_ns=1,
            payload_count=0,
            body={"versions": [1]},
        ),
    )
    response = receive(socket)
    assert response.type == "hello.ok"
    assert response.session_id
    return response.session_id


def advertise_sms(socket: zmq.Socket, session_id: str) -> None:
    send(
        socket,
        ProtocolMessage(
            type="capability.snapshot",
            id="caps-android",
            session_id=session_id,
            timestamp_ns=2,
            payload_count=0,
            body={"capabilities": [{"id": "sms_send", "version": 1}]},
        ),
    )
    assert receive(socket).type == "capability.snapshot.ok"


def test_desktop_sms_routes_to_exact_android_capability_and_returns_result() -> None:
    context = zmq.Context()
    endpoint = f"inproc://phone-sms-{time.time_ns()}"
    transport = config()
    principal = PrincipalContext("owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        principal=principal,
        context=context,
        config=transport,
    )
    gateway.start().result(timeout=1.0)

    desktop = context.socket(zmq.DEALER)
    android = context.socket(zmq.DEALER)
    for socket in (desktop, android):
        apply_socket_options(socket, transport, router=False)
        socket.connect(endpoint)

    try:
        desktop_session = hello(desktop, "hello-desktop")
        android_session = hello(android, "hello-android")
        advertise_sms(android, android_session)

        send(
            desktop,
            ProtocolMessage(
                type="phone.sms.send",
                id="sms-request",
                session_id=desktop_session,
                timestamp_ns=3,
                payload_count=0,
                body={"to": "+15551234567", "text": "hello"},
            ),
        )

        action = receive(android)
        assert action.type == "device.action.request"
        assert action.session_id == android_session
        assert action.body["capability"] == "sms_send"
        assert action.body["args"] == {"to": "+15551234567", "text": "hello"}
        action_id = action.body["action_id"]

        send(
            android,
            ProtocolMessage(
                type="device.action.accepted",
                id="accepted",
                session_id=android_session,
                timestamp_ns=4,
                payload_count=0,
                body={"action_id": action_id},
            ),
        )
        send(
            android,
            ProtocolMessage(
                type="device.action.result",
                id="result",
                session_id=android_session,
                timestamp_ns=5,
                payload_count=0,
                body={"action_id": action_id, "outcome": "completed"},
            ),
        )

        result = receive(desktop)
        assert result.type == "phone.sms.result"
        assert result.reply_to == "sms-request"
        assert result.session_id == desktop_session
        assert result.body == {"outcome": "completed"}
    finally:
        desktop.close(0)
        android.close(0)
        gateway.close(timeout=1.0)
        context.destroy(linger=0)


def test_android_phone_event_is_forwarded_to_other_principal_routes_not_echoed_to_origin() -> None:
    context = zmq.Context()
    endpoint = f"inproc://phone-event-{time.time_ns()}"
    transport = config()
    principal = PrincipalContext("owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        principal=principal,
        context=context,
        config=transport,
    )
    gateway.start().result(timeout=1.0)

    desktop = context.socket(zmq.DEALER)
    android = context.socket(zmq.DEALER)
    for socket in (desktop, android):
        apply_socket_options(socket, transport, router=False)
        socket.connect(endpoint)

    try:
        desktop_session = hello(desktop, "hello-desktop")
        android_session = hello(android, "hello-android")

        send(
            android,
            ProtocolMessage(
                type="device.event",
                id="phone-event-1",
                session_id=android_session,
                timestamp_ns=3,
                payload_count=0,
                body={
                    "event_id": "sms-1",
                    "kind": "sms.received",
                    "remote": "+15557654321",
                    "text": "hey",
                },
            ),
        )

        event = receive(desktop)
        assert event.type == "phone.event"
        assert event.session_id == desktop_session
        assert event.body == {
            "event_id": "sms-1",
            "kind": "sms.received",
            "remote": "+15557654321",
            "text": "hey",
            "greeting": "",
            "actions": ["alert", "voice_takeover"],
        }

        poller = zmq.Poller()
        poller.register(android, zmq.POLLIN)
        assert dict(poller.poll(50)).get(android) is None
    finally:
        desktop.close(0)
        android.close(0)
        gateway.close(timeout=1.0)
        context.destroy(linger=0)
