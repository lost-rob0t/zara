from __future__ import annotations

import concurrent.futures
import threading
import time
from types import MethodType

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class KnownConversationRaceSupervisor:
    """Publish the first turn event before the submit receipt can resolve."""

    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()
        self.future: concurrent.futures.Future = concurrent.futures.Future()
        self.submitted = threading.Event()
        self.command: SubmitTurn | None = None

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        assert isinstance(command, SubmitTurn)
        self.command = command
        self.bus.publish(
            events.TurnStarted(
                turn_id="turn-known-conversation",
                conversation_id=command.conversation_id,
            )
        )
        self.submitted.set()
        return self.future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def accept(self) -> None:
        assert self.command is not None
        self.future.set_result(
            CommandReceipt(
                request_id=self.command.request_id,
                turn_id="turn-known-conversation",
                detail="turn accepted",
            )
        )


class EarlyBufferProbe:
    def __init__(self, gateway: ZaraZmqGateway) -> None:
        self.condition = threading.Condition()
        self.count = 0
        original = gateway._buffer_early_turn_event

        def wrapped(_gateway, principal_id, turn_id, held):
            original(principal_id, turn_id, held)
            with self.condition:
                self.count += 1
                self.condition.notify_all()

        gateway._buffer_early_turn_event = MethodType(wrapped, gateway)

    def wait(self, timeout: float = 1.0) -> None:
        with self.condition:
            assert self.condition.wait_for(lambda: self.count == 1, timeout), (
                "turn.started bypassed pre-accept demux buffering for an already-open conversation"
            )


@pytest.fixture
def context():
    value = zmq.Context()
    try:
        yield value
    finally:
        value.term()


def config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        event_queue_size=16,
        pending_request_limit=8,
    )


def receive(dealer: zmq.Socket, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) == zmq.POLLIN
    return decode_message(dealer.recv_multipart()).message


def send(dealer: zmq.Socket, message: ProtocolMessage) -> None:
    dealer.send_multipart(encode_message(message))


def test_known_conversation_turn_event_cannot_overtake_turn_accepted(context):
    """Reproduce #1396 with a route already owned by conversation.open.

    Before the fix, conversation fallback routes turn.started immediately while
    the turn.submit receipt is still unresolved. Android then observes a
    runtime event where it requires turn.accepted and reports
    protocol.unexpected_message. The canonical demux must instead buffer the
    turn event by turn id until acceptance establishes turn ownership.
    """
    endpoint = f"inproc://known-conversation-race-{time.time_ns()}"
    supervisor = KnownConversationRaceSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=context,
        config=config(),
    )
    probe = EarlyBufferProbe(gateway)
    gateway.start().result(timeout=1.0)

    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config(), router=False)
    dealer.connect(endpoint)
    try:
        send(
            dealer,
            ProtocolMessage(
                type="hello",
                id="hello-known",
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            ),
        )
        hello = receive(dealer)
        assert hello.type == "hello.ok"
        session_id = hello.session_id
        assert session_id

        send(
            dealer,
            ProtocolMessage(
                type="conversation.open",
                id="open-known",
                session_id=session_id,
                conversation_id="conversation-known",
                timestamp_ns=2,
                payload_count=0,
            ),
        )
        opened = receive(dealer)
        assert opened.type == "conversation.opened"
        assert opened.conversation_id == "conversation-known"

        send(
            dealer,
            ProtocolMessage(
                type="turn.submit",
                id="submit-known",
                session_id=session_id,
                conversation_id="conversation-known",
                timestamp_ns=3,
                payload_count=0,
                body={"text": "race me"},
            ),
        )
        assert supervisor.submitted.wait(1.0)

        # Fixed ownership buffers here; buggy conversation fallback puts
        # turn.started straight on the wire and never reaches this seam.
        probe.wait()
        supervisor.accept()

        accepted = receive(dealer)
        started = receive(dealer)
        assert [accepted.type, started.type] == ["turn.accepted", "turn.started"]
        assert accepted.reply_to == "submit-known"
        assert accepted.session_id == session_id
        assert started.session_id == session_id
        assert accepted.turn_id == started.turn_id == "turn-known-conversation"
    finally:
        dealer.close(0)
        if not supervisor.future.done():
            supervisor.future.cancel()
        gateway.close(timeout=1.0)
