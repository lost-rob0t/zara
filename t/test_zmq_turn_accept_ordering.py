from __future__ import annotations

import concurrent.futures
import threading
import time
from collections import OrderedDict

import pytest
import zmq

import zara.zmq_transport
from zara.runtime import bridge, events
from zara.protocol import (
    ProtocolLimits,
    ProtocolMessage,
    decode_message,
    encode_message,
)
from zara.server import ServerState
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext
from zara.zmq_transport import (
    TransportConfig,
    ZaraZmqGateway,
    _RouteState,
    apply_socket_options,
)


class PublishBeforeAcceptSupervisor:
    """Supervisor whose runtime publishes TurnStarted before the submit future resolves.

    Mirrors RuntimeHost._submit_turn: the TurnStarted event is published while the
    turn is merely accepted, before the transport's completion callback can send
    the turn.accepted reply.
    """

    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        if isinstance(command, SubmitTurn):
            self.bus.publish(
                events.TurnStarted(
                    turn_id="turn-canonical",
                    conversation_id=command.conversation_id,
                    label="runtime-host",
                )
            )
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(
                request_id=command.request_id,
                turn_id="turn-canonical",
                detail="turn accepted",
            )
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


def unique_endpoint(prefix: str) -> str:
    return f"inproc://{prefix}-{time.time_ns()}"


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
    )


def receive_message(socket: zmq.Socket, *, timeout_ms: int = 1500):
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN, "no frame arrived"
    return decode_message(socket.recv_multipart()).message


def test_turn_events_arrive_after_turn_accepted_and_are_never_dropped(
    zmq_context,
    transport_config,
):
    endpoint = unique_endpoint("accept-ordering")
    supervisor = PublishBeforeAcceptSupervisor()
    principal = PrincipalContext("local-owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
    try:
        apply_socket_options(dealer, transport_config, router=False)
        dealer.connect(endpoint)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-1",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = receive_message(dealer)
        assert hello.type == "hello.ok"
        session_id = hello.session_id

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="turn.submit",
                    id="submit-1",
                    session_id=session_id,
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "hello"},
                )
            )
        )

        first = receive_message(dealer)
        assert first.type == "turn.accepted", (
            f"expected accepted first, got {first.type}"
        )

        second = receive_message(dealer)
        assert second.type == "turn.started", (
            f"runtime event must follow accepted, got {second.type} (dropped?)"
        )
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


class _RecorderSocket:
    def __init__(self) -> None:
        self.sent = []

    def send_multipart(self, frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        self.sent.append(tuple(frames))


class _SingleEventSubscription:
    def __init__(self, event) -> None:
        self._envelopes = [bridge.EventEnvelope(sequence=1, occurred_at=0.0, event=event)]

    def drain(self, limit=32):
        return [self._envelopes.pop(0)] if self._envelopes else []


def _bare_gateway(route: bytes) -> ZaraZmqGateway:
    gateway = object.__new__(ZaraZmqGateway)
    gateway._limits = ProtocolLimits()
    gateway._config = TransportConfig(event_queue_size=8, pending_request_limit=8)
    gateway._lock = threading.RLock()
    gateway._route_outbound = OrderedDict()
    gateway._routes = {
        route: _RouteState(
            session_id="session-1",
            principal_id="owner",
            ready=True,
        )
    }
    gateway._turn_routes = {}
    gateway._turns_awaiting_accept = set()
    gateway._early_turn_events = OrderedDict()
    gateway._approval_owners = {}
    return gateway


def test_turn_event_racing_accepted_flush_is_never_stranded(monkeypatch):
    """A drain that reads awaiting-accept just before completed()'s flush pops
    the early buffer empty and discards the key must not strand its event in
    the early buffer: the event still rides the FIFO after turn.accepted."""
    real_codec = zara.zmq_transport.runtime_event_to_message

    route = b"race-route"
    gateway = _bare_gateway(route)
    gateway._turn_routes[("owner", "turn-race")] = route
    gateway._turns_awaiting_accept.add(("owner", "turn-race"))

    accepted = ProtocolMessage(
        type="turn.accepted",
        id="accepted-race",
        reply_to="submit-race",
        session_id="session-1",
        turn_id="turn-race",
        timestamp_ns=2,
        payload_count=0,
    )

    def racing_codec(envelope, *, message_id, timestamp_ns):
        # Interleaving: while the drain sits between its awaiting-accept check
        # and its buffer append, completed()'s flush finishes - it pops the
        # early buffer empty, discards the awaiting key, and queues the reply.
        with gateway._lock:
            gateway._early_turn_events.pop(("owner", "turn-race"), None)
            gateway._turns_awaiting_accept.discard(("owner", "turn-race"))
        gateway._enqueue_outbound(route, accepted)
        return real_codec(envelope, message_id=message_id, timestamp_ns=timestamp_ns)

    monkeypatch.setattr(
        zara.zmq_transport,
        "runtime_event_to_message",
        racing_codec,
    )

    subscription = _SingleEventSubscription(
        events.TurnStarted(
            turn_id="turn-race",
            conversation_id="conversation-1",
            label="race",
        )
    )
    socket = _RecorderSocket()

    gateway._drain_runtime_subscription(socket, subscription, principal_id="owner")
    gateway._drain_outbound(socket)

    delivered = [decode_message(frames[1:]).message for frames in socket.sent]
    assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
