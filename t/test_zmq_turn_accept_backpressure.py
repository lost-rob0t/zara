from __future__ import annotations

import concurrent.futures
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class _PublishBeforeAcceptSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        if isinstance(command, SubmitTurn):
            self.bus.publish(
                events.TurnStarted(
                    turn_id="turn-backpressure",
                    conversation_id=command.conversation_id,
                    label="runtime-host",
                )
            )
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(
                request_id=command.request_id,
                turn_id="turn-backpressure",
                detail="turn accepted",
            )
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


class _DeferredTerminalSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()
        self.pending: dict[str, tuple[concurrent.futures.Future, str]] = {}

    def submit(self, principal, command):
        if not isinstance(command, SubmitTurn):
            raise AssertionError(f"unexpected command: {command!r}")
        turn_id = f"turn-{command.request_id}"
        future = concurrent.futures.Future()
        self.pending[command.request_id] = (future, turn_id)
        self.bus.publish(
            events.AgentCompleted(
                turn_id=turn_id,
                conversation_id=command.conversation_id,
                label="runtime-host",
                success=True,
            )
        )
        return future

    def accept(self, request_id: str) -> None:
        future, turn_id = self.pending[request_id]
        future.set_result(
            CommandReceipt(
                request_id=request_id,
                turn_id=turn_id,
                detail="turn accepted",
            )
        )

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


def _endpoint() -> str:
    return f"inproc://accept-backpressure-{time.time_ns()}"


def _receive(socket: zmq.Socket, *, timeout_ms: int = 1500):
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN, "no frame arrived"
    return decode_message(socket.recv_multipart()).message


def _wait_until(predicate, *, timeout: float = 2.0) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(0.005)
    assert predicate(), "condition did not become true before timeout"


def test_full_route_fifo_never_evicts_turn_accepted_for_a_later_turn_event():
    context = zmq.Context()
    endpoint = _endpoint()
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=10,
        event_queue_size=1,
        pending_request_limit=8,
        idempotency_cache_size=8,
    )
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=_PublishBeforeAcceptSupervisor(),
        principal=PrincipalContext("local-owner"),
        context=context,
        config=config,
    )
    gateway.start().result(timeout=1.0)

    dealer = context.socket(zmq.DEALER)
    try:
        apply_socket_options(dealer, config, router=False)
        dealer.connect(endpoint)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-backpressure",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = _receive(dealer)
        assert hello.type == "hello.ok"

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="turn.submit",
                    id="submit-backpressure",
                    session_id=hello.session_id,
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "hello"},
                )
            )
        )

        first = _receive(dealer)
        assert first.type == "turn.accepted", (
            "bounded route backpressure must not let a runtime event evict "
            f"turn.accepted; got {first.type}"
        )
        assert first.turn_id == "turn-backpressure"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.term()


def test_early_turn_capacity_preserves_oldest_terminal_and_fails_new_turn_closed():
    context = zmq.Context()
    endpoint = _endpoint()
    config = TransportConfig(
        sndhwm=256,
        rcvhwm=256,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=1,
        event_queue_size=256,
        pending_request_limit=128,
        idempotency_cache_size=128,
    )
    supervisor = _DeferredTerminalSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=context,
        config=config,
    )
    gateway.start().result(timeout=1.0)

    dealer = context.socket(zmq.DEALER)
    try:
        apply_socket_options(dealer, config, router=False)
        dealer.connect(endpoint)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-early-capacity",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = _receive(dealer)
        assert hello.type == "hello.ok"

        for index in range(65):
            dealer.send_multipart(
                encode_message(
                    ProtocolMessage(
                        type="turn.submit",
                        id=f"submit-{index}",
                        session_id=hello.session_id,
                        conversation_id=f"conversation-{index}",
                        timestamp_ns=index + 2,
                        payload_count=0,
                        body={"text": f"turn {index}"},
                    )
                )
            )

        _wait_until(lambda: len(supervisor.pending) == 65)

        def all_terminal_events_drained() -> bool:
            subscription = gateway._event_subscription
            return subscription is not None and subscription._queue.empty()

        _wait_until(all_terminal_events_drained)

        supervisor.accept("submit-0")
        accepted = _receive(dealer)
        assert accepted.type == "turn.accepted"
        assert accepted.turn_id == "turn-submit-0"
        terminal = _receive(dealer)
        assert terminal.type == "turn.completed", (
            "cross-turn early-event capacity must not silently discard the "
            f"oldest live turn terminal; got {terminal.type}"
        )
        assert terminal.turn_id == "turn-submit-0"

        supervisor.accept("submit-64")
        overflow = _receive(dealer)
        assert overflow.type == "protocol.error", (
            "the turn that cannot reserve bounded early-event capacity must fail "
            f"closed before acceptance; got {overflow.type}"
        )
        assert overflow.reply_to == "submit-64"
        assert overflow.body == {
            "code": "server_backpressure",
            "message": "too many turn events are awaiting acceptance",
            "retryable": True,
        }
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.term()
