from __future__ import annotations

import concurrent.futures
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class _DeferredSameTurnBurstSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()
        self.future: concurrent.futures.Future | None = None
        self.request_id: str | None = None
        self.turn_id = "turn-same-turn-overflow"

    def submit(self, principal, command):
        if not isinstance(command, SubmitTurn):
            raise AssertionError(f"unexpected command: {command!r}")
        self.request_id = command.request_id
        self.future = concurrent.futures.Future()
        for index in range(129):
            self.bus.publish(
                events.AssistantDelta(
                    turn_id=self.turn_id,
                    conversation_id=command.conversation_id,
                    label="runtime-host",
                    text=f"delta-{index:03d}",
                )
            )
        return self.future

    def accept(self) -> None:
        if self.future is None or self.request_id is None:
            raise AssertionError("turn was not submitted")
        self.future.set_result(
            CommandReceipt(
                request_id=self.request_id,
                turn_id=self.turn_id,
                detail="turn accepted",
            )
        )

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


def _endpoint() -> str:
    return f"inproc://same-turn-backpressure-{time.time_ns()}"


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


def test_same_turn_early_event_capacity_fails_submit_closed_without_silent_loss():
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
        pending_request_limit=8,
        idempotency_cache_size=8,
    )
    supervisor = _DeferredSameTurnBurstSupervisor()
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
                    id="hello-same-turn-overflow",
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
                    id="submit-same-turn-overflow",
                    session_id=hello.session_id,
                    conversation_id="conversation-same-turn-overflow",
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "stream a bounded burst"},
                )
            )
        )

        key = ("local-owner", supervisor.turn_id)

        def burst_is_buffered() -> bool:
            subscription = gateway._event_subscription
            if subscription is None or not subscription._queue.empty():
                return False
            with gateway._lock:
                return len(gateway._early_turn_events.get(key, ())) == 128

        _wait_until(burst_is_buffered)
        supervisor.accept()

        overflow = _receive(dealer)
        assert overflow.type == "protocol.error", (
            "a same-turn burst beyond the bounded early-event capacity must fail "
            f"closed before acceptance; got {overflow.type}"
        )
        assert overflow.reply_to == "submit-same-turn-overflow"
        assert overflow.body == {
            "code": "server_backpressure",
            "message": "too many turn events are awaiting acceptance",
            "retryable": True,
        }

        def overflow_state_is_released() -> bool:
            with gateway._lock:
                return (
                    key not in gateway._early_turn_events
                    and key not in gateway._turns_awaiting_accept
                    and key not in gateway._turn_routes
                )

        _wait_until(overflow_state_is_released)
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.term()
