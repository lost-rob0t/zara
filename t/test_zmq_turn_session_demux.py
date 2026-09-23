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


class InterleavingSupervisor:
    """Runtime fixture whose turn events deliberately precede acceptance."""

    def __init__(self, modes: list[str], *, turn_ids: list[str] | None = None) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()
        self.modes = list(modes)
        self.turn_ids = list(turn_ids or [f"turn-{index}" for index in range(len(modes))])
        self.commands: list[SubmitTurn] = []
        self.futures: list[concurrent.futures.Future] = []
        self.submitted = [threading.Event() for _ in modes]

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        assert isinstance(command, SubmitTurn)
        index = len(self.commands)
        assert index < len(self.modes)
        self.commands.append(command)
        turn_id = self.turn_ids[index]
        conversation_id = command.conversation_id
        for event in self._burst(self.modes[index], turn_id, conversation_id):
            self.bus.publish(event)
        future: concurrent.futures.Future = concurrent.futures.Future()
        self.futures.append(future)
        self.submitted[index].set()
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def complete(self, index: int) -> None:
        command = self.commands[index]
        self.futures[index].set_result(
            CommandReceipt(
                request_id=command.request_id,
                turn_id=self.turn_ids[index],
                detail="turn accepted",
            )
        )

    @staticmethod
    def _burst(mode: str, turn_id: str, conversation_id: str | None):
        common = {"turn_id": turn_id, "conversation_id": conversation_id}
        if mode == "success":
            return (
                events.TurnStarted(**common),
                events.AssistantStarted(**common),
                events.AssistantDelta(text="hel", **common),
                events.AssistantComplete(text="hello", success=True, **common),
                events.AgentCompleted(success=True, **common),
            )
        if mode == "cancelled":
            return (
                events.TurnStarted(**common),
                events.TurnCancelled(reason="operator", **common),
                events.AgentCompleted(success=False, **common),
            )
        if mode == "started":
            return (events.TurnStarted(**common),)
        raise AssertionError(f"unsupported fixture mode: {mode}")


class BufferProbe:
    """Observe the real early-event buffer without timing sleeps."""

    def __init__(self, gateway: ZaraZmqGateway) -> None:
        self._condition = threading.Condition()
        self.count = 0
        original = gateway._buffer_early_turn_event

        def wrapped(_gateway, principal_id, turn_id, held):
            original(principal_id, turn_id, held)
            with self._condition:
                self.count += 1
                self._condition.notify_all()

        gateway._buffer_early_turn_event = MethodType(wrapped, gateway)

    def wait_for(self, count: int, timeout: float = 1.0) -> None:
        with self._condition:
            assert self._condition.wait_for(lambda: self.count >= count, timeout), (
                f"only {self.count} turn event(s) reached the demux buffer; expected {count}"
            )


class DispatchProbe:
    """Signal when a request id has crossed the gateway demux N times."""

    def __init__(self, gateway: ZaraZmqGateway, request_id: str, target: int) -> None:
        self._condition = threading.Condition()
        self.count = 0
        self.request_id = request_id
        self.target = target
        original = gateway._dispatch_runtime

        def wrapped(_gateway, socket, route, state, message):
            original(socket, route, state, message)
            if message.id == self.request_id:
                with self._condition:
                    self.count += 1
                    self._condition.notify_all()

        gateway._dispatch_runtime = MethodType(wrapped, gateway)

    def wait(self, timeout: float = 1.0) -> None:
        with self._condition:
            assert self._condition.wait_for(lambda: self.count >= self.target, timeout), (
                f"request {self.request_id} crossed demux {self.count} time(s); expected {self.target}"
            )


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
        event_queue_size=16,
        pending_request_limit=8,
    )


def _endpoint(label: str) -> str:
    return f"inproc://{label}-{time.time_ns()}"


def _dealer(context: zmq.Context, endpoint: str, config: TransportConfig) -> zmq.Socket:
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    dealer.connect(endpoint)
    return dealer


def _receive(dealer: zmq.Socket, *, timeout_ms: int = 1500) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) == zmq.POLLIN, "no ZARA/1 frame arrived"
    return decode_message(dealer.recv_multipart()).message


def _assert_quiet(dealer: zmq.Socket, *, timeout_ms: int = 50) -> None:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) != zmq.POLLIN, "unexpected duplicate/late frame"


def _hello(dealer: zmq.Socket, request_id: str) -> str:
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id=request_id,
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    response = _receive(dealer)
    assert response.type == "hello.ok"
    assert response.reply_to == request_id
    assert response.session_id
    return response.session_id


def _submit(
    dealer: zmq.Socket,
    *,
    session_id: str,
    request_id: str,
    conversation_id: str = "conversation-wire",
    text: str = "hello",
) -> None:
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="turn.submit",
                id=request_id,
                session_id=session_id,
                conversation_id=conversation_id,
                timestamp_ns=2,
                payload_count=0,
                body={"text": text},
            )
        )
    )


def _gateway(
    endpoint: str,
    supervisor: InterleavingSupervisor,
    context: zmq.Context,
    config: TransportConfig,
) -> ZaraZmqGateway:
    return ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=context,
        config=config,
    )


def test_acceptance_owns_burst_until_started_delta_result_and_completion_are_ordered(
    zmq_context,
    transport_config,
):
    """RuntimeHost-style events may all exist before submit() resolves.

    The ZARA/1 session owner must still put turn.accepted on the wire first,
    then preserve event sequence through assistant result and turn completion.
    """
    endpoint = _endpoint("turn-session-order")
    supervisor = InterleavingSupervisor(["success"], turn_ids=["turn-wire"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    probe = BufferProbe(gateway)
    gateway.start().result(timeout=1.0)
    dealer = _dealer(zmq_context, endpoint, transport_config)
    try:
        session_id = _hello(dealer, "hello-order")
        _submit(dealer, session_id=session_id, request_id="submit-order")
        assert supervisor.submitted[0].wait(1.0)
        probe.wait_for(5)

        supervisor.complete(0)
        delivered = [_receive(dealer) for _ in range(6)]
        assert [message.type for message in delivered] == [
            "turn.accepted",
            "turn.started",
            "assistant.started",
            "assistant.delta",
            "assistant.completed",
            "turn.completed",
        ]
        assert delivered[0].reply_to == "submit-order"
        assert all(message.session_id == session_id for message in delivered)
        assert all(message.turn_id == "turn-wire" for message in delivered)
        assert [message.seq for message in delivered[1:]] == [1, 2, 3, 4, 5]
        _assert_quiet(dealer)
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_cancellation_published_before_acceptance_cannot_overtake_turn_accepted(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint("turn-session-cancel")
    supervisor = InterleavingSupervisor(["cancelled"], turn_ids=["turn-cancelled"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    probe = BufferProbe(gateway)
    gateway.start().result(timeout=1.0)
    dealer = _dealer(zmq_context, endpoint, transport_config)
    try:
        session_id = _hello(dealer, "hello-cancel")
        _submit(dealer, session_id=session_id, request_id="submit-cancel")
        assert supervisor.submitted[0].wait(1.0)
        probe.wait_for(3)

        supervisor.complete(0)
        delivered = [_receive(dealer) for _ in range(4)]
        assert [message.type for message in delivered] == [
            "turn.accepted",
            "turn.started",
            "turn.cancelled",
            "turn.completed",
        ]
        assert delivered[2].body == {"reason": "operator"}
        assert delivered[3].body == {"success": False}
        _assert_quiet(dealer)
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_reconnect_duplicate_inflight_submit_rebinds_session_without_second_chat_turn(
    zmq_context,
    transport_config,
):
    """A reconnect may retry the same stable request while its turn is pending.

    The new session becomes the delivery owner, while idempotency keeps exactly
    one runtime SubmitTurn and one accepted/event sequence for the retried chat.
    """
    endpoint = _endpoint("turn-session-reconnect")
    supervisor = InterleavingSupervisor(["started"], turn_ids=["turn-stable"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    buffer_probe = BufferProbe(gateway)
    dispatch_probe = DispatchProbe(gateway, "submit-stable", target=2)
    gateway.start().result(timeout=1.0)
    dealer = _dealer(zmq_context, endpoint, transport_config)
    try:
        first_session = _hello(dealer, "hello-before-reconnect")
        _submit(
            dealer,
            session_id=first_session,
            request_id="submit-stable",
            text="perform once",
        )
        assert supervisor.submitted[0].wait(1.0)
        buffer_probe.wait_for(1)

        second_session = _hello(dealer, "hello-after-reconnect")
        assert second_session != first_session
        _submit(
            dealer,
            session_id=second_session,
            request_id="submit-stable",
            text="perform once",
        )
        dispatch_probe.wait()
        assert [command.request_id for command in supervisor.commands] == ["submit-stable"]

        supervisor.complete(0)
        delivered = [_receive(dealer) for _ in range(2)]
        assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
        assert [message.session_id for message in delivered] == [second_session, second_session]
        assert [message.turn_id for message in delivered] == ["turn-stable", "turn-stable"]
        _assert_quiet(dealer)
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_gateway_generation_restart_cannot_replay_late_buffer_into_reused_turn_id(
    zmq_context,
    transport_config,
):
    """Old-generation buffered events are not owned by a future session.

    Reusing a turn id after gateway process recreation is intentionally
    adversarial: a stale pre-accept event must not become a duplicate frame in
    the new session even if the old completion arrives late.
    """
    endpoint = _endpoint("turn-session-generation")
    supervisor = InterleavingSupervisor(
        ["started", "started"],
        turn_ids=["turn-reused", "turn-reused"],
    )
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    probe = BufferProbe(gateway)
    gateway.start().result(timeout=1.0)
    first = _dealer(zmq_context, endpoint, transport_config)
    try:
        first_session = _hello(first, "hello-old-generation")
        _submit(first, session_id=first_session, request_id="submit-old-generation")
        assert supervisor.submitted[0].wait(1.0)
        probe.wait_for(1)

        first.close(0)
        gateway.close(timeout=1.0)
        gateway.start().result(timeout=1.0)

        # The old future resolves after the new gateway generation exists. Its
        # completion callback is fenced and must not publish into that generation.
        supervisor.complete(0)

        second = _dealer(zmq_context, endpoint, transport_config)
        try:
            second_session = _hello(second, "hello-new-generation")
            _submit(second, session_id=second_session, request_id="submit-new-generation")
            assert supervisor.submitted[1].wait(1.0)
            probe.wait_for(2)
            supervisor.complete(1)

            delivered = [_receive(second) for _ in range(2)]
            assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
            assert [message.session_id for message in delivered] == [second_session, second_session]
            assert [message.turn_id for message in delivered] == ["turn-reused", "turn-reused"]
            _assert_quiet(second)
        finally:
            second.close(0)
    finally:
        if gateway.is_alive:
            gateway.close(timeout=1.0)
        for future in supervisor.futures:
            if not future.done():
                future.cancel()
