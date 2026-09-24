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
        if mode == "approval":
            return (
                events.ToolWaitingForUser(
                    tool_run_id="tool-preaccept",
                    tool_name="reviewed_effect",
                    prompt="Approve reviewed_effect?",
                    **common,
                ),
            )
        if mode == "none":
            return ()
        raise AssertionError(f"unsupported fixture mode: {mode}")


class BufferProbe:
    """Observe the real early-event buffer without timing sleeps."""

    def __init__(self, gateway: ZaraZmqGateway) -> None:
        self._condition = threading.Condition()
        self.count = 0
        original = gateway._buffer_early_turn_event

        def wrapped(_gateway, principal_id, turn_id, held):
            buffered = original(principal_id, turn_id, held)
            with self._condition:
                self.count += 1
                self._condition.notify_all()
            return buffered

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

        key = ("local-owner", "turn-wire")
        deadline = time.monotonic() + 1.0
        while time.monotonic() < deadline:
            with gateway._lock:
                if (
                    key not in gateway._turn_routes
                    and key not in gateway._early_turn_events
                    and key not in gateway._turns_awaiting_accept
                ):
                    break
            time.sleep(0.005)
        with gateway._lock:
            assert key not in gateway._turn_routes
            assert key not in gateway._early_turn_events
            assert key not in gateway._turns_awaiting_accept

        supervisor.bus.publish(
            events.AssistantDelta(
                turn_id="turn-wire",
                conversation_id="conversation-wire",
                label="runtime-host",
                text="late-after-terminal",
            )
        )
        _assert_quiet(dealer, timeout_ms=100)
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




def test_distinct_route_retry_becomes_only_inflight_turn_delivery_owner(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint("turn-session-distinct-route-retry")
    supervisor = InterleavingSupervisor(["started"], turn_ids=["turn-stable-distinct"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    buffer_probe = BufferProbe(gateway)
    gateway.start().result(timeout=1.0)
    first = _dealer(zmq_context, endpoint, transport_config)
    second = _dealer(zmq_context, endpoint, transport_config)
    try:
        first_session = _hello(first, "hello-first-route")
        _submit(
            first,
            session_id=first_session,
            request_id="submit-stable-distinct",
            text="perform once",
        )
        assert supervisor.submitted[0].wait(1.0)
        buffer_probe.wait_for(1)

        second_session = _hello(second, "hello-second-route")
        _submit(
            second,
            session_id=second_session,
            request_id="submit-stable-distinct",
            text="perform once",
        )
        assert len(supervisor.commands) == 1

        supervisor.complete(0)
        delivered = [_receive(second) for _ in range(2)]
        assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
        assert [message.session_id for message in delivered] == [second_session, second_session]
        assert [message.turn_id for message in delivered] == [
            "turn-stable-distinct",
            "turn-stable-distinct",
        ]
        _assert_quiet(first, timeout_ms=100)
        _assert_quiet(second)
    finally:
        first.close(0)
        second.close(0)
        gateway.close(timeout=1.0)


def test_replay_before_accept_wire_transfers_barrier_to_retry_route(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint("turn-session-prewire-replay-transfer")
    supervisor = InterleavingSupervisor(["started"], turn_ids=["turn-prewire-retry"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    release_drain = threading.Event()
    original_drain = gateway._drain_outbound

    def gated_drain(_gateway, socket):
        with _gateway._lock:
            pending = ("local-owner", "turn-prewire-retry") in _gateway._turns_awaiting_accept
        if pending and not release_drain.is_set():
            return
        original_drain(socket)

    gateway._drain_outbound = MethodType(gated_drain, gateway)
    gateway.start().result(timeout=1.0)
    first = _dealer(zmq_context, endpoint, transport_config)
    second = _dealer(zmq_context, endpoint, transport_config)
    try:
        first_session = _hello(first, "hello-prewire-first")
        _submit(
            first,
            session_id=first_session,
            request_id="submit-prewire-retry",
            text="perform once",
        )
        assert supervisor.submitted[0].wait(1.0)
        supervisor.complete(0)

        key = ("local-owner", "turn-prewire-retry")
        deadline = time.monotonic() + 1.0
        while time.monotonic() < deadline:
            with gateway._lock:
                first_route = gateway._turn_routes.get(key)
                first_queue = gateway._route_outbound.get(first_route) if first_route else None
                if (
                    key in gateway._turns_awaiting_accept
                    and first_queue
                    and any(item.message.type == "turn.accepted" for item in first_queue)
                ):
                    break
            time.sleep(0.005)
        with gateway._lock:
            assert key in gateway._turns_awaiting_accept
            old_route = gateway._turn_routes[key]

        second_session = _hello(second, "hello-prewire-second")
        _submit(
            second,
            session_id=second_session,
            request_id="submit-prewire-retry",
            text="perform once",
        )

        deadline = time.monotonic() + 1.0
        while time.monotonic() < deadline:
            with gateway._lock:
                new_route = gateway._turn_routes.get(key)
                if new_route is not None and new_route != old_route:
                    old_queue = gateway._route_outbound.get(old_route, ())
                    if not any(item.message.type == "turn.accepted" for item in old_queue):
                        break
            time.sleep(0.005)

        with gateway._lock:
            assert gateway._turn_routes[key] != old_route
            assert not any(
                item.message.type == "turn.accepted"
                for item in gateway._route_outbound.get(old_route, ())
            )

        release_drain.set()
        delivered = [_receive(second) for _ in range(2)]
        assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
        assert [message.session_id for message in delivered] == [second_session, second_session]
        _assert_quiet(first, timeout_ms=100)
        _assert_quiet(second)
        assert len(supervisor.commands) == 1
    finally:
        release_drain.set()
        first.close(0)
        second.close(0)
        gateway.close(timeout=1.0)


def test_old_generation_completion_cannot_erase_reused_request_inflight_owner(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint("turn-session-generation-aba")
    supervisor = InterleavingSupervisor(
        ["started", "started", "started"],
        turn_ids=["turn-old", "turn-current", "turn-duplicate"],
    )
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    gateway.start().result(timeout=1.0)
    first = _dealer(zmq_context, endpoint, transport_config)
    try:
        first_session = _hello(first, "hello-old-aba")
        _submit(
            first,
            session_id=first_session,
            request_id="submit-stable-aba",
            text="same stable command",
        )
        assert supervisor.submitted[0].wait(1.0)

        first.close(0)
        gateway.close(timeout=1.0)
        gateway.start().result(timeout=1.0)

        second = _dealer(zmq_context, endpoint, transport_config)
        try:
            second_session = _hello(second, "hello-current-aba")
            _submit(
                second,
                session_id=second_session,
                request_id="submit-stable-aba",
                text="same stable command",
            )
            assert supervisor.submitted[1].wait(1.0)
            assert len(supervisor.commands) == 2

            supervisor.complete(0)
            _submit(
                second,
                session_id=second_session,
                request_id="submit-stable-aba",
                text="same stable command",
            )
            assert not supervisor.submitted[2].wait(0.1), (
                "a stale generation completion must not remove the current "
                "generation's stable-request inflight owner"
            )
            assert len(supervisor.commands) == 2

            supervisor.complete(1)
            delivered = [_receive(second) for _ in range(2)]
            assert [message.type for message in delivered] == ["turn.accepted", "turn.started"]
            assert [message.turn_id for message in delivered] == ["turn-current", "turn-current"]
            _assert_quiet(second)
        finally:
            second.close(0)
    finally:
        if gateway.is_alive:
            gateway.close(timeout=1.0)
        for future in supervisor.futures:
            if not future.done():
                future.cancel()


def test_preaccept_tool_waiting_installs_owner_only_when_prompt_becomes_deliverable(
    zmq_context,
    transport_config,
):
    endpoint = _endpoint("turn-session-preaccept-approval")
    supervisor = InterleavingSupervisor(["approval"], turn_ids=["turn-approval"])
    gateway = _gateway(endpoint, supervisor, zmq_context, transport_config)
    probe = BufferProbe(gateway)
    gateway.start().result(timeout=1.0)
    dealer = _dealer(zmq_context, endpoint, transport_config)
    try:
        session_id = _hello(dealer, "hello-approval")
        _submit(dealer, session_id=session_id, request_id="submit-approval")
        assert supervisor.submitted[0].wait(1.0)
        probe.wait_for(1)
        with gateway._lock:
            assert ("local-owner", "tool-preaccept") not in gateway._approval_owners

        supervisor.complete(0)
        delivered = [_receive(dealer) for _ in range(2)]
        assert [message.type for message in delivered] == ["turn.accepted", "tool.waiting"]
        assert delivered[1].body["tool_run_id"] == "tool-preaccept"
        with gateway._lock:
            owner = gateway._approval_owners[("local-owner", "tool-preaccept")]
            assert owner.session_id == session_id
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_post_future_pre_wire_overflow_replaces_acceptance_with_backpressure(
    zmq_context,
):
    endpoint = _endpoint("turn-session-post-future-overflow")
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
    )
    supervisor = InterleavingSupervisor(["none"], turn_ids=["turn-post-future"])
    gateway = _gateway(endpoint, supervisor, zmq_context, config)
    original_drain = gateway._drain_outbound

    def gated_drain(_gateway, socket):
        with _gateway._lock:
            awaiting = ("local-owner", "turn-post-future") in _gateway._turns_awaiting_accept
        if awaiting:
            return
        original_drain(socket)

    gateway._drain_outbound = MethodType(gated_drain, gateway)
    gateway.start().result(timeout=1.0)
    dealer = _dealer(zmq_context, endpoint, config)
    try:
        session_id = _hello(dealer, "hello-post-future")
        _submit(dealer, session_id=session_id, request_id="submit-post-future")
        assert supervisor.submitted[0].wait(1.0)
        supervisor.complete(0)

        deadline = time.monotonic() + 1.0
        key = ("local-owner", "turn-post-future")
        while time.monotonic() < deadline:
            with gateway._lock:
                if key in gateway._turns_awaiting_accept:
                    break
            time.sleep(0.005)
        with gateway._lock:
            assert key in gateway._turns_awaiting_accept

        for index in range(129):
            supervisor.bus.publish(
                events.AssistantDelta(
                    turn_id="turn-post-future",
                    conversation_id="conversation-wire",
                    label="runtime-host",
                    text=f"late-{index:03d}",
                )
            )

        response = _receive(dealer)
        assert response.type == "protocol.error"
        assert response.reply_to == "submit-post-future"
        assert response.body == {
            "code": "server_backpressure",
            "message": "too many turn events are awaiting acceptance",
            "retryable": True,
        }
        with gateway._lock:
            assert key not in gateway._turn_routes
            assert key not in gateway._early_turn_events
            assert key not in gateway._turns_awaiting_accept
        _assert_quiet(dealer, timeout_ms=100)
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_gateway_generation_restart_fences_late_old_completion_and_frames(
    zmq_context,
    transport_config,
):
    """A completion owned by an old gateway generation stays off the new wire."""
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
            _assert_quiet(second)

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
