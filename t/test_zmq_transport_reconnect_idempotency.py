from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import (
    ClientDisconnected,
    ProtocolRemoteError,
    TransportConfig,
    ZaraZmqGateway,
    ZmqZaraClient,
)


class CountingSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(request_id=command.request_id, turn_id="turn-stable")
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)


class DeferredSupervisor(CountingSupervisor):
    def __init__(self) -> None:
        super().__init__()
        self.future = concurrent.futures.Future()

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        return self.future


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def _config(*, cache_size: int = 8) -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        pending_request_limit=8,
        idempotency_cache_size=cache_size,
    )


def test_duplicate_submit_id_survives_client_reconnect_without_second_side_effect(zmq_context):
    config = _config()
    endpoint = f"inproc://reconnect-idempotency-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        command = SubmitTurn(request_id="stable-across-reconnect", text="do this once")
        first = client.submit(command).result(timeout=1.0)

        client.reconnect().result(timeout=1.0)
        second = client.submit(command).result(timeout=1.0)

        assert second == first
        submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
        assert submitted == [command]
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_reusing_request_id_for_different_command_fails_closed_after_reconnect(zmq_context):
    config = _config()
    endpoint = f"inproc://reconnect-idempotency-conflict-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        first = SubmitTurn(request_id="stable-conflict", text="first effect")
        client.submit(first).result(timeout=1.0)
        client.reconnect().result(timeout=1.0)

        conflicting = SubmitTurn(request_id="stable-conflict", text="different effect")
        with pytest.raises(ProtocolRemoteError) as captured:
            client.submit(conflicting).result(timeout=1.0)

        assert captured.value.code == "idempotency_conflict"
        assert captured.value.retryable is False
        submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
        assert submitted == [first]
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_inflight_duplicate_after_reconnect_attaches_to_original_side_effect(zmq_context):
    config = _config()
    endpoint = f"inproc://reconnect-idempotency-inflight-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = DeferredSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        command = SubmitTurn(request_id="inflight-across-reconnect", text="slow effect")
        abandoned = client.submit(command)
        deadline = time.monotonic() + 1.0
        while len(supervisor.commands) != 1 and time.monotonic() < deadline:
            time.sleep(0.005)
        assert [item for _, item in supervisor.commands] == [command]

        client.reconnect().result(timeout=1.0)
        with pytest.raises(ClientDisconnected):
            abandoned.result(timeout=1.0)

        retried = client.submit(command)
        time.sleep(0.05)
        assert [item for _, item in supervisor.commands] == [command]

        supervisor.future.set_result(
            CommandReceipt(request_id=command.request_id, turn_id="turn-inflight")
        )
        receipt = retried.result(timeout=1.0)
        assert receipt.request_id == command.request_id
        assert receipt.turn_id == "turn-inflight"
        assert [item for _, item in supervisor.commands] == [command]
    finally:
        if not supervisor.future.done():
            supervisor.future.cancel()
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_idempotency_cache_is_bounded_and_eviction_allows_old_id_to_execute_again(zmq_context):
    config = _config(cache_size=2)
    endpoint = f"inproc://reconnect-idempotency-eviction-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        first = SubmitTurn(request_id="cache-1", text="one")
        second = SubmitTurn(request_id="cache-2", text="two")
        third = SubmitTurn(request_id="cache-3", text="three")
        client.submit(first).result(timeout=1.0)
        client.submit(second).result(timeout=1.0)
        client.submit(third).result(timeout=1.0)

        client.reconnect().result(timeout=1.0)
        client.submit(first).result(timeout=1.0)

        submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
        assert submitted == [first, second, third, first]
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_reconnect_reopens_selected_conversation_before_unscoped_submit(zmq_context):
    config = _config()
    endpoint = f"inproc://reconnect-conversation-continuity-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)
        opened = client.open_conversation("durable-conversation").result(timeout=1.0)
        assert opened == "durable-conversation"

        client.reconnect().result(timeout=1.0)
        command = SubmitTurn(request_id="after-reconnect", text="continue here")
        client.submit(command).result(timeout=1.0)

        submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
        assert len(submitted) == 1
        assert submitted[0].conversation_id == "durable-conversation"
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_reconnect_discards_queued_old_session_events_but_keeps_subscription_live(zmq_context):
    config = _config()
    endpoint = f"inproc://reconnect-stale-events-{time.time_ns()}"
    principal = PrincipalContext("local-owner")
    supervisor = CountingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)
    subscription = client.subscribe()

    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)
        client.open_conversation("durable-conversation").result(timeout=1.0)

        supervisor.bus.publish(
            events.AssistantDelta(
                turn_id="old-turn",
                conversation_id="durable-conversation",
                text="stale",
            )
        )
        deadline = time.monotonic() + 1.0
        while subscription._queue.empty() and time.monotonic() < deadline:
            time.sleep(0.005)
        assert not subscription._queue.empty()

        client.reconnect().result(timeout=1.0)
        assert subscription.drain() == []

        supervisor.bus.publish(
            events.AssistantDelta(
                turn_id="new-turn",
                conversation_id="durable-conversation",
                text="current",
            )
        )
        deadline = time.monotonic() + 1.0
        while subscription._queue.empty() and time.monotonic() < deadline:
            time.sleep(0.005)
        delivered = subscription.drain()
        assert [envelope.event.text for envelope in delivered] == ["current"]
    finally:
        subscription.close()
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)
