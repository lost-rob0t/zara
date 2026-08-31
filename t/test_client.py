import queue
import time

import pytest

from zara.client import InProcessZaraClient, ZaraClientState
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import SubmitTurn
from zara.runtime.host import RuntimeNotReady


class EchoBackend(RuntimeBackend):
    def __init__(self):
        self.started = False
        self.stopped = False

    async def start(self):
        self.started = True

    async def submit_turn(self, text, *, turn_id, conversation_id=None, context_ids=(), latency_trace=None):
        return RuntimeTurnResult(response=f"echo:{text}")

    async def stop(self):
        self.stopped = True


class FailingBackend(RuntimeBackend):
    async def start(self):
        raise RuntimeError("backend startup failed")


def test_in_process_client_hides_runtime_host_and_delivers_events():
    backend = EchoBackend()
    client = InProcessZaraClient(backend_factory=lambda: backend, shutdown_timeout=1.0)
    subscription = client.subscribe()

    client.start().result(timeout=1.0)
    assert client.state is ZaraClientState.READY

    receipt = client.submit(SubmitTurn(text="hello")).result(timeout=1.0)
    assert receipt.turn_id

    response = None
    deadline = time.monotonic() + 1.0
    while time.monotonic() < deadline:
        envelope = subscription.get(timeout=max(0.01, deadline - time.monotonic()))
        if isinstance(envelope.event, events.ResponseText):
            response = envelope.event
            break

    assert response is not None
    assert response.text == "echo:hello"

    client.close(timeout=1.0)
    assert backend.stopped
    assert not client.is_alive
    assert client.state is ZaraClientState.STOPPED


def test_client_submit_before_start_fails_explicitly():
    client = InProcessZaraClient(
        backend_factory=EchoBackend,
        shutdown_timeout=0.2,
    )

    future = client.submit(SubmitTurn(text="too early"))
    with pytest.raises(RuntimeNotReady):
        future.result(timeout=0.2)

    client.close(timeout=0.2)


def test_client_close_before_start_is_idempotent_and_reports_stopped():
    client = InProcessZaraClient(
        backend_factory=EchoBackend,
        shutdown_timeout=0.2,
    )

    assert client.state is ZaraClientState.NEW
    client.close(timeout=0.2)
    client.close(timeout=0.2)

    assert not client.is_alive
    assert client.state is ZaraClientState.STOPPED


def test_client_maps_backend_start_failure_to_degraded_and_can_close():
    client = InProcessZaraClient(
        backend_factory=FailingBackend,
        shutdown_timeout=0.2,
    )

    with pytest.raises(RuntimeError, match="backend startup failed"):
        client.start().result(timeout=0.2)

    assert client.state is ZaraClientState.DEGRADED
    client.close(timeout=0.2)
    assert client.state is ZaraClientState.STOPPED


def test_client_subscription_queue_is_private_to_the_client():
    first_backend = EchoBackend()
    second_backend = EchoBackend()
    first = InProcessZaraClient(
        backend_factory=lambda: first_backend,
        shutdown_timeout=0.5,
    )
    second = InProcessZaraClient(
        backend_factory=lambda: second_backend,
        shutdown_timeout=0.5,
    )
    first_events = first.subscribe()
    second_events = second.subscribe()

    first.start().result(timeout=0.5)
    second.start().result(timeout=0.5)
    try:
        # Each client legitimately receives its own RuntimeStarted event. Clear
        # those local lifecycle events before testing cross-client turn leakage.
        first_events.drain()
        second_events.drain()

        first.submit(SubmitTurn(text="first-only")).result(timeout=0.5)

        deadline = time.monotonic() + 0.5
        observed = []
        while time.monotonic() < deadline:
            try:
                envelope = first_events.get(timeout=max(0.01, deadline - time.monotonic()))
            except queue.Empty:
                break
            observed.append(envelope.event)
            if isinstance(envelope.event, events.ResponseText):
                break

        assert any(
            isinstance(event, events.ResponseText) and event.text == "echo:first-only"
            for event in observed
        )
        leaked = second_events.drain()
        assert not any(
            isinstance(envelope.event, events.ResponseText)
            and envelope.event.text == "echo:first-only"
            for envelope in leaked
        )
    finally:
        first.close(timeout=0.5)
        second.close(timeout=0.5)
