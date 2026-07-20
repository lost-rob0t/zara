"""Deterministic actor tests for ZARA-023.

Tests cover:
- Event ordering and message delivery
- Bounded mailboxes with overflow policy
- Stale-event rejection by turn ID
- Actor failure/restart policy
- Shutdown and drain
- Cancellation races (idempotent, scoped by turn ID)
"""

from __future__ import annotations

import threading
import time

import pykka
import pytest

from zara.actors import (
    BoundedMailbox,
    CancelTurn,
    CancellationCompleted,
    Drain,
    Event,
    FinalTranscript,
    InterruptionDetected,
    StartTurn,
    TTSPlayback,
    TurnCancelledReply,
    TurnCoordinator,
    TurnStartedReply,
    WakeDetected,
)


@pytest.fixture(autouse=True)
def cleanup_actors():
    yield
    pykka.ActorRegistry.stop_all()


def test_turn_coordinator_starts_and_stops():
    ref = TurnCoordinator.start()
    assert ref.is_alive()
    ref.stop()
    assert not ref.is_alive()


def test_start_turn_returns_unique_turn_ids():
    ref = TurnCoordinator.start()
    reply1 = ref.ask(StartTurn(), timeout=5)
    reply2 = ref.ask(StartTurn(), timeout=5)
    assert isinstance(reply1, TurnStartedReply)
    assert isinstance(reply2, TurnStartedReply)
    assert reply1.turn_id != reply2.turn_id
    ref.stop()


def test_events_for_active_turn_are_accepted():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    accepted = ref.ask(FinalTranscript(turn_id=turn_id, text_length=10), timeout=5)
    assert accepted is True
    ref.stop()


def test_events_for_unknown_turn_are_rejected():
    ref = TurnCoordinator.start()
    accepted = ref.ask(FinalTranscript(turn_id="nonexistent"), timeout=5)
    assert accepted is False
    ref.stop()


def test_events_for_cancelled_turn_are_rejected():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    ref.tell(CancelTurn(turn_id=turn_id))
    time.sleep(0.1)

    accepted = ref.ask(FinalTranscript(turn_id=turn_id, text_length=10), timeout=5)
    assert accepted is False
    ref.stop()


def test_cancellation_is_idempotent():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    result1 = ref.ask(CancelTurn(turn_id=turn_id), timeout=5)
    result2 = ref.ask(CancelTurn(turn_id=turn_id), timeout=5)

    assert isinstance(result1, TurnCancelledReply)
    assert isinstance(result2, TurnCancelledReply)
    assert result1.was_already_cancelled is False
    assert result2.was_already_cancelled is True
    ref.stop()


def test_cancelled_turn_cannot_emit_playback():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    ref.tell(CancelTurn(turn_id=turn_id))
    time.sleep(0.1)

    accepted = ref.ask(
        TTSPlayback(turn_id=turn_id, kind="final", success=True),
        timeout=5,
    )
    assert accepted is False
    ref.stop()


def test_independent_turns_are_not_affected_by_cancellation():
    ref = TurnCoordinator.start()
    reply1 = ref.ask(StartTurn(), timeout=5)
    reply2 = ref.ask(StartTurn(), timeout=5)

    ref.tell(CancelTurn(turn_id=reply1.turn_id))
    time.sleep(0.1)

    accepted = ref.ask(
        FinalTranscript(turn_id=reply2.turn_id, text_length=5),
        timeout=5,
    )
    assert accepted is True
    ref.stop()


def test_bounded_mailbox_drops_oldest_on_overflow():
    mb = BoundedMailbox(maxsize=2, overflow="drop_oldest")
    mb.put_nowait("a")
    mb.put_nowait("b")
    assert mb.qsize() == 2

    mb.put_nowait("c")
    assert mb.qsize() == 2
    assert mb.dropped_count == 1
    assert mb.get_nowait() == "b"
    assert mb.get_nowait() == "c"


def test_bounded_mailbox_drops_newest_on_overflow():
    mb = BoundedMailbox(maxsize=2, overflow="drop_newest")
    mb.put_nowait("a")
    mb.put_nowait("b")
    mb.put_nowait("c")
    assert mb.qsize() == 2
    assert mb.dropped_count == 1
    assert mb.get_nowait() == "a"
    assert mb.get_nowait() == "b"


def test_bounded_mailbox_block_policy_does_not_drop():
    mb = BoundedMailbox(maxsize=2, overflow="block")
    mb.put_nowait("a")
    mb.put_nowait("b")

    results = []

    def putter():
        try:
            mb.put("c", timeout=0.5)
            results.append("put")
        except Exception:
            results.append("timeout")

    t = threading.Thread(target=putter)
    t.start()
    time.sleep(0.1)
    assert mb.qsize() == 2
    assert mb.dropped_count == 0
    t.join(timeout=2)
    assert results == ["timeout"]


def test_telemetry_receives_events_for_active_turn():
    received = []

    class TelemetrySpy(pykka.ThreadingActor):
        def on_receive(self, message):
            if isinstance(message, Event):
                received.append(type(message).__name__)

    spy = TelemetrySpy.start()
    ref = TurnCoordinator.start(telemetry_ref=spy)
    reply = ref.ask(StartTurn(), timeout=5)

    ref.ask(WakeDetected(turn_id=reply.turn_id), timeout=5)
    ref.ask(FinalTranscript(turn_id=reply.turn_id, text_length=3), timeout=5)
    time.sleep(0.2)

    assert received == ["WakeDetected", "FinalTranscript"]
    ref.stop()
    spy.stop()


def test_telemetry_does_not_receive_events_for_cancelled_turn():
    received = []

    class TelemetrySpy(pykka.ThreadingActor):
        def on_receive(self, message):
            if isinstance(message, Event):
                received.append(type(message).__name__)

    spy = TelemetrySpy.start()
    ref = TurnCoordinator.start(telemetry_ref=spy)
    reply = ref.ask(StartTurn(), timeout=5)

    ref.tell(CancelTurn(turn_id=reply.turn_id))
    time.sleep(0.1)

    ref.ask(FinalTranscript(turn_id=reply.turn_id, text_length=3), timeout=5)
    time.sleep(0.2)

    assert received == []
    ref.stop()
    spy.stop()


def test_drain_clears_turn_state():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)

    ref.ask(Drain(), timeout=5)
    accepted = ref.ask(
        FinalTranscript(turn_id=reply.turn_id, text_length=3),
        timeout=5,
    )
    assert accepted is False
    ref.stop()


def test_actor_failure_does_not_corrupt_turn_state():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    class FailingActor(pykka.ThreadingActor):
        def on_receive(self, message):
            raise RuntimeError("boom")

    failing = FailingActor.start()
    failing.tell("trigger")
    time.sleep(0.2)

    assert not failing.is_alive()
    assert ref.is_alive()

    accepted = ref.ask(FinalTranscript(turn_id=turn_id, text_length=5), timeout=5)
    assert accepted is True
    ref.stop()


def test_cancellation_race_between_two_threads():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    results = []
    barrier = threading.Barrier(2)

    def canceller():
        barrier.wait()
        result = ref.ask(CancelTurn(turn_id=turn_id), timeout=5)
        results.append(result)

    def emitter():
        barrier.wait()
        result = ref.ask(
            FinalTranscript(turn_id=turn_id, text_length=5),
            timeout=5,
        )
        results.append(result)

    t1 = threading.Thread(target=canceller)
    t2 = threading.Thread(target=emitter)
    t1.start()
    t2.start()
    t1.join(timeout=5)
    t2.join(timeout=5)

    cancelled_results = [r for r in results if isinstance(r, TurnCancelledReply)]
    event_results = [r for r in results if isinstance(r, bool)]

    assert len(cancelled_results) == 1
    assert len(event_results) == 1
    ref.stop()


def test_interruption_and_cancellation_events_are_accepted_for_active_turn():
    ref = TurnCoordinator.start()
    reply = ref.ask(StartTurn(), timeout=5)
    turn_id = reply.turn_id

    accepted1 = ref.ask(InterruptionDetected(turn_id=turn_id), timeout=5)
    accepted2 = ref.ask(CancellationCompleted(turn_id=turn_id), timeout=5)
    assert accepted1 is True
    assert accepted2 is True
    ref.stop()
