from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import MagicMock

import pytest

from zara.runtime import bridge, events
from zara.runtime.clarification import (
    DialogueTemplate,
    OPEN_APP_TEMPLATE,
    SlotSpec,
    SlotType,
    TEXT_MESSAGE_TEMPLATE,
    TIMER_SET_TEMPLATE,
    ClarificationCoordinator,
    ClarificationSession,
    SessionCloseReason,
    SessionState,
    parse_boolean,
    parse_datetime,
    parse_duration,
    parse_number,
    parse_slot_value,
)
from zara.runtime.frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    NumberValue,
    RefValue,
    SlotOrigin,
    TextValue,
    validate_value,
)


def test_frame_validation_and_missing_lookup_boundaries():
    assert validate_value(TextValue(text="")) == "empty"
    assert validate_value(NumberValue(value=True)) == "not_a_number"
    assert validate_value(NumberValue(value="1")) == "not_a_number"
    assert validate_value(DurationValue(seconds=True)) == "not_an_integer"
    assert validate_value(DateTimeValue(2026, 1, 0, 0, 0, 0)) == "day_range"
    assert validate_value(DateTimeValue(2026, 1, 1, 24, 0, 0)) == "hour_range"
    assert validate_value(DateTimeValue(2026, 1, 1, 0, 60, 0)) == "minute_range"
    assert validate_value(DateTimeValue(2026, 1, 1, 0, 0, 60)) == "second_range"
    assert validate_value(RefValue(kind="contact", id=" ")) == "empty_id"
    assert validate_value(BoolValue(value=1)) == "not_a_boolean"
    assert validate_value(object()) == "unknown_type"
    frame = IntentFrame("x", "y")
    assert frame.slot_value("missing") is None
    assert frame.origin_of("missing") is None


def test_dialogue_parsers_cover_numeric_typed_and_size_boundaries():
    template = DialogueTemplate(
        "test",
        "typed",
        (
            SlotSpec("number", SlotType.NUMBER),
            SlotSpec("flag", SlotType.BOOLEAN),
            SlotSpec("when", SlotType.DATETIME),
            SlotSpec("ref", SlotType.REF, ref_kind="thing"),
            SlotSpec("text", SlotType.TEXT),
        ),
    )
    assert template.arg_order_names() == ("number", "flag", "when", "ref", "text")
    with pytest.raises(KeyError):
        template.spec("missing")
    assert parse_number("12") == NumberValue(12)
    assert parse_number("12.5") is None
    assert parse_number("two hundred") == NumberValue(200)
    assert parse_number("twenty one") == NumberValue(21)
    assert parse_number("one two") is None
    assert parse_number("twenty thirty") is None
    assert parse_number("wat") is None
    assert parse_duration("1") is None
    assert parse_duration("one fortnight") is None
    assert parse_duration("999999 hours") is None
    assert parse_duration("2 hours") == 7200
    assert parse_boolean("YES") == BoolValue(True)
    assert parse_boolean("off") == BoolValue(False)
    assert parse_boolean("yes please") is None
    assert parse_boolean("maybe") is None
    assert parse_datetime("not a date") is None
    assert parse_datetime("2026-09-24 17:04") == DateTimeValue(2026, 9, 24, 17, 4, 0)

    number, flag, when, ref, text = template.specs
    assert parse_slot_value(number, "", max_chars=10) is None
    assert parse_slot_value(number, "123456", max_chars=3) is None
    assert parse_slot_value(number, "45", max_chars=10) == NumberValue(45)
    assert parse_slot_value(flag, "true", max_chars=10) == BoolValue(True)
    assert parse_slot_value(when, "2026-09-24 17:04:59", max_chars=32) == DateTimeValue(2026, 9, 24, 17, 4, 59)
    assert parse_slot_value(ref, "alpha beta", max_chars=32) == RefValue("thing", "alpha beta")
    assert parse_slot_value(ref, "alpha beta gamma", max_chars=32) is None
    assert parse_slot_value(text, "  keep Case  ", max_chars=32) == TextValue("keep Case")


def test_session_closed_expired_invalid_replace_and_coordinator_prune_paths():
    ids = iter(("f1", "f2", "f3"))
    session = ClarificationSession(
        template=TIMER_SET_TEMPLATE,
        principal="p",
        conversation_id="c",
        frame_id="f0",
        frame=IntentFrame(
            intent_ns="device",
            intent_name="timer.set",
            slots=(FilledSlot("duration", DurationValue(10), SlotOrigin.UTTERANCE),),
            status=FrameStatus.COMPLETE,
        ),
        deadline=20,
        max_attempts=2,
        id_factory=lambda: next(ids),
    )
    session.state = SessionState.READY
    assert session.submit_value(DurationValue(5), now=10).reason == "already_complete"
    corrected = session.correct_value("duration", DurationValue(5), now=10)
    assert corrected.frame_id == "f1"
    assert corrected.supersedes == "f0"
    assert corrected.frame.origin_of("duration") is SlotOrigin.CORRECTION
    assert session.correct_value("duration", DurationValue(-1), now=11).kind == "retry"
    assert session.correct_value("duration", DurationValue(-1), now=12).reason == "attempts_exhausted"
    assert session.close(SessionCloseReason.CANCELLED, now=13).reason == "closed"
    assert session.submit_value(DurationValue(1), now=13).reason == "closed"
    assert session.correct_value("duration", DurationValue(1), now=13).reason == "closed"

    expired = ClarificationSession(
        template=TIMER_SET_TEMPLATE,
        principal="p",
        conversation_id="e",
        frame_id="e0",
        frame=IntentFrame("device", "timer.set", missing=("duration",)),
        deadline=1,
        max_attempts=2,
        id_factory=lambda: "e1",
    )
    assert expired.submit_value(DurationValue(1), now=2).reason == "expired"

    coordinator = ClarificationCoordinator(
        clock=lambda: 100.0,
        id_factory=iter((f"g{i}" for i in range(20))).__next__,
        session_ttl=5,
    )
    with pytest.raises(ValueError, match="invalid prefilled"):
        coordinator.open(TIMER_SET_TEMPLATE, principal="p", conversation_id="bad", prefilled={"duration": DurationValue(-1)})
    with pytest.raises(ValueError, match="unknown prefilled"):
        coordinator.open(TIMER_SET_TEMPLATE, principal="p", conversation_id="bad2", prefilled={"wat": TextValue("x")})
    invalid = coordinator.open(TIMER_SET_TEMPLATE, principal="p", conversation_id="invalid", invalid_slot="duration", invalid_reason="bad")
    assert invalid.question == "For how long?"
    assert coordinator.submit_correction("duration", DurationValue(1), principal="p", conversation_id="missing").reason == "no_active_dialogue"
    assert coordinator.cancel(principal="p", conversation_id="missing").reason == "no_active_dialogue"
    assert coordinator.finish(principal="p", conversation_id="missing") is False

    opened = coordinator.open(TEXT_MESSAGE_TEMPLATE, principal="p", conversation_id="text", prefilled={"contact": RefValue("contact", "alice")})
    assert opened.question == "What should the message say?"
    with pytest.raises(ValueError, match="unknown slot"):
        coordinator.submit_correction("missing", TextValue("x"), principal="p", conversation_id="text")
    result = coordinator.submit_follow_up("actually hello", principal="p", conversation_id="text")
    assert result.frame.slot_value("message") == TextValue("hello")
    assert coordinator.submit_follow_up("ordinary", principal="p", conversation_id="text").reason == "already_complete"
    assert coordinator.cancel(principal="p", conversation_id="text", reason=SessionCloseReason.SHUTDOWN).reason == "shutdown"
    old = coordinator.session_for("p", "text")
    old.closed_at = 90.0
    coordinator.open(OPEN_APP_TEMPLATE, principal="p", conversation_id="new", now=100.0)
    assert coordinator.session_for("p", "text") is None


def test_event_bus_backpressure_sink_isolation_and_compatibility_wrappers(monkeypatch):
    bus = bridge.RuntimeEventBus()
    with pytest.raises(ValueError):
        bus.subscribe(maxsize=-1)
    with pytest.raises(ValueError):
        bus.register_legacy_sink("", lambda _event: None)
    with pytest.raises(TypeError):
        bus.publish(object())

    sub = bus.subscribe(maxsize=1)
    assert bus.publish(events.RuntimeStarted()).sequence == 1
    assert bus.publish(events.RuntimeIdle()).sequence == 2
    assert sub.dropped_count == 1
    assert [item.sequence for item in sub.drain(limit=1)] == [2]
    assert sub.drain() == []
    sub.close()
    sub.close()
    bus.publish(events.RuntimeIdle())
    assert sub.drain() == []

    good = []
    bus.register_legacy_sink("good", good.append)
    bus.register_legacy_sink("bad", MagicMock(side_effect=RuntimeError("boom")))
    stopped = events.RuntimeStopped(reason="done")
    bus.publish(stopped)
    assert good == [stopped]
    bus.unregister_legacy_sink("good")
    bus.unregister_legacy_sink("bad")
    bus.unregister_legacy_sink("missing")

    emitted = []
    monkeypatch.setattr(bridge, "publish", lambda event: emitted.append(event) or SimpleNamespace(event=event))
    bridge.model_started("m", turn_id="t", conversation_id="c")
    bridge.model_streaming("m", text="chunk", turn_id="t", conversation_id="c")
    bridge.model_completed(False, "m", text="done", turn_id="t", conversation_id="c")
    bridge.model_failed("bad", "m", turn_id="t", conversation_id="c")
    bridge.tool_started("tool", tool_run_id="run", turn_id="t", conversation_id="c")
    bridge.tool_completed(False, "tool", tool_run_id="run", turn_id="t", conversation_id="c")
    bridge.agent_started("a", turn_id="t", conversation_id="c")
    bridge.agent_completed(False, "a", turn_id="t", conversation_id="c")
    bridge.user_input_required("approval", "u", turn_id="t", conversation_id="c")
    bridge.user_responded("u", turn_id="t", conversation_id="c")
    bridge.output_ready("o", turn_id="t", conversation_id="c")
    bridge.response_text("x" * 281, "o", turn_id="t", conversation_id="c")
    bridge.output_seen("o", turn_id="t", conversation_id="c")
    bridge.task_cancelled("task", reason="cancel", turn_id="t", conversation_id="c")
    bridge.runtime_idle("runtime")
    bridge.provider_unavailable("offline", "provider")
    assert len(emitted) == 16
    response = next(event for event in emitted if isinstance(event, events.ResponseText))
    assert len(response.text) == 280 and response.truncated is True
