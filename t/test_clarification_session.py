from __future__ import annotations

import threading

from zara.runtime.clarification import (
    ClarificationCoordinator,
    TIMER_SET_TEMPLATE,
    TEXT_MESSAGE_TEMPLATE,
    OPEN_APP_TEMPLATE,
    strip_correction_marker,
)
from zara.runtime.frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    FrameStatus,
    IntentFrame,
    NumberValue,
    RefValue,
    SlotOrigin,
    TextValue,
    validate_value,
)


class FakeClock:
    def __init__(self) -> None:
        self.now_value = 1000.0

    def __call__(self) -> float:
        return self.now_value

    def advance(self, seconds: float) -> None:
        self.now_value += seconds


def make_ids():
    counter = iter(range(1000))

    def factory() -> str:
        return f"frame-{next(counter):03d}"

    return factory


def make_coordinator(clock=None, **kwargs) -> tuple[ClarificationCoordinator, FakeClock]:
    clock = clock or FakeClock()
    coordinator = ClarificationCoordinator(
        clock=clock,
        id_factory=make_ids(),
        **kwargs,
    )
    return coordinator, clock


def open_timer(coordinator, principal="alice", conversation="c1", **kwargs):
    return coordinator.open(
        TIMER_SET_TEMPLATE,
        principal=principal,
        conversation_id=conversation,
        **kwargs,
    )


def test_timer_missing_duration_asks_then_completes():
    coordinator, _clock = make_coordinator()

    opened = open_timer(coordinator)

    assert opened.kind == "opened"
    assert opened.question == "For how long?"
    assert opened.session.frame.status is FrameStatus.MISSING
    assert opened.session.frame.missing == ("duration",)
    assert opened.session.state == "eliciting"

    outcome = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )

    assert outcome.kind == "complete"
    assert outcome.frame.status is FrameStatus.COMPLETE
    duration = outcome.frame.slot_value("duration")
    assert duration == DurationValue(seconds=1200)
    assert outcome.frame.origin_of("duration") is SlotOrigin.FOLLOW_UP
    assert outcome.session.state == "ready"


def test_invalid_answer_retries_focused_without_state_change():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator)

    outcome = coordinator.submit_follow_up(
        "bananas", principal="alice", conversation_id="c1"
    )

    assert outcome.kind == "retry"
    assert outcome.question == "For how long?"
    assert outcome.session.state == "eliciting"
    assert outcome.frame.status is FrameStatus.MISSING
    assert outcome.frame.slots == ()

    recovered = coordinator.submit_follow_up(
        "90 seconds", principal="alice", conversation_id="c1"
    )
    assert recovered.kind == "complete"
    assert recovered.frame.slot_value("duration") == DurationValue(seconds=90)


def test_cancel_closes_and_late_answer_is_not_consumed():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator)

    cancelled = coordinator.submit_follow_up(
        "never mind", principal="alice", conversation_id="c1"
    )
    assert cancelled.kind == "cancelled"
    assert cancelled.message == "Cancelled."
    assert cancelled.frame.status is FrameStatus.CANCELLED
    assert cancelled.session.state == "closed"

    late = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert late.kind == "stale"
    assert late.message == ClarificationCoordinator.STALE_MESSAGE
    assert late.frame is None

    assert coordinator.active_question("alice", "c1") is None


def test_correction_before_execution_supersedes():
    coordinator, _clock = make_coordinator()
    opened = open_timer(coordinator)
    first = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert first.kind == "complete"
    prior_frame_id = first.frame_id

    corrected = coordinator.submit_follow_up(
        "actually five minutes", principal="alice", conversation_id="c1"
    )

    assert corrected.kind == "complete"
    assert corrected.frame.slot_value("duration") == DurationValue(seconds=300)
    assert corrected.frame.origin_of("duration") is SlotOrigin.CORRECTION
    assert corrected.supersedes == prior_frame_id
    assert corrected.frame_id != prior_frame_id
    assert corrected.session.state == "ready"


def test_explicit_correction_targets_named_slot():
    coordinator, _clock = make_coordinator()
    opened = coordinator.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="c1",
        prefilled={"contact": RefValue(kind="contact", id="sarah")},
    )
    assert opened.question == "What should the message say?"

    corrected = coordinator.submit_correction(
        "contact", RefValue(kind="contact", id="bob"), principal="alice", conversation_id="c1"
    )

    assert corrected.kind == "filled"
    assert corrected.frame.slot_value("contact") == RefValue(kind="contact", id="bob")
    assert corrected.frame.origin_of("contact") is SlotOrigin.CORRECTION
    assert corrected.frame.status is FrameStatus.MISSING
    assert corrected.frame.missing == ("message",)

    completed = coordinator.submit_follow_up(
        "call me at noon", principal="alice", conversation_id="c1"
    )
    assert completed.kind == "complete"
    assert completed.frame.slot_value("message") == TextValue(text="call me at noon")


def test_open_app_dialogue_fills_ref_slot():
    coordinator, _clock = make_coordinator()
    opened = coordinator.open(
        OPEN_APP_TEMPLATE, principal="alice", conversation_id="c1"
    )
    assert opened.question == "Which app?"

    outcome = coordinator.submit_follow_up(
        "Firefox", principal="alice", conversation_id="c1"
    )

    assert outcome.kind == "complete"
    assert outcome.frame.slot_value("target") == RefValue(kind="app_alias", id="firefox")


def test_text_prefilled_contact_asks_message():
    coordinator, _clock = make_coordinator()
    opened = coordinator.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="c1",
        prefilled={"contact": RefValue(kind="contact", id="sarah")},
    )

    assert opened.kind == "opened"
    assert opened.question == "What should the message say?"
    assert opened.session.frame.status is FrameStatus.MISSING
    assert opened.session.frame.missing == ("message",)
    assert opened.session.frame.origin_of("contact") is SlotOrigin.UTTERANCE


def test_two_missing_slots_ask_in_deterministic_order():
    coordinator, _clock = make_coordinator()
    opened = coordinator.open(
        TEXT_MESSAGE_TEMPLATE, principal="alice", conversation_id="c1"
    )

    assert opened.question == "Who should I message?"
    assert opened.session.frame.missing == ("contact", "message")

    first = coordinator.submit_follow_up("sarah", principal="alice", conversation_id="c1")
    assert first.kind == "filled"
    assert first.question == "What should the message say?"
    assert first.frame.missing == ("message",)

    second = coordinator.submit_follow_up("hi", principal="alice", conversation_id="c1")
    assert second.kind == "complete"
    assert second.frame.slot_value("contact") == RefValue(kind="contact", id="sarah")
    assert second.frame.slot_value("message") == TextValue(text="hi")


def test_principals_with_identical_dialogues_stay_isolated():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator, principal="alice", conversation="shared")
    open_timer(coordinator, principal="bob", conversation="shared")

    alice_fill = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="shared"
    )
    assert alice_fill.kind == "complete"

    bob_question = coordinator.active_question("bob", "shared")
    assert bob_question == "For how long?"

    bob_fill = coordinator.submit_follow_up(
        "twenty minutes", principal="bob", conversation_id="shared"
    )
    assert bob_fill.kind == "complete"
    assert bob_fill.frame_id != alice_fill.frame_id


def test_two_conversations_same_principal_do_not_steal():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator, principal="alice", conversation="phone")
    open_timer(coordinator, principal="alice", conversation="desktop")

    phone_fill = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="phone"
    )
    assert phone_fill.kind == "complete"

    assert coordinator.active_question("alice", "desktop") == "For how long?"
    desktop_fill = coordinator.submit_follow_up(
        "five minutes", principal="alice", conversation_id="desktop"
    )
    assert desktop_fill.frame.slot_value("duration") == DurationValue(seconds=300)


def test_stale_rejections_are_uniform_across_reasons():
    coordinator, clock = make_coordinator()

    no_session = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert no_session.kind == "stale"

    opened = open_timer(coordinator, principal="alice", conversation="expired")
    clock.advance(1000.0)
    expired = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="expired"
    )
    assert expired.kind == "stale"
    assert expired.session.frame.status is FrameStatus.CANCELLED
    assert expired.session.state == "closed"
    assert opened.session is not None

    open_timer(coordinator, principal="alice", conversation="cancelled")
    coordinator.cancel(principal="alice", conversation_id="cancelled")
    cancelled_late = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="cancelled"
    )
    assert cancelled_late.kind == "stale"

    open_timer(coordinator, principal="alice", conversation="restarted")
    coordinator.drop_all(reason="restart")
    restarted_late = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="restarted"
    )
    assert restarted_late.kind == "stale"

    messages = {
        no_session.message,
        expired.message,
        cancelled_late.message,
        restarted_late.message,
    }
    assert messages == {ClarificationCoordinator.STALE_MESSAGE}


def test_concurrent_submits_cannot_double_hold():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator)

    first = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    second = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )

    assert first.kind == "complete"
    assert second.kind == "stale"
    assert second.message == ClarificationCoordinator.STALE_MESSAGE


def test_cancel_racing_fill_is_serialized():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator)
    results: list[str] = []
    lock = threading.Lock()

    def fill():
        outcome = coordinator.submit_follow_up(
            "twenty minutes", principal="alice", conversation_id="c1"
        )
        with lock:
            results.append(outcome.kind)
    def cancel():
        outcome = coordinator.cancel(principal="alice", conversation_id="c1")
        with lock:
            results.append(outcome.kind)

    threads = [threading.Thread(target=fill), threading.Thread(target=cancel)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

    assert sorted(results) in (["cancelled", "complete"], ["cancelled", "stale"])
    assert coordinator.session_for("alice", "c1").state == "closed"


def test_capacity_rejection_is_bounded_and_uniform():
    coordinator, _clock = make_coordinator(max_sessions=2)
    open_timer(coordinator, principal="alice", conversation="a")
    open_timer(coordinator, principal="bob", conversation="b")

    rejected = coordinator.open(
        TIMER_SET_TEMPLATE, principal="carol", conversation_id="c"
    )

    assert rejected.kind == "capacity"
    assert rejected.session is None
    assert rejected.message == ClarificationCoordinator.CAPACITY_MESSAGE

    assert coordinator.active_question("alice", "a") == "For how long?"
    assert coordinator.active_question("bob", "b") == "For how long?"


def test_timeout_closes_session_with_virtual_clock():
    coordinator, clock = make_coordinator(session_ttl=60.0)
    open_timer(coordinator, principal="alice", conversation="c1")

    clock.advance(59.0)
    alive = coordinator.submit_follow_up("bananas", principal="alice", conversation_id="c1")
    assert alive.kind == "retry"

    clock.advance(2.0)
    outcome = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert outcome.kind == "stale"
    assert outcome.message == ClarificationCoordinator.STALE_MESSAGE

    expired_ids = coordinator.expire()
    assert expired_ids == ()
    assert coordinator.session_for("alice", "c1").frame.status is FrameStatus.CANCELLED


def test_expire_closes_only_elapsed_sessions():
    coordinator, clock = make_coordinator(session_ttl=60.0)
    open_timer(coordinator, principal="alice", conversation="old")
    clock.advance(100.0)
    open_timer(coordinator, principal="alice", conversation="new")

    closed = coordinator.expire()

    assert len(closed) == 1
    assert coordinator.session_for("alice", "old").state == "closed"
    assert coordinator.session_for("alice", "new").state == "eliciting"


def test_repeated_invalid_answers_exhaust_bounded_attempts():
    coordinator, _clock = make_coordinator(max_attempts=3)
    open_timer(coordinator, principal="alice", conversation="c1")

    first = coordinator.submit_follow_up("bananas", principal="alice", conversation_id="c1")
    second = coordinator.submit_follow_up("bananas", principal="alice", conversation_id="c1")
    assert first.kind == "retry"
    assert second.kind == "retry"

    third = coordinator.submit_follow_up("bananas", principal="alice", conversation_id="c1")
    assert third.kind == "stale"
    assert third.message == ClarificationCoordinator.STALE_MESSAGE
    assert coordinator.session_for("alice", "c1").state == "closed"
    assert coordinator.session_for("alice", "c1").frame.status is FrameStatus.CANCELLED


def test_oversized_answer_is_retried_and_never_stored():
    coordinator, _clock = make_coordinator(max_value_chars=16)
    open_timer(coordinator, principal="alice", conversation="c1")

    outcome = coordinator.submit_follow_up(
        "x" * 32 + " minutes", principal="alice", conversation_id="c1"
    )

    assert outcome.kind == "retry"
    assert outcome.frame.slots == ()
    assert outcome.session.attempts == 1


def test_ambiguous_frame_requires_explicit_choice():
    coordinator, _clock = make_coordinator()
    opened = open_timer(
        coordinator,
        alternatives=("twenty minutes", "five minutes"),
    )

    assert opened.session.frame.status is FrameStatus.AMBIGUOUS
    assert opened.question == "Did you mean: twenty minutes or five minutes?"

    wrong = coordinator.submit_follow_up("bananas", principal="alice", conversation_id="c1")
    assert wrong.kind == "retry"

    chosen = coordinator.submit_follow_up(
        "Five Minutes", principal="alice", conversation_id="c1"
    )
    assert chosen.kind == "complete"
    assert chosen.frame.status is FrameStatus.COMPLETE
    assert chosen.frame.slot_value("duration") == DurationValue(seconds=300)


def test_new_dialogue_supersedes_open_session_in_conversation():
    coordinator, _clock = make_coordinator()
    first = open_timer(coordinator, principal="alice", conversation="c1")
    prior_id = first.frame_id

    second = open_timer(coordinator, principal="alice", conversation="c1")

    assert second.kind == "opened"
    assert second.question == "For how long?"
    assert second.superseded_frame_id == prior_id
    assert coordinator.session_for("alice", "c1").frame_id == second.frame_id
    late = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert late.kind == "complete"
    assert late.session.frame_id == second.frame_id


def test_drop_all_reports_count_and_closes_sessions():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator, principal="alice", conversation="c1")
    open_timer(coordinator, principal="bob", conversation="c2")

    dropped = coordinator.drop_all(reason="restart")

    assert dropped == 2
    assert coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    ).kind == "stale"
    assert coordinator.submit_follow_up(
        "twenty minutes", principal="bob", conversation_id="c2"
    ).kind == "stale"


def test_correction_markers_are_stripped_deterministically():
    assert strip_correction_marker("actually five minutes") == ("five minutes", True)
    assert strip_correction_marker("make that five minutes") == ("five minutes", True)
    assert strip_correction_marker("no, five minutes") == ("five minutes", True)
    assert strip_correction_marker("five minutes") == ("five minutes", False)
    assert strip_correction_marker("no") == ("no", False)


def test_conversation_scoped_value_fill_only_active_slot():
    coordinator, _clock = make_coordinator()
    opened = coordinator.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="c1",
        prefilled={"contact": RefValue(kind="contact", id="sarah")},
    )
    assert opened.question == "What should the message say?"

    filled = coordinator.submit_follow_up("hello", principal="alice", conversation_id="c1")

    assert filled.kind == "complete"
    assert filled.frame.slot_value("contact") == RefValue(kind="contact", id="sarah")
    assert filled.frame.origin_of("contact") is SlotOrigin.UTTERANCE
    assert filled.frame.origin_of("message") is SlotOrigin.FOLLOW_UP


def test_finish_closes_ready_session_and_late_answers_stale():
    coordinator, _clock = make_coordinator()
    open_timer(coordinator, principal="alice", conversation="c1")
    coordinator.submit_follow_up("twenty minutes", principal="alice", conversation_id="c1")

    assert coordinator.finish(principal="alice", conversation_id="c1") is True
    session = coordinator.session_for("alice", "c1")
    assert session.state == "closed"
    assert session.frame.status is FrameStatus.COMPLETE

    late = coordinator.submit_follow_up(
        "actually five minutes", principal="alice", conversation_id="c1"
    )
    assert late.kind == "stale"


def test_frames_carry_no_envelope_metadata():
    frame = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        slots=(),
        status=FrameStatus.MISSING,
        missing=("duration",),
    )

    assert "frame_id" not in IntentFrame.__dataclass_fields__
    assert "supersedes" not in IntentFrame.__dataclass_fields__
    assert frame.status is FrameStatus.MISSING


def test_typed_value_validation_fails_closed():
    assert validate_value(DurationValue(seconds=-5)) == "negative"
    assert validate_value(TextValue(text="   ")) == "empty"
    assert validate_value(NumberValue(value=float("inf"))) == "non_finite"
    assert validate_value(RefValue(kind="", id="x")) == "empty_kind"
    assert validate_value(BoolValue(value=True)) is None
    assert validate_value(DateTimeValue(2026, 13, 1, 0, 0, 0)) == "month_range"
    assert validate_value(DurationValue(seconds=0)) is None
