from __future__ import annotations

import pytest

from zara.runtime.clarification import (
    ClarificationCoordinator,
    DialogueTemplate,
    OPEN_APP_TEMPLATE,
    SessionCloseReason,
    SessionState,
    SlotSpec,
    SlotType,
    TEXT_MESSAGE_TEMPLATE,
    TIMER_SET_TEMPLATE,
    parse_boolean,
    parse_datetime,
    parse_duration,
    parse_number,
)
from zara.runtime.frames import DurationValue, FrameStatus, RefValue, TextValue


class Clock:
    def __init__(self) -> None:
        self.now = 100.0

    def __call__(self) -> float:
        return self.now


def coordinator(**kwargs):
    clock = Clock()
    counter = iter(range(100))

    def ids():
        return f"edge-{next(counter)}"

    return ClarificationCoordinator(clock=clock, id_factory=ids, **kwargs), clock


def test_template_lookup_and_default_argument_order_edges():
    template = DialogueTemplate(
        "edge",
        "inspect",
        (SlotSpec("one", SlotType.TEXT), SlotSpec("two", SlotType.TEXT)),
    )
    assert template.arg_order_names() == ("one", "two")
    with pytest.raises(KeyError):
        template.spec("missing")


def test_scalar_parsers_reject_bad_shapes_and_cover_word_paths():
    assert parse_duration("seconds") is None
    assert parse_duration("many seconds") is None
    assert parse_duration("2 parsecs") is None
    assert parse_duration("999999999 seconds") is None
    assert parse_number("twenty one").value == 21
    assert parse_number("not a number") is None
    assert parse_boolean("yes please") is None
    assert parse_boolean("YES").value is True
    assert parse_boolean("off").value is False
    assert parse_boolean("maybe") is None
    assert parse_datetime("not-a-date") is None


def test_ambiguous_invalid_and_closed_questions_are_typed():
    c, clock = coordinator()
    one = c.open(
        OPEN_APP_TEMPLATE,
        principal="alice",
        conversation_id="one",
        alternatives=("firefox",),
    )
    assert one.session.question() == "Did you mean: firefox?"

    many = c.open(
        OPEN_APP_TEMPLATE,
        principal="alice",
        conversation_id="many",
        alternatives=("firefox", "chrome"),
    )
    assert many.session.question() == "Did you mean: firefox or chrome?"

    invalid = c.open(
        TIMER_SET_TEMPLATE,
        principal="alice",
        conversation_id="invalid",
        invalid_slot="duration",
        invalid_reason="not_duration",
    )
    assert invalid.session.frame.status is FrameStatus.INVALID
    assert invalid.session.question() == "For how long?"

    invalid.session.close(SessionCloseReason.CANCELLED, now=clock.now)
    assert invalid.session.question() is None
    assert invalid.session.expired(clock.now + 1000) is False


def test_session_close_is_idempotent_and_preserves_supersede_status():
    c, clock = coordinator()
    opened = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="c")
    first = opened.session.close("superseded_by_new_command", now=clock.now)
    assert first.kind == "stale"
    assert opened.session.frame.status is FrameStatus.SUPERSEDED
    second = opened.session.close(SessionCloseReason.CANCELLED, now=clock.now + 1)
    assert second.kind == "stale"
    assert second.reason == "closed"


def test_attempt_exhaustion_and_direct_submit_value_stale_edges():
    c, clock = coordinator(max_attempts=1, session_ttl=1)
    opened = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="bad")
    exhausted = opened.session.register_invalid(now=clock.now)
    assert exhausted.kind == "stale"
    assert opened.session.state is SessionState.CLOSED

    complete = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="complete")
    done = complete.session.submit_value(DurationValue(5), now=clock.now)
    assert done.kind == "complete"
    assert complete.session.submit_value(DurationValue(6), now=clock.now).kind == "stale"

    expired = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="expired")
    assert expired.session.submit_value(DurationValue(5), now=clock.now + 2).reason == "expired"


def test_direct_correction_rejects_closed_expired_and_invalid_values():
    c, clock = coordinator(max_attempts=2, session_ttl=1)
    opened = c.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="c",
        prefilled={"contact": RefValue("contact", "alice")},
    )
    invalid = opened.session.correct_value("contact", RefValue("contact", " "), now=clock.now)
    assert invalid.kind == "retry"

    expired = c.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="expired",
        prefilled={"contact": RefValue("contact", "alice")},
    )
    assert expired.session.correct_value(
        "contact", RefValue("contact", "bob"), now=clock.now + 2
    ).reason == "expired"

    closed = c.open(
        TEXT_MESSAGE_TEMPLATE,
        principal="alice",
        conversation_id="closed",
        prefilled={"contact": RefValue("contact", "alice")},
    )
    closed.session.close(SessionCloseReason.CANCELLED, now=clock.now)
    assert closed.session.correct_value(
        "contact", RefValue("contact", "bob"), now=clock.now
    ).reason == "closed"


def test_open_capacity_prefill_and_unknown_slot_fail_closed():
    c, _clock = coordinator(max_sessions=1)
    c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="one")
    capacity = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="two")
    assert capacity.kind == "capacity"

    fresh, _ = coordinator()
    with pytest.raises(ValueError, match="invalid prefilled"):
        fresh.open(
            TIMER_SET_TEMPLATE,
            principal="alice",
            conversation_id="bad",
            prefilled={"duration": DurationValue(-1)},
        )
    with pytest.raises(ValueError, match="unknown prefilled"):
        fresh.open(
            TIMER_SET_TEMPLATE,
            principal="alice",
            conversation_id="unknown",
            prefilled={"ghost": TextValue("x")},
        )


def test_followup_ready_correction_and_unknown_active_paths_are_bounded():
    c, clock = coordinator()
    opened = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="c")
    done = c.submit_follow_up("5 seconds", principal="alice", conversation_id="c")
    assert done.kind == "complete"
    assert c.submit_follow_up("anything", principal="alice", conversation_id="c").reason == "already_complete"
    corrected = c.submit_follow_up(
        "actually 7 seconds", principal="alice", conversation_id="c"
    )
    assert corrected.kind == "complete"
    assert corrected.frame.slot_value("duration") == DurationValue(7)

    assert c.submit_correction(
        "duration", DurationValue(1), principal="alice", conversation_id="missing"
    ).reason == "no_active_dialogue"
    with pytest.raises(ValueError, match="unknown slot"):
        c.submit_correction(
            "ghost", DurationValue(1), principal="alice", conversation_id="c"
        )

def test_cancel_finish_expire_and_prune_cover_terminal_registry_edges():
    c, clock = coordinator(session_ttl=1)
    assert c.cancel(principal="alice", conversation_id="missing").reason == "no_active_dialogue"
    assert c.finish(principal="alice", conversation_id="missing") is False

    ready = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="ready")
    c.submit_follow_up("5 seconds", principal="alice", conversation_id="ready")
    assert c.finish(principal="alice", conversation_id="ready", now=clock.now) is True
    assert c.finish(principal="alice", conversation_id="ready", now=clock.now) is False

    expiring = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="exp")
    clock.now += 2
    expired = c.cancel(principal="alice", conversation_id="exp")
    assert expired.reason == "expired"

    another = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="another")
    clock.now += 2
    ids = c.expire()
    assert another.session.frame_id in ids

    clock.now += 2
    c._prune_locked(clock.now)
    assert c.session_for("alice", "another") is None


def test_new_dialogue_supersedes_existing_session_without_cross_scope_mutation():
    c, clock = coordinator()
    first = c.open(TIMER_SET_TEMPLATE, principal="alice", conversation_id="c")
    second = c.open(OPEN_APP_TEMPLATE, principal="alice", conversation_id="c", now=clock.now)
    assert second.superseded_frame_id == first.frame_id
    assert first.session.frame.status is FrameStatus.SUPERSEDED
    assert c.active_question("alice", "c") == "Which app?"
