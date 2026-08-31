from __future__ import annotations

from zara.runtime.clarification import (
    ClarificationCoordinator,
    OPEN_APP_TEMPLATE,
    TEXT_MESSAGE_TEMPLATE,
    TIMER_SET_TEMPLATE,
)
from zara.runtime.frames import (
    DurationValue,
    FrameStatus,
    RefValue,
    SlotOrigin,
    TextValue,
)


class FakeClock:
    def __init__(self) -> None:
        self.now_value = 0.0

    def __call__(self) -> float:
        return self.now_value

    def advance(self, seconds: float) -> None:
        self.now_value += seconds


def make_ids():
    counter = iter(range(9000))
    return lambda: f"frame-{next(counter):04d}"


def test_scripted_dialogue_walks_every_mandatory_example():
    """Non-interactive dialogue script: the #155 mandatory examples as one
    deterministic conversation between two principals on two conversations."""
    clock = FakeClock()
    coordinator = ClarificationCoordinator(
        clock=clock, id_factory=make_ids(), session_ttl=120.0
    )

    # 1. "set a timer" -> ask duration; "twenty minutes" -> duration 1200
    opened = coordinator.open(
        TIMER_SET_TEMPLATE, principal="alice", conversation_id="phone"
    )
    assert opened.question == "For how long?"
    fill = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="phone"
    )
    assert fill.kind == "complete"
    assert fill.frame.slot_value("duration") == DurationValue(seconds=1200)

    # 4. correction from 20 -> 5 minutes before execution (READY window)
    corrected = coordinator.submit_follow_up(
        "actually five minutes", principal="alice", conversation_id="phone"
    )
    assert corrected.kind == "complete"
    assert corrected.frame.slot_value("duration") == DurationValue(seconds=300)
    assert corrected.frame.origin_of("duration") is SlotOrigin.CORRECTION
    assert corrected.supersedes == fill.frame_id

    # 3. cancellation: "never mind" -> cancelled; later answer not consumed
    cancelled = coordinator.submit_follow_up(
        "never mind", principal="alice", conversation_id="phone"
    )
    assert cancelled.kind == "cancelled"
    late = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="phone"
    )
    assert late.kind == "stale"
    assert late.message == ClarificationCoordinator.STALE_MESSAGE

    # 2. focused retry after invalid answer ("bananas")
    opened = coordinator.open(
        TIMER_SET_TEMPLATE, principal="alice", conversation_id="phone"
    )
    assert opened.question == "For how long?"
    retry = coordinator.submit_follow_up(
        "bananas", principal="alice", conversation_id="phone"
    )
    assert retry.kind == "retry"
    assert retry.question == "For how long?"
    recovered = coordinator.submit_follow_up(
        "ninety seconds", principal="alice", conversation_id="phone"
    )
    assert recovered.kind == "complete"
    assert recovered.frame.slot_value("duration") == DurationValue(seconds=90)
    assert coordinator.finish(principal="alice", conversation_id="phone") is True

    # 5. "open" -> ask app -> "Firefox"
    opened = coordinator.open(
        OPEN_APP_TEMPLATE, principal="alice", conversation_id="phone"
    )
    assert opened.question == "Which app?"
    done = coordinator.submit_follow_up(
        "Firefox", principal="alice", conversation_id="phone"
    )
    assert done.kind == "complete"
    assert done.frame.slot_value("target") == RefValue(kind="app_alias", id="firefox")

    # 6./7. "text Sarah" -> ask message; two missing slots in deterministic order
    opened = coordinator.open(
        TEXT_MESSAGE_TEMPLATE, principal="alice", conversation_id="desktop"
    )
    assert opened.question == "Who should I message?"
    first = coordinator.submit_follow_up(
        "sarah", principal="alice", conversation_id="desktop"
    )
    assert first.kind == "filled"
    assert first.question == "What should the message say?"
    second = coordinator.submit_follow_up(
        "hi there", principal="alice", conversation_id="desktop"
    )
    assert second.kind == "complete"
    assert second.frame.slot_value("message") == TextValue(text="hi there")

    # 8./9. simultaneous principals and conversations stay isolated
    a_open = coordinator.open(
        TIMER_SET_TEMPLATE, principal="alice", conversation_id="phone"
    )
    b_open = coordinator.open(
        TIMER_SET_TEMPLATE, principal="bob", conversation_id="phone"
    )
    assert a_open.question == b_open.question == "For how long?"
    a_fill = coordinator.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="phone"
    )
    assert a_fill.kind == "complete"
    assert coordinator.active_question("bob", "phone") == "For how long?"

    # 13. timeout/expiry with a virtual clock; stale answer rejected
    clock.advance(200.0)
    stale = coordinator.submit_follow_up(
        "five minutes", principal="bob", conversation_id="phone"
    )
    assert stale.kind == "stale"
    assert stale.message == ClarificationCoordinator.STALE_MESSAGE

    closed = coordinator.expire()
    assert len(closed) == 2
    assert coordinator.session_for("bob", "phone").frame.status is FrameStatus.CANCELLED
    assert coordinator.session_for("alice", "phone").frame.status is FrameStatus.CANCELLED

    # No cross-principal inference: every rejection wording is byte-identical
    no_session = coordinator.submit_follow_up(
        "five minutes", principal="carol", conversation_id="anywhere"
    )
    assert no_session.message == late.message == stale.message
