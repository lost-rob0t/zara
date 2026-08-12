"""Tests for the Zarathushtra pet state priority policy and aggregation.

Covers: state priority, simultaneous activity aggregation, state
transitions, cancellation, repeated identical state events, unread/ready
semantics.
"""

from __future__ import annotations

import pytest

from zara.pets import ActivityKind, PetState, PetStatusDeriver


def test_priority_order_is_needs_input_blocked_ready_running_idle():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    assert d.aggregate() is PetState.RUNNING
    d.start(ActivityKind.TOOL, PetState.READY)
    assert d.aggregate() is PetState.READY
    d.start(ActivityKind.AGENT, PetState.BLOCKED)
    assert d.aggregate() is PetState.BLOCKED
    d.start(ActivityKind.INDEXING, PetState.NEEDS_INPUT)
    assert d.aggregate() is PetState.NEEDS_INPUT


def test_priority_needs_input_beats_blocked():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.BLOCKED)
    d.start(ActivityKind.TOOL, PetState.NEEDS_INPUT)
    assert d.aggregate() is PetState.NEEDS_INPUT


def test_priority_blocked_beats_ready():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.READY)
    d.start(ActivityKind.TOOL, PetState.BLOCKED)
    assert d.aggregate() is PetState.BLOCKED


def test_priority_ready_beats_running():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.start(ActivityKind.TOOL, PetState.READY)
    assert d.aggregate() is PetState.READY


def test_priority_running_beats_idle():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.IDLE)
    d.start(ActivityKind.TOOL, PetState.RUNNING)
    assert d.aggregate() is PetState.RUNNING


def test_simultaneous_activities_aggregate_to_highest_priority():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.start(ActivityKind.TOOL, PetState.RUNNING)
    d.start(ActivityKind.AGENT, PetState.READY)
    d.start(ActivityKind.INDEXING, PetState.RUNNING)
    d.start(ActivityKind.SEARCH, PetState.RUNNING)
    d.start(ActivityKind.BACKGROUND, PetState.RUNNING)
    assert d.aggregate() is PetState.READY
    d.transition(ActivityKind.AGENT, PetState.NEEDS_INPUT)
    assert d.aggregate() is PetState.NEEDS_INPUT


def test_cancellation_returns_to_next_highest_not_idle():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.start(ActivityKind.AGENT, PetState.NEEDS_INPUT)
    assert d.aggregate() is PetState.NEEDS_INPUT
    d.cancel(ActivityKind.AGENT)
    assert d.aggregate() is PetState.RUNNING


def test_cancellation_of_only_activity_returns_to_idle():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.cancel(ActivityKind.MODEL)
    assert d.aggregate() is PetState.IDLE


def test_transition_changes_state_in_place():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.transition(ActivityKind.MODEL, PetState.READY)
    assert d.aggregate() is PetState.READY
    d.transition(ActivityKind.MODEL, PetState.NEEDS_INPUT)
    assert d.aggregate() is PetState.NEEDS_INPUT


def test_repeated_identical_transition_is_idempotent():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    first = d.aggregate()
    d.transition(ActivityKind.MODEL, PetState.RUNNING)
    d.transition(ActivityKind.MODEL, PetState.RUNNING)
    assert d.aggregate() is first


def test_ready_without_unread_falls_back_to_idle():
    d = PetStatusDeriver()
    activity = d.start(ActivityKind.MODEL, PetState.READY)
    activity.unread = False
    assert d.aggregate() is PetState.IDLE


def test_ready_with_unread_stays_ready():
    d = PetStatusDeriver()
    activity = d.start(ActivityKind.MODEL, PetState.READY)
    activity.unread = True
    assert d.aggregate() is PetState.READY


def test_idle_transition_clears_unread():
    d = PetStatusDeriver()
    activity = d.start(ActivityKind.MODEL, PetState.READY)
    activity.unread = True
    d.transition(ActivityKind.MODEL, PetState.IDLE)
    assert activity.unread is False
    assert d.aggregate() is PetState.IDLE


def test_start_replaces_prior_activity_of_same_kind():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.start(ActivityKind.MODEL, PetState.BLOCKED)
    assert d.aggregate() is PetState.BLOCKED
    assert len(d.activities) == 1


def test_mark_unread_toggles_ready_visibility():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.READY)
    d.mark_unread(ActivityKind.MODEL, True)
    assert d.aggregate() is PetState.READY
    d.mark_unread(ActivityKind.MODEL, False)
    assert d.aggregate() is PetState.IDLE


def test_pending_labels_only_for_aggregate_state():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING, label="llm")
    d.start(ActivityKind.AGENT, PetState.NEEDS_INPUT, label="approve")
    assert d.aggregate() is PetState.NEEDS_INPUT
    assert d.pending_labels() == ["approve"]


def test_clear_resets_aggregate():
    d = PetStatusDeriver()
    d.start(ActivityKind.MODEL, PetState.RUNNING)
    d.clear()
    assert d.aggregate() is PetState.IDLE
    assert d.activities == {}


def test_pet_state_from_str_round_trips():
    for state in PetState:
        assert PetState.from_str(state.value) is state


def test_pet_state_from_str_rejects_unknown():
    with pytest.raises(ValueError):
        PetState.from_str("unknown")