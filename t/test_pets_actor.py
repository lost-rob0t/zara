"""Tests for the PetStateActor (Pykka) event mapping and transitions."""

from __future__ import annotations

import pytest
import pykka

from zara.pets import PetState
from zara.pets import events
from zara.pets.actor import PetStateActor, _GetState


@pytest.fixture(autouse=True)
def cleanup_actors():
    yield
    pykka.ActorRegistry.stop_all()


def _actor():
    return PetStateActor.start()


def state_of(ref) -> PetState:
    return ref.ask(_GetState(), timeout=5)


def test_actor_starts_idle():
    ref = _actor()
    assert state_of(ref) is PetState.IDLE
    ref.stop()


def test_model_started_transitions_to_running():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.ask(_GetState(), timeout=5)  # sync round-trip
    assert state_of(ref) is PetState.RUNNING
    ref.stop()


def test_model_streaming_keeps_running():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.tell(events.ModelStreaming())
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.RUNNING
    ref.stop()


def test_model_completed_success_transitions_to_ready():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.tell(events.ModelCompleted(success=True))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.READY
    ref.stop()


def test_model_failed_transitions_to_blocked():
    ref = _actor()
    ref.tell(events.ModelFailed(reason="boom"))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.BLOCKED
    ref.stop()


def test_user_input_required_transitions_to_needs_input():
    ref = _actor()
    ref.tell(events.AgentStarted())
    ref.tell(events.UserInputRequired(kind="approval"))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.NEEDS_INPUT
    ref.stop()


def test_user_responded_returns_to_running():
    ref = _actor()
    ref.tell(events.AgentStarted())
    ref.tell(events.UserInputRequired())
    ref.tell(events.UserResponded())
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.RUNNING
    ref.stop()


def test_task_cancelled_removes_activity_and_re_derives():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.tell(events.AgentStarted())
    ref.tell(events.UserInputRequired())
    ref.tell(events.TaskCancelled())
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.RUNNING
    ref.stop()


def test_output_seen_clears_ready_to_idle():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.tell(events.ModelCompleted(success=True))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.READY
    ref.tell(events.OutputSeen())
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.IDLE
    ref.stop()


def test_subscriber_receives_state_changes():
    seen: list = []
    subscriber = lambda state, labels: seen.append((state, labels))  # noqa: E731
    ref = PetStateActor.start(subscriber=subscriber)
    ref.tell(events.ModelStarted())
    ref.ask(_GetState(), timeout=5)
    ref.stop()
    states = [s for s, _ in seen]
    assert PetState.RUNNING in states


def test_repeated_identical_events_do_not_restart():
    ref = _actor()
    ref.tell(events.ModelStarted())
    ref.tell(events.ModelStarted())
    ref.tell(events.ModelStarted())
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.RUNNING
    ref.stop()


def test_provider_unavailable_transitions_to_blocked():
    ref = _actor()
    ref.tell(events.ProviderUnavailable(reason="offline"))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.BLOCKED
    ref.stop()


def test_simultaneous_model_and_tool_aggregate():
    ref = _actor()
    ref.tell(events.ModelStarted(label="llm"))
    ref.tell(events.ToolStarted(label="search"))
    ref.tell(events.ToolFailed(reason="err"))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.BLOCKED
    ref.stop()


def test_tool_completed_success_transitions_to_ready():
    ref = _actor()
    ref.tell(events.ToolStarted())
    ref.tell(events.ToolCompleted(success=True))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.READY
    ref.stop()


def test_agent_failed_transitions_to_blocked():
    ref = _actor()
    ref.tell(events.AgentStarted())
    ref.tell(events.AgentFailed(reason="crash"))
    ref.ask(_GetState(), timeout=5)
    assert state_of(ref) is PetState.BLOCKED
    ref.stop()