from __future__ import annotations

import json
from dataclasses import replace

import pytest

from zara.wraith.contracts import (
    WRAITH_PROTOCOL,
    AgentSpec,
    Budget,
    MessageType,
    RuntimeBinding,
    TaskRecord,
    TaskState,
    WraithMessage,
    canonical_json,
)


def sample_agent() -> AgentSpec:
    return AgentSpec(
        agent_id="researcher-1",
        profile="researcher",
        parent_id="supervisor-1",
        runtime=RuntimeBinding(runtime_id="zara-python", generation=4),
        capabilities=("web-search", "kb-read"),
        budgets=(Budget("tokens", 5000), Budget("tool-calls", 20)),
    )


def test_agent_contract_round_trips_deterministically() -> None:
    agent = sample_agent()

    wire = agent.to_wire()
    restored = AgentSpec.from_wire(wire)

    assert restored == agent
    first = canonical_json(agent)
    second = canonical_json(restored)
    assert first == second
    assert json.loads(first)["protocol"] == WRAITH_PROTOCOL


def test_budget_rejects_negative_limits_and_booleans() -> None:
    with pytest.raises(ValueError, match="non-negative"):
        Budget("tokens", -1)
    with pytest.raises(ValueError, match="non-negative"):
        Budget("tokens", True)


def test_agent_rejects_duplicate_capabilities_and_budget_kinds() -> None:
    with pytest.raises(ValueError, match="duplicate entries"):
        replace(sample_agent(), capabilities=("kb-read", "kb-read"))
    with pytest.raises(ValueError, match="duplicate kinds"):
        replace(
            sample_agent(),
            budgets=(Budget("tokens", 1), Budget("tokens", 2)),
        )


def test_agent_cannot_be_its_own_parent() -> None:
    with pytest.raises(ValueError, match="own parent"):
        replace(sample_agent(), parent_id="researcher-1")


def test_runtime_binding_requires_positive_generation() -> None:
    with pytest.raises(ValueError, match="positive integer"):
        RuntimeBinding(runtime_id="zara-python", generation=0)


def test_terminal_task_state_cannot_return_to_running() -> None:
    task = TaskRecord(
        task_id="task-1",
        goal_ref="goal:compile-kb",
        owner_id="researcher-1",
    )
    task = task.transition(TaskState.READY).transition(TaskState.RUNNING)
    task = task.transition(TaskState.COMPLETED)

    assert task.terminal is True
    with pytest.raises(ValueError, match="completed -> running"):
        task.transition(TaskState.RUNNING)


def test_invalid_task_transition_fails_closed() -> None:
    task = TaskRecord(
        task_id="task-1",
        goal_ref="goal:compile-kb",
        owner_id="researcher-1",
    )

    with pytest.raises(ValueError, match="created -> completed"):
        task.transition(TaskState.COMPLETED)


def test_message_requires_typed_kind_and_opaque_payload_reference() -> None:
    message = WraithMessage(
        message_id="message-1",
        sender_id="supervisor-1",
        recipient_id="researcher-1",
        message_type=MessageType.TASK,
        payload_ref="payload:task-1",
        trace_ref="trace:run-42",
    )

    assert WraithMessage.from_wire(message.to_wire()) == message

    with pytest.raises(ValueError, match="payload_ref"):
        replace(message, payload_ref="shell:rm-everything")


def test_agent_values_are_isolated_immutable_records() -> None:
    first = sample_agent()
    second = replace(first, agent_id="researcher-2")

    assert first.agent_id == "researcher-1"
    assert second.agent_id == "researcher-2"
    assert first.runtime == second.runtime
