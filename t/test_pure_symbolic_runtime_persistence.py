from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_pure_symbolic_dialogue_context_survives_backend_restart(tmp_path) -> None:
    store = ConversationStore(DatabaseManager(tmp_path / "symbolic-context.db"))
    conversation = store.create_conversation(
        "Symbolic continuity",
        conversation_id="conv-symbolic-continuity",
    )

    first_backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await first_backend.start()
    try:
        first = await first_backend.submit_turn(
            "timer",
            turn_id="turn-symbolic-1",
            conversation_id=conversation.id,
        )
        assert first.response == "How long should I set the timer for?"
        first_backend.commit_turn_result(
            first,
            turn_id="turn-symbolic-1",
            conversation_id=conversation.id,
        )
    finally:
        await first_backend.stop()

    first_projection = store.load_symbolic_projection(conversation.id)
    assert first_projection is not None
    first_projection.assert_pure_symbolic()
    assert first_projection.projection_generation == 1
    assert first_projection.runtime_generation == 1
    assert first_projection.dialogue_act == "clarify"
    assert "partial_frame" in first_projection.dialogue_state["prolog_context_term"]

    restarted_backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await restarted_backend.start()
    try:
        second = await restarted_backend.submit_turn(
            "5 minutes",
            turn_id="turn-symbolic-2",
            conversation_id=conversation.id,
        )
        assert second.response == (
            "That action needs capability-checked execution before I can report success."
        )
        assert second.metadata["response_act"].startswith("dispatch_required(")
        restarted_backend.commit_turn_result(
            second,
            turn_id="turn-symbolic-2",
            conversation_id=conversation.id,
        )
    finally:
        await restarted_backend.stop()

    second_projection = store.load_symbolic_projection(conversation.id)
    assert second_projection is not None
    second_projection.assert_pure_symbolic()
    assert second_projection.projection_generation == 2
    assert second_projection.runtime_generation == 2
    assert second_projection.dialogue_act == "dispatch_required"
    assert "completed_frame" in second_projection.dialogue_state["prolog_context_term"]
    assert second_projection.providers_enabled is False
    assert second_projection.max_model_calls == 0
    assert second_projection.provider_calls == 0
    assert second_projection.model_calls == 0


@pytest.mark.asyncio
async def test_project_switch_fences_stale_pure_symbolic_dialogue_context(tmp_path) -> None:
    """A new project must not inherit symbolic knowledge from the old project."""
    store = ConversationStore(DatabaseManager(tmp_path / "symbolic-project-fence.db"))
    conversation = store.create_conversation(
        "Symbolic project fence",
        conversation_id="conv-symbolic-project-fence",
    )
    adapter = PureSymbolicProjectionAdapter(store)
    backend = PureSymbolicRuntimeBackend(projection_adapter=adapter)
    await backend.start()
    try:
        first = await backend.submit_turn(
            "timer",
            turn_id="turn-project-a",
            conversation_id=conversation.id,
        )
        backend.commit_turn_result(
            first,
            turn_id="turn-project-a",
            conversation_id=conversation.id,
        )
    finally:
        await backend.stop()

    project_a = store.load_symbolic_projection(conversation.id)
    assert project_a is not None
    project_a.assert_pure_symbolic()
    assert "partial_frame" in project_a.dialogue_state["prolog_context_term"]

    switched = store.save_symbolic_projection(
        replace(
            project_a,
            projection_generation=project_a.projection_generation + 1,
            runtime_generation=project_a.runtime_generation + 1,
            turn_id="turn-project-switch",
            outcome="cancelled",
            project_id="project-b",
            project_generation=project_a.project_generation + 1,
            dialogue_act="cancelled",
            dialogue_state={
                **project_a.dialogue_state,
                "project_fact": "project-a-only",
            },
            discourse_entities=[{"ref": "that", "entity_id": "file:project-a.nix"}],
            expert_evidence=[{"expert": "DotfilesExpert", "evidence_id": "project-a"}],
            verified_facts=[{"fact_id": "project-a", "value": "project-a.nix"}],
            verified_outcome_refs=[
                "zara.verified-outcome/v2:2:outcome:project-switch-receipt"
            ],
        ),
        expected_generation=project_a.projection_generation,
    )
    switched.assert_pure_symbolic()
    assert "partial_frame" in switched.dialogue_state["prolog_context_term"]

    context_term, generation = adapter.load_dialogue_context(conversation.id)

    assert generation == switched.projection_generation
    assert context_term == "[]"

    project_b_backend = PureSymbolicRuntimeBackend(projection_adapter=adapter)
    await project_b_backend.start()
    try:
        next_turn = await project_b_backend.submit_turn(
            "hello",
            turn_id="turn-project-b",
            conversation_id=conversation.id,
        )
        assert next_turn.metadata["providers_enabled"] is False
        assert next_turn.metadata["max_model_calls"] == 0
        assert next_turn.metadata["provider_calls"] == 0
        assert next_turn.metadata["model_calls"] == 0
        project_b_backend.commit_turn_result(
            next_turn,
            turn_id="turn-project-b",
            conversation_id=conversation.id,
        )
    finally:
        await project_b_backend.stop()

    project_b = store.load_symbolic_projection(conversation.id)
    assert project_b is not None
    project_b.assert_pure_symbolic()
    assert project_b.project_id == "project-b"
    assert project_b.project_generation == switched.project_generation
    assert "project_fact" not in project_b.dialogue_state
    assert project_b.discourse_entities == []
    assert project_b.unresolved_questions == []
    assert project_b.expert_evidence == []
    assert project_b.verified_facts == []
    assert project_b.verified_outcome_refs == switched.verified_outcome_refs
