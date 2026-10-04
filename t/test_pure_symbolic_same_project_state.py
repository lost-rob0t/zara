from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_projection import SymbolicConversationProjection
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_first_symbolic_dialogue_preserves_same_project_expert_state(tmp_path) -> None:
    store = ConversationStore(DatabaseManager(tmp_path / "same-project-expert-state.db"))
    conversation = store.create_conversation(
        "Same-project expert continuity",
        conversation_id="conv-same-project-expert-continuity",
    )
    expert_evidence = [
        {"expert": "NixExpert", "evidence_id": "nix-evidence"},
        {"expert": "BashExpert", "evidence_id": "bash-evidence"},
    ]
    verified_facts = [
        {"fact_id": "nix-style", "value": "flake-first"},
        {"fact_id": "bash-style", "value": "set-euo-pipefail"},
    ]
    discourse_entities = [
        {"ref": "that flake", "entity_id": "file:flake.nix"},
    ]
    prior_questions = [
        {
            "id": "question:expert-follow-up",
            "text": "Which shell should I target?",
            "source": "expert",
        }
    ]

    seeded = store.save_symbolic_projection(
        SymbolicConversationProjection(
            conversation_id=conversation.id,
            projection_generation=1,
            runtime_generation=1,
            turn_id="turn-expert-answer",
            outcome="success",
            project_id="workspace:nix-bash",
            project_generation=7,
            dialogue_act="expert.answer",
            dialogue_state={"expert_topic": "nix-bash"},
            discourse_entities=discourse_entities,
            unresolved_questions=prior_questions,
            expert_evidence=expert_evidence,
            verified_facts=verified_facts,
            verified_outcome_refs=[],
            renderer_provenance="symbolic-dcg/v1",
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    seeded.assert_pure_symbolic()
    assert "prolog_context_project_id" not in seeded.dialogue_state
    assert "prolog_context_project_generation" not in seeded.dialogue_state

    adapter = PureSymbolicProjectionAdapter(store)
    context_term, generation = adapter.load_dialogue_context(conversation.id)
    assert context_term == "[]"
    assert generation == 1

    backend = PureSymbolicRuntimeBackend(projection_adapter=adapter)
    await backend.start()
    try:
        result = await backend.submit_turn(
            "timer",
            turn_id="turn-first-dialogue",
            conversation_id=conversation.id,
        )
        assert result.response == "How long should I set the timer for?"
        assert result.metadata["providers_enabled"] is False
        assert result.metadata["max_model_calls"] == 0
        assert result.metadata["provider_calls"] == 0
        assert result.metadata["model_calls"] == 0
        backend.commit_turn_result(
            result,
            turn_id="turn-first-dialogue",
            conversation_id=conversation.id,
        )
    finally:
        await backend.stop()

    clarified = store.load_symbolic_projection(conversation.id)
    assert clarified is not None
    clarified.assert_pure_symbolic()
    assert clarified.projection_generation == 2
    assert clarified.project_id == "workspace:nix-bash"
    assert clarified.project_generation == 7
    assert clarified.dialogue_act == "clarify"
    assert clarified.expert_evidence == expert_evidence
    assert clarified.verified_facts == verified_facts
    assert clarified.discourse_entities == discourse_entities
    assert prior_questions[0] in clarified.unresolved_questions
    assert any(
        question.get("source") == "symbolic_dialogue"
        for question in clarified.unresolved_questions
    )
    assert clarified.dialogue_state["expert_topic"] == "nix-bash"
    assert clarified.dialogue_state["prolog_context_project_id"] == "workspace:nix-bash"
    assert clarified.dialogue_state["prolog_context_project_generation"] == 7
    assert "partial_frame" in clarified.dialogue_state["prolog_context_term"]
    assert clarified.providers_enabled is False
    assert clarified.max_model_calls == 0
    assert clarified.provider_calls == 0
    assert clarified.model_calls == 0
