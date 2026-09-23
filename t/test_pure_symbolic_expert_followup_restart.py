from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_projection import SymbolicConversationProjection
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_persisted_expert_answer_routes_why_after_restart(tmp_path) -> None:
    database_path = tmp_path / "expert-followup-restart.db"
    database = DatabaseManager(database_path)
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Expert follow-up restart",
        conversation_id="conv-expert-followup-restart",
    )
    evidence_ref = "evidence:nix:sha256:0123456789abcdef"
    prior_act = (
        "answer(expert,\"NixExpert inspected source.\",evidence('"
        + evidence_ref
        + "'))"
    )
    expert_evidence = [
        {
            "expert_id": "zara:expert/nix",
            "evidence_refs": [evidence_ref],
            "model_calls": 0,
            "effect_receipts": [],
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
            project_generation=3,
            dialogue_act="expert.answer",
            dialogue_state={
                "response_act_term": prior_act,
                "prolog_context_term": "[]",
                "prolog_context_project_id": "workspace:nix-bash",
                "prolog_context_project_generation": 3,
            },
            expert_evidence=expert_evidence,
            renderer_provenance="symbolic-dcg/v1",
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    seeded.assert_pure_symbolic()
    database.close()

    reopened_database = DatabaseManager(database_path)
    reopened_store = ConversationStore(reopened_database)
    recovered = reopened_store.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.dialogue_state["response_act_term"] == prior_act
    assert recovered.expert_evidence == expert_evidence

    adapter = PureSymbolicProjectionAdapter(reopened_store)
    assert adapter.load_previous_response_act(conversation.id) == prior_act

    backend = PureSymbolicRuntimeBackend(projection_adapter=adapter)
    await backend.start()
    try:
        result = await backend.submit_turn(
            "why?",
            turn_id="turn-why",
            conversation_id=conversation.id,
        )
        assert result.response == f"I answered from evidence {evidence_ref}."
        assert result.metadata["route"] == "pure_symbolic"
        assert result.metadata["providers_enabled"] is False
        assert result.metadata["max_provider_calls"] == 0
        assert result.metadata["max_model_calls"] == 0
        assert result.metadata["provider_calls"] == 0
        assert result.metadata["model_calls"] == 0

        backend.commit_turn_result(
            result,
            turn_id="turn-why",
            conversation_id=conversation.id,
        )
    finally:
        await backend.stop()

    current = reopened_store.load_symbolic_projection(conversation.id)
    assert current is not None
    current.assert_pure_symbolic()
    assert current.dialogue_act == "answer"
    assert current.expert_evidence == expert_evidence
    assert current.dialogue_state["prolog_context_term"] == "[]"
    assert evidence_ref in current.dialogue_state["response_act_term"]
    assert current.providers_enabled is False
    assert current.max_model_calls == 0
    assert current.provider_calls == 0
    assert current.model_calls == 0
    reopened_database.close()
