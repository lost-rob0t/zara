from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_expert_why_follow_up_survives_restart_without_provider_calls(tmp_path) -> None:
    """A terse why? must resolve against the durable prior expert act after restart."""
    database_path = tmp_path / "symbolic-discourse.db"
    first_database = DatabaseManager(database_path)
    first_store = ConversationStore(first_database)
    conversation = first_store.create_conversation(
        "Symbolic discourse",
        conversation_id="conv-symbolic-discourse",
    )
    first_adapter = PureSymbolicProjectionAdapter(first_store)
    first_adapter.commit_turn(
        conversation_id=conversation.id,
        expected_generation=0,
        turn_id="turn-expert-1",
        response="The flake input is stale.",
        dialogue_act="expert_answer",
        response_act_term=(
            'answer(expert,"The flake input is stale.",'
            'evidence("dotfiles:flake-lock"))'
        ),
        context_term="[]",
        renderer_provenance="symbolic-dcg/v1",
        expert_evidence_ref="dotfiles:flake-lock",
    )

    seeded = first_store.load_symbolic_projection(conversation.id)
    assert seeded is not None
    seeded.assert_pure_symbolic()
    assert seeded.dialogue_state["response_act_term"].startswith("answer(expert,")
    assert seeded.providers_enabled is False
    assert seeded.max_model_calls == 0
    assert seeded.provider_calls == 0
    assert seeded.model_calls == 0
    first_database.close()

    reopened_database = DatabaseManager(database_path)
    reopened_store = ConversationStore(reopened_database)
    restarted_backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(reopened_store),
    )
    await restarted_backend.start()
    try:
        follow_up = await restarted_backend.submit_turn(
            "why?",
            turn_id="turn-expert-why-2",
            conversation_id=conversation.id,
        )
        assert follow_up.response == "I answered from evidence dotfiles:flake-lock."
        assert follow_up.metadata["response_act"].startswith("answer(expert,")
        assert follow_up.metadata["expert_evidence_ref"] == "dotfiles:flake-lock"
        assert follow_up.metadata["providers_enabled"] is False
        assert follow_up.metadata["max_provider_calls"] == 0
        assert follow_up.metadata["max_model_calls"] == 0
        assert follow_up.metadata["provider_calls"] == 0
        assert follow_up.metadata["model_calls"] == 0
        restarted_backend.commit_turn_result(
            follow_up,
            turn_id="turn-expert-why-2",
            conversation_id=conversation.id,
        )
    finally:
        await restarted_backend.stop()
        reopened_database.close()

    verification_database = DatabaseManager(database_path)
    verification_store = ConversationStore(verification_database)
    recovered = verification_store.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.projection_generation == 2
    assert recovered.dialogue_act == "expert_answer"
    assert recovered.expert_evidence == [{"ref": "dotfiles:flake-lock"}]
    assert recovered.providers_enabled is False
    assert recovered.max_model_calls == 0
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0
    verification_database.close()
