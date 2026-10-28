from __future__ import annotations

from pathlib import Path

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import (
    PURE_SYMBOLIC_RENDER_ERROR,
    PureSymbolicRuntimeBackend,
)


class RendererGapEngine:
    """Deterministically fail both discourse and ordinary symbolic render queries."""

    def consult(self, _path: Path) -> None:
        return None

    def query_once(self, _goal: str):
        return None


@pytest.mark.asyncio
async def test_renderer_gap_does_not_erase_restart_safe_expert_why_context(tmp_path) -> None:
    """A renderer error must not replace the last usable expert discourse act."""
    database_path = tmp_path / "symbolic-error-discourse.db"
    first_database = DatabaseManager(database_path)
    first_store = ConversationStore(first_database)
    conversation = first_store.create_conversation(
        "Symbolic error discourse",
        conversation_id="conv-symbolic-error-discourse",
    )
    adapter = PureSymbolicProjectionAdapter(first_store)
    expert_act = (
        'answer(expert,"The flake input is stale.",'
        'evidence("dotfiles:flake-lock"))'
    )
    adapter.commit_turn(
        conversation_id=conversation.id,
        expected_generation=0,
        turn_id="turn-expert-1",
        response="The flake input is stale.",
        dialogue_act="expert_answer",
        response_act_term=expert_act,
        context_term="[]",
        renderer_provenance="symbolic-dcg/v1",
        expert_evidence_ref="dotfiles:flake-lock",
    )

    gap_backend = PureSymbolicRuntimeBackend(
        engine_factory=RendererGapEngine,
        projection_adapter=adapter,
    )
    await gap_backend.start()
    try:
        gap = await gap_backend.submit_turn(
            "continue",
            turn_id="turn-gap-2",
            conversation_id=conversation.id,
        )
        assert gap.response == PURE_SYMBOLIC_RENDER_ERROR
        assert gap.metadata["response_act"] == "error(renderer_unavailable)"
        assert gap.metadata["providers_enabled"] is False
        assert gap.metadata["max_provider_calls"] == 0
        assert gap.metadata["max_model_calls"] == 0
        assert gap.metadata["provider_calls"] == 0
        assert gap.metadata["model_calls"] == 0
        gap_backend.commit_turn_result(
            gap,
            turn_id="turn-gap-2",
            conversation_id=conversation.id,
        )
    finally:
        await gap_backend.stop()

    after_gap = first_store.load_symbolic_projection(conversation.id)
    assert after_gap is not None
    after_gap.assert_pure_symbolic()
    assert after_gap.outcome == "error"
    assert after_gap.dialogue_act == "error"
    assert after_gap.dialogue_state["response_act_term"] == expert_act
    assert after_gap.expert_evidence == [{"ref": "dotfiles:flake-lock"}]
    assert after_gap.providers_enabled is False
    assert after_gap.max_model_calls == 0
    assert after_gap.provider_calls == 0
    assert after_gap.model_calls == 0
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
            turn_id="turn-why-3",
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
    finally:
        await restarted_backend.stop()
        reopened_database.close()
