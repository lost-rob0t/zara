from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


def _projection(
    conversation_id: str,
    *,
    generation: int,
    runtime_generation: int,
    turn_id: str,
    outcome: str,
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
        runtime_generation=runtime_generation,
        turn_id=turn_id,
        outcome=outcome,
        project_id="project-a",
        project_generation=1,
        dialogue_state={"act": "clarify", "slot": "target"},
        discourse_entities=[{"entity_id": "file:flake.nix"}],
        unresolved_questions=[{"slot": "target"}],
        expert_evidence=[{"evidence_id": "ev-before"}],
        verified_facts=[{"fact_id": "fact-before"}],
        renderer_provenance="symbolic-dcg/v1",
        provider_calls=0,
        model_calls=0,
    )


def test_terminal_same_turn_projection_is_immutable_after_cancellation(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "terminal-immutable.db"))
    conversation = store.create_conversation(
        "Terminal immutability",
        conversation_id="conv-terminal-immutable",
    )
    pending = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=41,
            turn_id="turn-41",
            outcome="pending",
        ),
        expected_generation=0,
    )
    cancelled = store.save_symbolic_projection(
        replace(
            pending,
            projection_generation=2,
            outcome="cancelled",
            updated_at="",
        ),
        expected_generation=1,
    )
    cancelled.assert_pure_symbolic()

    late_cancelled_callback = replace(
        cancelled,
        projection_generation=3,
        dialogue_state={"act": "effect_completed", "late": True},
        expert_evidence=[{"evidence_id": "ev-late"}],
        verified_facts=[{"fact_id": "fact-late", "value": "should-not-persist"}],
        updated_at="",
    )

    with pytest.raises(RuntimeError, match="terminal turn projection is immutable"):
        store.save_symbolic_projection(
            late_cancelled_callback,
            expected_generation=cancelled.projection_generation,
        )

    recovered = store.load_symbolic_projection(conversation.id)
    assert recovered == cancelled
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0
    assert recovered.expert_evidence == [{"evidence_id": "ev-before"}]
    assert recovered.verified_facts == [{"fact_id": "fact-before"}]
