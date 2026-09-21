from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"


def _v1_receipt(index: int) -> str:
    return f"zara.verified-outcome/v1:outcome:postcondition/tool-run-{index}"


def _v2_receipt(runtime_generation: int, index: int) -> str:
    return (
        "zara.verified-outcome/v2:"
        f"{runtime_generation}:outcome:postcondition/tool-run-{index}"
    )


def _projection(
    conversation_id: str,
    *,
    projection_generation: int,
    runtime_generation: int,
    turn_id: str,
    outcome: str,
    dialogue_act: str,
    receipts: list[str],
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=projection_generation,
        runtime_generation=runtime_generation,
        turn_id=turn_id,
        outcome=outcome,
        dialogue_act=dialogue_act,
        dialogue_state={"act": dialogue_act},
        verified_outcome_refs=receipts,
        renderer_provenance=_SYMBOLIC_RENDERER,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def test_first_verified_write_cannot_mint_success_from_legacy_v1_only(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "fresh-v1-first-write.db"))
    conversation = store.create_conversation(
        "Fresh verified v1 rejection",
        conversation_id="conv-fresh-v1-first-write",
    )

    with pytest.raises(RuntimeError, match="verified projection requires fresh outcome evidence"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=1,
                runtime_generation=1,
                turn_id="turn-1",
                outcome="success",
                dialogue_act="verified",
                receipts=[_v1_receipt(1)],
            ),
            expected_generation=0,
        )


def test_same_turn_verified_promotion_requires_current_generation_v2_evidence(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "same-turn-stale-v2.db"))
    conversation = store.create_conversation(
        "Same-turn verified freshness fence",
        conversation_id="conv-same-turn-stale-v2",
    )
    pending = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=1,
            runtime_generation=8,
            turn_id="turn-8",
            outcome="pending",
            dialogue_act="pending",
            receipts=[_v2_receipt(7, 7)],
        ),
        expected_generation=0,
    )

    with pytest.raises(RuntimeError, match="verified projection requires fresh outcome evidence"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=2,
                runtime_generation=8,
                turn_id="turn-8",
                outcome="success",
                dialogue_act="verified",
                receipts=pending.verified_outcome_refs,
            ),
            expected_generation=pending.projection_generation,
        )
