from __future__ import annotations

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
_STALE_RECEIPT = "zara.verified-outcome/v1:effect:tool-run-7"
_FRESH_RECEIPT = "zara.verified-outcome/v1:outcome:postcondition/tool-run-8"


def _verified_projection(
    conversation_id: str,
    *,
    generation: int,
    runtime_generation: int,
    turn_id: str,
    receipt: str,
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
        runtime_generation=runtime_generation,
        turn_id=turn_id,
        outcome="success",
        dialogue_act="verified",
        dialogue_state={"act": "verified"},
        verified_outcome_refs=[receipt],
        renderer_provenance=_SYMBOLIC_RENDERER,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def test_new_verified_turn_rejects_reused_postcondition_receipt(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness.db"))
    conversation = store.create_conversation(
        "Verified freshness",
        conversation_id="conv-verified-freshness",
    )
    first = store.save_symbolic_projection(
        _verified_projection(
            conversation.id,
            generation=1,
            runtime_generation=7,
            turn_id="turn-7",
            receipt=_STALE_RECEIPT,
        ),
        expected_generation=0,
    )

    try:
        store.save_symbolic_projection(
            _verified_projection(
                conversation.id,
                generation=2,
                runtime_generation=8,
                turn_id="turn-8",
                receipt=_STALE_RECEIPT,
            ),
            expected_generation=first.projection_generation,
        )
    except RuntimeError as error:
        assert "verified projection requires fresh outcome evidence" in str(error)
    else:
        raise AssertionError("new verified turn reused a stale postcondition receipt")


def test_new_verified_turn_accepts_fresh_postcondition_receipt(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness-ok.db"))
    conversation = store.create_conversation(
        "Verified freshness",
        conversation_id="conv-verified-freshness-ok",
    )
    first = store.save_symbolic_projection(
        _verified_projection(
            conversation.id,
            generation=1,
            runtime_generation=7,
            turn_id="turn-7",
            receipt=_STALE_RECEIPT,
        ),
        expected_generation=0,
    )

    second = store.save_symbolic_projection(
        _verified_projection(
            conversation.id,
            generation=2,
            runtime_generation=8,
            turn_id="turn-8",
            receipt=_FRESH_RECEIPT,
        ),
        expected_generation=first.projection_generation,
    )

    second.assert_pure_symbolic()
    assert second.verified_outcome_refs == [_FRESH_RECEIPT]
    assert second.provider_calls == 0
    assert second.model_calls == 0
