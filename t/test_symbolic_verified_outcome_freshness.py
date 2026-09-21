from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
_STALE_RECEIPT = "zara.verified-outcome/v1:effect:tool-run-7"
_FRESH_RECEIPT = "zara.verified-outcome/v1:outcome:postcondition/tool-run-8"


def _v2_receipt(runtime_generation: int, index: int) -> str:
    return (
        "zara.verified-outcome/v2:"
        f"{runtime_generation}:outcome:postcondition/tool-run-{index}"
    )


def _projection(
    conversation_id: str,
    *,
    generation: int,
    runtime_generation: int,
    turn_id: str,
    receipts: list[str],
    outcome: str = "success",
    dialogue_act: str = "verified",
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
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


def _seed_legacy_verified_projection(
    store: ConversationStore,
    conversation_id: str,
) -> SymbolicConversationProjection:
    """Simulate a pre-v2 persisted row without minting it through the current writer."""
    pending = store.save_symbolic_projection(
        _projection(
            conversation_id,
            generation=1,
            runtime_generation=7,
            turn_id="turn-7",
            receipts=[_STALE_RECEIPT],
            outcome="pending",
            dialogue_act="pending",
        ),
        expected_generation=0,
    )
    assert pending.model_calls == 0
    store.database.execute(
        """
        UPDATE desktop_symbolic_projections
        SET outcome = 'success',
            dialogue_act = 'verified',
            dialogue_state_json = '{"act":"verified"}'
        WHERE conversation_id = ? AND principal_id = ?
        """,
        (conversation_id, store.storage_principal_id),
    )
    migrated = store.load_symbolic_projection(conversation_id)
    assert migrated is not None
    migrated.assert_pure_symbolic()
    return migrated


def test_fresh_first_verified_write_rejects_generation_unbound_v1_evidence(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness-first-v1.db"))
    conversation = store.create_conversation(
        "Fresh verified v1 fence",
        conversation_id="conv-verified-freshness-first-v1",
    )

    with pytest.raises(RuntimeError, match="verified projection requires fresh outcome evidence"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=1,
                runtime_generation=1,
                turn_id="turn-1",
                receipts=[_STALE_RECEIPT],
            ),
            expected_generation=0,
        )


def test_migrated_legacy_v1_projection_remains_readable_and_continuable(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness-legacy.db"))
    conversation = store.create_conversation(
        "Migrated legacy verified state",
        conversation_id="conv-verified-freshness-legacy",
    )
    current = _seed_legacy_verified_projection(store, conversation.id)

    second = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=8,
            turn_id="turn-8",
            receipts=[_STALE_RECEIPT, _FRESH_RECEIPT],
        ),
        expected_generation=current.projection_generation,
    )

    second.assert_pure_symbolic()
    assert second.verified_outcome_refs == [_STALE_RECEIPT, _FRESH_RECEIPT]
    assert second.max_model_calls == 0
    assert second.provider_calls == 0
    assert second.model_calls == 0


def test_new_verified_turn_rejects_reused_postcondition_receipt(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness.db"))
    conversation = store.create_conversation(
        "Verified freshness",
        conversation_id="conv-verified-freshness",
    )
    first = _seed_legacy_verified_projection(store, conversation.id)

    with pytest.raises(RuntimeError, match="verified projection requires fresh outcome evidence"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                runtime_generation=8,
                turn_id="turn-8",
                receipts=[_STALE_RECEIPT],
            ),
            expected_generation=first.projection_generation,
        )


def test_new_verified_turn_accepts_fresh_postcondition_receipt_without_dropping_history(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness-ok.db"))
    conversation = store.create_conversation(
        "Verified freshness",
        conversation_id="conv-verified-freshness-ok",
    )
    first = _seed_legacy_verified_projection(store, conversation.id)

    second = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=8,
            turn_id="turn-8",
            receipts=[_STALE_RECEIPT, _FRESH_RECEIPT],
        ),
        expected_generation=first.projection_generation,
    )

    second.assert_pure_symbolic()
    assert second.verified_outcome_refs == [_STALE_RECEIPT, _FRESH_RECEIPT]
    assert second.provider_calls == 0
    assert second.model_calls == 0


def test_same_turn_verified_promotion_requires_current_generation_v2_evidence(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-freshness-same-turn-v2.db"))
    conversation = store.create_conversation(
        "Same turn v2 freshness",
        conversation_id="conv-verified-freshness-same-turn-v2",
    )
    pending = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=8,
            turn_id="turn-8",
            receipts=[_v2_receipt(7, 7)],
            outcome="pending",
            dialogue_act="pending",
        ),
        expected_generation=0,
    )

    with pytest.raises(RuntimeError, match="verified projection requires fresh outcome evidence"):
        store.save_symbolic_projection(
            replace(
                pending,
                projection_generation=2,
                outcome="success",
                dialogue_act="verified",
                dialogue_state={"act": "verified"},
                updated_at="",
            ),
            expected_generation=pending.projection_generation,
        )
