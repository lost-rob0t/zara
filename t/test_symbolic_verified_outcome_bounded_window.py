from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
_WINDOW = 64


def _receipt(index: int) -> str:
    return f"zara.verified-outcome/v1:outcome:postcondition/tool-run-{index}"


def _projection(
    conversation_id: str,
    *,
    generation: int,
    runtime_generation: int,
    turn_id: str,
    receipts: list[str],
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
        runtime_generation=runtime_generation,
        turn_id=turn_id,
        outcome="success",
        dialogue_act="verified",
        dialogue_state={"act": "verified"},
        verified_outcome_refs=receipts,
        renderer_provenance=_SYMBOLIC_RENDERER,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def _store_with_full_window(tmp_path, *, conversation_id: str):
    store = ConversationStore(DatabaseManager(tmp_path / f"{conversation_id}.db"))
    conversation = store.create_conversation(
        "Verified bounded receipt window",
        conversation_id=conversation_id,
    )
    initial_receipts = [_receipt(index) for index in range(1, _WINDOW + 1)]
    current = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=1,
            turn_id="turn-64",
            receipts=initial_receipts,
        ),
        expected_generation=0,
    )
    return store, conversation, current, initial_receipts


def test_verified_effect_conversation_advances_past_bounded_receipt_window(tmp_path):
    store, conversation, current, initial_receipts = _store_with_full_window(
        tmp_path,
        conversation_id="conv-verified-window",
    )

    next_receipts = initial_receipts[1:] + [_receipt(_WINDOW + 1)]
    advanced = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=2,
            turn_id="turn-65",
            receipts=next_receipts,
        ),
        expected_generation=current.projection_generation,
    )

    advanced.assert_pure_symbolic()
    assert len(advanced.verified_outcome_refs) == _WINDOW
    assert advanced.verified_outcome_refs == next_receipts
    assert advanced.max_model_calls == 0
    assert advanced.provider_calls == 0
    assert advanced.model_calls == 0


def test_retired_receipt_cannot_reenter_as_fresh_after_window_compaction(tmp_path):
    store, conversation, current, initial_receipts = _store_with_full_window(
        tmp_path,
        conversation_id="conv-verified-window-replay",
    )

    compacted_receipts = initial_receipts[1:] + [_receipt(_WINDOW + 1)]
    compacted = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=2,
            turn_id="turn-65",
            receipts=compacted_receipts,
        ),
        expected_generation=current.projection_generation,
    )

    replayed_receipts = compacted_receipts[1:] + [initial_receipts[0]]
    with pytest.raises(RuntimeError, match="retired verified outcome replay rejected"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=3,
                runtime_generation=3,
                turn_id="turn-66",
                receipts=replayed_receipts,
            ),
            expected_generation=compacted.projection_generation,
        )
