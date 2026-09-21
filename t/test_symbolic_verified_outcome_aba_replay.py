from __future__ import annotations

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
_R1 = "zara.verified-outcome/v1:effect:tool-run-1"
_R2 = "zara.verified-outcome/v1:outcome:postcondition/tool-run-2"
_R3 = "zara.verified-outcome/v1:outcome:postcondition/tool-run-3"


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


def test_non_adjacent_verified_receipt_replay_is_rejected(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-aba.db"))
    conversation = store.create_conversation(
        "Verified ABA replay",
        conversation_id="conv-verified-aba",
    )
    first = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=1,
            turn_id="turn-1",
            receipts=[_R1],
        ),
        expected_generation=0,
    )
    second = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=2,
            turn_id="turn-2",
            receipts=[_R1, _R2],
        ),
        expected_generation=first.projection_generation,
    )

    try:
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=3,
                runtime_generation=3,
                turn_id="turn-3",
                receipts=[_R1],
            ),
            expected_generation=second.projection_generation,
        )
    except RuntimeError as error:
        assert "verified outcome evidence rewind rejected" in str(error)
    else:
        raise AssertionError(
            "later verified turn dropped durable receipts and replayed old R1 as fresh evidence"
        )


def test_verified_receipts_remain_monotonic_and_zero_model(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-monotonic.db"))
    conversation = store.create_conversation(
        "Verified monotonic evidence",
        conversation_id="conv-verified-monotonic",
    )
    first = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=1,
            turn_id="turn-1",
            receipts=[_R1],
        ),
        expected_generation=0,
    )
    second = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=2,
            turn_id="turn-2",
            receipts=[_R1, _R2],
        ),
        expected_generation=first.projection_generation,
    )
    third = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=3,
            runtime_generation=3,
            turn_id="turn-3",
            receipts=[_R1, _R2, _R3],
        ),
        expected_generation=second.projection_generation,
    )

    third.assert_pure_symbolic()
    assert third.verified_outcome_refs == [_R1, _R2, _R3]
    assert third.max_model_calls == 0
    assert third.provider_calls == 0
    assert third.model_calls == 0
