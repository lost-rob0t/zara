from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
_WINDOW = 64


def _legacy_receipt(index: int) -> str:
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
    receipts: list[str],
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=projection_generation,
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


def test_full_legacy_window_cuts_over_to_generation_bound_v2_and_survives_restart(tmp_path):
    database_path = tmp_path / "verified-v2-cutover.db"
    first = ConversationStore(DatabaseManager(database_path))
    conversation = first.create_conversation(
        "Verified v2 cutover",
        conversation_id="conv-verified-v2-cutover",
    )
    legacy = [_legacy_receipt(index) for index in range(1, _WINDOW + 1)]
    current = first.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=1,
            runtime_generation=1,
            turn_id="turn-64",
            receipts=legacy,
        ),
        expected_generation=0,
    )

    cutover_receipt = _v2_receipt(2, _WINDOW + 1)
    cutover = first.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=2,
            runtime_generation=2,
            turn_id="turn-65",
            receipts=legacy[1:] + [cutover_receipt],
        ),
        expected_generation=current.projection_generation,
    )
    cutover.assert_pure_symbolic()
    assert len(cutover.verified_outcome_refs) == _WINDOW
    first.database.close()

    reopened = ConversationStore(DatabaseManager(database_path))
    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered == cutover

    next_receipt = _v2_receipt(3, _WINDOW + 2)
    advanced = reopened.save_symbolic_projection(
        replace(
            recovered,
            projection_generation=3,
            runtime_generation=3,
            turn_id="turn-66",
            verified_outcome_refs=recovered.verified_outcome_refs[1:] + [next_receipt],
            updated_at="",
        ),
        expected_generation=recovered.projection_generation,
    )
    advanced.assert_pure_symbolic()
    assert len(advanced.verified_outcome_refs) == _WINDOW
    assert advanced.max_model_calls == 0
    assert advanced.provider_calls == 0
    assert advanced.model_calls == 0


def test_v2_cutover_rejects_retired_legacy_receipt(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-v2-legacy-replay.db"))
    conversation = store.create_conversation(
        "Verified v2 legacy replay fence",
        conversation_id="conv-verified-v2-legacy-replay",
    )
    legacy = [_legacy_receipt(index) for index in range(1, _WINDOW + 1)]
    current = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=1,
            runtime_generation=1,
            turn_id="turn-64",
            receipts=legacy,
        ),
        expected_generation=0,
    )
    cutover = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=2,
            runtime_generation=2,
            turn_id="turn-65",
            receipts=legacy[1:] + [_v2_receipt(2, _WINDOW + 1)],
        ),
        expected_generation=current.projection_generation,
    )

    with pytest.raises(RuntimeError, match="retired verified outcome replay rejected"):
        store.save_symbolic_projection(
            replace(
                cutover,
                projection_generation=3,
                runtime_generation=3,
                turn_id="turn-66-legacy-replay",
                verified_outcome_refs=cutover.verified_outcome_refs[1:] + [legacy[0]],
                updated_at="",
            ),
            expected_generation=cutover.projection_generation,
        )


def test_retired_v2_receipt_cannot_reenter_as_fresh(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-v2-generation-replay.db"))
    conversation = store.create_conversation(
        "Verified v2 generation replay fence",
        conversation_id="conv-verified-v2-generation-replay",
    )
    initial = [_v2_receipt(index, index) for index in range(1, _WINDOW + 1)]
    current = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=1,
            runtime_generation=_WINDOW,
            turn_id="turn-64",
            receipts=initial,
        ),
        expected_generation=0,
    )
    compacted = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=2,
            runtime_generation=_WINDOW + 1,
            turn_id="turn-65",
            receipts=initial[1:] + [_v2_receipt(_WINDOW + 1, _WINDOW + 1)],
        ),
        expected_generation=current.projection_generation,
    )

    with pytest.raises(RuntimeError, match="retired verified outcome replay rejected"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=3,
                runtime_generation=_WINDOW + 2,
                turn_id="turn-66-replay",
                receipts=compacted.verified_outcome_refs[1:] + [initial[0]],
            ),
            expected_generation=compacted.projection_generation,
        )


def test_new_v2_receipt_must_bind_to_the_new_runtime_generation(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "verified-v2-generation.db"))
    conversation = store.create_conversation(
        "Verified v2 generation fence",
        conversation_id="conv-verified-v2-generation",
    )
    current = store.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=1,
            runtime_generation=7,
            turn_id="turn-7",
            receipts=[_v2_receipt(7, 7)],
        ),
        expected_generation=0,
    )

    with pytest.raises(RuntimeError, match="verified outcome generation mismatch rejected"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=2,
                runtime_generation=8,
                turn_id="turn-8",
                receipts=current.verified_outcome_refs + [_v2_receipt(7, 8)],
            ),
            expected_generation=current.projection_generation,
        )
