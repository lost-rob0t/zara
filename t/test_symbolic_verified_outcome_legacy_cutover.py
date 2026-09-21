from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"


def _legacy_receipt(name: str) -> str:
    return f"zara.verified-outcome/v1:outcome:postcondition/{name}"


def _v2_receipt(runtime_generation: int, name: str) -> str:
    return (
        "zara.verified-outcome/v2:"
        f"{runtime_generation}:outcome:postcondition/{name}"
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


def _seed_migrated_legacy_projection(
    store: ConversationStore,
    *,
    conversation_id: str,
    receipt: str,
) -> SymbolicConversationProjection:
    with store.database.transaction(immediate=True) as connection:
        connection.execute(
            """
            INSERT INTO desktop_symbolic_projections (
                conversation_id, principal_id, turn_id, outcome,
                projection_generation, runtime_generation, project_id,
                project_generation, dialogue_act, dialogue_state_json,
                discourse_entities_json, unresolved_questions_json,
                expert_evidence_json, verified_facts_json,
                verified_outcome_refs, renderer_provenance,
                providers_enabled, max_model_calls,
                provider_calls, model_calls, updated_at
            ) VALUES (?, ?, ?, 'success', 1, 1, NULL, 0, 'verified', ?,
                      '[]', '[]', '[]', '[]', ?, ?, 0, 0, 0, 0, ?)
            """,
            (
                conversation_id,
                store.storage_principal_id,
                "turn-legacy",
                '{"act":"verified"}',
                receipt,
                _SYMBOLIC_RENDERER,
                "2026-09-21T00:00:00.000000",
            ),
        )
    projection = store.load_symbolic_projection(conversation_id)
    assert projection is not None
    projection.assert_pure_symbolic()
    return projection


def test_migrated_legacy_projection_cannot_mint_new_v1_and_cuts_over_after_restart(tmp_path):
    database_path = tmp_path / "verified-v1-cutover.db"
    first = ConversationStore(DatabaseManager(database_path))
    conversation = first.create_conversation(
        "Verified legacy cutover",
        conversation_id="conv-verified-legacy-cutover",
    )
    legacy_a = _legacy_receipt("legacy-a")
    legacy_b = _legacy_receipt("legacy-b")
    current = _seed_migrated_legacy_projection(
        first,
        conversation_id=conversation.id,
        receipt=legacy_a,
    )

    with pytest.raises(RuntimeError, match="retired verified outcome replay rejected"):
        first.save_symbolic_projection(
            _projection(
                conversation.id,
                projection_generation=2,
                runtime_generation=2,
                turn_id="turn-v1-rejected",
                receipts=[legacy_a, legacy_b],
            ),
            expected_generation=current.projection_generation,
        )

    first.database.close()
    reopened = ConversationStore(DatabaseManager(database_path))
    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    assert recovered.verified_outcome_refs == [legacy_a]

    cutover = reopened.save_symbolic_projection(
        _projection(
            conversation.id,
            projection_generation=2,
            runtime_generation=2,
            turn_id="turn-v2-cutover",
            receipts=[legacy_a, _v2_receipt(2, "fresh-b")],
        ),
        expected_generation=recovered.projection_generation,
    )
    cutover.assert_pure_symbolic()
    assert cutover.providers_enabled is False
    assert cutover.max_model_calls == 0
    assert cutover.provider_calls == 0
    assert cutover.model_calls == 0
