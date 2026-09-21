from __future__ import annotations

import json

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_SYMBOLIC_RENDERER = "symbolic-dcg/v1"


def _typed_evidence() -> dict[str, object]:
    return {
        "expert_id": "zara:expert/python",
        "invocation_id": "invocation:python:1",
        "evidence_refs": ["evidence:python:1"],
        "verdict": "succeeded",
        "model_calls": 0,
        "explanation": {
            "symbolic_terms": ["python", "inspect"],
            "trace": ["expert.invoke", "expert.complete"],
        },
    }


def _projection(
    conversation_id: str,
    evidence: list[dict[str, object]],
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=1,
        runtime_generation=1,
        turn_id="turn:expert:1",
        outcome="success",
        project_id="project:zara",
        project_generation=1,
        dialogue_act="expert.answer",
        dialogue_state={"active_project": "project:zara"},
        expert_evidence=evidence,
        renderer_provenance=_SYMBOLIC_RENDERER,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def test_typed_symbolic_expert_evidence_is_accepted() -> None:
    projection = _projection("conversation:typed-evidence", [_typed_evidence()])

    projection.assert_pure_symbolic()


def test_provider_shaped_expert_metadata_is_rejected_before_write(tmp_path) -> None:
    database = DatabaseManager(tmp_path / "provider-shaped.db")
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Provider-shaped evidence",
        conversation_id="conversation:provider-shaped",
    )
    poisoned = _typed_evidence()
    poisoned["usage"] = {"provider_calls": 1}

    with pytest.raises((TypeError, ValueError, AssertionError)):
        store.save_symbolic_projection(
            _projection(conversation.id, [poisoned]),
            expected_generation=0,
        )


def test_expert_model_calls_must_be_exact_integer_zero_before_write(tmp_path) -> None:
    database = DatabaseManager(tmp_path / "model-calls.db")
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Expert model calls",
        conversation_id="conversation:expert-model-calls",
    )

    for invalid in (True, 1, 1.0, "0"):
        poisoned = _typed_evidence()
        poisoned["model_calls"] = invalid
        with pytest.raises((TypeError, ValueError, AssertionError)):
            store.save_symbolic_projection(
                _projection(conversation.id, [poisoned]),
                expected_generation=0,
            )


def test_restart_rejects_corrupted_provider_metadata_inside_expert_evidence(tmp_path) -> None:
    path = tmp_path / "restart-provider-shaped.db"
    database = DatabaseManager(path)
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Restart provider-shaped evidence",
        conversation_id="conversation:restart-provider-shaped",
    )
    stored = store.save_symbolic_projection(
        _projection(conversation.id, [_typed_evidence()]),
        expected_generation=0,
    )
    stored.assert_pure_symbolic()

    poisoned = _typed_evidence()
    poisoned["provider_calls"] = 1
    with database.transaction(immediate=True) as connection:
        connection.execute(
            """
            UPDATE desktop_symbolic_projections
            SET expert_evidence_json = ?
            WHERE conversation_id = ? AND principal_id = ?
            """,
            (
                json.dumps([poisoned], sort_keys=True, separators=(",", ":")),
                conversation.id,
                store.storage_principal_id,
            ),
        )
    database.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database)
    with pytest.raises((TypeError, ValueError, AssertionError)):
        reopened.load_symbolic_projection(conversation.id)
    reopened_database.close()
