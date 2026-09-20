from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


def _projection(
    conversation_id: str,
    *,
    generation: int = 1,
    runtime_generation: int = 7,
    project_id: str | None = "project-a",
    project_generation: int = 1,
    provider_calls: int = 0,
    model_calls: int = 0,
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
        runtime_generation=runtime_generation,
        project_id=project_id,
        project_generation=project_generation,
        dialogue_state={"act": "clarify", "intent": "inspect_project"},
        discourse_entities=[{"ref": "that", "entity_id": "file:flake.nix"}],
        unresolved_questions=[{"slot": "target", "prompt": "Which target?"}],
        expert_evidence=[{"expert": "DotfilesExpert", "evidence_id": "ev-1"}],
        verified_facts=[{"fact_id": "fact-1", "value": "flake.nix"}],
        renderer_provenance="symbolic-nlg/v1",
        provider_calls=provider_calls,
        model_calls=model_calls,
    )


def test_symbolic_projection_survives_restart_with_zero_provider_and_model_calls(tmp_path):
    path = tmp_path / "symbolic.db"
    db = DatabaseManager(path)
    store = ConversationStore(db)
    conversation = store.create_conversation("Pure symbolic", conversation_id="conv-symbolic")

    stored = store.save_symbolic_projection(
        _projection(conversation.id),
        expected_generation=0,
    )
    stored.assert_pure_symbolic()
    assert stored.provider_calls == 0
    assert stored.model_calls == 0
    assert stored.projection_generation == 1
    assert stored.unresolved_questions[0]["slot"] == "target"
    assert stored.expert_evidence[0]["evidence_id"] == "ev-1"
    db.close()

    reopened_db = DatabaseManager(path)
    reopened = ConversationStore(reopened_db)
    recovered = reopened.load_symbolic_projection(conversation.id)

    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0
    assert recovered.runtime_generation == 7
    assert recovered.project_id == "project-a"
    assert recovered.project_generation == 1
    assert recovered.dialogue_state == {"act": "clarify", "intent": "inspect_project"}
    assert recovered.discourse_entities == [{"entity_id": "file:flake.nix", "ref": "that"}]
    assert recovered.unresolved_questions == [{"prompt": "Which target?", "slot": "target"}]
    assert recovered.expert_evidence == [{"evidence_id": "ev-1", "expert": "DotfilesExpert"}]
    assert recovered.verified_facts == [{"fact_id": "fact-1", "value": "flake.nix"}]
    assert recovered.renderer_provenance == "symbolic-nlg/v1"
    reopened_db.close()


def test_symbolic_projection_is_conversation_scoped(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "isolation.db"))
    first = store.create_conversation("A", conversation_id="conv-a")
    second = store.create_conversation("B", conversation_id="conv-b")
    store.save_symbolic_projection(_projection(first.id), expected_generation=0)

    assert store.load_symbolic_projection(first.id) is not None
    assert store.load_symbolic_projection(second.id) is None
    assert store.load_symbolic_projection("missing") is None


def test_symbolic_projection_rejects_stale_runtime_and_usage_rewind(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "fences.db"))
    conversation = store.create_conversation("Fenced", conversation_id="conv-fenced")
    first = store.save_symbolic_projection(
        _projection(
            conversation.id,
            runtime_generation=9,
            provider_calls=2,
            model_calls=2,
        ),
        expected_generation=0,
    )

    with pytest.raises(RuntimeError, match="stale symbolic projection write"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                runtime_generation=10,
                provider_calls=2,
                model_calls=2,
            ),
            expected_generation=0,
        )

    with pytest.raises(RuntimeError, match="runtime_generation regression"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                runtime_generation=8,
                provider_calls=2,
                model_calls=2,
            ),
            expected_generation=first.projection_generation,
        )

    with pytest.raises(RuntimeError, match="provider-call ledger rewind"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                runtime_generation=9,
                provider_calls=1,
                model_calls=2,
            ),
            expected_generation=first.projection_generation,
        )

    with pytest.raises(RuntimeError, match="model-call ledger rewind"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                runtime_generation=9,
                provider_calls=2,
                model_calls=1,
            ),
            expected_generation=first.projection_generation,
        )


def test_pure_symbolic_assertion_rejects_provider_or_model_use():
    with pytest.raises(AssertionError, match="provider_calls=1"):
        _projection("provider-used", provider_calls=1).assert_pure_symbolic()
    with pytest.raises(AssertionError, match="model_calls=1"):
        _projection("model-used", model_calls=1).assert_pure_symbolic()


def test_project_switch_requires_new_generation(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "project-fence.db"))
    conversation = store.create_conversation("Projects", conversation_id="conv-project")
    first = store.save_symbolic_projection(
        _projection(conversation.id, project_id="project-a", project_generation=4),
        expected_generation=0,
    )

    with pytest.raises(RuntimeError, match="project switch must advance"):
        store.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=2,
                project_id="project-b",
                project_generation=4,
            ),
            expected_generation=first.projection_generation,
        )

    switched = store.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=2,
            runtime_generation=8,
            project_id="project-b",
            project_generation=5,
        ),
        expected_generation=first.projection_generation,
    )
    assert switched.project_id == "project-b"
    assert switched.project_generation == 5


def test_projection_corruption_fails_explicitly(tmp_path):
    db = DatabaseManager(tmp_path / "corrupt.db")
    store = ConversationStore(db)
    conversation = store.create_conversation("Corrupt", conversation_id="conv-corrupt")
    store.save_symbolic_projection(_projection(conversation.id), expected_generation=0)

    db.execute(
        """
        UPDATE desktop_symbolic_projections
        SET dialogue_state_json = '[]'
        WHERE conversation_id = ? AND principal_id = ?
        """,
        (conversation.id, store.storage_principal_id),
    )

    with pytest.raises(ValueError, match="not an object"):
        store.load_symbolic_projection(conversation.id)
