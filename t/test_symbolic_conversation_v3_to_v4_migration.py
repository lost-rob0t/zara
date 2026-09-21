from __future__ import annotations

from dataclasses import replace
import sqlite3

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore


_V3_SCHEMA = """
CREATE TABLE desktop_conversations (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    provider TEXT NOT NULL DEFAULT '',
    model TEXT NOT NULL DEFAULT '',
    principal_id TEXT NOT NULL DEFAULT 'local:owner'
);
CREATE TABLE desktop_symbolic_projections (
    conversation_id TEXT NOT NULL,
    principal_id TEXT NOT NULL DEFAULT 'local:owner',
    turn_id TEXT,
    outcome TEXT NOT NULL DEFAULT 'unknown'
        CHECK (outcome IN ('unknown', 'pending', 'success', 'cancelled', 'interrupted', 'error')),
    projection_generation INTEGER NOT NULL DEFAULT 0 CHECK (projection_generation >= 0),
    runtime_generation INTEGER NOT NULL DEFAULT 0 CHECK (runtime_generation >= 0),
    project_id TEXT,
    project_generation INTEGER NOT NULL DEFAULT 0 CHECK (project_generation >= 0),
    dialogue_act TEXT NOT NULL DEFAULT 'unknown',
    dialogue_state_json TEXT NOT NULL DEFAULT '{}',
    discourse_entities_json TEXT NOT NULL DEFAULT '[]',
    unresolved_questions_json TEXT NOT NULL DEFAULT '[]',
    expert_evidence_json TEXT NOT NULL DEFAULT '[]',
    verified_facts_json TEXT NOT NULL DEFAULT '[]',
    verified_outcome_refs TEXT NOT NULL DEFAULT '',
    renderer_provenance TEXT NOT NULL DEFAULT '',
    provider_calls INTEGER NOT NULL DEFAULT 0 CHECK (provider_calls >= 0),
    model_calls INTEGER NOT NULL DEFAULT 0 CHECK (model_calls >= 0),
    updated_at TEXT NOT NULL,
    PRIMARY KEY(conversation_id, principal_id),
    FOREIGN KEY(conversation_id)
        REFERENCES desktop_conversations(id) ON DELETE CASCADE
);
"""


def _seed_v3(path) -> None:
    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA foreign_keys = ON")
        connection.executescript(_V3_SCHEMA)
        connection.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, '', '', 'local:owner')
            """,
            (
                "conv-v3",
                "Existing v3 symbolic conversation",
                "2026-09-20T12:00:00.000000",
                "2026-09-20T12:01:00.000000",
            ),
        )
        connection.execute(
            """
            INSERT INTO desktop_symbolic_projections (
                conversation_id, principal_id, turn_id, outcome,
                projection_generation, runtime_generation, project_id,
                project_generation, dialogue_act, dialogue_state_json,
                discourse_entities_json, unresolved_questions_json,
                expert_evidence_json, verified_facts_json,
                verified_outcome_refs, renderer_provenance,
                provider_calls, model_calls, updated_at
            ) VALUES (?, 'local:owner', ?, 'pending', 3, 7, ?, 2, ?, ?, '[]', ?, ?, ?, ?, ?, 0, 0, ?)
            """,
            (
                "conv-v3",
                "turn-v3",
                "project-v3",
                "clarify",
                '{"topic":"migration"}',
                '[{"question":"which one?"}]',
                '[{"expert":"dotfiles"}]',
                '[{"fact":"history survives"}]',
                "zara.verified-outcome/v1:effect:v3-existing-effect",
                "symbolic-dcg/v1",
                "2026-09-20T12:01:00.000000",
            ),
        )
        connection.execute("PRAGMA user_version = 3")
        connection.commit()
    finally:
        connection.close()


def test_real_v3_projection_upgrades_to_v4_fail_closed_and_preserves_state(tmp_path) -> None:
    path = tmp_path / "portable-v3.db"
    _seed_v3(path)

    database = DatabaseManager(path)
    store = ConversationStore(database)

    columns = {
        row["name"]
        for row in database.fetch_all("PRAGMA table_info(desktop_symbolic_projections)")
    }
    assert "providers_enabled" in columns
    assert "max_model_calls" in columns

    projection = store.load_symbolic_projection("conv-v3")
    assert projection is not None
    assert projection.projection_generation == 3
    assert projection.runtime_generation == 7
    assert projection.project_id == "project-v3"
    assert projection.project_generation == 2
    assert projection.turn_id == "turn-v3"
    assert projection.dialogue_act == "clarify"
    assert projection.dialogue_state == {"topic": "migration"}
    assert projection.unresolved_questions == [{"question": "which one?"}]
    assert projection.expert_evidence == [{"expert": "dotfiles"}]
    assert projection.verified_facts == [{"fact": "history survives"}]
    assert projection.renderer_provenance == "symbolic-dcg/v1"
    assert projection.provider_calls == 0
    assert projection.model_calls == 0

    assert projection.providers_enabled is True
    assert projection.max_model_calls == 1
    with pytest.raises(AssertionError):
        projection.assert_pure_symbolic()

    authoritative = store.save_symbolic_projection(
        replace(
            projection,
            projection_generation=4,
            providers_enabled=False,
            max_model_calls=0,
        ),
        expected_generation=3,
    )
    authoritative.assert_pure_symbolic()
    database.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database)
    try:
        recovered = reopened.load_symbolic_projection("conv-v3")
        assert recovered is not None
        recovered.assert_pure_symbolic()
        assert recovered.projection_generation == 4
        assert recovered.dialogue_state == {"topic": "migration"}
        assert recovered.unresolved_questions == [{"question": "which one?"}]
        assert recovered.expert_evidence == [{"expert": "dotfiles"}]
        assert recovered.verified_facts == [{"fact": "history survives"}]
        assert recovered.providers_enabled is False
        assert recovered.max_model_calls == 0
        assert recovered.provider_calls == 0
        assert recovered.model_calls == 0
    finally:
        reopened_database.close()
