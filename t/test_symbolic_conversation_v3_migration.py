from __future__ import annotations

import sqlite3

from zara.conversation_schema import CONVERSATION_SCHEMA_VERSION
from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


_VERIFIED_EFFECT_REF = "zara.verified-outcome/v1:effect:migrated-v2-turn"

_V2_SCHEMA = """
CREATE TABLE desktop_conversations (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    provider TEXT NOT NULL DEFAULT '',
    model TEXT NOT NULL DEFAULT '',
    principal_id TEXT NOT NULL DEFAULT 'local:owner'
);
CREATE TABLE desktop_messages (
    id TEXT PRIMARY KEY,
    conversation_id TEXT NOT NULL,
    sequence INTEGER NOT NULL,
    turn_id TEXT,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    status TEXT NOT NULL,
    error TEXT NOT NULL DEFAULT '',
    tool_run_id TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    principal_id TEXT NOT NULL DEFAULT 'local:owner',
    FOREIGN KEY(conversation_id)
        REFERENCES desktop_conversations(id) ON DELETE CASCADE,
    UNIQUE(conversation_id, sequence)
);
CREATE INDEX idx_desktop_messages_conversation
    ON desktop_messages(conversation_id, sequence);
"""


def _create_v2_database(path) -> None:
    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA foreign_keys = ON")
        connection.executescript(_V2_SCHEMA)
        connection.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                "conv-v2",
                "Existing v2 conversation",
                "2026-09-19T23:00:00.000000",
                "2026-09-19T23:01:00.000000",
                "",
                "",
                "local:owner",
            ),
        )
        connection.execute(
            """
            INSERT INTO desktop_messages
                (id, conversation_id, sequence, turn_id, role, content, status,
                 error, tool_run_id, created_at, updated_at, principal_id)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                "msg-v2",
                "conv-v2",
                1,
                "turn-v2",
                "user",
                "keep me byte-for-byte",
                "complete",
                "",
                None,
                "2026-09-19T23:00:30.000000",
                "2026-09-19T23:00:30.000000",
                "local:owner",
            ),
        )
        connection.execute("PRAGMA user_version = 2")
        connection.commit()
    finally:
        connection.close()


def test_existing_v2_database_upgrades_to_v3_without_losing_history(tmp_path):
    path = tmp_path / "portable-v2.db"
    _create_v2_database(path)

    assert CONVERSATION_SCHEMA_VERSION == 3

    database = DatabaseManager(path)
    store = ConversationStore(database)

    conversation = store.get_conversation("conv-v2")
    assert conversation is not None
    assert conversation.title == "Existing v2 conversation"
    messages = store.load_messages("conv-v2")
    assert len(messages) == 1
    assert messages[0].id == "msg-v2"
    assert messages[0].content == "keep me byte-for-byte"
    assert messages[0].turn_id == "turn-v2"

    projection = store.save_symbolic_projection(
        SymbolicConversationProjection(
            conversation_id="conv-v2",
            projection_generation=1,
            runtime_generation=1,
            turn_id="turn-symbolic",
            outcome="pending",
            dialogue_act="clarify",
            dialogue_state={"act": "clarify"},
            verified_outcome_refs=[_VERIFIED_EFFECT_REF],
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    projection.assert_pure_symbolic()
    assert projection.dialogue_act == "clarify"
    assert projection.verified_outcome_refs == [_VERIFIED_EFFECT_REF]
    database.close()

    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA foreign_keys = ON")
        tables = {
            row[0]
            for row in connection.execute(
                "SELECT name FROM sqlite_master WHERE type='table'"
            )
        }
        assert "desktop_symbolic_projections" in tables
        projection_columns = {
            row[1]
            for row in connection.execute("PRAGMA table_info(desktop_symbolic_projections)")
        }
        assert "dialogue_act" in projection_columns
        assert "verified_outcome_refs" in projection_columns
        indexes = {
            row[0]
            for row in connection.execute(
                "SELECT name FROM sqlite_master WHERE type='index'"
            )
        }
        assert "idx_desktop_symbolic_project" in indexes
        assert "idx_desktop_symbolic_turn" in indexes
        assert connection.execute(
            "SELECT content FROM desktop_messages WHERE id='msg-v2'"
        ).fetchone()[0] == "keep me byte-for-byte"
        dialogue_act, verified_refs = connection.execute(
            """
            SELECT dialogue_act, verified_outcome_refs
            FROM desktop_symbolic_projections
            WHERE conversation_id='conv-v2'
            """
        ).fetchone()
        assert dialogue_act == "clarify"
        assert verified_refs == _VERIFIED_EFFECT_REF

        connection.execute("DELETE FROM desktop_conversations WHERE id='conv-v2'")
        connection.commit()
        assert connection.execute(
            "SELECT COUNT(*) FROM desktop_messages WHERE conversation_id='conv-v2'"
        ).fetchone()[0] == 0
        assert connection.execute(
            "SELECT COUNT(*) FROM desktop_symbolic_projections WHERE conversation_id='conv-v2'"
        ).fetchone()[0] == 0
    finally:
        connection.close()
