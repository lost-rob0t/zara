from __future__ import annotations

import sqlite3
from typing import Any

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection


@pytest.mark.parametrize(
    ("field", "raw_value", "expected_storage_class"),
    [
        ("projection_generation", 1.5, "real"),
        ("runtime_generation", 1.5, "real"),
        ("project_generation", 1.5, "real"),
        ("max_model_calls", 0.5, "real"),
        ("provider_calls", 0.5, "real"),
        ("model_calls", 0.5, "real"),
        ("max_model_calls", "not-an-integer", "text"),
    ],
)
def test_reopen_rejects_non_integer_persisted_numeric_storage(
    tmp_path,
    field: str,
    raw_value: Any,
    expected_storage_class: str,
) -> None:
    path = tmp_path / f"corrupt-{field}-{expected_storage_class}.db"
    database = DatabaseManager(path)
    store = ConversationStore(database)
    store.create_conversation("storage-class", conversation_id="conv-storage-class")
    stored = store.save_symbolic_projection(
        SymbolicConversationProjection(
            conversation_id="conv-storage-class",
            projection_generation=1,
            runtime_generation=1,
            turn_id="turn-storage-class",
            outcome="pending",
            project_id="project-storage-class",
            project_generation=1,
            dialogue_act="clarify",
            dialogue_state={"act": "clarify"},
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    stored.assert_pure_symbolic()
    database.close()

    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA ignore_check_constraints = ON")
        connection.execute(
            f"UPDATE desktop_symbolic_projections SET {field} = ? "
            "WHERE conversation_id = ?",
            (raw_value, "conv-storage-class"),
        )
        connection.commit()
        storage_class = connection.execute(
            f"SELECT typeof({field}) FROM desktop_symbolic_projections "
            "WHERE conversation_id = ?",
            ("conv-storage-class",),
        ).fetchone()[0]
        assert storage_class == expected_storage_class
    finally:
        connection.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database)
    try:
        with pytest.raises((TypeError, ValueError, RuntimeError, sqlite3.DatabaseError)):
            reopened.load_symbolic_projection("conv-storage-class")
    finally:
        reopened_database.close()


def test_schema_requires_integer_storage_class_for_symbolic_numeric_fields(tmp_path) -> None:
    path = tmp_path / "strict-symbolic-numeric-schema.db"
    database = DatabaseManager(path)
    ConversationStore(database)
    database.close()

    connection = sqlite3.connect(path)
    try:
        create_sql = connection.execute(
            "SELECT sql FROM sqlite_master "
            "WHERE type = 'table' AND name = 'desktop_symbolic_projections'"
        ).fetchone()[0]
    finally:
        connection.close()

    for field in (
        "projection_generation",
        "runtime_generation",
        "project_generation",
        "providers_enabled",
        "max_model_calls",
        "provider_calls",
        "model_calls",
    ):
        assert f"typeof({field})" in create_sql
        assert "'integer'" in create_sql
