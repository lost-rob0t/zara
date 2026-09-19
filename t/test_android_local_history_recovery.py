from __future__ import annotations

import sqlite3

from zara.conversation_schema import (
    PORTABLE_LOCAL_PRINCIPAL_ID,
    conversation_schema_statements,
)
from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, MessageStatus
from zara.principals import PrincipalContext


_TIMESTAMP = "2026-09-18T12:00:00.000000"


def _install_schema(path) -> sqlite3.Connection:
    connection = sqlite3.connect(path)
    for statement in conversation_schema_statements():
        connection.execute(statement)
    return connection


def _insert_conversation(
    connection: sqlite3.Connection,
    *,
    conversation_id: str,
    principal_id: str,
) -> None:
    connection.execute(
        """
        INSERT INTO desktop_conversations
            (id, title, created_at, updated_at, provider, model, principal_id)
        VALUES (?, ?, ?, ?, '', '', ?)
        """,
        (
            conversation_id,
            "Android local turn",
            _TIMESTAMP,
            _TIMESTAMP,
            principal_id,
        ),
    )


def _insert_message(
    connection: sqlite3.Connection,
    *,
    message_id: str,
    conversation_id: str,
    sequence: int,
    role: str,
    content: str,
    status: str,
    principal_id: str,
    turn_id: str | None = None,
) -> None:
    connection.execute(
        """
        INSERT INTO desktop_messages (
            id, conversation_id, sequence, turn_id, role, content, status,
            error, tool_run_id, created_at, updated_at, principal_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, '', NULL, ?, ?, ?)
        """,
        (
            message_id,
            conversation_id,
            sequence,
            turn_id,
            role,
            content,
            status,
            _TIMESTAMP,
            _TIMESTAMP,
            principal_id,
        ),
    )


def test_android_pending_local_turn_recovers_cancelled_and_persists_fence(tmp_path):
    """A process restart must never resurrect an Android in-flight local turn."""

    path = tmp_path / "zara.db"
    connection = _install_schema(path)
    try:
        _insert_conversation(
            connection,
            conversation_id="android-local",
            principal_id=PORTABLE_LOCAL_PRINCIPAL_ID,
        )
        _insert_message(
            connection,
            message_id="android-user",
            conversation_id="android-local",
            sequence=1,
            role="user",
            content="work offline",
            status="complete",
            principal_id=PORTABLE_LOCAL_PRINCIPAL_ID,
        )
        _insert_message(
            connection,
            message_id="android-assistant",
            conversation_id="android-local",
            sequence=2,
            turn_id="local-turn-1",
            role="assistant",
            content="",
            status="pending",
            principal_id=PORTABLE_LOCAL_PRINCIPAL_ID,
        )
        connection.commit()
    finally:
        connection.close()

    db = DatabaseManager(path)
    store = ConversationStore(
        db,
        principal=PrincipalContext("uid:9001", kind="local-owner"),
    )
    state = store.load_state("android-local")

    assert [message.status for message in state.messages] == [
        MessageStatus.COMPLETE,
        MessageStatus.CANCELLED,
    ]
    assert state.messages[1].error == "Interrupted when Zara stopped."
    assert state.active_turn_id is None

    recovered = db.fetch_one(
        "SELECT status, error FROM desktop_messages WHERE id = ?",
        ("android-assistant",),
    )
    assert recovered["status"] == MessageStatus.CANCELLED.value
    assert recovered["error"] == "Interrupted when Zara stopped."
    db.close()


def test_local_restart_recovery_does_not_claim_authenticated_pending_turn(tmp_path):
    """Local crash recovery must not mutate another principal's pending work."""

    path = tmp_path / "zara.db"
    connection = _install_schema(path)
    try:
        _insert_conversation(
            connection,
            conversation_id="authenticated",
            principal_id="user:alice",
        )
        _insert_message(
            connection,
            message_id="authenticated-pending",
            conversation_id="authenticated",
            sequence=1,
            turn_id="remote-turn-1",
            role="assistant",
            content="",
            status="pending",
            principal_id="user:alice",
        )
        connection.commit()
    finally:
        connection.close()

    db = DatabaseManager(path)
    store = ConversationStore(
        db,
        principal=PrincipalContext("uid:9001", kind="local-owner"),
    )

    assert store.get_conversation("authenticated") is None
    row = db.fetch_one(
        "SELECT principal_id, status, error FROM desktop_messages WHERE id = ?",
        ("authenticated-pending",),
    )
    assert row["principal_id"] == "user:alice"
    assert row["status"] == MessageStatus.PENDING.value
    assert row["error"] == ""
    db.close()
