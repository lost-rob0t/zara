from __future__ import annotations

import sqlite3

from zara.conversation_schema import (
    CONVERSATION_SCHEMA_VERSION,
    LEGACY_LOCAL_PRINCIPAL_ID,
    PORTABLE_LOCAL_PRINCIPAL_ID,
    conversation_schema_statements,
)
from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.principals import PrincipalContext


def test_canonical_schema_has_desktop_compatible_tables_and_columns(tmp_path):
    path = tmp_path / "portable.db"
    conn = sqlite3.connect(path)
    try:
        for statement in conversation_schema_statements():
            conn.execute(statement)
        conversation_columns = {
            row[1] for row in conn.execute("PRAGMA table_info(desktop_conversations)")
        }
        message_columns = {
            row[1] for row in conn.execute("PRAGMA table_info(desktop_messages)")
        }
    finally:
        conn.close()

    assert CONVERSATION_SCHEMA_VERSION == 3
    assert conversation_columns == {
        "id",
        "title",
        "created_at",
        "updated_at",
        "provider",
        "model",
        "principal_id",
    }
    assert message_columns == {
        "id",
        "conversation_id",
        "sequence",
        "turn_id",
        "role",
        "content",
        "status",
        "error",
        "tool_run_id",
        "created_at",
        "updated_at",
        "principal_id",
    }


def test_local_owner_rows_use_platform_neutral_principal(tmp_path):
    db = DatabaseManager(tmp_path / "zara.db")
    store = ConversationStore(
        db,
        principal=PrincipalContext("uid:12345", kind="local-owner"),
    )
    conversation = store.create_conversation("Portable")

    row = db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        (conversation.id,),
    )
    assert row["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    assert store.storage_principal_id == PORTABLE_LOCAL_PRINCIPAL_ID


def test_android_style_portable_row_is_readable_by_different_desktop_uid(tmp_path):
    path = tmp_path / "from-android.db"
    conn = sqlite3.connect(path)
    try:
        for statement in conversation_schema_statements():
            conn.execute(statement)
        conn.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, '', '', ?)
            """,
            (
                "android-conversation",
                "Moved from Android",
                "2026-09-17T01:00:00.000000",
                "2026-09-17T01:00:00.000000",
                PORTABLE_LOCAL_PRINCIPAL_ID,
            ),
        )
        conn.commit()
    finally:
        conn.close()

    store = ConversationStore(
        DatabaseManager(path),
        principal=PrincipalContext("uid:99999", kind="local-owner"),
    )
    record = store.get_conversation("android-conversation")
    assert record is not None
    assert record.title == "Moved from Android"


def test_legacy_local_principals_migrate_to_portable_owner(tmp_path):
    path = tmp_path / "legacy.db"
    owner = PrincipalContext("uid:4242", kind="local-owner")
    db = DatabaseManager(path)
    store = ConversationStore(db, principal=owner)
    first = store.create_conversation("Current UID")
    second = store.create_conversation("Legacy sentinel")
    db.execute(
        "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
        (owner.principal_id, first.id),
    )
    db.execute(
        "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
        (LEGACY_LOCAL_PRINCIPAL_ID, second.id),
    )
    db.close()

    reopened_db = DatabaseManager(path)
    reopened = ConversationStore(reopened_db, principal=owner)
    rows = reopened_db.fetch_all(
        "SELECT id, principal_id FROM desktop_conversations ORDER BY id"
    )
    assert {row["principal_id"] for row in rows} == {PORTABLE_LOCAL_PRINCIPAL_ID}
    assert {record.id for record in reopened.list_conversations()} == {first.id, second.id}


def test_local_owner_claims_history_from_previous_numeric_uid_without_claiming_other_principals(tmp_path):
    path = tmp_path / "cross-uid.db"
    current_owner = PrincipalContext("uid:9001", kind="local-owner")
    db = DatabaseManager(path)
    store = ConversationStore(db, principal=current_owner)
    conversation = store.create_conversation("Restored local history", conversation_id="restored-local")
    message = MessageRecord(
        id="restored-message",
        conversation_id=conversation.id,
        sequence=store.next_sequence(conversation.id),
        turn_id="restored-turn",
        role=MessageRole.ASSISTANT,
        content="survives account migration",
        status=MessageStatus.COMPLETE,
        created_at="2026-09-17T01:00:00.000000",
        updated_at="2026-09-17T01:00:00.000000",
    )
    store.save_message(message)

    previous_uid = "uid:4242"
    db.execute(
        "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
        (previous_uid, conversation.id),
    )
    db.execute(
        "UPDATE desktop_messages SET principal_id = ? WHERE id = ?",
        (previous_uid, message.id),
    )
    for conversation_id, title, principal_id in (
        ("authenticated-history", "Authenticated history", "user:alice"),
        ("nonnumeric-uid-history", "Nonnumeric UID history", "uid:service"),
    ):
        db.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, '', '', ?)
            """,
            (
                conversation_id,
                title,
                "2026-09-17T01:00:00.000000",
                "2026-09-17T01:00:00.000000",
                principal_id,
            ),
        )
    db.close()

    reopened_db = DatabaseManager(path)
    reopened = ConversationStore(reopened_db, principal=current_owner)
    state = reopened.load_state(conversation.id)

    assert state.conversation.title == "Restored local history"
    assert [item.content for item in state.messages] == ["survives account migration"]
    migrated = reopened_db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        (conversation.id,),
    )
    authenticated = reopened_db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        ("authenticated-history",),
    )
    nonnumeric_uid = reopened_db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        ("nonnumeric-uid-history",),
    )
    assert migrated["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    assert authenticated["principal_id"] == "user:alice"
    assert nonnumeric_uid["principal_id"] == "uid:service"


def test_pre_portable_schema_upgrade_preserves_history_and_messages(tmp_path):
    path = tmp_path / "pre-portable.db"
    conn = sqlite3.connect(path)
    try:
        conn.executescript(
            """
            CREATE TABLE desktop_conversations (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                provider TEXT NOT NULL DEFAULT '',
                model TEXT NOT NULL DEFAULT ''
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
                FOREIGN KEY (conversation_id) REFERENCES desktop_conversations(id) ON DELETE CASCADE
            );
            """
        )
        timestamp = "2026-09-17T01:00:00.000000"
        conn.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model)
            VALUES (?, ?, ?, ?, '', '')
            """,
            ("legacy-conversation", "Before portable history", timestamp, timestamp),
        )
        conn.execute(
            """
            INSERT INTO desktop_messages
                (id, conversation_id, sequence, turn_id, role, content, status,
                 error, tool_run_id, created_at, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, '', NULL, ?, ?)
            """,
            (
                "legacy-message",
                "legacy-conversation",
                1,
                "legacy-turn",
                "assistant",
                "this row must survive the upgrade",
                "complete",
                timestamp,
                timestamp,
            ),
        )
        conn.commit()
    finally:
        conn.close()

    db = DatabaseManager(path)
    store = ConversationStore(
        db,
        principal=PrincipalContext("uid:9001", kind="local-owner"),
    )
    state = store.load_state("legacy-conversation")

    assert state.conversation.title == "Before portable history"
    assert [message.id for message in state.messages] == ["legacy-message"]
    assert [message.content for message in state.messages] == [
        "this row must survive the upgrade"
    ]
    conversation_owner = db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        ("legacy-conversation",),
    )
    message_owner = db.fetch_one(
        "SELECT principal_id FROM desktop_messages WHERE id = ?",
        ("legacy-message",),
    )
    assert conversation_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    assert message_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    db.close()
