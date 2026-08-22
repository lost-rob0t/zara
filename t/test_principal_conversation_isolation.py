from __future__ import annotations

import sqlite3

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.server import PrincipalContext


def principal(name: str, kind: str = "authenticated") -> PrincipalContext:
    return PrincipalContext(principal_id=f"user:{name}", kind=kind)


def message(conversation_id: str, message_id: str, content: str) -> MessageRecord:
    return MessageRecord(
        id=message_id,
        conversation_id=conversation_id,
        sequence=1,
        turn_id="turn-1",
        role=MessageRole.USER,
        content=content,
        status=MessageStatus.COMPLETE,
        created_at="2026-08-22T00:00:00.000000",
        updated_at="2026-08-22T00:00:00.000000",
    )


def test_known_foreign_conversation_id_is_indistinguishable_from_missing(tmp_path):
    db = DatabaseManager(tmp_path / "isolation.db")
    alice = ConversationStore(db, principal=principal("alice"))
    bob = ConversationStore(db, principal=principal("bob"))
    secret = bob.create_conversation("Bob private", conversation_id="known-id")
    bob.save_message(message(secret.id, "bob-message", "needle only Bob knows"))

    assert alice.get_conversation(secret.id) is None
    assert alice.get_conversation("does-not-exist") is None
    assert alice.list_conversations("needle only Bob knows") == []
    assert alice.load_messages(secret.id) == []

    with pytest.raises(KeyError):
        alice.rename_conversation(secret.id, "stolen")
    with pytest.raises(KeyError):
        alice.rename_conversation("does-not-exist", "stolen")


def test_message_write_cannot_target_foreign_conversation(tmp_path):
    db = DatabaseManager(tmp_path / "message-owner.db")
    alice = ConversationStore(db, principal=principal("alice"))
    bob = ConversationStore(db, principal=principal("bob"))
    conversation = bob.create_conversation("Bob", conversation_id="shared-looking-id")

    with pytest.raises(KeyError):
        alice.save_message(message(conversation.id, "alice-message", "cross-user write"))

    assert bob.load_messages(conversation.id) == []


def test_identical_labels_and_search_text_remain_principal_local(tmp_path):
    db = DatabaseManager(tmp_path / "labels.db")
    alice = ConversationStore(db, principal=principal("alice"))
    bob = ConversationStore(db, principal=principal("bob"))

    alice_chat = alice.create_conversation("Same label", conversation_id="alice-conv")
    bob_chat = bob.create_conversation("Same label", conversation_id="bob-conv")
    alice.save_message(message(alice_chat.id, "alice-msg", "same searchable phrase"))
    bob.save_message(message(bob_chat.id, "bob-msg", "same searchable phrase"))

    assert [item.id for item in alice.list_conversations("same searchable phrase")] == [
        alice_chat.id
    ]
    assert [item.id for item in bob.list_conversations("same searchable phrase")] == [
        bob_chat.id
    ]


def test_persisted_rows_carry_immutable_principal_owner(tmp_path):
    db = DatabaseManager(tmp_path / "owner-column.db")
    alice = ConversationStore(db, principal=principal("alice"))
    conversation = alice.create_conversation("Owned", conversation_id="owned-conv")
    alice.save_message(message(conversation.id, "owned-message", "owned text"))

    conversation_row = db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        (conversation.id,),
    )
    message_row = db.fetch_one(
        "SELECT principal_id FROM desktop_messages WHERE id = ?",
        ("owned-message",),
    )

    assert conversation_row["principal_id"] == "user:alice"
    assert message_row["principal_id"] == "user:alice"


def test_remote_principal_cannot_claim_legacy_unowned_rows(tmp_path):
    path = tmp_path / "legacy.db"
    connection = sqlite3.connect(path)
    connection.executescript(
        """
        CREATE TABLE schema_migrations (
            version INTEGER PRIMARY KEY,
            applied_at INTEGER NOT NULL
        );
        INSERT INTO schema_migrations(version, applied_at) VALUES (2, 0);
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
            FOREIGN KEY(conversation_id) REFERENCES desktop_conversations(id) ON DELETE CASCADE,
            UNIQUE(conversation_id, sequence)
        );
        INSERT INTO desktop_conversations(
            id, title, created_at, updated_at, provider, model
        ) VALUES (
            'legacy-conv', 'Legacy private', '2026-01-01', '2026-01-01', '', ''
        );
        """
    )
    connection.commit()
    connection.close()

    remote = ConversationStore(
        DatabaseManager(path),
        principal=PrincipalContext("user:remote", kind="authenticated"),
    )

    assert remote.get_conversation("legacy-conv") is None


def test_local_owner_claims_legacy_rows_without_exposing_them_to_remote(tmp_path):
    path = tmp_path / "legacy-local.db"
    connection = sqlite3.connect(path)
    connection.executescript(
        """
        CREATE TABLE schema_migrations (
            version INTEGER PRIMARY KEY,
            applied_at INTEGER NOT NULL
        );
        INSERT INTO schema_migrations(version, applied_at) VALUES (2, 0);
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
            FOREIGN KEY(conversation_id) REFERENCES desktop_conversations(id) ON DELETE CASCADE,
            UNIQUE(conversation_id, sequence)
        );
        INSERT INTO desktop_conversations(
            id, title, created_at, updated_at, provider, model
        ) VALUES (
            'legacy-conv', 'Legacy private', '2026-01-01', '2026-01-01', '', ''
        );
        """
    )
    connection.commit()
    connection.close()

    local_owner = ConversationStore(
        DatabaseManager(path),
        principal=PrincipalContext.local_owner(),
    )
    assert local_owner.get_conversation("legacy-conv").title == "Legacy private"

    remote = ConversationStore(
        DatabaseManager(path),
        principal=principal("remote"),
    )
    assert remote.get_conversation("legacy-conv") is None
