from __future__ import annotations

from zara.conversation_schema import PORTABLE_LOCAL_PRINCIPAL_ID
from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.principals import PrincipalContext


def _message(
    store: ConversationStore,
    conversation_id: str,
    *,
    message_id: str,
    status: MessageStatus = MessageStatus.COMPLETE,
    error: str = "",
) -> MessageRecord:
    return MessageRecord(
        id=message_id,
        conversation_id=conversation_id,
        sequence=store.next_sequence(conversation_id),
        turn_id=f"turn-{message_id}",
        role=MessageRole.ASSISTANT,
        content=f"payload-{message_id}",
        status=status,
        error=error,
        created_at="2026-09-17T01:00:00.000000",
        updated_at="2026-09-17T01:00:00.000000",
    )


def test_repeated_cross_uid_migration_is_idempotent_and_does_not_claim_lookalikes(tmp_path):
    path = tmp_path / "migration-idempotent.db"
    db = DatabaseManager(path)
    original_owner = PrincipalContext("uid:9001", kind="local-owner")
    store = ConversationStore(db, principal=original_owner)
    conversation = store.create_conversation(
        "Restored local history",
        conversation_id="restored-local",
    )
    store.save_message(
        _message(store, conversation.id, message_id="restored-message")
    )

    previous_uid = "uid:4242"
    db.execute(
        "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
        (previous_uid, conversation.id),
    )
    db.execute(
        "UPDATE desktop_messages SET principal_id = ? WHERE id = ?",
        (previous_uid, "restored-message"),
    )

    timestamp = "2026-09-17T01:00:00.000000"
    untouched_principals = {
        "empty-uid": "uid:",
        "signed-uid": "uid:+1",
        "mixed-uid": "uid:12x",
        "service-uid": "uid:service",
        "authenticated": "user:alice",
    }
    for conversation_id, principal_id in untouched_principals.items():
        db.execute(
            """
            INSERT INTO desktop_conversations
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, '', '', ?)
            """,
            (conversation_id, conversation_id, timestamp, timestamp, principal_id),
        )
    db.close()

    for current_uid in ("uid:9001", "uid:7777", "uid:31337"):
        reopened_db = DatabaseManager(path)
        reopened = ConversationStore(
            reopened_db,
            principal=PrincipalContext(current_uid, kind="local-owner"),
        )
        state = reopened.load_state(conversation.id)
        assert [message.id for message in state.messages] == ["restored-message"]
        assert state.messages[0].content == "payload-restored-message"
        assert reopened.storage_principal_id == PORTABLE_LOCAL_PRINCIPAL_ID
        reopened_db.close()

    inspection_db = DatabaseManager(path)
    inspection_db.connect()
    restored_owner = inspection_db.fetch_one(
        "SELECT principal_id FROM desktop_conversations WHERE id = ?",
        (conversation.id,),
    )
    restored_message_owner = inspection_db.fetch_one(
        "SELECT principal_id FROM desktop_messages WHERE id = ?",
        ("restored-message",),
    )
    assert restored_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID
    assert restored_message_owner["principal_id"] == PORTABLE_LOCAL_PRINCIPAL_ID

    rows = inspection_db.fetch_all(
        "SELECT id, principal_id FROM desktop_conversations"
    )
    principals_by_id = {row["id"]: row["principal_id"] for row in rows}
    for conversation_id, principal_id in untouched_principals.items():
        assert principals_by_id[conversation_id] == principal_id
    assert len(rows) == 1 + len(untouched_principals)
    assert inspection_db.fetch_one(
        "SELECT COUNT(*) AS count FROM desktop_messages"
    )["count"] == 1
    inspection_db.close()


def test_interrupted_turn_recovery_is_idempotent_and_preserves_existing_error(tmp_path):
    path = tmp_path / "recovery-idempotent.db"
    db = DatabaseManager(path)
    store = ConversationStore(db)
    conversation = store.create_conversation("Crash recovery")
    store.save_message(
        _message(
            store,
            conversation.id,
            message_id="pending-with-error",
            status=MessageStatus.PENDING,
            error="provider disconnected",
        )
    )
    store.save_message(
        _message(
            store,
            conversation.id,
            message_id="streaming-empty-error",
            status=MessageStatus.STREAMING,
        )
    )
    db.close()

    first_db = DatabaseManager(path)
    first = ConversationStore(first_db)
    first_state = first.load_state(conversation.id)
    first_rows = {message.id: message for message in first_state.messages}
    assert first_state.active_turn_id is None
    assert first_rows["pending-with-error"].status is MessageStatus.CANCELLED
    assert first_rows["pending-with-error"].error == "provider disconnected"
    assert first_rows["streaming-empty-error"].status is MessageStatus.CANCELLED
    assert first_rows["streaming-empty-error"].error == "Interrupted when Zara stopped."
    first_updated = {
        message.id: message.updated_at for message in first.load_messages(conversation.id)
    }
    first_db.close()

    second_db = DatabaseManager(path)
    second = ConversationStore(second_db)
    second_state = second.load_state(conversation.id)
    second_rows = {message.id: message for message in second_state.messages}
    second_updated = {
        message.id: message.updated_at for message in second.load_messages(conversation.id)
    }

    assert second_state.active_turn_id is None
    assert {message.status for message in second_rows.values()} == {MessageStatus.CANCELLED}
    assert second_rows["pending-with-error"].error == "provider disconnected"
    assert second_rows["streaming-empty-error"].error == "Interrupted when Zara stopped."
    assert second_updated == first_updated
    assert len(second_rows) == 2
    second_db.close()
