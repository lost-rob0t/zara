from __future__ import annotations

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)


def test_conversations_and_messages_round_trip_through_shared_database(tmp_path):
    path = tmp_path / "zara.db"
    db = DatabaseManager(path)
    store = ConversationStore(db)
    conversation = store.create_conversation("First chat", conversation_id="conv-1")

    message = MessageRecord(
        id="message-1",
        conversation_id=conversation.id,
        sequence=store.next_sequence(conversation.id),
        turn_id="turn-1",
        role=MessageRole.ASSISTANT,
        content="hello",
        status=MessageStatus.STREAMING,
        error="",
        tool_run_id=None,
        created_at="2026-08-12T12:00:00.000000",
        updated_at="2026-08-12T12:00:00.000000",
    )
    store.save_message(message)
    message.content += " world"
    message.status = MessageStatus.COMPLETE
    store.save_message(message)
    db.close()

    reopened_db = DatabaseManager(path)
    reopened = ConversationStore(reopened_db)
    state = reopened.load_state("conv-1")

    assert state.conversation.title == "First chat"
    assert state.messages[0].id == "message-1"
    assert state.messages[0].turn_id == "turn-1"
    assert state.messages[0].content == "hello world"
    assert state.messages[0].status is MessageStatus.COMPLETE
    reopened_db.close()


def test_history_rename_and_message_search_are_durable(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "history.db"))
    first = store.create_conversation("Alpha")
    second = store.create_conversation("Beta")

    message = MessageRecord(
        id="needle-message",
        conversation_id=second.id,
        sequence=store.next_sequence(second.id),
        turn_id=None,
        role=MessageRole.USER,
        content="find the quasar needle",
        status=MessageStatus.COMPLETE,
        created_at="2026-08-12T12:00:00.000000",
        updated_at="2026-08-12T12:00:00.000000",
    )
    store.save_message(message)

    renamed = store.rename_conversation(first.id, "Renamed Alpha")
    assert renamed.title == "Renamed Alpha"
    assert store.get_conversation(first.id).title == "Renamed Alpha"

    title_matches = store.list_conversations("renamed")
    assert [record.id for record in title_matches] == [first.id]

    body_matches = store.list_conversations("quasar needle")
    assert [record.id for record in body_matches] == [second.id]


def test_conversation_migration_coexists_with_later_registration_of_v1(tmp_path):
    db = DatabaseManager(tmp_path / "ordering.db")
    store = ConversationStore(db)
    store.create_conversation("Desktop first")

    db.register_migration(1, ["CREATE TABLE IF NOT EXISTS late_v1 (id INTEGER PRIMARY KEY)"])
    assert db.fetch_one("SELECT name FROM sqlite_master WHERE name = 'late_v1'") is not None
    versions = {row["version"] for row in db.fetch_all("SELECT version FROM schema_migrations")}
    assert versions == {1, 2}
