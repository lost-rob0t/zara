from __future__ import annotations

import threading

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.server import PrincipalContext


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


def test_reload_marks_incomplete_turns_interrupted_instead_of_restoring_phantom_activity(tmp_path):
    path = tmp_path / "interrupted.db"
    db = DatabaseManager(path)
    store = ConversationStore(db)
    conversation = store.create_conversation("Interrupted")
    message = MessageRecord(
        id="interrupted-message",
        conversation_id=conversation.id,
        sequence=store.next_sequence(conversation.id),
        turn_id="dead-turn",
        role=MessageRole.ASSISTANT,
        content="partial response",
        status=MessageStatus.STREAMING,
        created_at="2026-08-12T12:00:00.000000",
        updated_at="2026-08-12T12:00:00.000000",
    )
    store.save_message(message)
    db.close()

    reopened_db = DatabaseManager(path)
    reopened = ConversationStore(reopened_db)
    state = reopened.load_state(conversation.id)

    assert state.active_turn_id is None
    assert state.messages[0].status is MessageStatus.CANCELLED
    assert state.messages[0].error == "Interrupted when Zara stopped."
    persisted = reopened.load_messages(conversation.id)[0]
    assert persisted.status is MessageStatus.CANCELLED
    assert persisted.error == "Interrupted when Zara stopped."
    reopened_db.close()


def test_append_message_once_is_idempotent_for_exact_replay(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "once.db"))
    conversation = store.create_conversation("Voice")

    first, first_inserted = store.append_message_once(
        conversation.id,
        message_id="voice-final:session-1:final-1",
        role=MessageRole.USER,
        content="normalized final transcript",
        status=MessageStatus.COMPLETE,
        turn_id="turn-1",
    )
    replay, replay_inserted = store.append_message_once(
        conversation.id,
        message_id="voice-final:session-1:final-1",
        role=MessageRole.USER,
        content="normalized final transcript",
        status=MessageStatus.COMPLETE,
        turn_id="turn-1",
    )

    assert first_inserted is True
    assert replay_inserted is False
    assert replay == first
    assert [message.id for message in store.load_messages(conversation.id)] == [first.id]


def test_append_message_once_rejects_conflicting_replay(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "conflict.db"))
    conversation = store.create_conversation("Voice")
    store.append_message_once(
        conversation.id,
        message_id="voice-final:session-1:final-1",
        role=MessageRole.USER,
        content="first normalized final",
        status=MessageStatus.COMPLETE,
        turn_id="turn-1",
    )

    with pytest.raises(ValueError, match="message replay conflicts"):
        store.append_message_once(
            conversation.id,
            message_id="voice-final:session-1:final-1",
            role=MessageRole.USER,
            content="different transcript",
            status=MessageStatus.COMPLETE,
            turn_id="turn-1",
        )

    persisted = store.load_messages(conversation.id)
    assert [(message.id, message.content) for message in persisted] == [
        ("voice-final:session-1:final-1", "first normalized final")
    ]


def test_append_message_once_assigns_unique_sequences_under_concurrency(tmp_path):
    store = ConversationStore(DatabaseManager(tmp_path / "concurrent.db"))
    conversation = store.create_conversation("Voice")
    barrier = threading.Barrier(8)
    results: list[MessageRecord] = []
    failures: list[BaseException] = []
    lock = threading.Lock()

    def append(index: int) -> None:
        try:
            barrier.wait()
            message, inserted = store.append_message_once(
                conversation.id,
                message_id=f"voice-final:session-1:{index}",
                role=MessageRole.USER,
                content=f"message {index}",
                status=MessageStatus.COMPLETE,
                turn_id=f"turn-{index}",
            )
            assert inserted is True
            with lock:
                results.append(message)
        except BaseException as error:
            with lock:
                failures.append(error)

    threads = [threading.Thread(target=append, args=(index,)) for index in range(8)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

    assert failures == []
    assert sorted(message.sequence for message in results) == list(range(1, 9))
    assert [message.sequence for message in store.load_messages(conversation.id)] == list(range(1, 9))


def test_append_message_once_never_crosses_principal_or_conversation_ownership(tmp_path):
    db = DatabaseManager(tmp_path / "principals.db")
    alice = ConversationStore(db, principal=PrincipalContext("device:alice", "remote-device"))
    bob = ConversationStore(db, principal=PrincipalContext("device:bob", "remote-device"))
    alice_conversation = alice.create_conversation("Alice", conversation_id="alice-conv")
    bob_conversation = bob.create_conversation("Bob", conversation_id="bob-conv")

    alice.append_message_once(
        alice_conversation.id,
        message_id="shared-replay-id",
        role=MessageRole.USER,
        content="alice transcript",
        status=MessageStatus.COMPLETE,
    )

    with pytest.raises(KeyError):
        bob.append_message_once(
            alice_conversation.id,
            message_id="shared-replay-id",
            role=MessageRole.USER,
            content="alice transcript",
            status=MessageStatus.COMPLETE,
        )

    bob_message, inserted = bob.append_message_once(
        bob_conversation.id,
        message_id="shared-replay-id",
        role=MessageRole.USER,
        content="bob transcript",
        status=MessageStatus.COMPLETE,
    )
    assert inserted is True
    assert bob_message.conversation_id == bob_conversation.id
    assert alice.load_messages(alice_conversation.id)[0].content == "alice transcript"
    assert bob.load_messages(bob_conversation.id)[0].content == "bob transcript"
