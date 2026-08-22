from __future__ import annotations

import pytest

import zara.memory as memory_module
from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.memory import MemoryManager


def _message(conversation_id: str, message_id: str, content: str) -> MessageRecord:
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


def test_conversation_store_requires_nonempty_principal(tmp_path):
    db = DatabaseManager(tmp_path / "principal.db")
    with pytest.raises(ValueError):
        ConversationStore(db, principal_id="   ")


def test_conversation_known_id_is_not_bearer_authority(tmp_path):
    db = DatabaseManager(tmp_path / "conversation-isolation.db")
    alice = ConversationStore(db, principal_id="alice")
    bob = ConversationStore(db, principal_id="bob")
    secret = bob.create_conversation("Bob secret", conversation_id="known-id")
    bob.save_message(_message(secret.id, "bob-message", "needle-private-content"))

    assert alice.get_conversation(secret.id) is None
    assert alice.list_conversations("Bob secret") == []
    assert alice.list_conversations("needle-private-content") == []
    assert alice.load_messages(secret.id) == []
    with pytest.raises(KeyError):
        alice.rename_conversation(secret.id, "stolen")
    with pytest.raises(KeyError):
        alice.next_sequence(secret.id)
    with pytest.raises(KeyError):
        alice.save_message(_message(secret.id, "alice-write", "cross owner"))

    assert bob.get_conversation(secret.id) is not None
    assert bob.load_messages(secret.id)[0].content == "needle-private-content"


def test_cross_owner_message_id_collision_cannot_mutate_existing_message(tmp_path):
    db = DatabaseManager(tmp_path / "message-id-isolation.db")
    alice = ConversationStore(db, principal_id="alice")
    bob = ConversationStore(db, principal_id="bob")
    alice_conversation = alice.create_conversation("Alice")
    bob_conversation = bob.create_conversation("Bob")
    bob.save_message(_message(bob_conversation.id, "shared-message-id", "bob secret"))

    with pytest.raises(KeyError):
        alice.save_message(
            _message(alice_conversation.id, "shared-message-id", "overwrite attempt")
        )

    assert bob.load_messages(bob_conversation.id)[0].content == "bob secret"
    assert alice.load_messages(alice_conversation.id) == []


def test_same_conversation_titles_remain_principal_local(tmp_path):
    db = DatabaseManager(tmp_path / "same-title.db")
    alice = ConversationStore(db, principal_id="alice")
    bob = ConversationStore(db, principal_id="bob")
    alice_conv = alice.create_conversation("Same label")
    bob_conv = bob.create_conversation("Same label")

    assert [record.id for record in alice.list_conversations()] == [alice_conv.id]
    assert [record.id for record in bob.list_conversations()] == [bob_conv.id]


def test_conversation_delete_and_export_are_owner_scoped(tmp_path):
    db = DatabaseManager(tmp_path / "delete-export.db")
    alice = ConversationStore(db, principal_id="alice")
    bob = ConversationStore(db, principal_id="bob")
    alice_conv = alice.create_conversation("Alice private")
    bob_conv = bob.create_conversation("Bob private", conversation_id="bob-known")
    alice.save_message(_message(alice_conv.id, "alice-message", "alice export body"))
    bob.save_message(_message(bob_conv.id, "bob-message", "bob export body"))

    assert alice.delete_conversation(bob_conv.id) is False
    assert bob.get_conversation(bob_conv.id) is not None

    export = alice.export_conversations()
    assert [entry["conversation"]["id"] for entry in export] == [alice_conv.id]
    assert export[0]["messages"][0]["content"] == "alice export body"
    assert "bob export body" not in repr(export)

    assert alice.delete_conversation(alice_conv.id) is True
    assert alice.get_conversation(alice_conv.id) is None
    assert bob.get_conversation(bob_conv.id) is not None


def test_memory_manager_requires_nonempty_principal(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    with pytest.raises(ValueError):
        MemoryManager(enabled=True, principal_id="\t")


def test_local_memory_isolation_duplicate_retrieval_and_forget(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    alice = MemoryManager(enabled=True, principal_id="alice")
    bob = MemoryManager(enabled=True, principal_id="bob")

    alice_id = alice.remember_fact("same private fact")
    bob_id = bob.remember_fact("same private fact")
    assert alice_id != bob_id

    assert [entry["id"] for entry in alice.retrieve("private fact")] == [alice_id]
    assert [entry["id"] for entry in bob.retrieve("private fact")] == [bob_id]
    assert alice.list_memories()[0]["metadata"]["principal_id"] == "alice"
    assert bob.list_memories()[0]["metadata"]["principal_id"] == "bob"

    assert alice.forget(all_memories=True) == 1
    assert alice.list_memories() == []
    assert [entry["id"] for entry in bob.list_memories()] == [bob_id]


def test_memory_export_is_owner_scoped(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    alice = MemoryManager(enabled=True, principal_id="alice")
    bob = MemoryManager(enabled=True, principal_id="bob")
    alice.remember_fact("alice export secret")
    bob.remember_fact("bob export secret")

    assert [entry["text"] for entry in alice.export_memories()] == ["alice export secret"]
    assert [entry["text"] for entry in bob.export_memories()] == ["bob export secret"]


def test_ephemeral_memory_never_opens_persistent_backend_and_dies_with_instance(monkeypatch):
    class ExplodingChroma:
        @staticmethod
        def Client():
            raise AssertionError("ephemeral memory must not open Chroma")

        @staticmethod
        def PersistentClient(*, path):
            raise AssertionError(f"ephemeral memory must not persist to {path}")

    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", True)
    monkeypatch.setattr(memory_module, "chromadb", ExplodingChroma)

    first = MemoryManager(
        enabled=True,
        principal_id="guest-1",
        persist_directory="/must/not/open",
        ephemeral=True,
    )
    first.remember_fact("temporary guest fact")
    assert first.get_health()["status"] == "ephemeral"
    assert [entry["text"] for entry in first.list_memories()] == ["temporary guest fact"]

    second = MemoryManager(
        enabled=True,
        principal_id="guest-1",
        persist_directory="/must/not/open",
        ephemeral=True,
    )
    assert second.list_memories() == []


def test_chroma_query_pushes_owner_filter_before_candidate_selection(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager(enabled=True, principal_id="alice")

    class FakeCollection:
        def __init__(self):
            self.query_kwargs = None

        def query(self, **kwargs):
            self.query_kwargs = kwargs
            return {
                "documents": [["owned"]],
                "ids": [["memory-a"]],
                "metadatas": [[{"principal_id": "alice", "kind": "fact", "tags": ""}]],
                "distances": [[0.1]],
            }

    fake = FakeCollection()
    manager._collection = fake
    result = manager.retrieve("owned", k=1)

    assert result[0]["id"] == "memory-a"
    assert fake.query_kwargs["where"] == {"principal_id": "alice"}


def test_chroma_listing_pushes_owner_filter_into_backend_get(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager(enabled=True, principal_id="alice")

    class FakeCollection:
        def __init__(self):
            self.get_kwargs = None

        def get(self, **kwargs):
            self.get_kwargs = kwargs
            return {
                "ids": ["memory-a"],
                "documents": ["owned"],
                "metadatas": [
                    {
                        "principal_id": "alice",
                        "kind": "fact",
                        "tags": "",
                        "created_at": "2026-08-22T00:00:00+00:00",
                    }
                ],
            }

    fake = FakeCollection()
    manager._collection = fake

    assert manager.list_memories()[0]["id"] == "memory-a"
    assert fake.get_kwargs["where"] == {"principal_id": "alice"}
