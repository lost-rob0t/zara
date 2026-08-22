from __future__ import annotations

from types import SimpleNamespace

import pytest

import zara.memory as memory_module
from zara.memory import MemoryManager
from zara.server import PrincipalContext


def principal(name: str, kind: str = "authenticated") -> PrincipalContext:
    return PrincipalContext(principal_id=f"user:{name}", kind=kind)


class FakeCollection:
    def __init__(self) -> None:
        self.upserts = []
        self.queries = []
        self.deleted_ids = []
        self.query_result = {
            "documents": [[]],
            "ids": [[]],
            "metadatas": [[]],
            "distances": [[]],
        }

    def upsert(self, **kwargs):
        self.upserts.append(kwargs)

    def query(self, **kwargs):
        self.queries.append(kwargs)
        return self.query_result

    def get(self, **_kwargs):
        return {
            "ids": [item["ids"][0] for item in self.upserts],
            "documents": [item["documents"][0] for item in self.upserts],
            "metadatas": [item["metadatas"][0] for item in self.upserts],
        }

    def delete(self, ids):
        self.deleted_ids.extend(ids)


class RecordingClient:
    def __init__(self) -> None:
        self.collections = {}
        self.requested_names = []

    def get_or_create_collection(self, *, name, **_kwargs):
        self.requested_names.append(name)
        return self.collections.setdefault(name, FakeCollection())


def install_chroma(monkeypatch, client: RecordingClient) -> None:
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", True)
    monkeypatch.setattr(
        memory_module,
        "chromadb",
        SimpleNamespace(Client=lambda: client, PersistentClient=lambda path: client),
    )


def test_same_fact_text_is_not_deduplicated_across_principals(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    alice = MemoryManager(principal=principal("alice"))
    bob = MemoryManager(principal=principal("bob"))

    alice_id = alice.remember_fact("The shared-looking fact")
    bob_id = bob.remember_fact("The shared-looking fact")

    assert alice_id != bob_id
    assert [item["text"] for item in alice.retrieve("shared-looking fact")] == [
        "The shared-looking fact"
    ]
    assert [item["text"] for item in bob.retrieve("shared-looking fact")] == [
        "The shared-looking fact"
    ]
    assert all(
        item["metadata"]["principal_id"] == "user:alice"
        for item in alice.list_memories()
    )
    assert all(
        item["metadata"]["principal_id"] == "user:bob"
        for item in bob.list_memories()
    )


def test_same_session_label_is_independent_between_principals(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    alice = MemoryManager(principal=principal("alice"))
    bob = MemoryManager(principal=principal("bob"))

    alice.start_session(session_id="same-session")
    bob.start_session(session_id="same-session")
    alice.add_message("same-session", "user", "alice private")
    bob.add_message("same-session", "user", "bob private")

    assert alice.sessions["same-session"] == [("user", "alice private")]
    assert bob.sessions["same-session"] == [("user", "bob private")]
    assert alice.current_session_id == "same-session"
    assert bob.current_session_id == "same-session"


def test_forget_all_clears_only_bound_principal(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    alice = MemoryManager(principal=principal("alice"))
    bob = MemoryManager(principal=principal("bob"))
    alice.remember_fact("alice fact")
    bob.remember_fact("bob fact")

    assert alice.forget(all_memories=True) == 1
    assert alice.list_memories() == []
    assert [item["text"] for item in bob.list_memories()] == ["bob fact"]


def test_persistent_collections_are_principal_namespaced_and_owner_tagged(monkeypatch):
    client = RecordingClient()
    install_chroma(monkeypatch, client)

    alice = MemoryManager(
        principal=principal("alice"),
        collection_name="zara_memory",
        embedding_function=lambda texts: [[1.0] for _ in texts],
    )
    bob = MemoryManager(
        principal=principal("bob"),
        collection_name="zara_memory",
        embedding_function=lambda texts: [[1.0] for _ in texts],
    )
    alice.remember_fact("alice fact")
    bob.remember_fact("bob fact")

    assert len(set(client.requested_names)) == 2
    assert all(name.startswith("zara_memory_") for name in client.requested_names)
    alice_collection = client.collections[client.requested_names[0]]
    bob_collection = client.collections[client.requested_names[1]]
    assert alice_collection.upserts[0]["metadatas"][0]["principal_id"] == "user:alice"
    assert bob_collection.upserts[0]["metadatas"][0]["principal_id"] == "user:bob"


def test_persistent_query_requires_owner_filter_and_drops_mismatched_results(monkeypatch):
    client = RecordingClient()
    install_chroma(monkeypatch, client)
    alice = MemoryManager(
        principal=principal("alice"),
        collection_name="zara_memory",
        embedding_function=lambda texts: [[1.0] for _ in texts],
    )
    collection = client.collections[client.requested_names[0]]
    collection.query_result = {
        "documents": [["foreign secret", "alice fact"]],
        "ids": [["foreign", "alice"]],
        "metadatas": [[
            {"kind": "fact", "tags": "", "principal_id": "user:bob"},
            {"kind": "fact", "tags": "", "principal_id": "user:alice"},
        ]],
        "distances": [[0.01, 0.02]],
    }

    results = alice.retrieve("fact")

    assert collection.queries[-1]["where"] == {"principal_id": "user:alice"}
    assert [item["id"] for item in results] == ["alice"]


def test_guest_memory_never_initializes_persistent_backend(monkeypatch, tmp_path):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", True)
    monkeypatch.setattr(
        memory_module,
        "chromadb",
        SimpleNamespace(
            PersistentClient=lambda path: pytest.fail("guest must not open persistent memory"),
            Client=lambda: pytest.fail("guest must not open persistent memory"),
        ),
    )
    guest = MemoryManager(
        principal=PrincipalContext("guest:1", kind="guest"),
        persist_directory=str(tmp_path / "memory"),
        embedding_function=lambda texts: [[1.0] for _ in texts],
    )

    guest.remember_fact("ephemeral secret")
    assert [item["text"] for item in guest.list_memories()] == ["ephemeral secret"]
    guest.clear_principal_state()
    assert guest.list_memories() == []
    assert guest.sessions == {}
    assert guest.current_session_id is None
