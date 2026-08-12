import urllib.error
from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage, HumanMessage, SystemMessage

import zara.agent as agent_module
import zara.memory as memory_module
from zara.agent import AgentManager
from zara.agent.conversation import ConversationManager
from zara.memory import MemoryManager
from zara.agent.tools.builtin_tools import build_forget_tool, build_memory_list_tool


class FakeCollection:
    def __init__(self, query_error=None, delete_error=None):
        self.query_error = query_error
        self.delete_error = delete_error
        self.upserts = []
        self.deleted_ids = []

    def query(self, **kwargs):
        if self.query_error:
            raise self.query_error
        return {
            "documents": [["remembered text"]],
            "ids": [["memory-1"]],
            "metadatas": [[{"kind": "fact", "tags": ""}]],
            "distances": [[0.1]],
        }

    def upsert(self, **kwargs):
        self.upserts.append(kwargs)

    def get(self, **_kwargs):
        return {
            "ids": [item["ids"][0] for item in self.upserts],
            "documents": [item["documents"][0] for item in self.upserts],
            "metadatas": [item["metadatas"][0] for item in self.upserts],
        }

    def delete(self, ids):
        if self.delete_error:
            raise self.delete_error
        self.deleted_ids.extend(ids)
        selected = set(ids)
        self.upserts = [
            item for item in self.upserts if item["ids"][0] not in selected
        ]


class FakeClient:
    def __init__(self, collection):
        self.collection = collection

    def get_or_create_collection(self, **kwargs):
        return self.collection


def install_chroma(monkeypatch, client):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", True)
    monkeypatch.setattr(
        memory_module,
        "chromadb",
        SimpleNamespace(Client=lambda: client, PersistentClient=lambda path: client),
    )


def test_disabled_memory_does_not_initialize_backend(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", True)
    monkeypatch.setattr(
        memory_module,
        "chromadb",
        SimpleNamespace(Client=lambda: pytest.fail("backend should not initialize")),
    )

    manager = MemoryManager(enabled=False)

    assert manager.get_health() == {"status": "disabled", "error": None}
    assert manager.retrieve("anything") == []


def test_chroma_client_failure_uses_local_fallback(monkeypatch, caplog):
    install_chroma(
        monkeypatch,
        SimpleNamespace(get_or_create_collection=lambda **kwargs: None),
    )
    monkeypatch.setattr(
        memory_module.chromadb,
        "Client",
        lambda: (_ for _ in ()).throw(RuntimeError("client unavailable")),
    )

    manager = MemoryManager(embedding_function=lambda texts: [[1.0] for _ in texts])

    assert manager.health_status == "local_fallback"
    assert "client unavailable" in manager.health_error
    assert "using local memory" in caplog.text


def test_embedding_initialization_failure_uses_local_fallback(monkeypatch):
    install_chroma(monkeypatch, FakeClient(FakeCollection()))
    monkeypatch.setattr(
        memory_module,
        "embedding_functions",
        SimpleNamespace(
            ONNXMiniLM_L6_V2=lambda: (_ for _ in ()).throw(
                RuntimeError("embedding unavailable")
            )
        ),
    )

    manager = MemoryManager(settings={"embedding_backend": "onnx"})

    assert manager.health_status == "local_fallback"
    assert "embedding unavailable" in manager.health_error


def test_unavailable_ollama_embedding_falls_back_during_retrieval(monkeypatch):
    collection = FakeCollection(urllib.error.URLError("ollama unavailable"))
    install_chroma(monkeypatch, FakeClient(collection))
    manager = MemoryManager(settings={"embedding_backend": "ollama"})

    assert manager.health_status == "persistent"
    assert manager.retrieve("query") == []
    assert manager.health_status == "local_fallback"
    assert "ollama unavailable" in manager.health_error


def test_successful_persistent_memory(monkeypatch):
    collection = FakeCollection()
    install_chroma(monkeypatch, FakeClient(collection))
    manager = MemoryManager(embedding_function=lambda texts: [[1.0] for _ in texts])

    memory_id = manager.remember_fact("remembered text")

    assert manager.get_health() == {"status": "persistent", "error": None}
    assert memory_id
    assert len(collection.upserts) == 1
    assert manager.retrieve("remembered") == [
        {
            "id": "memory-1",
            "text": "remembered text",
            "metadata": {"kind": "fact", "tags": ""},
            "score": 0.1,
        }
    ]


def test_session_transcript_and_summary_are_distinct_and_bounded(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager(settings={"summary_max_chars": 12})
    session_id = manager.start_session()
    manager.add_message(session_id, "user", "a long message")

    transcript = manager.summarise_session(session_id)
    summary = manager.summarise_session(session_id, summary_text="a concise summary")

    assert transcript == "user: a long"
    assert summary == "a concise su"
    assert [record["metadata"]["kind"] for record in manager._memories] == [
        "transcript",
        "summary",
    ]
    assert all(len(record["text"]) <= 12 for record in manager._memories)


def test_direct_session_summary_is_bounded(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager(settings={"summary_max_chars": 5})

    manager.store_session_summary("session", "123456789")

    assert manager._memories[0]["text"] == "12345"


def test_local_recall_matches_meaningful_query_terms(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()
    manager.remember_fact("My favorite color is blue")

    results = manager.retrieve("what is my favorite color")

    assert [entry["text"] for entry in results] == ["My favorite color is blue"]


def test_duplicate_facts_reuse_existing_memory(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()

    first_id = manager.remember_fact("My favorite color is blue")
    second_id = manager.remember_fact("  my FAVORITE color is blue  ")

    assert second_id == first_id
    assert len(manager.list_memories()) == 1


def test_forget_matching_memory_preserves_unrelated_records(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()
    manager.remember_fact("My favorite color is blue")
    manager.remember_fact("My dog's name is Pixel")

    deleted = manager.forget(query="forget everything about my favorite color")

    assert deleted == 1
    assert [entry["text"] for entry in manager.list_memories()] == [
        "My dog's name is Pixel"
    ]


def test_forget_current_session_clears_records_and_buffer(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()
    session_id = manager.start_session()
    manager.add_message(session_id, "user", "private conversation")
    manager.remember_fact("Session fact", session_id=session_id)
    manager.remember_fact("Unrelated fact", session_id="other-session")

    deleted = manager.forget(session_id=session_id)

    assert deleted == 1
    assert session_id not in manager.sessions
    assert manager.current_session_id is None
    assert [entry["text"] for entry in manager.list_memories()] == [
        "Unrelated fact"
    ]


def test_persistent_forget_deletes_exact_backend_ids(monkeypatch):
    collection = FakeCollection()
    install_chroma(monkeypatch, FakeClient(collection))
    manager = MemoryManager(embedding_function=lambda texts: [[1.0] for _ in texts])
    target_id = manager.remember_fact("Forget this favorite color")
    manager.remember_fact("Keep this dog's name")

    deleted = manager.forget(memory_id=target_id)

    assert deleted == 1
    assert collection.deleted_ids == [target_id]
    assert [entry["text"] for entry in manager.list_memories()] == [
        "Keep this dog's name"
    ]


def test_persistent_forget_reports_delete_failure(monkeypatch):
    collection = FakeCollection(delete_error=RuntimeError("backend refused"))
    install_chroma(monkeypatch, FakeClient(collection))
    manager = MemoryManager(embedding_function=lambda texts: [[1.0] for _ in texts])
    memory_id = manager.remember_fact("Do not pretend this was deleted")

    with pytest.raises(memory_module.MemoryOperationError, match="backend refused"):
        manager.forget(memory_id=memory_id)


def test_forget_tool_requires_confirmation_for_all_memories(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()
    manager.remember_fact("A private fact")
    tool = build_forget_tool(manager)

    refused = tool.invoke({"all_memories": True, "confirm": False})
    deleted = tool.invoke({"all_memories": True, "confirm": True})

    assert "Refusing" in refused
    assert deleted == "Permanently deleted 1 memory."
    assert manager.list_memories() == []


def test_memory_list_tool_exposes_ids_for_targeted_deletion(monkeypatch):
    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    manager = MemoryManager()
    memory_id = manager.remember_fact("A visible fact")
    tool = build_memory_list_tool(manager)

    result = tool.invoke({"limit": 20})

    assert memory_id in result
    assert "A visible fact" in result


@pytest.mark.asyncio
async def test_memory_context_is_transient_across_turns(monkeypatch):
    class FakeConfig:
        def get_section(self, name):
            if name == "agent":
                return {"max_steps": 3}
            return {}

        def get_agent_system_prompt(self):
            return "system prompt"

    memory = SimpleNamespace(
        retrieve=lambda query, k: [
            {"text": f"memory for {query}", "metadata": {"kind": "fact"}}
        ]
    )
    manager = AgentManager.__new__(AgentManager)
    manager.config = FakeConfig()
    manager.llm_client = object()
    manager.tool_registry = object()
    manager.memory_manager = memory
    manager.memory_context_limit = 1200
    manager.memory_top_k = 5
    manager.conversation_manager = ConversationManager()

    calls = []

    async def fake_loop(llm_client, tool_registry, state):
        calls.append(list(state["messages"]))
        return {
            "messages": [*state["messages"], AIMessage(content="answer")],
            "response": "answer",
            "tool_results": [],
        }

    monkeypatch.setattr(agent_module, "run_conversation_loop", fake_loop)

    await manager.process_async("first")
    await manager.process_async("second")

    persisted = manager.conversation_manager.conversation_history
    assert sum(isinstance(message, SystemMessage) for message in persisted) == 1
    assert isinstance(persisted[0], SystemMessage)
    assert all("Relevant memories:" not in str(message.content) for message in persisted)
    assert [type(message) for message in persisted] == [
        SystemMessage,
        HumanMessage,
        AIMessage,
        HumanMessage,
        AIMessage,
    ]
    assert all(
        sum(isinstance(message, SystemMessage) for message in call) == 2
        for call in calls
    )
