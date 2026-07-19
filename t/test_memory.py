import urllib.error
from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage, HumanMessage, SystemMessage

import zara.agent as agent_module
import zara.memory as memory_module
from zara.agent import AgentManager
from zara.agent.conversation import ConversationManager
from zara.memory import MemoryManager


class FakeCollection:
    def __init__(self, query_error=None):
        self.query_error = query_error
        self.upserts = []

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
