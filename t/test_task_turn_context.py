from pathlib import Path

import pytest

from langchain_core.messages import AIMessage, HumanMessage, SystemMessage

from zara.agent import AgentManager
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.server import PrincipalContext


class CapturingLLM:
    def __init__(self, response: str = "TASK_COMPLETE: ok") -> None:
        self.response = response
        self.calls: list[list] = []

    def bind_tools(self, _tools):
        return self

    async def ainvoke(self, messages):
        self.calls.append(list(messages))
        return AIMessage(content=self.response)


class ContextConfig:
    config_dir = Path("/nonexistent-zara-task-context-config")

    def get_llm_config(self):
        return {}

    def get_section(self, name: str):
        sections = {
            "agent": {"conversation_timeout": 60, "max_steps": 4},
            "memory": {"max_chars": 1200, "top_k": 5},
            "tool_approval": {"required_tools": [], "timeout_seconds": 5.0, "max_pending": 4},
        }
        return sections.get(name, {})

    def get_module_search_paths(self):
        return []

    def get_tool_config(self):
        return {"file_tools": False}

    def get_agent_system_prompt(self):
        return "Test assistant."


class EmptyMemory:
    def retrieve(self, _query, k=5):
        return []


def build_manager(llm):
    manager = AgentManager(
        config=ContextConfig(),
        memory_manager=EmptyMemory(),
        principal=PrincipalContext("test:context-owner", kind="test"),
    )
    manager.llm_client = llm
    return manager


def test_extra_system_context_reaches_llm_and_history_stays_isolated(monkeypatch):
    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda _self, _config: llm)
    manager = build_manager(llm)
    prior = [HumanMessage(content="previous chat message")]
    manager.conversation_manager.conversation_history = list(prior)

    result = manager.process_async(
        "continue the task",
        turn_id="task-turn-1",
        conversation_history=[],
        extra_system_context="GOAL: finish the report",
    )
    import asyncio

    result = asyncio.run(result)

    assert result["response"] == "TASK_COMPLETE: ok"
    messages = llm.calls[0]
    assert messages[-1].content == "continue the task"
    assert any(
        isinstance(message, SystemMessage) and "GOAL: finish" in str(message.content)
        for message in messages
    )
    assert "previous chat message" not in [str(m.content) for m in messages]

    assert manager.conversation_manager.conversation_history == prior


def test_extra_system_context_inserted_after_system_prompt(monkeypatch):
    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda _self, _config: llm)
    manager = build_manager(llm)

    import asyncio

    asyncio.run(
        manager.process_async(
            "step instruction",
            turn_id="task-turn-2",
            conversation_history=[],
            extra_system_context="TASK CONTEXT PAYLOAD",
        )
    )

    messages = llm.calls[0]
    assert isinstance(messages[0], SystemMessage)
    assert str(messages[1].content) == "TASK CONTEXT PAYLOAD"
    assert isinstance(messages[-1], HumanMessage)


def test_default_process_async_still_persists_history(monkeypatch):
    llm = CapturingLLM(response="plain answer")
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda _self, _config: llm)
    manager = build_manager(llm)
    manager.conversation_manager.conversation_history = []

    import asyncio

    asyncio.run(manager.process_async("hello there", turn_id="user-turn-1"))

    history = manager.conversation_manager.conversation_history
    assert history, "default path must keep persisting conversation history"
    assert any("hello there" in str(message.content) for message in history)


def test_backend_submit_turn_forwards_task_context(monkeypatch):
    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda _self, _config: llm)
    manager = build_manager(llm)
    backend = LangGraphRuntimeBackend(lambda: manager)

    import asyncio

    from zara.latency import LatencyTrace

    trace = LatencyTrace(trace_id="task-turn-3")

    async def drive():
        await backend.start()
        try:
            await backend.submit_turn(
                "step instruction",
                turn_id="task-turn-3",
                conversation_id=None,
                system_context="CONTEXT VIA BACKEND",
                conversation_history=[],
                latency_trace=trace,
            )
        finally:
            await backend.stop()

    asyncio.run(drive())

    messages = llm.calls[0]
    assert any(
        str(message.content) == "CONTEXT VIA BACKEND" for message in messages
    )
    assert messages[-1].content == "step instruction"
    assert manager.conversation_manager.conversation_history == []
