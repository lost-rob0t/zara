from __future__ import annotations

from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage, SystemMessage

import zara.agent as agent_module
from zara.agent import AgentManager
from zara.agent.context import ContextConfig, ContextManager
from zara.agent.conversation import ConversationManager


class _Config:
    def get_section(self, name):
        if name == "agent":
            return {"max_steps": 3}
        return {}

    def get_agent_system_prompt(self):
        return "canonical"


class _DynamicRegistry:
    def __init__(self):
        self.prepare_calls = 0

    async def prepare_async(self):
        self.prepare_calls += 1

    def dynamic_system_context(self):
        return "MCP server docs exposes tool search"


@pytest.mark.asyncio
async def test_agent_prepares_mcp_before_context_and_does_not_double_inject(monkeypatch):
    manager = AgentManager.__new__(AgentManager)
    manager.config = _Config()
    manager.llm_client = object()
    manager.tool_registry = _DynamicRegistry()
    manager.memory_manager = None
    manager.memory_context_limit = 1200
    manager.memory_top_k = 5
    manager.skill_registry = None
    manager.principal = None
    manager.context_manager = ContextManager(
        system_prompt="canonical",
        config=ContextConfig(
            max_tokens=100,
            summary_max_tokens=20,
            skill_max_tokens=20,
        ),
        token_counter=lambda messages: len(messages),
    )
    manager.conversation_manager = ConversationManager(
        history_provider=lambda: manager.context_manager.history,
        history_clear=manager.context_manager.clear,
    )
    manager.approval_controller = SimpleNamespace(publisher=None)

    calls = []

    async def fake_loop(llm_client, tool_registry, state, **_kwargs):
        calls.append((tool_registry, list(state["messages"])))
        assert tool_registry.dynamic_system_context() is None
        return {
            "messages": [*state["messages"], AIMessage(content="done")],
            "response": "done",
            "tool_results": [],
        }

    monkeypatch.setattr(agent_module, "run_conversation_loop", fake_loop)

    result = await manager.process_async("use the docs MCP")

    assert result["response"] == "done"
    assert manager.tool_registry.prepare_calls == 1
    assert len(calls) == 1
    system_text = [
        str(message.content)
        for message in calls[0][1]
        if isinstance(message, SystemMessage)
    ]
    assert system_text.count("MCP server docs exposes tool search") == 1
    assert all(
        "MCP server docs" not in str(message.content)
        for message in manager.context_manager.history
    )
