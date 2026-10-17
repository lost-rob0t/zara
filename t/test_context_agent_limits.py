from __future__ import annotations

import asyncio
from types import SimpleNamespace

import pytest
from langchain_core.messages import HumanMessage

from t.test_task_turn_context import CapturingLLM, build_manager
from zara.agent import AgentManager
from zara.agent.context import ContextBudgetError, ContextConfig, ContextManager
from zara.agent.graph import create_agent_node


def test_agent_rejects_oversized_input_without_provider_or_history_mutation(monkeypatch):
    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    manager = build_manager(llm)
    manager.context_manager = ContextManager(
        system_prompt="base", config=ContextConfig(max_tokens=500, summary_max_tokens=100, skill_max_tokens=100),
    )
    with pytest.raises(ContextBudgetError):
        asyncio.run(manager.process_async("x" * 10000))
    assert llm.calls == []
    assert manager.context_manager.history == ()


@pytest.mark.asyncio
async def test_every_graph_provider_step_checks_current_context_budget():
    from zara.agent.context import provider_context_scope

    llm = CapturingLLM()
    registry = SimpleNamespace(to_langchain_tools=lambda: [])
    manager = ContextManager(system_prompt="base", config=ContextConfig(max_tokens=500, summary_max_tokens=100, skill_max_tokens=100))
    lease = manager.begin_turn("tool-follow-up")
    node = create_agent_node(llm, registry)
    with provider_context_scope(manager, lease):
        with pytest.raises(ContextBudgetError):
            await node({"messages": [HumanMessage(content="x" * 10000)]})
    assert llm.calls == []


def test_failed_task_context_cannot_change_interactive_transcript(monkeypatch):
    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    manager = build_manager(llm)
    manager.conversation_manager.conversation_history = [HumanMessage(content="private chat")]
    with pytest.raises(ContextBudgetError):
        asyncio.run(manager.process_async("task", conversation_history=[], extra_system_context="x" * 100000))
    assert [message.content for message in manager.conversation_manager.conversation_history] == ["private chat"]
    assert llm.calls == []


def test_manual_compact_rejects_busy_interactive_turn_without_superseding_it(monkeypatch):
    from zara.agent.context import ContextError

    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    manager = build_manager(llm)
    lease = manager.context_manager.begin_turn("active")
    with pytest.raises(ContextError, match="busy"):
        asyncio.run(manager.compact_context())
    manager.context_manager._require_active(lease)
    assert llm.calls == []


def test_explicit_cancel_invalidates_active_interactive_context(monkeypatch):
    from zara.agent.context import StaleContextTurn

    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    manager = build_manager(llm)
    lease = manager.context_manager.begin_turn("cancelled")
    asyncio.run(manager.cancel_turn("cancelled"))
    with pytest.raises(StaleContextTurn):
        manager.context_manager._require_active(lease)


@pytest.mark.asyncio
async def test_provider_step_rejects_incomplete_tool_group_without_invoking_model():
    from langchain_core.messages import AIMessage, SystemMessage
    from zara.agent.context import ContextError, provider_context_scope

    llm = CapturingLLM()
    manager = ContextManager(system_prompt="base")
    lease = manager.begin_turn("incomplete")
    node = create_agent_node(llm, SimpleNamespace(to_langchain_tools=lambda: []))
    messages = [SystemMessage(content="base", id="context:base"), HumanMessage(content="tool"),
                AIMessage(content="", tool_calls=[{"id": "missing", "name": "echo", "args": {}}])]
    with provider_context_scope(manager, lease):
        with pytest.raises(ContextError, match="tool-call"):
            await node({"messages": messages})
    assert llm.calls == []


def test_agent_manual_compaction_uses_bounded_real_summary_path(monkeypatch):
    llm = CapturingLLM(response="bounded summary")
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    manager = build_manager(llm)
    manager.context_manager.config = ContextConfig(strategy="compress", max_tokens=10000,
                                                   preserve_recent_turns=1, summary_max_tokens=1000,
                                                   skill_max_tokens=1000)

    async def drive():
        for index in range(3):
            await manager.process_async(f"question {index}")
        return await manager.compact_context()

    result = asyncio.run(drive())
    assert result.changed
    assert len(llm.calls) == 4
    assert "Summarize conversation data" in llm.calls[-1][0].content
    assert len([message for message in manager.context_manager.history if message.id == "context:summary"]) == 1
