from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage, HumanMessage

import zara.agent as agent_module
from zara.agent import AgentManager
from zara.agent.conversation import ConversationManager
from zara.agent.hooks import AgentLoopAdviceRegistry


class FakeConfig:
    def __init__(self, *, enabled=True, allow_override=False):
        self.hooks = {
            "enabled": enabled,
            "allow_override": allow_override,
        }

    def get_hooks_config(self):
        return dict(self.hooks)

    def get_section(self, name):
        if name == "agent":
            return {"max_steps": 7}
        if name == "memory":
            return {"max_chars": 1200, "top_k": 5}
        if name == "tool_approval":
            return {"timeout_seconds": 30.0, "max_pending": 4}
        return {}

    def get_llm_config(self):
        return {"provider": "fake"}

    def get_module_search_paths(self):
        return []

    def get_agent_system_prompt(self):
        return ""


def build_manager(*, enabled=True, allow_override=False):
    manager = AgentManager.__new__(AgentManager)
    manager.config = FakeConfig(enabled=enabled, allow_override=allow_override)
    manager.llm_client = object()
    manager.tool_registry = object()
    manager.memory_manager = None
    manager.memory_context_limit = 1200
    manager.memory_top_k = 5
    manager.conversation_manager = ConversationManager()
    manager.principal = SimpleNamespace(principal_id="principal-a")
    manager.approval_controller = SimpleNamespace(publisher=object())
    manager.agent_loop_advice = AgentLoopAdviceRegistry(
        enabled=enabled,
        allow_override=allow_override,
    )
    return manager


def test_agent_manager_constructs_owned_registry_from_hook_policy(monkeypatch):
    config = FakeConfig(enabled=True, allow_override=True)

    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda self, _config: object())
    monkeypatch.setattr(agent_module, "build_memory_manager", lambda *_args, **_kwargs: None)
    monkeypatch.setattr(agent_module, "ToolRegistry", lambda *_args, **_kwargs: SimpleNamespace(
        load_builtin_tools=lambda *_args, **_kwargs: None,
        load_user_tools=lambda *_args, **_kwargs: None,
    ))

    manager = AgentManager(config=config)

    assert isinstance(manager.agent_loop_advice, AgentLoopAdviceRegistry)
    assert manager.agent_loop_advice.enabled is True
    assert manager.agent_loop_advice.allow_override is True


@pytest.mark.asyncio
async def test_process_async_routes_canonical_loop_through_advice(monkeypatch):
    manager = build_manager()
    events = []
    captured = {}
    stream_publisher = object()

    manager.agent_loop_advice.register(
        "before",
        "test",
        0,
        lambda *_args, **_kwargs: events.append("before"),
    )
    manager.agent_loop_advice.register(
        "after",
        "test",
        0,
        lambda _result: events.append("after"),
    )

    async def fake_loop(llm_client, tool_registry, state, **kwargs):
        events.append("base")
        captured.update(kwargs)
        captured["llm_client"] = llm_client
        captured["tool_registry"] = tool_registry
        captured["state"] = state
        return {
            "messages": [*state["messages"], AIMessage(content="answer")],
            "response": "answer",
            "tool_results": ["tool-result"],
        }

    monkeypatch.setattr(agent_module, "run_conversation_loop", fake_loop)

    result = await manager.process_async(
        "hello",
        turn_id="turn-1",
        conversation_id="conversation-1",
        stream_publisher=stream_publisher,
    )

    assert events == ["before", "base", "after"]
    assert captured["llm_client"] is manager.llm_client
    assert captured["tool_registry"] is manager.tool_registry
    assert captured["approval_controller"] is manager.approval_controller
    assert captured["publisher"] is manager.approval_controller.publisher
    assert captured["principal_id"] == "principal-a"
    assert captured["stream_publisher"] is stream_publisher
    assert captured["state"]["max_steps"] == 7
    assert captured["state"]["turn_id"] == "turn-1"
    assert captured["state"]["conversation_id"] == "conversation-1"
    assert result == {
        "response": "answer",
        "tool_results": ["tool-result"],
        "turn_id": "turn-1",
        "conversation_id": "conversation-1",
    }


@pytest.mark.asyncio
async def test_override_result_still_uses_agent_manager_history_postprocessing(monkeypatch):
    manager = build_manager(allow_override=True)
    base_calls = []

    async def fake_loop(*_args, **_kwargs):
        base_calls.append("base")
        raise AssertionError("override must replace only the canonical loop call")

    async def override(_llm_client, _tool_registry, state, **_kwargs):
        return {
            "messages": [*state["messages"], AIMessage(content="custom")],
            "response": "custom",
            "tool_results": [],
        }

    manager.agent_loop_advice.register("override", "test", 0, override)
    monkeypatch.setattr(agent_module, "run_conversation_loop", fake_loop)

    result = await manager.process_async("hello", turn_id="turn-override")

    assert base_calls == []
    assert result["response"] == "custom"
    assert isinstance(manager.conversation_manager.conversation_history[-2], HumanMessage)
    assert isinstance(manager.conversation_manager.conversation_history[-1], AIMessage)
    assert manager.conversation_manager.conversation_history[-1].content == "custom"
