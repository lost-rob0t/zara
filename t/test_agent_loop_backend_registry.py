from __future__ import annotations

from types import SimpleNamespace

import pytest
from langchain_core.messages import AIMessage

from zara.agent import AgentManager
from zara.agent.conversation import ConversationManager
from zara.agent.hooks import AgentLoopAdviceRegistry
from zara.agent.loops import (
    AgentLoopBackendOverrideDisabled,
    AgentLoopRegistry,
    UnknownAgentLoopBackend,
)


async def _loop_a(*_args, **_kwargs):
    return {"messages": [], "response": "a", "tool_results": []}


async def _loop_b(*_args, **_kwargs):
    return {"messages": [], "response": "b", "tool_results": []}


def test_registry_resolves_named_backend_and_owner():
    registry = AgentLoopRegistry()
    registration_id = registry.register("custom", "user:test", _loop_a)

    registration = registry.resolve("custom")

    assert registration.registration_id == registration_id
    assert registration.name == "custom"
    assert registration.owner == "user:test"
    assert registration.callback is _loop_a


def test_registry_rejects_duplicate_backend_name():
    registry = AgentLoopRegistry()
    registry.register("custom", "user:first", _loop_a)

    with pytest.raises(ValueError, match="already registered"):
        registry.register("custom", "user:second", _loop_b)


def test_registry_unregister_is_owner_safe():
    registry = AgentLoopRegistry()
    registration_id = registry.register("custom", "user:first", _loop_a)

    assert registry.unregister(registration_id, owner="user:other") is False
    assert registry.resolve("custom").callback is _loop_a
    assert registry.unregister(registration_id, owner="user:first") is True

    with pytest.raises(UnknownAgentLoopBackend, match="custom"):
        registry.resolve("custom")


def test_registry_unknown_backend_fails_closed():
    registry = AgentLoopRegistry()

    with pytest.raises(UnknownAgentLoopBackend, match="missing"):
        registry.resolve("missing")


class FakeConfig:
    def __init__(self, backend: str | None):
        self.backend = backend

    def get_section(self, name):
        if name == "agent":
            section = {"max_steps": 7}
            if self.backend is not None:
                section["backend"] = self.backend
            return section
        return {}

    def get_agent_system_prompt(self):
        return ""


def _build_manager(
    backend: str | None,
    *,
    hooks_enabled: bool = True,
    allow_override: bool = False,
) -> AgentManager:
    manager = AgentManager.__new__(AgentManager)
    manager.config = FakeConfig(backend)
    manager.llm_client = object()
    manager.tool_registry = object()
    manager.memory_manager = None
    manager.memory_context_limit = 1200
    manager.memory_top_k = 5
    manager.conversation_manager = ConversationManager()
    manager.principal = SimpleNamespace(principal_id="principal-a")
    manager.approval_controller = SimpleNamespace(publisher=object())
    manager.agent_loop_advice = AgentLoopAdviceRegistry(
        enabled=hooks_enabled,
        allow_override=allow_override,
    )
    manager.agent_loop_registry = AgentLoopRegistry()
    return manager


@pytest.mark.asyncio
async def test_agent_manager_invokes_configured_backend_through_existing_advice():
    manager = _build_manager("custom", allow_override=True)
    events = []

    async def custom_loop(_llm_client, _tool_registry, state, **_kwargs):
        events.append("backend")
        return {
            "messages": [*state["messages"], AIMessage(content="custom")],
            "response": "custom",
            "tool_results": [],
        }

    manager.agent_loop_registry.register("custom", "test", custom_loop)
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

    result = await manager.process_async("hello", turn_id="turn-backend")

    assert events == ["before", "backend", "after"]
    assert result["response"] == "custom"


@pytest.mark.asyncio
async def test_custom_backend_requires_explicit_override_policy():
    manager = _build_manager("custom", hooks_enabled=True, allow_override=False)
    manager.agent_loop_registry.register("custom", "test", _loop_a)

    with pytest.raises(AgentLoopBackendOverrideDisabled, match="allow_override"):
        await manager.process_async("hello", turn_id="turn-denied")


@pytest.mark.asyncio
async def test_agent_manager_defaults_to_langgraph_when_backend_is_omitted():
    manager = _build_manager(None, hooks_enabled=False, allow_override=False)

    async def default_loop(_llm_client, _tool_registry, state, **_kwargs):
        return {
            "messages": [*state["messages"], AIMessage(content="default")],
            "response": "default",
            "tool_results": [],
        }

    manager.agent_loop_registry.register("langgraph", "test", default_loop)

    result = await manager.process_async("hello", turn_id="turn-default")

    assert result["response"] == "default"
