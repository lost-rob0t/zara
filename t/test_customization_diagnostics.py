from __future__ import annotations

from types import SimpleNamespace

from zara.agent import AgentManager
from zara.agent.hooks import AgentLoopAdviceRegistry
from zara.agent.loops import AgentLoopRegistry


async def _loop(*_args, **_kwargs):
    return {"messages": [], "response": "ok", "tool_results": []}


class FakeConfig:
    def __init__(self, backend: str):
        self.backend = backend

    def get_section(self, name):
        if name == "agent":
            return {"backend": self.backend}
        return {}


def _manager(backend: str = "langgraph") -> AgentManager:
    manager = AgentManager.__new__(AgentManager)
    manager.config = FakeConfig(backend)
    manager.agent_loop_advice = AgentLoopAdviceRegistry(
        enabled=True,
        allow_override=True,
    )
    manager.agent_loop_registry = AgentLoopRegistry()
    manager.agent_loop_registry.register("langgraph", "core:langgraph", _loop)
    return manager


def test_customization_diagnostics_are_metadata_only_and_report_winner():
    manager = _manager()
    before_callback = lambda: None
    override_callback = lambda: None
    manager.agent_loop_advice.register("before", "user:hooks.py", 10, before_callback)
    winner_id = manager.agent_loop_advice.register(
        "override",
        "plugin:trusted",
        20,
        override_callback,
    )

    diagnostic = manager.customization_diagnostics()

    assert diagnostic.hooks_enabled is True
    assert diagnostic.allow_override is True
    assert diagnostic.configured_backend == "langgraph"
    assert diagnostic.backend_known is True
    assert diagnostic.backend_owner == "core:langgraph"
    assert diagnostic.override_winner_id == winner_id
    assert diagnostic.override_winner_owner == "plugin:trusted"
    assert diagnostic.override_conflict is False
    assert [item.owner for item in diagnostic.advice] == [
        "user:hooks.py",
        "plugin:trusted",
    ]
    assert [item.kind for item in diagnostic.advice] == ["before", "override"]
    assert all(not hasattr(item, "callback") for item in diagnostic.advice)
    assert all(not hasattr(item, "callback") for item in diagnostic.backends)


def test_customization_diagnostics_do_not_guess_multiple_override_winner():
    manager = _manager()
    manager.agent_loop_advice.register("override", "user:a", 0, lambda: None)
    manager.agent_loop_advice.register("override", "user:b", 1, lambda: None)

    diagnostic = manager.customization_diagnostics()

    assert diagnostic.override_conflict is True
    assert diagnostic.override_winner_id is None
    assert diagnostic.override_winner_owner is None


def test_customization_diagnostics_report_unknown_configured_backend_without_fallback():
    manager = _manager("missing")

    diagnostic = manager.customization_diagnostics()

    assert diagnostic.configured_backend == "missing"
    assert diagnostic.backend_known is False
    assert diagnostic.backend_owner is None
    assert [item.name for item in diagnostic.backends] == ["langgraph"]
