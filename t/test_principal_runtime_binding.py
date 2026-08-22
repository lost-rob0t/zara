from __future__ import annotations

from types import SimpleNamespace

import pytest

import zara.agent as agent_module
from zara.agent import AgentManager
from zara.runtime import bridge
from zara.runtime.backend import create_runtime_backend
from zara.server import PrincipalContext, RuntimeSupervisor


class FakeConfig:
    def get(self, section, key, default=None):
        if (section, key) == ("agent", "backend"):
            return "langgraph"
        return default

    def get_llm_config(self):
        return {"provider": "fake"}

    def get_section(self, name):
        if name == "memory":
            return {"enabled": True, "top_k": 5, "max_chars": 1200}
        if name == "agent":
            return {"conversation_timeout": 60}
        return {}

    def get_module_search_paths(self):
        return []

    def get_agent_system_prompt(self):
        return "system"


def test_supervisor_default_host_carries_authenticated_principal_into_host():
    supervisor = RuntimeSupervisor(config=FakeConfig())
    principal = PrincipalContext("alice", "curve")
    host = supervisor._build_default_host(principal, bridge.RuntimeEventBus())

    assert host.principal_id == "alice"


def test_agent_manager_binds_memory_builder_to_principal(monkeypatch):
    captured = {}

    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda self, _config: object(),
    )

    def fake_build_memory_manager(config, *, principal_id):
        captured["config"] = config
        captured["principal_id"] = principal_id
        return SimpleNamespace()

    monkeypatch.setattr(agent_module, "build_memory_manager", fake_build_memory_manager)
    monkeypatch.setattr(
        agent_module,
        "ToolRegistry",
        lambda *_args, **_kwargs: SimpleNamespace(
            load_builtin_tools=lambda _memory: None,
            load_user_tools=lambda _path: None,
        ),
    )

    manager = AgentManager(config=FakeConfig(), principal_id="alice")

    assert manager.principal_id == "alice"
    assert captured["principal_id"] == "alice"


@pytest.mark.asyncio
async def test_runtime_backend_factory_constructs_principal_bound_agent(monkeypatch):
    captured = {}

    class FakeManager:
        def __init__(self, *, config, principal_id):
            captured["config"] = config
            captured["principal_id"] = principal_id

        def exit_conversation(self):
            pass

    monkeypatch.setattr(agent_module, "AgentManager", FakeManager)
    backend = create_runtime_backend(FakeConfig(), principal_id="alice")
    await backend.start()
    try:
        assert captured["principal_id"] == "alice"
    finally:
        await backend.stop()
