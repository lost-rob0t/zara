from __future__ import annotations

from types import SimpleNamespace

import pytest

import zara.agent as agent_module
import zara.memory as memory_module
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


def test_supervisor_default_host_seals_authenticated_principal_into_backend():
    supervisor = RuntimeSupervisor(config=FakeConfig())
    principal = PrincipalContext("alice", "curve")
    host = supervisor._build_default_host(principal, bridge.RuntimeEventBus())

    backend = host._backend_factory()
    assert backend.principal_id == "alice"
    assert backend.principal_kind == "curve"


@pytest.mark.asyncio
async def test_runtime_backend_injects_principal_bound_memory_without_widening_agent_api(
    monkeypatch,
):
    captured = {}
    fake_memory = SimpleNamespace(principal_id="alice", ephemeral=False)

    def fake_build_memory_manager(config, *, principal_id, ephemeral=False):
        captured["memory_config"] = config
        captured["memory_principal_id"] = principal_id
        captured["memory_ephemeral"] = ephemeral
        return fake_memory

    class FakeManager:
        def __init__(self, *, config, memory_manager):
            captured["manager_config"] = config
            captured["manager_memory"] = memory_manager

        def exit_conversation(self):
            pass

    monkeypatch.setattr(memory_module, "build_memory_manager", fake_build_memory_manager)
    monkeypatch.setattr(agent_module, "AgentManager", FakeManager)

    backend = create_runtime_backend(
        FakeConfig(),
        principal_id="alice",
        principal_kind="curve",
    )
    await backend.start()
    try:
        assert backend.principal_id == "alice"
        assert backend.principal_kind == "curve"
        assert captured["memory_principal_id"] == "alice"
        assert captured["memory_ephemeral"] is False
        assert captured["manager_memory"] is fake_memory
    finally:
        await backend.stop()


@pytest.mark.asyncio
@pytest.mark.parametrize("principal_kind", ["guest", "ephemeral"])
async def test_guest_runtime_injects_non_durable_memory(monkeypatch, principal_kind):
    captured = {}
    fake_memory = SimpleNamespace(principal_id="guest-1", ephemeral=True)

    def fake_build_memory_manager(config, *, principal_id, ephemeral=False):
        captured["principal_id"] = principal_id
        captured["ephemeral"] = ephemeral
        return fake_memory

    class FakeManager:
        def __init__(self, *, config, memory_manager):
            captured["memory_manager"] = memory_manager

        def exit_conversation(self):
            pass

    monkeypatch.setattr(memory_module, "build_memory_manager", fake_build_memory_manager)
    monkeypatch.setattr(agent_module, "AgentManager", FakeManager)

    backend = create_runtime_backend(
        FakeConfig(),
        principal_id="guest-1",
        principal_kind=principal_kind,
    )
    await backend.start()
    try:
        assert captured["principal_id"] == "guest-1"
        assert captured["ephemeral"] is True
        assert captured["memory_manager"] is fake_memory
    finally:
        await backend.stop()
