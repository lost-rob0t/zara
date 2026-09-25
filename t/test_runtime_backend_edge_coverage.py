from __future__ import annotations

import asyncio

import pytest

from zara.runtime.backend import (
    AgentRuntimeBackend,
    LangGraphRuntimeBackend,
    UnsupportedRuntimeCommand,
    create_runtime_backend,
)


class Principal:
    principal_id = "user:edge"


class Registry:
    def __init__(self):
        self.registered = []
        self.unregistered = []
        self.invoked = []

    def register_tools(self, tools):
        self.registered.extend(tools)

    def unregister_tools(self, names):
        self.unregistered.extend(names)

    def requires_approval(self, name):
        return name == "danger"

    def invoke_composed_tool(self, name, request):
        self.invoked.append((name, request))
        return "ok"


class Advice:
    def register(self, kind, **kwargs):
        return 9

    def unregister(self, registration_id):
        return registration_id == 9


class Manager:
    def __init__(self):
        self.principal = Principal()
        self.tool_registry = Registry()
        self.agent_loop_advice = Advice()
        self.cancelled = []
        self.approved = []
        self.rejected = []
        self.exited = False

    async def cancel_turn(self, turn_id):
        self.cancelled.append(turn_id)

    async def approve_tool(self, tool_run_id):
        self.approved.append(tool_run_id)

    async def reject_tool(self, tool_run_id, reason):
        self.rejected.append((tool_run_id, reason))

    def customization_diagnostics(self):
        return ("edge",)

    def exit_conversation(self):
        self.exited = True


def started(manager=None):
    manager = manager or Manager()
    backend = LangGraphRuntimeBackend(lambda: manager)
    asyncio.run(backend.start())
    return backend, manager


def test_backend_capabilities_require_started_manager():
    backend = LangGraphRuntimeBackend(lambda: Manager())
    for call in (
        lambda: backend.register_tools([]),
        lambda: backend.requires_composed_tool_approval("x"),
        lambda: backend.invoke_composed_tool("user:edge", "x", {}),
        lambda: backend.register_agent_loop_advice("before", "owner", 0, lambda: None),
        lambda: backend.customization_diagnostics(),
    ):
        with pytest.raises(RuntimeError, match="not started"):
            call()
    with pytest.raises(RuntimeError, match="not started"):
        asyncio.run(backend.approve_tool("a"))
    with pytest.raises(RuntimeError, match="not started"):
        asyncio.run(backend.reject_tool("a"))
    assert backend.unregister_agent_loop_advice(1) is False
    backend.unregister_tools(["x"])
    asyncio.run(backend.cancel_turn("turn"))


def test_backend_tool_identity_and_optional_advice_paths():
    backend, manager = started()
    backend.register_tools(["one"])
    backend.unregister_tools(["one"])
    assert manager.tool_registry.registered == ["one"]
    assert manager.tool_registry.unregistered == ["one"]
    assert backend.requires_composed_tool_approval("danger") is True

    with pytest.raises(PermissionError, match="does not own"):
        backend.invoke_composed_tool("user:other", "tool", {})
    assert backend.invoke_composed_tool("user:edge", "tool", {"x": 1}) == "ok"

    assert backend.register_agent_loop_advice("before", "owner", 1, lambda: None) == 9
    assert backend.unregister_agent_loop_advice(9) is True
    assert backend.customization_diagnostics() == ("edge",)

    manager.agent_loop_advice = None
    with pytest.raises(UnsupportedRuntimeCommand, match="advice"):
        backend.register_agent_loop_advice("before", "owner", 1, lambda: None)
    assert backend.unregister_agent_loop_advice(9) is False

    manager.customization_diagnostics = None
    with pytest.raises(UnsupportedRuntimeCommand, match="diagnostics"):
        backend.customization_diagnostics()


def test_backend_tool_approval_missing_hooks_and_stop_fallback():
    manager = Manager()
    manager.approve_tool = None
    manager.reject_tool = None
    backend, manager = started(manager)

    with pytest.raises(UnsupportedRuntimeCommand, match="approval"):
        asyncio.run(backend.approve_tool("a"))
    with pytest.raises(UnsupportedRuntimeCommand, match="rejection"):
        asyncio.run(backend.reject_tool("r"))

    asyncio.run(backend.stop())
    assert manager.exited is True
    asyncio.run(backend.stop())


def test_backend_cancel_and_bind_are_optional_hooks():
    class Minimal:
        principal = Principal()
        tool_registry = Registry()

        def exit_conversation(self):
            pass

    backend, manager = started(Minimal())
    backend.bind_event_publisher(lambda _event: None)
    asyncio.run(backend.cancel_turn("turn"))
    asyncio.run(backend.stop())


def test_backend_memory_absence_is_a_noop():
    backend, _manager = started()
    assert backend._memory_manager() is None
    asyncio.run(backend._persist_turn("hello", ""))
    asyncio.run(backend._rotate_memory_session())


def test_noncanonical_backend_config_is_rejected():
    class Config:
        def get(self, section, key, default=None):
            assert (section, key) == ("agent", "backend")
            return "parallel"

    with pytest.raises(ValueError, match="Unsupported agent backend"):
        create_runtime_backend(Config())


def test_agent_runtime_facade_delegates_control_surface():
    manager = Manager()
    backend = AgentRuntimeBackend(lambda: manager)
    asyncio.run(backend.start())
    assert backend.principal_id == "user:edge"
    backend.register_tools(["tool"])
    backend.unregister_tools(["tool"])
    assert backend.requires_composed_tool_approval("danger")
    assert backend.invoke_composed_tool("user:edge", "tool", {}) == "ok"
    assert backend.register_agent_loop_advice("before", "owner", 1, lambda: None) == 9
    assert backend.unregister_agent_loop_advice(9)
    assert backend.customization_diagnostics() == ("edge",)
    asyncio.run(backend.cancel_turn("turn"))
    asyncio.run(backend.approve_tool("approve"))
    asyncio.run(backend.reject_tool("reject", "reason"))
    asyncio.run(backend.stop())
