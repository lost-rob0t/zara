from __future__ import annotations

import pytest

from zara.runtime.backend import LangGraphRuntimeBackend, RuntimeBackend
from zara.runtime.host import RuntimeHost


class _Principal:
    principal_id = "principal-a"


class _Registry:
    def __init__(self):
        self.calls = []

    def requires_approval(self, name):
        self.calls.append(("approval", name))
        return name == "danger"

    def invoke_composed_tool(self, name, request):
        self.calls.append(("invoke", name, dict(request)))
        return {"ok": True, "name": name}


class _Manager:
    def __init__(self):
        self.principal = _Principal()
        self.tool_registry = _Registry()


@pytest.mark.asyncio
async def test_langgraph_backend_exposes_principal_bound_composition_hooks():
    manager = _Manager()
    backend = LangGraphRuntimeBackend(lambda: manager)
    await backend.start()

    assert backend.requires_composed_tool_approval("safe") is False
    assert backend.requires_composed_tool_approval("danger") is True
    assert backend.invoke_composed_tool("principal-a", "safe", {"value": 1}) == {
        "ok": True,
        "name": "safe",
    }
    assert manager.tool_registry.calls == [
        ("approval", "safe"),
        ("approval", "danger"),
        ("invoke", "safe", {"value": 1}),
    ]


@pytest.mark.asyncio
async def test_langgraph_backend_rejects_cross_principal_composition_before_registry_call():
    manager = _Manager()
    backend = LangGraphRuntimeBackend(lambda: manager)
    await backend.start()

    with pytest.raises(PermissionError, match="principal"):
        backend.invoke_composed_tool("principal-b", "safe", {"value": 1})

    assert manager.tool_registry.calls == []


@pytest.mark.asyncio
async def test_langgraph_backend_composition_hooks_fail_after_stop():
    manager = _Manager()
    manager.shutdown_async = lambda: _noop()
    backend = LangGraphRuntimeBackend(lambda: manager)
    await backend.start()
    await backend.stop()

    with pytest.raises(RuntimeError, match="not started"):
        backend.requires_composed_tool_approval("safe")
    with pytest.raises(RuntimeError, match="not started"):
        backend.invoke_composed_tool("principal-a", "safe", {})


async def _noop():
    return None


class _PluginConfig:
    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 1.0,
            "event_queue_size": 8,
            "max_managed_workers": 1,
        }

    def get_plugin_config(self, _name):
        return {}


@pytest.mark.asyncio
async def test_runtime_host_injects_backend_owned_composition_hooks(monkeypatch):
    captured = {}

    class _Backend:
        principal_id = "principal-a"

        def register_tools(self, _tools):
            pass

        def unregister_tools(self, _names):
            pass

        def register_agent_loop_advice(self, *_args):
            return 1

        def unregister_agent_loop_advice(self, _registration_id):
            return True

        def requires_composed_tool_approval(self, name):
            return name == "danger"

        def invoke_composed_tool(self, principal_id, name, request):
            return principal_id, name, dict(request)

    class _PluginManager:
        def __init__(self, _paths, **kwargs):
            captured.update(kwargs)

        async def start(self):
            pass

    monkeypatch.setattr("zara.runtime.host.PluginManager", _PluginManager)
    backend = _Backend()
    host = RuntimeHost(
        backend_factory=lambda: backend,
        plugin_paths=(),
        config=_PluginConfig(),
        publisher=lambda _event: None,
        subscriber=lambda **_kwargs: None,
    )
    host._backend = backend

    await host._start_plugins()

    approval_provider = captured["capability_approval_provider"]
    assert approval_provider("safe") is False
    assert approval_provider("danger") is True
    assert getattr(approval_provider, "__self__", None) is backend
    assert captured["capability_invoker"]("safe", {"value": 1}) == (
        "principal-a",
        "safe",
        {"value": 1},
    )


@pytest.mark.asyncio
async def test_runtime_host_keeps_plugins_available_when_composition_is_unsupported(monkeypatch):
    captured = {}

    class _Backend(RuntimeBackend):
        def register_tools(self, _tools):
            pass

        def unregister_tools(self, _names):
            pass

    class _PluginManager:
        def __init__(self, _paths, **kwargs):
            captured["kwargs"] = kwargs

        async def start(self):
            captured["started"] = True

    monkeypatch.setattr("zara.runtime.host.PluginManager", _PluginManager)
    backend = _Backend()
    host = RuntimeHost(
        backend_factory=lambda: backend,
        plugin_paths=(),
        config=_PluginConfig(),
        publisher=lambda _event: None,
        subscriber=lambda **_kwargs: None,
    )
    host._backend = backend

    await host._start_plugins()

    assert captured["started"] is True
    assert captured["kwargs"].get("capability_approval_provider") is None
    assert captured["kwargs"].get("capability_invoker") is None


class _MemoryProvider:
    def __init__(self):
        self.bound = []

    def bind_principal(self, principal):
        self.bound.append(principal)


@pytest.mark.asyncio
async def test_langgraph_backend_binds_memory_provider_to_manager_principal():
    manager = _Manager()
    manager.memory_manager = object()
    manager.replace_memory_manager = lambda provider: setattr(manager, "memory_manager", provider)
    provider = _MemoryProvider()
    backend = LangGraphRuntimeBackend(lambda: manager)
    await backend.start()

    backend.bind_memory_provider(provider)

    assert provider.bound == [manager.principal]
    assert manager.memory_manager is provider


@pytest.mark.asyncio
async def test_runtime_host_binds_only_exact_configured_memory_plugin_owner():
    provider = _MemoryProvider()

    class _Config(_PluginConfig):
        def get_section(self, name):
            if name == "memory":
                return {"provider": "plugin:zara-symbolic-memory"}
            return {}

    class _Backend:
        def __init__(self):
            self.bound = []

        def bind_memory_provider(self, value):
            self.bound.append(value)

    backend = _Backend()
    host = RuntimeHost(
        backend_factory=lambda: backend,
        plugin_paths=(),
        config=_Config(),
        publisher=lambda _event: None,
        subscriber=lambda **_kwargs: None,
    )
    host._backend = backend
    host._symbol_registry.register(
        symbol="memory.provider",
        kind="memory-provider",
        owner="plugin:zara-symbolic-memory",
        value=provider,
    )

    await host._bind_configured_memory_provider()

    assert backend.bound == [provider]


@pytest.mark.asyncio
async def test_runtime_host_fails_closed_when_configured_memory_plugin_is_missing():
    class _Config(_PluginConfig):
        def get_section(self, name):
            if name == "memory":
                return {"provider": "plugin:zara-symbolic-memory"}
            return {}

    class _Backend:
        def bind_memory_provider(self, _value):
            raise AssertionError("missing provider must not be bound")

    host = RuntimeHost(
        backend_factory=lambda: _Backend(),
        plugin_paths=(),
        config=_Config(),
        publisher=lambda _event: None,
        subscriber=lambda **_kwargs: None,
    )
    host._backend = _Backend()

    with pytest.raises(Exception, match="zara-symbolic-memory"):
        await host._bind_configured_memory_provider()


@pytest.mark.asyncio
async def test_runtime_host_rejects_memory_provider_from_wrong_plugin_owner():
    provider = _MemoryProvider()

    class _Config(_PluginConfig):
        def get_section(self, name):
            if name == "memory":
                return {"provider": "plugin:zara-symbolic-memory"}
            return {}

    class _Backend:
        def bind_memory_provider(self, _value):
            raise AssertionError("wrong owner must not be bound")

    host = RuntimeHost(
        backend_factory=lambda: _Backend(),
        plugin_paths=(),
        config=_Config(),
        publisher=lambda _event: None,
        subscriber=lambda **_kwargs: None,
    )
    host._backend = _Backend()
    host._symbol_registry.register(
        symbol="memory.provider",
        kind="memory-provider",
        owner="plugin:other-memory",
        value=provider,
    )

    with pytest.raises(Exception, match="owner"):
        await host._bind_configured_memory_provider()
