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
