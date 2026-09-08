from __future__ import annotations

import asyncio
import textwrap
import threading

import pytest
from langchain_core.tools import StructuredTool

from zara.agent.tools.registry import ToolRegistry
from zara.plugins import PluginManager, PluginState, RuntimeStatus
from zara.runtime.bridge import RuntimeEventBus
from zara.runtime.turn_context import TurnCapabilityLease, bind_turn_capability_lease


class _Config:
    def __init__(self, required=()):
        self._required = list(required)

    def get_section(self, name):
        if name == "tool_approval":
            return {"required_tools": list(self._required)}
        return {}


def _tool(name: str, approval=None):
    def invoke(value: str) -> str:
        return value

    metadata = {} if approval is None else {"zara_requires_approval": approval}
    return StructuredTool.from_function(
        invoke,
        name=name,
        description="approval contract test tool",
        metadata=metadata,
    )


def test_registered_approval_requirement_is_removed_with_tool():
    registry = ToolRegistry(config=_Config())

    registry.register_tools([_tool("plugin_mutate", True)])

    assert registry.requires_approval("plugin_mutate") is True
    registry.unregister_tools(["plugin_mutate"])
    assert registry.get_tool("plugin_mutate") is None
    assert registry.requires_approval("plugin_mutate") is False


def test_dynamic_unregistration_cannot_weaken_configured_approval():
    registry = ToolRegistry(config=_Config(required=("plugin_mutate",)))
    registry.register_tools([_tool("plugin_mutate", False)])

    registry.unregister_tools(["plugin_mutate"])

    assert registry.requires_approval("plugin_mutate") is True


def test_conflicting_registration_does_not_leak_approval_metadata():
    registry = ToolRegistry(config=_Config())
    registry.register_tools([_tool("same_name")])

    with pytest.raises(ValueError, match="already registered"):
        registry.register_tools([_tool("same_name", True)])

    assert registry.requires_approval("same_name") is False


def test_malformed_approval_marker_fails_registration_closed():
    registry = ToolRegistry(config=_Config())

    with pytest.raises(ValueError, match="must be true or false"):
        registry.register_tools([_tool("plugin_mutate", "yes")])

    assert registry.get_tool("plugin_mutate") is None
    assert registry.requires_approval("plugin_mutate") is False


def test_composed_tool_execution_uses_canonical_registry_and_structured_request():
    registry = ToolRegistry(config=_Config())
    seen = []

    def invoke(value: str) -> str:
        seen.append(value)
        return f"ok:{value}"

    registry.register_tool(
        StructuredTool.from_function(
            invoke,
            name="plugin_read",
            description="composition contract test tool",
        )
    )

    result = registry.invoke_composed_tool("plugin_read", {"value": "status"})

    assert result == "ok:status"
    assert seen == ["status"]


def test_composed_tool_execution_fails_closed_for_missing_or_approval_required_tool():
    registry = ToolRegistry(config=_Config(required=("plugin_mutate",)))
    calls = []

    def mutate(value: str) -> str:
        calls.append(value)
        return value

    registry.register_tool(
        StructuredTool.from_function(
            mutate,
            name="plugin_mutate",
            description="composition approval test tool",
        )
    )

    with pytest.raises(LookupError, match="tool is unavailable"):
        registry.invoke_composed_tool("missing_tool", {})
    with pytest.raises(PermissionError, match="requires canonical interactive approval"):
        registry.invoke_composed_tool("plugin_mutate", {"value": "danger"})

    assert calls == []


def test_composed_tool_execution_rejects_non_mapping_request_before_tool_lookup():
    registry = ToolRegistry(config=_Config())

    with pytest.raises(TypeError, match="mapping"):
        registry.invoke_composed_tool("missing_tool", [])


def _write_plugin(path, approval_marker):
    path.write_text(
        textwrap.dedent(
            f"""
            from langchain_core.tools import StructuredTool
            from zara.plugins import PluginMetadata, ServicePlugin

            def mutate(value: str) -> str:
                return value

            class Plugin(ServicePlugin):
                metadata = PluginMetadata(name="approval-test", version="1")

                def tools(self):
                    return [StructuredTool.from_function(
                        mutate,
                        name="plugin_mutate",
                        description="approval integration test tool",
                        metadata={{"zara_requires_approval": {approval_marker!r}}},
                    )]

                def start(self, runtime):
                    pass

                def stop(self):
                    pass

            def create_plugin():
                return Plugin()
            """
        )
    )


def _manager(
    tmp_path,
    registry,
    *,
    capability_approval_provider=None,
    capability_invoker=None,
):
    bus = RuntimeEventBus()
    return PluginManager(
        (tmp_path,),
        configuration_provider=lambda _name: {},
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda _command: None,
        subscriber=bus.subscribe,
        tool_registrar=registry.register_tools,
        tool_unregistrar=registry.unregister_tools,
        publisher=lambda _event: None,
        lifecycle_timeout=1.0,
        capability_approval_provider=capability_approval_provider,
        capability_invoker=capability_invoker,
    )


@pytest.mark.asyncio
async def test_service_plugin_declares_canonical_approval_and_unloads_atomically(tmp_path):
    _write_plugin(tmp_path / "approval_plugin.py", True)
    registry = ToolRegistry(config=_Config())
    manager = _manager(tmp_path, registry)

    await manager.start()

    assert registry.get_tool("plugin_mutate") is not None
    assert registry.requires_approval("plugin_mutate") is True
    assert manager.diagnostics()[0].state is PluginState.RUNNING

    await manager.stop()

    assert registry.get_tool("plugin_mutate") is None
    assert registry.requires_approval("plugin_mutate") is False


@pytest.mark.asyncio
async def test_service_plugin_malformed_approval_marker_fails_startup_closed(tmp_path):
    _write_plugin(tmp_path / "approval_plugin.py", "yes")
    registry = ToolRegistry(config=_Config())
    manager = _manager(tmp_path, registry)

    await manager.start()

    diagnostic = manager.diagnostics()[0]
    assert diagnostic.state is PluginState.FAILED
    assert "must be true or false" in diagnostic.error
    assert registry.get_tool("plugin_mutate") is None
    assert registry.requires_approval("plugin_mutate") is False


@pytest.mark.asyncio
async def test_inflight_composed_invocation_fences_provider_unload(tmp_path):
    _write_plugin(tmp_path / "approval_plugin.py", False)
    registry = ToolRegistry(config=_Config())
    invocation_started = threading.Event()
    release_invocation = threading.Event()

    def invoke(_name, _request):
        invocation_started.set()
        assert release_invocation.wait(timeout=1.0)
        return {"status": "ok"}

    manager = _manager(
        tmp_path,
        registry,
        capability_approval_provider=lambda _name: False,
        capability_invoker=invoke,
    )
    await manager.start()
    handle = manager._resolve_capability("plugin_mutate")
    assert handle is not None
    lease = TurnCapabilityLease("turn-unload-test")

    def invoke_bound():
        with bind_turn_capability_lease(lease):
            return manager._invoke_capability(
                "approval-test",
                handle,
                {"value": "status"},
            )

    invocation = asyncio.create_task(asyncio.to_thread(invoke_bound))
    assert await asyncio.to_thread(invocation_started.wait, 1.0)

    stopping = asyncio.create_task(manager.stop())
    await asyncio.sleep(0.05)
    assert not stopping.done(), "provider unload overtook an accepted composed invocation"

    release_invocation.set()
    assert await invocation == {"status": "ok"}
    await stopping
    assert registry.get_tool("plugin_mutate") is None
