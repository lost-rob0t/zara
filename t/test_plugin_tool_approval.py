from __future__ import annotations

import textwrap

import pytest
from langchain_core.tools import StructuredTool

from zara.agent.tools.registry import ToolRegistry
from zara.plugins import PluginManager, PluginState, RuntimeStatus
from zara.runtime.bridge import RuntimeEventBus


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


def _manager(tmp_path, registry):
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
