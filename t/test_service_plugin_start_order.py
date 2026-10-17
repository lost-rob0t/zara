from __future__ import annotations

import textwrap

import pytest
from langchain_core.tools import StructuredTool

from zara.agent.tools.registry import ToolRegistry
from zara.plugins import PluginManager, PluginState, RuntimeStatus
from zara.plugins.loader import load_plugin_module
from zara.runtime.bridge import RuntimeEventBus
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import RestartRuntime
from zara.runtime.host import RuntimeHost


def configured_plugin(path):
    path.write_text(textwrap.dedent('''
        from langchain_core.tools import StructuredTool
        from zara.plugins import PluginMetadata, ServicePlugin, StartupUnavailable

        ORDER = []
        RUNTIME = None
        SUBSCRIPTION = None

        class ConfiguredPlugin(ServicePlugin):
            metadata = PluginMetadata(name="configured-service")

            def start(self, runtime):
                global RUNTIME, SUBSCRIPTION
                ORDER.append("start")
                RUNTIME = runtime
                self.configuration = dict(runtime.configuration)
                SUBSCRIPTION = runtime.subscribe()
                mode = self.configuration.get("mode")
                if mode == "unavailable":
                    return StartupUnavailable("missing_credentials")
                if mode == "invalid_start":
                    return "ready"
                if mode == "start_error":
                    raise RuntimeError("start failed")
                if mode == "worker_failure":
                    def broken_worker(_stop_event):
                        raise RuntimeError("worker failed during startup")
                    runtime.start_worker("broken", broken_worker).join(timeout=1)

            def tools(self):
                ORDER.append("tools")
                mode = self.configuration.get("mode")
                if mode == "tools_error":
                    raise RuntimeError("tools failed")
                if mode == "invalid_tools":
                    return [object()]
                if mode == "tools_worker_failure":
                    def broken_worker(_stop_event):
                        raise RuntimeError("worker failed during tools")
                    RUNTIME.start_worker("broken", broken_worker).join(timeout=1)
                def echo(value: str) -> str:
                    return value
                return [StructuredTool.from_function(
                    echo, name=self.configuration["tool_name"], description="Configured echo"
                )]

            def stop(self):
                ORDER.append("stop")

        def create_plugin():
            return ConfiguredPlugin()
    '''))
    return load_plugin_module(path)


def manager_for(tmp_path, registry, published, *, mode=None):
    bus = RuntimeEventBus()
    return PluginManager(
        (tmp_path,),
        configuration_provider=lambda name: {
            "enabled": name == "configured-service", "tool_name": "configured_echo", "mode": mode,
        },
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda command: None,
        subscriber=bus.subscribe,
        tool_registrar=registry.register_tools,
        tool_unregistrar=registry.unregister_tools,
        publisher=published.append,
        lifecycle_timeout=0.2,
    )


@pytest.mark.asyncio
async def test_tools_use_completed_start_configuration_then_are_removed_on_stop(tmp_path):
    module = configured_plugin(tmp_path / "configured.py")
    registry = ToolRegistry()
    published = []
    manager = manager_for(tmp_path, registry, published)
    try:
        await manager.start()
        assert module.ORDER == ["start", "tools"]
        assert registry.list_tools() == ["configured_echo"]
        assert registry.get_tool("configured_echo").invoke({"value": "ready"}) == "ready"
        assert manager.diagnostics()[0].state is PluginState.RUNNING
        assert published == []
    finally:
        await manager.stop()
    assert module.ORDER == ["start", "tools", "stop"]
    assert registry.list_tools() == []
    assert module.SUBSCRIPTION.closed


@pytest.mark.asyncio
@pytest.mark.parametrize("mode", ["tools_error", "invalid_tools", "tools_worker_failure"])
async def test_tools_failure_rolls_back_started_plugin_and_owned_resources(tmp_path, mode):
    module = configured_plugin(tmp_path / "configured.py")
    registry = ToolRegistry()
    published = []
    manager = manager_for(tmp_path, registry, published, mode=mode)
    await manager.start()
    assert module.ORDER == ["start", "tools", "stop"]
    assert registry.list_tools() == []
    assert module.SUBSCRIPTION.closed
    assert manager.diagnostics()[0].state is PluginState.FAILED
    assert len(published) == 1
    await manager.stop()
    assert module.ORDER.count("stop") == 1


@pytest.mark.asyncio
async def test_duplicate_registration_preserves_existing_tool_and_stops_new_plugin(tmp_path):
    module = configured_plugin(tmp_path / "configured.py")
    registry = ToolRegistry()
    existing = StructuredTool.from_function(lambda value: value, name="configured_echo", description="Existing owner")
    registry.register_tools([existing])
    published = []
    manager = manager_for(tmp_path, registry, published)
    await manager.start()
    assert module.ORDER == ["start", "tools", "stop"]
    assert registry.list_tools() == ["configured_echo"]
    assert registry.get_tool("configured_echo").description == "Existing owner"
    assert module.SUBSCRIPTION.closed
    assert manager.diagnostics()[0].state is PluginState.FAILED
    assert len(published) == 1
    await manager.stop()
    assert registry.list_tools() == ["configured_echo"]


@pytest.mark.asyncio
@pytest.mark.parametrize("mode", ["unavailable", "invalid_start", "start_error", "worker_failure"])
async def test_unsuccessful_start_never_constructs_or_registers_tools(tmp_path, mode):
    module = configured_plugin(tmp_path / "configured.py")
    registry = ToolRegistry()
    manager = manager_for(tmp_path, registry, [], mode=mode)
    await manager.start()
    assert "tools" not in module.ORDER
    assert registry.list_tools() == []
    assert module.SUBSCRIPTION.closed
    expected = PluginState.UNAVAILABLE if mode == "unavailable" else PluginState.FAILED
    assert manager.diagnostics()[0].state is expected
    await manager.stop()
    assert module.ORDER == ["start", "stop"]


def test_runtime_host_restart_rebuilds_configured_tools_and_cleans_both_generations(tmp_path):
    module = configured_plugin(tmp_path / "configured.py")
    backends = []

    class Backend(RuntimeBackend):
        def __init__(self):
            self.registry = ToolRegistry()

        def register_tools(self, tools):
            self.registry.register_tools(tools)

        def unregister_tools(self, names):
            self.registry.unregister_tools(names)

    class Configuration:
        def get_plugin_runtime_config(self):
            return {"lifecycle_timeout": 0.5, "event_queue_size": 4, "max_managed_workers": 2}

        def get_plugin_config(self, name):
            return {"enabled": name == "configured-service", "tool_name": "configured_echo"}

    def backend_factory():
        backend = Backend()
        backends.append(backend)
        return backend

    host = RuntimeHost(backend_factory, plugin_paths=(tmp_path,), config=Configuration())
    try:
        host.start().result(timeout=5)
        assert backends[0].registry.list_tools() == ["configured_echo"]
        first_subscription = module.SUBSCRIPTION
        host.submit(RestartRuntime()).result(timeout=5)
        assert module.ORDER == ["start", "tools", "stop", "start", "tools"]
        assert first_subscription.closed
        assert backends[0].registry.list_tools() == []
        assert backends[1].registry.list_tools() == ["configured_echo"]
    finally:
        host.shutdown("test cleanup").result(timeout=5)
        host.join(timeout=5)
    assert module.ORDER == ["start", "tools", "stop", "start", "tools", "stop"]
    assert backends[1].registry.list_tools() == []
    assert module.SUBSCRIPTION.closed
