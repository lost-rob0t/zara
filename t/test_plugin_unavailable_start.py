from __future__ import annotations

import asyncio
import dataclasses
import textwrap
import time

import pytest

from zara.agent.tools.registry import ToolRegistry
from zara.plugins import PluginManager, PluginState, RuntimeStatus, StartupUnavailable
from zara.plugins.loader import load_plugin_module
from zara.runtime import events
from zara.runtime.bridge import RuntimeEventBus


def _write_plugin(path, *, name: str, start_body: str, start_def: str = "def"):
    path.write_text(
        textwrap.dedent(
            f"""
            from langchain_core.tools import StructuredTool
            from zara.plugins import PluginMetadata, ServicePlugin, StartupUnavailable

            STOP_EVENTS = []

            def read(value: str) -> str:
                return value

            class Plugin(ServicePlugin):
                metadata = PluginMetadata(name={name!r}, version="1")

                def tools(self):
                    return [StructuredTool.from_function(
                        read,
                        name={name!r} + ".read",
                        description="unavailable startup lifecycle test tool",
                    )]

                {start_def} start(self, runtime):
                    {textwrap.indent(start_body, ' ' * 20).lstrip()}

                def stop(self):
                    STOP_EVENTS.append("stopped")

            def create_plugin():
                return Plugin()
            """
        )
    )


class RecordingPublisher:
    def __init__(self):
        self.events = []

    def __call__(self, event):
        self.events.append(event)


def _manager(tmp_path, registry, publisher, *, allowed=()):
    bus = RuntimeEventBus()

    def configuration(plugin_name):
        config = {}
        if allowed:
            config["compose_capabilities"] = list(allowed)
        return config

    return PluginManager(
        (tmp_path,),
        configuration_provider=configuration,
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda _command: None,
        subscriber=bus.subscribe,
        tool_registrar=registry.register_tools,
        tool_unregistrar=registry.unregister_tools,
        publisher=publisher,
        lifecycle_timeout=1.0,
        capability_approval_provider=lambda _name: False,
        capability_invoker=lambda name, request: {"name": name, **request},
    )


def _diagnostic(manager, name):
    return next(item for item in manager.diagnostics() if item.name == name)


@pytest.mark.asyncio
async def test_start_returning_none_remains_running(tmp_path):
    _write_plugin(tmp_path / "legacy_plugin.py", name="legacy", start_body="return None")
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, RecordingPublisher())

    await manager.start()

    diagnostic = _diagnostic(manager, "legacy")
    assert diagnostic.state is PluginState.RUNNING
    assert diagnostic.error == ""

    await manager.stop()


@pytest.mark.asyncio
async def test_start_returning_unavailable_is_marked_unavailable(tmp_path):
    _write_plugin(
        tmp_path / "gated_plugin.py",
        name="gated",
        start_body="return StartupUnavailable('missing_credentials')",
    )
    registry = ToolRegistry()
    publisher = RecordingPublisher()
    manager = _manager(tmp_path, registry, publisher)

    await manager.start()

    diagnostic = _diagnostic(manager, "gated")
    assert diagnostic.state is PluginState.UNAVAILABLE
    assert diagnostic.error == "missing_credentials"
    assert not any(isinstance(event, events.RuntimeError) for event in publisher.events)

    await manager.stop()


@pytest.mark.asyncio
async def test_unavailable_plugin_does_not_retain_tools_or_capabilities(tmp_path):
    _write_plugin(
        tmp_path / "gated_plugin.py",
        name="gated",
        start_body="return StartupUnavailable('missing_credentials')",
    )
    _write_plugin(tmp_path / "healthy_plugin.py", name="healthy", start_body="return None")
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, RecordingPublisher(), allowed=("gated.read",))

    await manager.start()

    assert registry.get_tool("gated.read") is None
    assert registry.get_tool("healthy.read") is not None
    with pytest.raises(RuntimeError, match="calling plugin is not running"):
        manager._resolve_capability("gated", "gated.read")
    assert manager._resolve_capability("healthy", "gated.read") is None

    await manager.stop()


@pytest.mark.asyncio
async def test_start_exception_remains_failed(tmp_path):
    _write_plugin(
        tmp_path / "broken_plugin.py",
        name="broken",
        start_body="raise RuntimeError('provider endpoint rejected secret-value')",
    )
    registry = ToolRegistry()
    publisher = RecordingPublisher()
    manager = _manager(tmp_path, registry, publisher)

    await manager.start()

    diagnostic = _diagnostic(manager, "broken")
    assert diagnostic.state is PluginState.FAILED
    assert registry.get_tool("broken.read") is None
    assert any(isinstance(event, events.RuntimeError) for event in publisher.events)

    await manager.stop()


@pytest.mark.asyncio
async def test_unexpected_start_return_is_failed(tmp_path):
    _write_plugin(
        tmp_path / "sloppy_plugin.py",
        name="sloppy",
        start_body="return 'started'",
    )
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, RecordingPublisher())

    await manager.start()

    diagnostic = _diagnostic(manager, "sloppy")
    assert diagnostic.state is PluginState.FAILED
    assert "StartupUnavailable" in diagnostic.error
    assert registry.get_tool("sloppy.read") is None

    await manager.stop()


@pytest.mark.asyncio
async def test_stop_after_unavailable_invokes_plugin_stop(tmp_path):
    _write_plugin(
        tmp_path / "gated_plugin.py",
        name="gated",
        start_body="return StartupUnavailable('missing_credentials')",
    )
    module = load_plugin_module(tmp_path / "gated_plugin.py")
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, RecordingPublisher())

    await manager.start()
    await manager.stop()

    diagnostic = _diagnostic(manager, "gated")
    assert diagnostic.state is PluginState.STOPPED
    assert diagnostic.error == "missing_credentials"
    assert module.STOP_EVENTS == ["stopped"]


@pytest.mark.asyncio
async def test_runtime_failure_during_start_outranks_unavailable(tmp_path):
    _write_plugin(
        tmp_path / "gated_plugin.py",
        name="gated",
        start_body=textwrap.dedent(
            """
            def explode(stop_event):
                raise RuntimeError('worker exploded')

            runtime.start_worker('explode', explode)
            import time

            time.sleep(0.25)
            return StartupUnavailable('missing_credentials')
            """
        ),
    )
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, RecordingPublisher())

    await manager.start()

    deadline = time.monotonic() + 2
    while time.monotonic() < deadline:
        if _diagnostic(manager, "gated").state is PluginState.FAILED:
            break
        await asyncio.sleep(0.05)
    diagnostic = _diagnostic(manager, "gated")
    assert diagnostic.state is PluginState.FAILED
    assert "worker exploded" in diagnostic.error

    await manager.stop()


@pytest.mark.asyncio
async def test_async_start_returning_unavailable_is_marked_unavailable(tmp_path):
    _write_plugin(
        tmp_path / "async_gated_plugin.py",
        name="async-gated",
        start_def="async def",
        start_body="return StartupUnavailable('missing_credentials')",
    )
    registry = ToolRegistry()
    publisher = RecordingPublisher()
    manager = _manager(tmp_path, registry, publisher)

    await manager.start()

    diagnostic = _diagnostic(manager, "async-gated")
    assert diagnostic.state is PluginState.UNAVAILABLE
    assert diagnostic.error == "missing_credentials"
    assert registry.get_tool("async-gated.read") is None
    assert not any(isinstance(event, events.RuntimeError) for event in publisher.events)

    await manager.stop()


def test_unavailable_reason_code_is_bounded_and_non_secret():
    assert StartupUnavailable("missing_credentials").reason == "missing_credentials"

    with pytest.raises(ValueError):
        StartupUnavailable("")
    with pytest.raises(ValueError):
        StartupUnavailable("x" * 65)
    with pytest.raises(ValueError):
        StartupUnavailable("token=sk-super-secret")
    with pytest.raises(ValueError):
        StartupUnavailable("has spaces")
    with pytest.raises(ValueError):
        StartupUnavailable("UPPER")
    with pytest.raises(ValueError):
        StartupUnavailable(None)

    frozen = StartupUnavailable("missing_credentials")
    with pytest.raises(dataclasses.FrozenInstanceError):
        frozen.reason = "other"
