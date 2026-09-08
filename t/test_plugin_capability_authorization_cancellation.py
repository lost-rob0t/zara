from __future__ import annotations

import asyncio
import textwrap
import threading

import pytest
from langchain_core.tools import StructuredTool

from zara.agent.tools.registry import ToolRegistry
from zara.plugins import PluginManager, RuntimeStatus
from zara.runtime.bridge import RuntimeEventBus
from zara.runtime.turn_context import TurnCapabilityLease, bind_turn_capability_lease


def _write_plugin(path, *, name: str, tool_name: str | None = None):
    tool_block = ""
    if tool_name is not None:
        tool_block = textwrap.dedent(
            f"""
            def read(value: str) -> str:
                return value

            def _tools():
                return [StructuredTool.from_function(
                    read,
                    name={tool_name!r},
                    description="composition authorization test tool",
                )]
            """
        )
    else:
        tool_block = "def _tools():\n    return []\n"

    path.write_text(
        textwrap.dedent(
            f"""
            from langchain_core.tools import StructuredTool
            from zara.plugins import PluginMetadata, ServicePlugin

            {textwrap.indent(tool_block, '            ').lstrip()}

            class Plugin(ServicePlugin):
                metadata = PluginMetadata(name={name!r}, version="1")

                def tools(self):
                    return _tools()

                def start(self, runtime):
                    pass

                def stop(self):
                    pass

            def create_plugin():
                return Plugin()
            """
        )
    )


def _manager(tmp_path, registry, *, allowed=(), invoker=None):
    bus = RuntimeEventBus()

    def configuration(plugin_name):
        if plugin_name == "consumer":
            return {"compose_capabilities": list(allowed)}
        return {}

    return PluginManager(
        (tmp_path,),
        configuration_provider=configuration,
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda _command: None,
        subscriber=bus.subscribe,
        tool_registrar=registry.register_tools,
        tool_unregistrar=registry.unregister_tools,
        publisher=lambda _event: None,
        lifecycle_timeout=1.0,
        capability_approval_provider=lambda _name: False,
        capability_invoker=invoker or (lambda name, request: {"name": name, **request}),
    )


@pytest.mark.asyncio
async def test_capability_resolution_is_authorized_for_the_calling_plugin(tmp_path):
    _write_plugin(tmp_path / "consumer.py", name="consumer")
    _write_plugin(tmp_path / "provider.py", name="provider", tool_name="provider.read")
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, allowed=("provider.read",))
    await manager.start()

    handle = manager._resolve_capability("consumer", "provider.read")
    assert handle is not None
    assert handle.plugin_name == "provider"

    await manager.stop()


@pytest.mark.asyncio
async def test_unauthorized_caller_cannot_resolve_capability(tmp_path):
    _write_plugin(tmp_path / "consumer.py", name="consumer")
    _write_plugin(tmp_path / "provider.py", name="provider", tool_name="provider.read")
    registry = ToolRegistry()
    manager = _manager(tmp_path, registry, allowed=())
    await manager.start()

    with pytest.raises(PermissionError, match="not authorized"):
        manager._resolve_capability("consumer", "provider.read")

    await manager.stop()


@pytest.mark.asyncio
async def test_turn_cancellation_fences_inflight_composed_result(tmp_path):
    _write_plugin(tmp_path / "consumer.py", name="consumer")
    _write_plugin(tmp_path / "provider.py", name="provider", tool_name="provider.read")
    registry = ToolRegistry()
    invocation_started = threading.Event()
    release_invocation = threading.Event()

    def invoke(_name, _request):
        invocation_started.set()
        assert release_invocation.wait(timeout=1.0)
        return {"status": "stale"}

    manager = _manager(
        tmp_path,
        registry,
        allowed=("provider.read",),
        invoker=invoke,
    )
    await manager.start()
    handle = manager._resolve_capability("consumer", "provider.read")
    assert handle is not None
    lease = TurnCapabilityLease("turn-1")

    def invoke_bound():
        with bind_turn_capability_lease(lease):
            return manager._invoke_capability(
                "consumer",
                handle,
                {"value": "status"},
            )

    invocation = asyncio.create_task(asyncio.to_thread(invoke_bound))
    assert await asyncio.to_thread(invocation_started.wait, 1.0)

    lease.invalidate()
    release_invocation.set()

    with pytest.raises(RuntimeError, match="cancelled|stale"):
        await invocation

    await manager.stop()


@pytest.mark.asyncio
async def test_turn_cancellation_before_registration_fails_closed(tmp_path):
    _write_plugin(tmp_path / "consumer.py", name="consumer")
    _write_plugin(tmp_path / "provider.py", name="provider", tool_name="provider.read")
    registry = ToolRegistry()
    underlying_calls = 0

    def invoke(_name, _request):
        nonlocal underlying_calls
        underlying_calls += 1
        return {"status": "should-not-run"}

    manager = _manager(
        tmp_path,
        registry,
        allowed=("provider.read",),
        invoker=invoke,
    )
    await manager.start()
    handle = manager._resolve_capability("consumer", "provider.read")
    assert handle is not None
    lease = TurnCapabilityLease("turn-1")
    lease.invalidate()

    with bind_turn_capability_lease(lease):
        with pytest.raises(RuntimeError, match="cancelled|stale"):
            manager._invoke_capability(
                "consumer",
                handle,
                {"value": "status"},
            )

    assert underlying_calls == 0
    await manager.stop()


@pytest.mark.asyncio
async def test_turn_scoped_composition_fails_closed_when_plain_thread_loses_context(tmp_path):
    _write_plugin(tmp_path / "consumer.py", name="consumer")
    _write_plugin(tmp_path / "provider.py", name="provider", tool_name="provider.read")
    registry = ToolRegistry()
    underlying_calls = 0

    def invoke(_name, _request):
        nonlocal underlying_calls
        underlying_calls += 1
        return {"status": "escaped"}

    manager = _manager(
        tmp_path,
        registry,
        allowed=("provider.read",),
        invoker=invoke,
    )
    await manager.start()
    handle = manager._resolve_capability("consumer", "provider.read")
    assert handle is not None
    lease = TurnCapabilityLease("turn-1")
    outcome = []

    def invoke_without_propagated_context():
        try:
            manager._invoke_capability("consumer", handle, {"value": "status"})
        except Exception as error:
            outcome.append(error)

    with bind_turn_capability_lease(lease):
        worker = threading.Thread(target=invoke_without_propagated_context)
        worker.start()
        worker.join(timeout=1.0)

    assert not worker.is_alive()
    assert len(outcome) == 1
    assert isinstance(outcome[0], RuntimeError)
    assert "turn capability context" in str(outcome[0])
    assert underlying_calls == 0
    await manager.stop()
