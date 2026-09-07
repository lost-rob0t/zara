import asyncio
import threading

import pytest
from langchain_core.messages import AIMessage
from langchain_core.tools import tool

from zara.agent.graph import create_tools_node
from zara.agent.tools.registry import ToolRegistry
from zara.plugins import ToolCancellation, current_tool_cancellation
from zara.runtime import events


def _state(name: str, call_id: str):
    return {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {"name": name, "args": {}, "id": call_id, "type": "tool_call"}
                ],
            )
        ],
        "turn_id": f"turn-{call_id}",
        "conversation_id": f"conversation-{call_id}",
        "tool_decisions": {},
    }


def _cancellable(tool_instance):
    tool_instance.metadata = dict(tool_instance.metadata or {})
    tool_instance.metadata["zara_supports_cancellation"] = True
    return tool_instance


@pytest.mark.asyncio
async def test_sync_tool_observes_signal_before_owned_work_continues():
    entered = threading.Event()
    observed = threading.Event()
    side_effect = []

    @_cancellable
    @tool("sync_effect")
    def sync_effect() -> str:
        """Wait for canonical cancellation before releasing owned work."""
        cancellation = current_tool_cancellation()
        assert isinstance(cancellation, ToolCancellation)
        entered.set()
        if cancellation.wait(timeout=2.0):
            observed.set()
            return "cancelled"
        side_effect.append("escaped")
        return "completed"

    registry = ToolRegistry()
    registry.register_tool(sync_effect)
    published = []
    node = create_tools_node(registry, publisher=published.append)
    invocation = asyncio.create_task(node(_state("sync_effect", "sync-1"), {}))

    assert await asyncio.to_thread(entered.wait, 1.0)
    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation

    assert await asyncio.to_thread(observed.wait, 1.0)
    assert side_effect == []
    terminal = [
        event
        for event in published
        if getattr(event, "tool_run_id", None) == "sync-1"
        and isinstance(event, (events.ToolCancelled, events.ToolCompleted, events.ToolFailed))
    ]
    assert len(terminal) == 1
    assert isinstance(terminal[0], events.ToolCancelled)


@pytest.mark.asyncio
async def test_async_tool_sees_signal_before_cancelled_error():
    entered = asyncio.Event()
    observed = asyncio.Event()

    @_cancellable
    @tool("async_effect")
    async def async_effect() -> str:
        """Observe canonical signal before task cancellation reaches the body."""
        cancellation = current_tool_cancellation()
        entered.set()
        try:
            await asyncio.Event().wait()
        except asyncio.CancelledError:
            if cancellation.cancelled:
                observed.set()
            raise

    registry = ToolRegistry()
    registry.register_tool(async_effect)
    node = create_tools_node(registry)
    invocation = asyncio.create_task(node(_state("async_effect", "async-1"), {}))

    await asyncio.wait_for(entered.wait(), timeout=1.0)
    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation
    assert observed.is_set()


@pytest.mark.asyncio
async def test_concurrent_invocations_have_isolated_signals():
    first_entered = threading.Event()
    first_cancelled = threading.Event()
    second_entered = threading.Event()
    second_cancelled = threading.Event()
    release_second = threading.Event()
    signal_ids = set()
    lock = threading.Lock()

    @_cancellable
    @tool("first_effect")
    def first_effect() -> str:
        """Observe only the first invocation cancellation."""
        cancellation = current_tool_cancellation()
        with lock:
            signal_ids.add(id(cancellation))
        first_entered.set()
        if cancellation.wait(timeout=2.0):
            first_cancelled.set()
            return "cancelled"
        return "missed"

    @_cancellable
    @tool("second_effect")
    def second_effect() -> str:
        """Remain live while the first invocation is cancelled."""
        cancellation = current_tool_cancellation()
        with lock:
            signal_ids.add(id(cancellation))
        second_entered.set()
        while not release_second.wait(timeout=0.01):
            if cancellation.cancelled:
                second_cancelled.set()
                return "wrongly-cancelled"
        return "completed"

    first_registry = ToolRegistry()
    first_registry.register_tool(first_effect)
    second_registry = ToolRegistry()
    second_registry.register_tool(second_effect)
    first = asyncio.create_task(create_tools_node(first_registry)(_state("first_effect", "first-1"), {}))
    second = asyncio.create_task(create_tools_node(second_registry)(_state("second_effect", "second-1"), {}))

    assert await asyncio.to_thread(first_entered.wait, 1.0)
    assert await asyncio.to_thread(second_entered.wait, 1.0)
    first.cancel()
    with pytest.raises(asyncio.CancelledError):
        await first
    assert await asyncio.to_thread(first_cancelled.wait, 1.0)
    assert not second_cancelled.is_set()
    release_second.set()
    await asyncio.wait_for(second, timeout=1.0)
    assert len(signal_ids) == 2


def test_cancellation_context_is_not_model_visible_or_globally_available():
    @_cancellable
    @tool("schema_probe")
    def schema_probe(value: str) -> str:
        """Expose only the ordinary model argument."""
        current_tool_cancellation()
        return value

    assert set(schema_probe.tool_call_schema.model_json_schema()["properties"]) == {"value"}
    with pytest.raises(RuntimeError, match="no active cancellable tool invocation"):
        current_tool_cancellation()
