import asyncio
import threading

import pytest
from langchain_core.messages import AIMessage
from langchain_core.tools import tool

from zara.agent.graph import create_tools_node
from zara.agent.tools.registry import ToolRegistry
from zara.plugins import current_tool_cancellation
from zara.runtime import events


def _state(name: str, call_id: str, args=None):
    return {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": name,
                        "args": args or {},
                        "id": call_id,
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": f"turn-{call_id}",
        "conversation_id": f"conversation-{call_id}",
        "tool_decisions": {},
    }


@pytest.mark.asyncio
async def test_running_sync_tool_observes_canonical_turn_cancellation():
    entered = threading.Event()
    observed = threading.Event()
    side_effects = []

    @tool("cooperative_effect")
    def cooperative_effect(value: str) -> str:
        """Wait for Core cancellation before releasing owned work."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        entered.set()
        if cancellation.wait(timeout=2.0):
            observed.set()
            return "cancelled"
        side_effects.append(value)
        return "completed"

    registry = ToolRegistry()
    registry.register_tool(cooperative_effect)
    published = []
    node = create_tools_node(registry, publisher=published.append)

    invocation = asyncio.create_task(
        node(_state("cooperative_effect", "sync-1", {"value": "must-not-run"}), {})
    )
    assert await asyncio.to_thread(entered.wait, 1.0)

    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation

    assert await asyncio.to_thread(observed.wait, 1.0)
    assert side_effects == []
    terminal = [
        event
        for event in published
        if getattr(event, "tool_run_id", None) == "sync-1"
        and isinstance(
            event,
            (events.ToolCancelled, events.ToolCompleted, events.ToolFailed),
        )
    ]
    assert len(terminal) == 1
    assert isinstance(terminal[0], events.ToolCancelled)


@pytest.mark.asyncio
async def test_async_tool_sees_signal_before_cancelled_error():
    entered = asyncio.Event()
    signalled_before_cancelled_error = asyncio.Event()

    @tool("async_effect")
    async def async_effect() -> str:
        """Observe Core cancellation before task cancellation reaches the tool."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        entered.set()
        try:
            await asyncio.sleep(10)
        except asyncio.CancelledError:
            if cancellation.cancelled:
                signalled_before_cancelled_error.set()
            raise
        return "unexpected"

    registry = ToolRegistry()
    registry.register_tool(async_effect)
    node = create_tools_node(registry)

    invocation = asyncio.create_task(node(_state("async_effect", "async-1"), {}))
    await asyncio.wait_for(entered.wait(), timeout=1.0)
    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation

    assert signalled_before_cancelled_error.is_set()


@pytest.mark.asyncio
async def test_cancellation_is_isolated_between_concurrent_invocations():
    first_entered = threading.Event()
    first_cancelled = threading.Event()
    second_entered = threading.Event()
    second_cancelled = threading.Event()
    release_second = threading.Event()

    @tool("first_effect")
    def first_effect() -> str:
        """Observe cancellation for only the first invocation."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        first_entered.set()
        if cancellation.wait(timeout=2.0):
            first_cancelled.set()
            return "cancelled"
        return "missed"

    @tool("second_effect")
    def second_effect() -> str:
        """Stay live while another invocation is cancelled."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
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
    first_node = create_tools_node(first_registry)
    second_node = create_tools_node(second_registry)

    first = asyncio.create_task(first_node(_state("first_effect", "first-1"), {}))
    second = asyncio.create_task(second_node(_state("second_effect", "second-1"), {}))
    assert await asyncio.to_thread(first_entered.wait, 1.0)
    assert await asyncio.to_thread(second_entered.wait, 1.0)

    first.cancel()
    with pytest.raises(asyncio.CancelledError):
        await first
    assert await asyncio.to_thread(first_cancelled.wait, 1.0)
    assert second_cancelled.is_set() is False

    release_second.set()
    await asyncio.wait_for(second, timeout=1.0)
    assert second_cancelled.is_set() is False


@pytest.mark.asyncio
async def test_rejected_tool_never_enters_cancellation_scope():
    called = False

    @tool("approved_effect")
    def approved_effect() -> str:
        """Must never execute after rejection."""
        nonlocal called
        called = True
        return "unexpected"

    approved_effect.metadata = {"zara_requires_approval": True}
    registry = ToolRegistry()
    registry.register_tool(approved_effect)
    node = create_tools_node(registry)
    state = _state("approved_effect", "approval-1")
    state["tool_decisions"] = {"approval-1": {"decision": "reject"}}

    result = await node(state, {})
    assert called is False
    assert result["messages"][0].status == "error"
    assert current_tool_cancellation() is None
