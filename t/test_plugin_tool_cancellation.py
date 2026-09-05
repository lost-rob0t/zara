import asyncio
import threading

import pytest

from langchain_core.messages import AIMessage
from langchain_core.tools import tool

from zara.agent.graph import create_tools_node
from zara.agent.tools.registry import ToolRegistry
from zara.plugins import current_tool_cancellation
from zara.runtime import events


@pytest.mark.asyncio
async def test_running_sync_tool_observes_canonical_turn_cancellation():
    tool_entered = threading.Event()
    cancellation_observed = threading.Event()
    side_effects: list[str] = []

    @tool("cooperative_effect")
    def cooperative_effect(value: str) -> str:
        """Wait for Core cancellation before releasing owned work."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        tool_entered.set()
        if cancellation.wait(timeout=2.0):
            cancellation_observed.set()
            return "cancelled cooperatively"
        side_effects.append(value)
        return "effect complete"

    registry = ToolRegistry()
    registry.register_tool(cooperative_effect)
    published: list[events.RuntimeEvent] = []
    node = create_tools_node(registry, publisher=published.append)
    state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "cooperative_effect",
                        "args": {"value": "must-not-run"},
                        "id": "cooperative-call-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-388",
        "conversation_id": "conversation-388",
        "tool_decisions": {},
    }

    invocation = asyncio.create_task(node(state, {}))
    assert await asyncio.to_thread(tool_entered.wait, 1.0)
    assert any(isinstance(event, events.ToolStarted) for event in published)

    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation

    assert await asyncio.to_thread(cancellation_observed.wait, 1.0)
    assert side_effects == []
    terminal = [
        event
        for event in published
        if getattr(event, "tool_run_id", None) == "cooperative-call-1"
        and isinstance(
            event,
            (events.ToolCancelled, events.ToolCompleted, events.ToolFailed),
        )
    ]
    assert len(terminal) == 1
    assert isinstance(terminal[0], events.ToolCancelled)
    assert terminal[0].reason == "turn cancelled"


@pytest.mark.asyncio
async def test_cancellation_is_isolated_between_concurrent_tool_invocations():
    first_started = threading.Event()
    first_cancelled = threading.Event()
    second_started = threading.Event()
    second_cancelled = threading.Event()
    release_second = threading.Event()

    @tool("first_effect")
    def first_effect() -> str:
        """Observe cancellation for the first invocation only."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        first_started.set()
        if cancellation.wait(timeout=2.0):
            first_cancelled.set()
            return "cancelled"
        return "missed cancellation"

    @tool("second_effect")
    def second_effect() -> str:
        """Remain live while the first invocation is cancelled."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        second_started.set()
        while not release_second.wait(timeout=0.01):
            if cancellation.cancelled:
                second_cancelled.set()
                return "wrongly cancelled"
        return "completed"

    first_registry = ToolRegistry()
    first_registry.register_tool(first_effect)
    second_registry = ToolRegistry()
    second_registry.register_tool(second_effect)
    first_node = create_tools_node(first_registry)
    second_node = create_tools_node(second_registry)

    first_state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "first_effect",
                        "args": {},
                        "id": "first-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-first",
        "tool_decisions": {},
    }
    second_state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "second_effect",
                        "args": {},
                        "id": "second-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-second",
        "tool_decisions": {},
    }

    first_task = asyncio.create_task(first_node(first_state, {}))
    second_task = asyncio.create_task(second_node(second_state, {}))
    assert await asyncio.to_thread(first_started.wait, 1.0)
    assert await asyncio.to_thread(second_started.wait, 1.0)

    first_task.cancel()
    with pytest.raises(asyncio.CancelledError):
        await first_task
    assert await asyncio.to_thread(first_cancelled.wait, 1.0)
    assert second_cancelled.is_set() is False

    release_second.set()
    await asyncio.wait_for(second_task, timeout=1.0)
    assert second_cancelled.is_set() is False


@pytest.mark.asyncio
async def test_async_tool_sees_signal_before_cancelled_error():
    entered = asyncio.Event()
    saw_signalled_cancel = asyncio.Event()

    @tool("async_effect")
    async def async_effect() -> str:
        """Observe the same Core cancellation signal as synchronous tools."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        entered.set()
        try:
            await asyncio.sleep(10)
        except asyncio.CancelledError:
            if cancellation.cancelled:
                saw_signalled_cancel.set()
            raise
        return "unexpected"

    registry = ToolRegistry()
    registry.register_tool(async_effect)
    node = create_tools_node(registry)
    state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "async_effect",
                        "args": {},
                        "id": "async-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-async",
        "tool_decisions": {},
    }

    invocation = asyncio.create_task(node(state, {}))
    await asyncio.wait_for(entered.wait(), timeout=1.0)
    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation
    assert saw_signalled_cancel.is_set()


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
    state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "approved_effect",
                        "args": {},
                        "id": "approved-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-rejected",
        "tool_decisions": {"approved-1": {"decision": "reject"}},
    }

    result = await node(state, {})
    assert called is False
    assert result["messages"][0].status == "error"
    assert current_tool_cancellation() is None
