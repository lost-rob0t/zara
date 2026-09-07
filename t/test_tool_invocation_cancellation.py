import asyncio

import pytest
from langchain_core.messages import AIMessage
from langchain_core.tools import StructuredTool

from zara.agent.graph import create_tools_node
from zara.agent.tools.registry import ToolRegistry
from zara.plugins import tool_cancellation_requested


def _state(tool_name: str, tool_run_id: str, value: str):
    return {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": tool_name,
                        "args": {"value": value},
                        "id": tool_run_id,
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": f"turn-{value}",
        "conversation_id": "conversation-1",
    }


@pytest.mark.asyncio
async def test_running_tool_observes_canonical_cancellation_during_cleanup():
    entered = asyncio.Event()
    cleanup = asyncio.Event()
    observed = {}

    async def long_running(value: str) -> str:
        assert tool_cancellation_requested() is False
        entered.set()
        try:
            await asyncio.Event().wait()
        except asyncio.CancelledError:
            observed["cancelled"] = tool_cancellation_requested()
            cleanup.set()
            raise

    tool = StructuredTool.from_function(
        coroutine=long_running,
        name="long_running",
        description="Wait until the canonical runtime cancels this invocation.",
    )
    registry = ToolRegistry()
    registry.register_tool(tool)
    node = create_tools_node(registry)

    task = asyncio.create_task(
        node(
            _state("long_running", "tool-run-1", "one"),
            {"configurable": {"thread_id": "test-thread"}},
        )
    )
    await asyncio.wait_for(entered.wait(), timeout=1.0)
    task.cancel()

    with pytest.raises(asyncio.CancelledError):
        await task
    await asyncio.wait_for(cleanup.wait(), timeout=1.0)

    assert observed == {"cancelled": True}
    assert tool_cancellation_requested() is False


@pytest.mark.asyncio
async def test_cancelling_one_tool_invocation_does_not_mark_another_cancelled():
    entered = {"a": asyncio.Event(), "b": asyncio.Event()}
    release_b = asyncio.Event()
    observed = {}

    async def capture(value: str) -> str:
        observed[f"{value}-before"] = tool_cancellation_requested()
        entered[value].set()
        try:
            if value == "a":
                await asyncio.Event().wait()
            await release_b.wait()
            return value
        except asyncio.CancelledError:
            observed[f"{value}-cancel"] = tool_cancellation_requested()
            raise

    tool = StructuredTool.from_function(
        coroutine=capture,
        name="capture_cancellation",
        description="Observe the current invocation's cancellation state.",
    )
    registry = ToolRegistry()
    registry.register_tool(tool)
    node = create_tools_node(registry)
    config = {"configurable": {"thread_id": "test"}}

    first = asyncio.create_task(node(_state("capture_cancellation", "run-a", "a"), config))
    second = asyncio.create_task(node(_state("capture_cancellation", "run-b", "b"), config))
    await asyncio.wait_for(entered["a"].wait(), timeout=1.0)
    await asyncio.wait_for(entered["b"].wait(), timeout=1.0)

    first.cancel()
    with pytest.raises(asyncio.CancelledError):
        await first

    observed["b-after-a-cancel"] = tool_cancellation_requested()
    release_b.set()
    await second

    assert observed == {
        "a-before": False,
        "b-before": False,
        "a-cancel": True,
        "b-after-a-cancel": False,
    }


@pytest.mark.asyncio
async def test_tool_timeout_is_not_reported_as_canonical_cancellation():
    observed = {}

    async def times_out(value: str) -> str:
        try:
            async with asyncio.timeout(0):
                await asyncio.sleep(1)
        except TimeoutError:
            observed["cancelled"] = tool_cancellation_requested()
            return value
        raise AssertionError("timeout did not fire")

    tool = StructuredTool.from_function(
        coroutine=times_out,
        name="times_out",
        description="Exercise timeout separately from canonical cancellation.",
    )
    registry = ToolRegistry()
    registry.register_tool(tool)
    node = create_tools_node(registry)

    await node(
        _state("times_out", "run-timeout", "timeout"),
        {"configurable": {"thread_id": "test-timeout"}},
    )

    assert observed == {"cancelled": False}
