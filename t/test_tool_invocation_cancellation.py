import asyncio

import pytest
from langchain_core.messages import AIMessage
from langchain_core.tools import StructuredTool

from zara.agent.graph import create_tools_node
from zara.agent.tool_invocation import current_tool_invocation
from zara.agent.tools.registry import ToolRegistry


@pytest.mark.asyncio
async def test_running_tool_observes_exact_scoped_cancellation_before_cleanup():
    entered = asyncio.Event()
    cleanup = asyncio.Event()
    observed = {}

    async def long_running(value: str) -> str:
        invocation = current_tool_invocation()
        assert invocation is not None
        observed["principal_id"] = invocation.principal_id
        observed["turn_id"] = invocation.turn_id
        observed["conversation_id"] = invocation.conversation_id
        observed["tool_run_id"] = invocation.tool_run_id
        entered.set()
        try:
            await asyncio.Event().wait()
        except asyncio.CancelledError:
            observed["cancelled"] = invocation.cancelled
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
    state = {
        "messages": [
            AIMessage(
                content="",
                tool_calls=[
                    {
                        "name": "long_running",
                        "args": {"value": "x"},
                        "id": "tool-run-1",
                        "type": "tool_call",
                    }
                ],
            )
        ],
        "turn_id": "turn-1",
        "conversation_id": "conversation-1",
    }
    config = {
        "configurable": {
            "thread_id": "test-thread",
            "principal_id": "principal-1",
        }
    }

    task = asyncio.create_task(node(state, config))
    await asyncio.wait_for(entered.wait(), timeout=1.0)
    task.cancel()

    with pytest.raises(asyncio.CancelledError):
        await task
    await asyncio.wait_for(cleanup.wait(), timeout=1.0)

    assert observed == {
        "principal_id": "principal-1",
        "turn_id": "turn-1",
        "conversation_id": "conversation-1",
        "tool_run_id": "tool-run-1",
        "cancelled": True,
    }
    assert current_tool_invocation() is None


@pytest.mark.asyncio
async def test_concurrent_tool_invocation_contexts_are_isolated():
    release = asyncio.Event()
    seen = {}

    async def capture(value: str) -> str:
        invocation = current_tool_invocation()
        assert invocation is not None
        seen[value] = (
            invocation.principal_id,
            invocation.turn_id,
            invocation.tool_run_id,
        )
        await release.wait()
        return value

    tool = StructuredTool.from_function(
        coroutine=capture,
        name="capture_context",
        description="Capture the current Core-owned invocation context.",
    )
    registry = ToolRegistry()
    registry.register_tool(tool)
    node = create_tools_node(registry)

    def state(turn_id: str, tool_run_id: str, value: str):
        return {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {
                            "name": "capture_context",
                            "args": {"value": value},
                            "id": tool_run_id,
                            "type": "tool_call",
                        }
                    ],
                )
            ],
            "turn_id": turn_id,
            "conversation_id": "conversation-1",
        }

    config = {"configurable": {"thread_id": "test", "principal_id": "principal-1"}}
    first = asyncio.create_task(node(state("turn-a", "run-a", "a"), config))
    second = asyncio.create_task(node(state("turn-b", "run-b", "b"), config))

    for _ in range(100):
        if len(seen) == 2:
            break
        await asyncio.sleep(0)
    assert seen == {
        "a": ("principal-1", "turn-a", "run-a"),
        "b": ("principal-1", "turn-b", "run-b"),
    }

    release.set()
    await asyncio.gather(first, second)
    assert current_tool_invocation() is None
