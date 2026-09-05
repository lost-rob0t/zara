import asyncio
import threading

import pytest

from langchain_core.messages import AIMessage
from langchain_core.tools import tool

from zara.agent.graph import create_tools_node
from zara.agent.tools.registry import ToolRegistry
from zara.plugins import current_tool_cancellation
from zara.plugins.cancellation import bind_tool_cancellation
from zara.runtime import events


@pytest.mark.asyncio
async def test_running_sync_tool_observes_canonical_turn_cancellation():
    cancellation_observed = threading.Event()
    side_effects: list[str] = []

    @tool("cooperative_effect")
    def cooperative_effect(value: str) -> str:
        """Wait for Core cancellation before releasing owned work."""
        cancellation = current_tool_cancellation()
        assert cancellation is not None
        if cancellation.wait(timeout=2.0):
            cancellation_observed.set()
            return "cancelled cooperatively"
        side_effects.append(value)
        return "effect complete"

    registry = ToolRegistry()
    registry.register_tool(bind_tool_cancellation(cooperative_effect))
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
    deadline = asyncio.get_running_loop().time() + 2.0
    while not any(isinstance(event, events.ToolStarted) for event in published):
        assert asyncio.get_running_loop().time() < deadline
        await asyncio.sleep(0.01)

    invocation.cancel()
    with pytest.raises(asyncio.CancelledError):
        await invocation

    assert await asyncio.to_thread(cancellation_observed.wait, 1.0)
    assert side_effects == []
    terminal = [
        event
        for event in published
        if getattr(event, "tool_run_id", None) == "cooperative-call-1"
        and isinstance(event, (events.ToolCancelled, events.ToolCompleted, events.ToolFailed))
    ]
    assert len(terminal) == 1
    assert isinstance(terminal[0], events.ToolCancelled)
    assert terminal[0].reason == "turn cancelled"
