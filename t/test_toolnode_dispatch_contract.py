from __future__ import annotations

import pytest
from langchain_core.messages import AIMessage, ToolMessage
from langchain_core.tools import tool
from langgraph.prebuilt import ToolNode


@pytest.mark.asyncio
async def test_toolnode_uses_registered_tool_public_ainvoke(monkeypatch):
    entered = False
    public_inputs = []

    @tool("dispatch_probe")
    def dispatch_probe(value: str) -> str:
        """Probe the exact ToolNode-to-BaseTool dispatch boundary."""
        nonlocal entered
        entered = True
        return value

    tool_type = type(dispatch_probe)
    original_ainvoke = tool_type.ainvoke

    async def traced_ainvoke(self, input, config=None, **kwargs):
        if self is dispatch_probe:
            public_inputs.append(input)
        return await original_ainvoke(self, input, config=config, **kwargs)

    monkeypatch.setattr(tool_type, "ainvoke", traced_ainvoke)

    node = ToolNode([dispatch_probe])
    output = await node.ainvoke(
        {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {
                            "name": "dispatch_probe",
                            "args": {"value": "ok"},
                            "id": "dispatch-probe-1",
                            "type": "tool_call",
                        }
                    ],
                )
            ]
        }
    )

    assert entered is True
    assert len(public_inputs) == 1
    assert isinstance(public_inputs[0], dict)
    assert public_inputs[0]["name"] == "dispatch_probe"
    assert public_inputs[0]["args"] == {"value": "ok"}
    assert public_inputs[0]["id"] == "dispatch-probe-1"

    messages = output["messages"]
    assert len(messages) == 1
    assert isinstance(messages[0], ToolMessage)
    assert messages[0].content == "ok"
