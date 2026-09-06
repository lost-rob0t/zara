from __future__ import annotations

import asyncio

import pytest
from langchain_core.messages import AIMessage, ToolMessage
from langchain_core.runnables import RunnableConfig
from langchain_core.tools import BaseTool, tool
from langgraph.graph import MessagesState, StateGraph
from langgraph.prebuilt import ToolNode
from pydantic import PrivateAttr


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

    graph = StateGraph(MessagesState)
    graph.add_node("tools", ToolNode([dispatch_probe]))
    graph.set_entry_point("tools")
    graph.set_finish_point("tools")
    app = graph.compile()

    output = await app.ainvoke(
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
    assert isinstance(messages[-1], ToolMessage)
    assert messages[-1].content == "ok"


@pytest.mark.asyncio
async def test_toolnode_whole_tool_call_survives_public_forwarding_wrapper():
    """Characterize the exact r15 forwarding shape without cancellation state."""
    entered = False
    wrapper_inputs = []

    @tool("forward_probe")
    def forward_probe(value: str) -> str:
        """Prove whole ToolCall forwarding reaches the original sync body."""
        nonlocal entered
        entered = True
        return value

    class ForwardingTool(BaseTool):
        _inner: BaseTool = PrivateAttr()

        def __init__(self, inner: BaseTool) -> None:
            super().__init__(
                name=inner.name,
                description=inner.description,
                args_schema=inner.args_schema,
                return_direct=inner.return_direct,
                response_format=inner.response_format,
                metadata=inner.metadata,
                tags=inner.tags,
            )
            self._inner = inner

        @property
        def tool_call_schema(self):
            return self._inner.tool_call_schema

        async def ainvoke(
            self,
            input,
            config: RunnableConfig | None = None,
            **kwargs,
        ):
            wrapper_inputs.append(input)
            return await asyncio.to_thread(
                self._inner.invoke,
                input,
                config,
                **kwargs,
            )

        def _run(self, *args, **kwargs):
            raise RuntimeError("forwarding wrapper must execute through ainvoke()")

    wrapped = ForwardingTool(forward_probe)
    graph = StateGraph(MessagesState)
    graph.add_node("tools", ToolNode([wrapped]))
    graph.set_entry_point("tools")
    graph.set_finish_point("tools")
    app = graph.compile()

    output = await app.ainvoke(
        {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {
                            "name": "forward_probe",
                            "args": {"value": "ok"},
                            "id": "forward-probe-1",
                            "type": "tool_call",
                        }
                    ],
                )
            ]
        }
    )

    assert entered is True
    assert len(wrapper_inputs) == 1
    assert wrapper_inputs[0]["name"] == "forward_probe"
    assert wrapper_inputs[0]["args"] == {"value": "ok"}
    assert wrapper_inputs[0]["id"] == "forward-probe-1"
    messages = output["messages"]
    assert isinstance(messages[-1], ToolMessage)
    assert messages[-1].content == "ok"
