from __future__ import annotations

from importlib.metadata import version
from pathlib import Path
import sys

import pytest

pytest.importorskip("mcp")
if int(version("mcp").split(".", 1)[0]) < 2:
    pytest.skip("MCP SDK v2 is required", allow_module_level=True)
pytest.importorskip("langchain_core")

from langchain_core.messages import AIMessage, HumanMessage, ToolMessage

from zara.agent.graph import run_conversation_loop
from zara.agent.tools.registry import ToolRegistry


FIXTURE = Path(__file__).parent / "fixtures" / "mcp_test_server.py"


class FakeConfig:
    def __init__(self, root: Path):
        self.config_dir = root
        self._mcp = {
            "servers": {
                "fixture": {
                    "transport": "stdio",
                    "command": sys.executable,
                    "args": [str(FIXTURE)],
                    "connect_timeout": 5.0,
                    "request_timeout": 2.0,
                }
            }
        }

    def get_section(self, name: str):
        return self._mcp if name == "mcp" else {}


class RoutingFakeLLM:
    """Deterministic LLM double proving the normal agent/tool path."""

    def __init__(self):
        self.bound_tools = []
        self.selected_tool = None

    def bind_tools(self, tools):
        self.bound_tools = list(tools)
        return self

    async def ainvoke(self, messages):
        last = messages[-1]
        if isinstance(last, ToolMessage):
            return AIMessage(content=f"MCP result received: {last.content}")

        user_messages = [message for message in messages if isinstance(message, HumanMessage)]
        assert user_messages
        assert "RETE" in str(user_messages[-1].content)
        tool = next(tool for tool in self.bound_tools if tool.name.startswith("mcp__fixture__echo"))
        self.selected_tool = tool.name
        return AIMessage(
            content="",
            tool_calls=[
                {
                    "name": tool.name,
                    "args": {"text": "RETE networks"},
                    "id": "call-mcp-1",
                    "type": "tool_call",
                }
            ],
        )


@pytest.mark.asyncio
async def test_natural_language_turn_selects_discovered_mcp_tool(tmp_path):
    registry = ToolRegistry(config=FakeConfig(tmp_path))
    llm = RoutingFakeLLM()
    state = {
        "messages": [HumanMessage(content="Zara search my documents for RETE networks")],
        "step_count": 0,
        "max_steps": 4,
        "tool_results": [],
    }

    try:
        result = await run_conversation_loop(llm, registry, state)
        assert llm.selected_tool is not None
        assert llm.selected_tool.startswith("mcp__fixture__echo")
        assert any(tool.name == llm.selected_tool for tool in llm.bound_tools)
        assert "RETE networks" in str(result["response"])
        assert result["tool_results"]
        assert result["tool_results"][0]["tool"] == llm.selected_tool
    finally:
        await registry.shutdown_async()
