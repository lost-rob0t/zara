from __future__ import annotations

import pytest
from langchain_core.tools import StructuredTool

from zara.agent.tools.registry import ToolRegistry


def _tool(name: str, calls: list[str]):
    def invoke(value: str) -> str:
        calls.append(value)
        return value

    return StructuredTool.from_function(
        invoke,
        name=name,
        description="live approval policy regression tool",
        metadata={"zara_requires_approval": False},
    )


def test_composed_invocation_rechecks_live_approval_metadata_before_execution():
    calls: list[str] = []
    registry = ToolRegistry()
    tool = _tool("plugin_mutate", calls)
    registry.register_tool(tool)

    assert registry.requires_approval("plugin_mutate") is False
    tool.metadata["zara_requires_approval"] = True

    assert registry.requires_approval("plugin_mutate") is True
    with pytest.raises(PermissionError, match="requires canonical interactive approval"):
        registry.invoke_composed_tool("plugin_mutate", {"value": "danger"})
    assert calls == []


def test_malformed_live_approval_metadata_fails_closed_before_execution():
    calls: list[str] = []
    registry = ToolRegistry()
    tool = _tool("plugin_mutate", calls)
    registry.register_tool(tool)

    tool.metadata["zara_requires_approval"] = "yes"

    with pytest.raises(ValueError, match="must be true or false"):
        registry.invoke_composed_tool("plugin_mutate", {"value": "danger"})
    assert calls == []
