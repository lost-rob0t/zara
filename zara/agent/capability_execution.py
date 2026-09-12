"""Canonical direct tool execution used by service-plugin composition."""

from __future__ import annotations

import asyncio
import json
import uuid
from typing import Any, Mapping

from langchain_core.messages import AIMessage, ToolMessage

from .approval import ApprovalRequest
from .graph import create_tools_node

_MAX_RESULT_BYTES = 262144


def _result(
    *,
    success: bool,
    value: Any = None,
    error: str = "",
    cancelled: bool = False,
    tool_run_id: str,
) -> dict[str, Any]:
    return {
        "success": success,
        "value": value,
        "error": error,
        "cancelled": cancelled,
        "tool_run_id": tool_run_id,
    }


def _structured_content(content: Any) -> tuple[bool, Any]:
    value = {"content": content}
    try:
        encoded = json.dumps(
            value,
            ensure_ascii=False,
            separators=(",", ":"),
            sort_keys=True,
        ).encode("utf-8")
    except (TypeError, ValueError):
        return False, None
    if len(encoded) > _MAX_RESULT_BYTES:
        return False, None
    return True, value


async def invoke_plugin_capability(
    manager,
    tool_name: str,
    arguments: Mapping[str, Any],
    *,
    source_plugin: str,
    timeout: float,
) -> dict[str, Any]:
    """Invoke one current registered tool through Zara's approval/tool node boundary."""

    tool = manager.tool_registry.get_tool(tool_name)
    if tool is None:
        raise RuntimeError("plugin capability is unavailable")

    tool_run_id = f"plugin-cap-{uuid.uuid4().hex}"
    turn_id = f"plugin-cap-turn-{uuid.uuid4().hex}"
    decisions: dict[str, dict[str, str]] = {}

    async def execute() -> dict[str, Any]:
        if manager.tool_registry.requires_approval(tool_name):
            resolution = await manager.approval_controller.wait_for_decision(
                ApprovalRequest(
                    tool_run_id=tool_run_id,
                    tool_name=tool_name,
                    turn_id=turn_id,
                    conversation_id=None,
                )
            )
            if resolution.decision == "cancel":
                return _result(
                    success=False,
                    error="capability invocation cancelled",
                    cancelled=True,
                    tool_run_id=tool_run_id,
                )
            if resolution.decision != "approve":
                return _result(
                    success=False,
                    error="capability invocation rejected",
                    tool_run_id=tool_run_id,
                )
            decisions[tool_run_id] = {"decision": "approve"}

        node = create_tools_node(
            manager.tool_registry,
            publisher=manager.approval_controller.publisher,
        )
        state = {
            "messages": [
                AIMessage(
                    content="",
                    tool_calls=[
                        {
                            "name": tool_name,
                            "args": dict(arguments),
                            "id": tool_run_id,
                            "type": "tool_call",
                        }
                    ],
                )
            ],
            "turn_id": turn_id,
            "conversation_id": None,
            "tool_decisions": decisions,
        }
        output = await node(state, {})
        messages = output.get("messages", [])
        if len(messages) != 1 or not isinstance(messages[0], ToolMessage):
            raise RuntimeError("plugin capability returned an invalid tool result")
        message = messages[0]
        if getattr(message, "status", "success") == "error":
            return _result(
                success=False,
                error="capability tool execution failed",
                tool_run_id=tool_run_id,
            )
        structured, value = _structured_content(message.content)
        if not structured:
            return _result(
                success=False,
                error="capability result is not bounded structured data",
                tool_run_id=tool_run_id,
            )
        return _result(
            success=True,
            value=value,
            tool_run_id=tool_run_id,
        )

    try:
        return await asyncio.wait_for(execute(), timeout=timeout)
    except TimeoutError:
        return _result(
            success=False,
            error="capability invocation timed out",
            cancelled=True,
            tool_run_id=tool_run_id,
        )
