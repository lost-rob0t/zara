"""LangGraph-based conversation flow."""

from __future__ import annotations

import asyncio
import hashlib
import logging
import uuid
from typing import Annotated, Any, Dict, List, Literal, Optional, TypedDict

from langchain_core.messages import AIMessage, BaseMessage, SystemMessage, ToolMessage
from langchain_core.runnables import RunnableConfig
from langgraph.checkpoint.memory import InMemorySaver
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode
from langgraph.types import Command, interrupt

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events

from .approval import (
    ApprovalRequest,
    MAX_TOOL_RUN_ID_LENGTH,
    ToolApprovalController,
    valid_tool_name,
)
from . import stream_events
from .sentence_chunker import SentenceChunker
from .tool_cancellation import tool_cancellation_scope

logger = logging.getLogger(__name__)


def _content_text(content: Any) -> str:
    """Extract user-facing text from a message content payload.

    Provider content-block lists (Anthropic) may carry hidden reasoning or
    tool protocol blocks; only text blocks are surfaced.
    """
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict) and block.get("type") == "text":
                text = block.get("text")
                if isinstance(text, str):
                    parts.append(text)
        return "".join(parts)
    return ""

# ----------------------------------------------------------------------
# LangGraph message reducer (critical)

try:
    # LangGraph >= 0.2 style
    from langgraph.graph.message import add_messages  # type: ignore
except Exception:  # pragma: no cover
    try:
        # Some versions export it here
        from langgraph.graph import add_messages  # type: ignore
    except Exception:  # pragma: no cover
        add_messages = None  # type: ignore


# ----------------------------------------------------------------------
# State schema

class AgentState(TypedDict, total=False):
    messages: Annotated[List[BaseMessage], add_messages]  # type: ignore
    conversation_id: str
    turn_id: str
    tool_decisions: Dict[str, Dict[str, str]]


# ----------------------------------------------------------------------
# Conversation graph construction


def _tool_call_name(tool_call: Any) -> Optional[str]:
    if isinstance(tool_call, dict):
        name = tool_call.get("name")
    else:
        name = getattr(tool_call, "name", None)
    return name if isinstance(name, str) else None


def _tool_call_id(tool_call: Any) -> Optional[str]:
    if isinstance(tool_call, dict):
        tool_id = tool_call.get("id")
    else:
        tool_id = getattr(tool_call, "id", None)
    return tool_id if isinstance(tool_id, str) else None


def _tool_call_args(tool_call: Any) -> Dict[str, Any]:
    if isinstance(tool_call, dict):
        args = tool_call.get("args", {})
    else:
        args = getattr(tool_call, "args", {})
    return args if isinstance(args, dict) else {}


def _safe_tool_label(name: str) -> str:
    return name if len(name) <= 64 else f"{name[:61]}..."


def _make_run_id(turn_id: Optional[str], tool_call_id: str) -> str:
    if turn_id:
        return f"{turn_id}:{tool_call_id}"
    return tool_call_id


def _approval_controller(tool_registry, publisher):
    return ToolApprovalController(
        registry=tool_registry,
        publisher=publisher,
    )


def _build_approval_request(
    controller: ToolApprovalController,
    *,
    tool_name: str,
    tool_run_id: str,
    args: Dict[str, Any],
    turn_id: Optional[str],
    conversation_id: Optional[str],
) -> ApprovalRequest:
    return controller.build_request(
        tool_name=tool_name,
        tool_run_id=tool_run_id,
        args=args,
        turn_id=turn_id,
        conversation_id=conversation_id,
    )


def _valid_resolution(
    resolution: Any,
    *,
    expected_tool_name: str,
    expected_tool_run_id: str,
) -> bool:
    if not isinstance(resolution, dict):
        return False
    tool_name = resolution.get("tool_name")
    tool_run_id = resolution.get("tool_run_id")
    decision = resolution.get("decision")
    if tool_name is not None and tool_name != expected_tool_name:
        return False
    if tool_run_id is not None and tool_run_id != expected_tool_run_id:
        return False
    return decision in {"approve", "reject"}


def create_approval_node(tool_registry, publisher=None):
    publish = publisher or runtime_bridge.publish
    controller = _approval_controller(tool_registry, publish)

    async def approval_node(state: Dict[str, Any], config: RunnableConfig):
        messages = state.get("messages", [])
        if not messages or not isinstance(messages[-1], AIMessage):
            return {"tool_decisions": {}}

        decisions: Dict[str, Dict[str, str]] = {}
        turn_id = state.get("turn_id")
        conversation_id = state.get("conversation_id")
        for tool_call in messages[-1].tool_calls:
            tool_name = _tool_call_name(tool_call)
            tool_run_id = _tool_call_id(tool_call)
            if not tool_name or not tool_run_id:
                raise ValueError("tool call metadata is invalid")
            if not tool_registry.requires_approval(tool_name):
                decisions[tool_run_id] = {"decision": "approve"}
                continue
            request = _build_approval_request(
                controller,
                tool_name=tool_name,
                tool_run_id=tool_run_id,
                args=_tool_call_args(tool_call),
                turn_id=turn_id,
                conversation_id=conversation_id,
            )
            resolution = interrupt(request.to_payload())
            if not _valid_resolution(
                resolution,
                expected_tool_name=tool_name,
                expected_tool_run_id=tool_run_id,
            ):
                raise ValueError("tool approval decision is invalid")
            decisions[str(tool_run_id)] = {
                "decision": resolution["decision"],
            }
        return {"tool_decisions": decisions}

    return approval_node


def create_tools_node(tool_registry, publisher=None, stream_publisher=None):
    tools = tool_registry.to_langchain_tools()
    tool_node = ToolNode(tools)
    bindings = {tool.name: tool for tool in tools}
    publish = publisher or runtime_bridge.publish

    def publish_tool_result(tool_name: str, tool_run_id: str) -> None:
        if stream_publisher is not None:
            stream_publisher(
                stream_events.ToolResult(name=tool_name, id=tool_run_id)
            )

    async def invoke_tool_node(single_call: AIMessage, config: RunnableConfig):
        with tool_cancellation_scope() as cancellation_signal:
            invocation_config = cancellation_signal.invocation_config(config)
            execution = asyncio.create_task(
                tool_node.ainvoke(
                    {"messages": [single_call]},
                    invocation_config,
                )
            )
            try:
                return await asyncio.shield(execution)
            except asyncio.CancelledError:
                cancellation_signal.cancel()
                execution.cancel()
                try:
                    await execution
                except asyncio.CancelledError:
                    pass
                raise

    async def gated_tools_node(
        state: Dict[str, Any],
        config: RunnableConfig,
    ) -> Dict[str, Any]:
        messages = state.get("messages", [])
        if not messages or not isinstance(messages[-1], AIMessage):
            raise ValueError("tool executor requires a pending assistant tool call")

        decisions = state.get("tool_decisions", {})
        results: List[ToolMessage] = []
        turn_id = state.get("turn_id")
        conversation_id = state.get("conversation_id")
        for tool_call in messages[-1].tool_calls:
            tool_name = _tool_call_name(tool_call)
            tool_run_id = _tool_call_id(tool_call)
            if not tool_name or not tool_run_id:
                raise ValueError("tool call metadata is invalid")

            if tool_registry.requires_approval(tool_name):
                decision = decisions.get(tool_run_id, {}).get("decision")
                if decision != "approve":
                    results.append(
                        ToolMessage(
                            content=f"Tool {tool_name} was not approved.",
                            name=tool_name,
                            tool_call_id=tool_run_id,
                            status="error",
                        )
                    )
                    publish_tool_result(tool_name, tool_run_id)
                    continue
            else:
                publish(
                    events.ToolQueued(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                    )
                )

            if tool_registry.get_tool(tool_name) is not bindings.get(tool_name):
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason="tool is no longer available",
                    )
                )
                results.append(
                    ToolMessage(
                        content=f"Tool {tool_name} is no longer available.",
                        name=tool_name,
                        tool_call_id=tool_run_id,
                        status="error",
                    )
                )
                publish_tool_result(tool_name, tool_run_id)
                continue

            publish(
                events.ToolStarted(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    label=tool_name,
                    tool_run_id=tool_run_id,
                    tool_name=tool_name,
                )
            )
            single_call = AIMessage(content="", tool_calls=[tool_call])
            try:
                output = await invoke_tool_node(single_call, config)
            except asyncio.CancelledError:
                publish(
                    events.ToolCancelled(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason="turn cancelled",
                    )
                )
                raise
            except Exception as error:
                reason = str(error) or type(error).__name__
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason=reason,
                    )
                )
                results.append(
                    ToolMessage(
                        content=f"Tool {tool_name} failed: {reason}",
                        name=tool_name,
                        tool_call_id=tool_run_id,
                        status="error",
                    )
                )
                publish_tool_result(tool_name, tool_run_id)
                continue

            output_messages = output.get("messages", []) if isinstance(output, dict) else []
            if not output_messages:
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason="tool returned no result",
                    )
                )
                results.append(
                    ToolMessage(
                        content=f"Tool {tool_name} returned no result.",
                        name=tool_name,
                        tool_call_id=tool_run_id,
                        status="error",
                    )
                )
                publish_tool_result(tool_name, tool_run_id)
                continue

            tool_message = output_messages[-1]
            results.append(tool_message)
            if getattr(tool_message, "status", None) == "error":
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason=str(tool_message.content),
                    )
                )
            else:
                publish(
                    events.ToolCompleted(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                    )
                )
            publish_tool_result(tool_name, tool_run_id)

        return {"messages": results}

    return gated_tools_node


def create_agent_graph(
    *,
    model,
    tool_registry,
    publisher=None,
    stream_publisher=None,
    checkpointer=None,
):
    """Create Zara's canonical agent graph."""
    graph = StateGraph(AgentState)
    graph.add_node("approval", create_approval_node(tool_registry, publisher=publisher))
    graph.add_node(
        "tools",
        create_tools_node(
            tool_registry,
            publisher=publisher,
            stream_publisher=stream_publisher,
        ),
    )

    async def call_model(state: Dict[str, Any], config: RunnableConfig):
        messages = state.get("messages", [])
        tools = tool_registry.to_langchain_tools()
        bound = model.bind_tools(tools) if tools else model
        response = await bound.ainvoke(messages, config)
        return {"messages": [response]}

    def route_after_model(state: Dict[str, Any]) -> Literal["approval", "tools", "__end__"]:
        messages = state.get("messages", [])
        if not messages:
            return END
        message = messages[-1]
        if not isinstance(message, AIMessage) or not message.tool_calls:
            return END
        for tool_call in message.tool_calls:
            name = _tool_call_name(tool_call)
            if name and tool_registry.requires_approval(name):
                return "approval"
        return "tools"

    def route_after_approval(state: Dict[str, Any]) -> Literal["tools", "__end__"]:
        messages = state.get("messages", [])
        if not messages:
            return END
        message = messages[-1]
        if not isinstance(message, AIMessage) or not message.tool_calls:
            return END
        return "tools"

    def route_after_tools(state: Dict[str, Any]) -> Literal["model", "__end__"]:
        messages = state.get("messages", [])
        if not messages:
            return END
        return "model"

    graph.set_entry_point("model")
    graph.add_node("model", call_model)
    graph.add_edge("model", "approval")
    graph.add_conditional_edges(
        "model",
        route_after_model,
        {"approval": "approval", "tools": "tools", END: END},
    )
    graph.add_conditional_edges(
        "approval",
        route_after_approval,
        {"tools": "tools", END: END},
    )
    graph.add_conditional_edges(
        "tools",
        route_after_tools,
        {"model": "model", END: END},
    )
    return graph.compile(checkpointer=checkpointer or InMemorySaver())
