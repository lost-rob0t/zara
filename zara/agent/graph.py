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

logger = logging.getLogger(__name__)

# ----------------------------------------------------------------------
# LangGraph message reducer (critical)

try:
    # LangGraph >= 0.2 style
    from langgraph.graph.message import add_messages  # type: ignore
except Exception:  # pragma: no cover
    try:
        # Some versions export it here
        from langgraph.graph import add_messages  # type: ignore
    except Exception as e:  # pragma: no cover
        raise ImportError(
            "Could not import langgraph add_messages reducer. "
            "Your langgraph version is missing message reducers."
        ) from e


class AgentState(TypedDict, total=False):
    """Runtime state passed through the LangGraph workflow."""

    # Conversation context
    messages: Annotated[List[BaseMessage], add_messages]
    turn_id: str
    conversation_id: Optional[str]

    # Metadata / loop control
    step_count: int
    max_steps: int

    # Optional extras (kept for compatibility with existing callers)
    user_input: str
    response: Optional[str]
    tool_calls: List[Dict[str, Any]]
    tool_results: List[Dict[str, Any]]
    tool_decisions: Dict[str, Dict[str, str]]
    latency_trace: Any


# ----------------------------------------------------------------------
# Helpers

def _tool_call_id(tool_call: Any) -> Optional[str]:
    """
    Tool call ids vary by provider / langchain version:
    - dict: {"id": "..."}
    - object: .id
    """
    if tool_call is None:
        return None
    if isinstance(tool_call, dict):
        return tool_call.get("id")
    return getattr(tool_call, "id", None)


def _tool_call_name(tool_call: Any) -> Optional[str]:
    if tool_call is None:
        return None
    if isinstance(tool_call, dict):
        return tool_call.get("name")
    return getattr(tool_call, "name", None)


def validate_and_clean_messages(messages: List[BaseMessage]) -> List[BaseMessage]:
    """Preserve valid contiguous tool-result groups and drop invalid results."""
    cleaned: List[BaseMessage] = []
    index = 0
    while index < len(messages):
        message = messages[index]
        if isinstance(message, ToolMessage):
            logger.warning(
                "[ValidateMessages] Dropping orphan ToolMessage at index %d with id=%s",
                index,
                getattr(message, "tool_call_id", None),
            )
            index += 1
            continue

        cleaned.append(message)
        index += 1
        if not isinstance(message, AIMessage) or not getattr(message, "tool_calls", None):
            continue

        call_ids = [
            tool_id
            for tool_id in (_tool_call_id(call) for call in message.tool_calls)
            if tool_id is not None
        ]
        results: Dict[str, ToolMessage] = {}
        while index < len(messages) and isinstance(messages[index], ToolMessage):
            result = messages[index]
            tool_id = getattr(result, "tool_call_id", None)
            if not tool_id or tool_id not in call_ids:
                logger.warning(
                    "[ValidateMessages] Dropping unknown ToolMessage at index %d "
                    "with id=%s; expected=%s",
                    index,
                    tool_id,
                    call_ids,
                )
            elif tool_id in results:
                logger.warning(
                    "[ValidateMessages] Dropping duplicate ToolMessage at index %d "
                    "with id=%s",
                    index,
                    tool_id,
                )
            else:
                results[tool_id] = result
            index += 1

        cleaned.extend(results[tool_id] for tool_id in call_ids if tool_id in results)

    return cleaned


# ----------------------------------------------------------------------
# Nodes

def create_agent_node(llm_client, tool_registry):
    tools = tool_registry.to_langchain_tools()
    llm_with_tools = llm_client.bind_tools(tools) if tools else llm_client

    async def agent_node(state: Dict[str, Any]) -> Dict[str, Any]:
        import time

        turn_id = state.get("turn_id")
        conversation_id = state.get("conversation_id")
        runtime_bridge.model_started(
            label="llm",
            turn_id=turn_id,
            conversation_id=conversation_id,
        )

        msgs = state.get("messages", [])
        assert isinstance(msgs, list), "state['messages'] must be a list"

        # NOTE: history is cleaned in AgentManager; we keep logging here light.
        logger.info("[AgentNode] Calling LLM with %d messages", len(msgs))
        logger.info(
            "[AgentNode] Message types=%s",
            [type(m).__name__ for m in msgs[-6:]],
        )
        logger.info(
            "[AgentNode] Last message preview=%r",
            getattr(msgs[-1], "content", None),
        )

        trace = state.get("latency_trace")
        request_index = int(state.get("step_count", 0))
        if trace is not None:
            trace.record(
                "llm_request",
                provider=type(llm_client).__name__,
                request_index=request_index,
            )
        start_time = time.monotonic()
        try:
            response = await llm_with_tools.ainvoke(msgs)
        except Exception as error:
            runtime_bridge.model_failed(
                reason=str(error),
                label="llm",
                turn_id=turn_id,
                conversation_id=conversation_id,
            )
            raise
        elapsed = time.monotonic() - start_time

        if trace is not None:
            # ainvoke() buffers a complete AIMessage. Record the observable
            # completion boundary as an explicit buffered first-token proxy.
            trace.record(
                "llm_first_token",
                provider=type(llm_client).__name__,
                request_index=request_index,
                buffered_proxy=True,
            )
            trace.record(
                "llm_final_token",
                provider=type(llm_client).__name__,
                request_index=request_index,
            )
            trace.flush()

        logger.info("[AgentNode] LLM response time: %.2f seconds", elapsed)
        logger.info("[AgentNode] LLM response type=%s", type(response).__name__)
        if getattr(response, "tool_calls", None):
            ids = [_tool_call_id(tc) for tc in response.tool_calls]  # type: ignore[attr-defined]
            logger.info("[AgentNode] tool_calls=%s", [i for i in ids if i])

        # With add_messages reducer, this APPENDS.
        step_count = int(state.get("step_count", 0)) + 1
        runtime_bridge.model_completed(
            success=True,
            label="llm",
            turn_id=turn_id,
            conversation_id=conversation_id,
        )
        return {"messages": [response], "step_count": step_count}

    return agent_node


def create_approval_node(tool_registry):
    def approval_node(state: Dict[str, Any]) -> Dict[str, Any]:
        messages = state.get("messages", [])
        if not messages or not isinstance(messages[-1], AIMessage):
            raise ValueError("approval gate requires a pending assistant tool call")

        tool_calls = messages[-1].tool_calls
        tool_run_ids = [_tool_call_id(tool_call) for tool_call in tool_calls]
        tool_names = [_tool_call_name(tool_call) for tool_call in tool_calls]
        if (
            any(
                not tool_run_id or len(tool_run_id) > MAX_TOOL_RUN_ID_LENGTH
                for tool_run_id in tool_run_ids
            )
            or len(set(tool_run_ids)) != len(tool_run_ids)
            or any(not valid_tool_name(tool_name) for tool_name in tool_names)
        ):
            raise ValueError("tool call metadata is invalid")

        decisions: Dict[str, Dict[str, str]] = {}
        for tool_call in tool_calls:
            tool_name = _tool_call_name(tool_call)
            if not tool_name or not tool_registry.requires_approval(tool_name):
                continue
            tool_run_id = _tool_call_id(tool_call)
            resolution = interrupt(
                {
                    "tool_run_id": tool_run_id,
                    "tool_name": tool_name,
                }
            )
            if not isinstance(resolution, dict):
                raise ValueError("tool approval decision is invalid")
            decision = resolution.get("decision")
            if decision not in {"approve", "reject"}:
                raise ValueError("tool approval decision is invalid")
            decisions[str(tool_run_id)] = {
                "decision": decision,
            }
        return {"tool_decisions": decisions}

    return approval_node


def create_tools_node(tool_registry, publisher=None):
    tools = tool_registry.to_langchain_tools()
    tool_node = ToolNode(tools)
    bindings = {tool.name: tool for tool in tools}
    publish = publisher or runtime_bridge.publish

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
                output = await tool_node.ainvoke(
                    {"messages": [single_call]},
                    config,
                )
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
            except Exception:
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason="tool execution failed",
                    )
                )
                raise RuntimeError(f"tool {tool_name} execution failed") from None

            tool_messages = output.get("messages", [])
            if len(tool_messages) != 1 or not isinstance(tool_messages[0], ToolMessage):
                raise RuntimeError("tool executor returned an invalid result")
            result = tool_messages[0]
            results.append(result)
            if getattr(result, "status", "success") == "error":
                publish(
                    events.ToolFailed(
                        turn_id=turn_id,
                        conversation_id=conversation_id,
                        label=tool_name,
                        tool_run_id=tool_run_id,
                        tool_name=tool_name,
                        reason="tool execution failed",
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
                        success=True,
                    )
                )
        return {"messages": results, "tool_decisions": {}}

    return gated_tools_node


def should_continue(state: Dict[str, Any]) -> Literal["approval", "end"]:
    msgs = state.get("messages", [])
    assert isinstance(msgs, list) and msgs, "state['messages'] must be a non-empty list"
    last = msgs[-1]

    step_count = int(state.get("step_count", 0))
    max_steps = int(state.get("max_steps", 10))

    # Hard stop to avoid infinite tool loops
    if step_count >= max_steps:
        logger.warning("[Routing] max_steps reached (%d >= %d); ending turn", step_count, max_steps)
        return "end"

    if isinstance(last, AIMessage) and getattr(last, "tool_calls", None):
        return "approval"

    return "end"


# ----------------------------------------------------------------------
# Graph + runner

def create_agent_graph(llm_client, tool_registry, *, checkpointer=None, publisher=None):
    agent_node = create_agent_node(llm_client, tool_registry)
    approval_node = create_approval_node(tool_registry)
    tools_node = create_tools_node(tool_registry, publisher)

    workflow = StateGraph(AgentState)

    workflow.add_node("agent", agent_node)
    workflow.add_node("approval", approval_node)
    workflow.add_node("tools", tools_node)

    workflow.set_entry_point("agent")

    workflow.add_conditional_edges(
        "agent",
        should_continue,
        {
            "approval": "approval",
            "end": END,
        },
    )

    workflow.add_edge("approval", "tools")
    workflow.add_edge("tools", "agent")

    return workflow.compile(checkpointer=checkpointer)


async def run_conversation_loop(
    llm_client,
    tool_registry,
    state: Dict[str, Any],
    *,
    approval_controller: Optional[ToolApprovalController] = None,
    publisher=None,
    principal_id: str = "local",
) -> Dict[str, Any]:
    """
    Execute the graph until a final answer is produced or max_steps is hit.

    Dynamic capability providers are prepared before the graph is compiled so
    their tools are bound exactly like native Zara tools for this turn.
    """
    prepare = getattr(tool_registry, "prepare_async", None)
    if prepare is not None:
        await prepare()

    dynamic_context = getattr(tool_registry, "dynamic_system_context", lambda: None)()
    if dynamic_context:
        messages = list(state.get("messages", []))
        messages = [
            message
            for message in messages
            if getattr(message, "id", None) != "zara-dynamic-capabilities"
        ]
        context_message = SystemMessage(
            content=dynamic_context,
            id="zara-dynamic-capabilities",
        )
        insert_at = max(0, len(messages) - 1)
        messages.insert(insert_at, context_message)
        state = dict(state)
        state["messages"] = messages

    saver = InMemorySaver()
    graph = create_agent_graph(
        llm_client,
        tool_registry,
        checkpointer=saver,
        publisher=publisher,
    )
    turn_id = str(state.get("turn_id") or f"agent-{uuid.uuid4().hex}")
    conversation_id = state.get("conversation_id")
    scope = hashlib.blake2s(principal_id.encode("utf-8"), digest_size=10).hexdigest()
    checkpoint_thread_id = f"zara:{scope}:{turn_id}"
    config = {"configurable": {"thread_id": checkpoint_thread_id}}

    try:
        result: Dict[str, Any] = await graph.ainvoke(state, config)
        while result.get("__interrupt__"):
            if approval_controller is None:
                raise RuntimeError("tool approval controller is unavailable")
            interrupts = result["__interrupt__"]
            if len(interrupts) != 1 or not isinstance(interrupts[0].value, dict):
                raise RuntimeError("tool approval interrupt is invalid")
            value = interrupts[0].value
            request = ApprovalRequest(
                tool_run_id=str(value.get("tool_run_id") or ""),
                tool_name=str(value.get("tool_name") or ""),
                turn_id=turn_id,
                conversation_id=conversation_id,
            )
            resolution = await approval_controller.wait_for_decision(request)
            if resolution.decision == "cancel":
                raise asyncio.CancelledError
            result = await graph.ainvoke(
                Command(resume={"decision": resolution.decision}),
                config,
            )
    finally:
        await saver.adelete_thread(checkpoint_thread_id)

    messages = result.get("messages", [])
    if not messages:
        result["response"] = "I'm not sure how to respond to that."
        result["tool_results"] = []
        return result

    last = messages[-1]

    # Prefer a final AIMessage without tool_calls
    if isinstance(last, AIMessage) and not getattr(last, "tool_calls", None):
        result["response"] = last.content
    elif isinstance(last, AIMessage):
        # We ended due to max_steps or some interruption
        result["response"] = last.content or "I got stuck in a tool loop and had to stop."
    else:
        result["response"] = "I'm not sure how to respond to that."

    tool_results = []
    for msg in messages:
        if isinstance(msg, ToolMessage):
            tool_results.append(
                {
                    "tool": getattr(msg, "name", "unknown"),
                    "success": getattr(msg, "status", "success") != "error",
                    "result": msg.content,
                }
            )
    result["tool_results"] = tool_results
    return result
