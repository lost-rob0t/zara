"""LangGraph-based conversation flow."""

from __future__ import annotations

import logging
from typing import Any, Dict, List, Literal, Optional, TypedDict, Annotated

from langchain_core.messages import AIMessage, BaseMessage, ToolMessage
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode

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

    # Metadata / loop control
    step_count: int
    max_steps: int

    # Optional extras (kept for compatibility with existing callers)
    user_input: str
    response: Optional[str]
    tool_calls: List[Dict[str, Any]]
    tool_results: List[Dict[str, Any]]


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

        start_time = time.time()
        response = await llm_with_tools.ainvoke(msgs)
        elapsed = time.time() - start_time

        logger.info("[AgentNode] LLM response time: %.2f seconds", elapsed)
        logger.info("[AgentNode] LLM response type=%s", type(response).__name__)
        if getattr(response, "tool_calls", None):
            ids = [_tool_call_id(tc) for tc in response.tool_calls]  # type: ignore[attr-defined]
            logger.info("[AgentNode] tool_calls=%s", [i for i in ids if i])

        # With add_messages reducer, this APPENDS.
        step_count = int(state.get("step_count", 0)) + 1
        return {"messages": [response], "step_count": step_count}

    return agent_node


def create_tools_node(tool_registry):
    tools = tool_registry.to_langchain_tools()
    return ToolNode(tools)


def should_continue(state: Dict[str, Any]) -> Literal["tools", "end"]:
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
        return "tools"

    return "end"


# ----------------------------------------------------------------------
# Graph + runner

def create_agent_graph(llm_client, tool_registry):
    agent_node = create_agent_node(llm_client, tool_registry)
    tools_node = create_tools_node(tool_registry)

    workflow = StateGraph(AgentState)

    workflow.add_node("agent", agent_node)
    workflow.add_node("tools", tools_node)

    workflow.set_entry_point("agent")

    workflow.add_conditional_edges(
        "agent",
        should_continue,
        {
            "tools": "tools",
            "end": END,
        },
    )

    workflow.add_edge("tools", "agent")

    return workflow.compile()


async def run_conversation_loop(llm_client, tool_registry, state: Dict[str, Any]) -> Dict[str, Any]:
    """
    Execute the graph until a final answer is produced or max_steps is hit.
    """
    graph = create_agent_graph(llm_client, tool_registry)
    result: Dict[str, Any] = await graph.ainvoke(state)

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
                    "success": "Error" not in (msg.content or ""),
                    "result": msg.content,
                }
            )
    result["tool_results"] = tool_results
    return result
