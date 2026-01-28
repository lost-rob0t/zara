"""
LangGraph-based conversation flow.

Implements the conversation flow using LangGraph:
1. Agent node: LLM decides whether to respond or call tools
2. Tools node: Execute requested tools
3. Routing: Conditionally route between agent → tools → agent or agent → end
"""

from typing import Dict, Any, Literal, List
from langchain_core.messages import ToolMessage, AIMessage, BaseMessage
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode
import logging

logger = logging.getLogger(__name__)


def validate_and_clean_messages(messages: List[BaseMessage]) -> List[BaseMessage]:
    """
    Validate and clean message history to ensure it's compatible with Anthropic API.

    Anthropic requires:
    - Each ToolMessage must immediately follow an AIMessage with corresponding tool_calls
    - No orphaned ToolMessages without matching tool_use_id
    - Messages cannot start or end with orphaned ToolMessages

    This function removes orphaned ToolMessages and ensures proper message structure.

    Args:
        messages: List of conversation messages

    Returns:
        Cleaned list of messages
    """
    if not messages:
        return messages

    logger.info(f"[ValidateMessages] Starting validation of {len(messages)} messages")

    cleaned = []
    i = 0

    while i < len(messages):
        msg = messages[i]

        # If it's a ToolMessage, check if previous message is AIMessage with matching tool_calls
        if isinstance(msg, ToolMessage):
            # Check if there's a previous message and it's an AIMessage with tool_calls
            if (cleaned and
                isinstance(cleaned[-1], AIMessage) and
                hasattr(cleaned[-1], 'tool_calls') and
                cleaned[-1].tool_calls):

                # Check if this tool_call_id matches any tool_calls in the AIMessage
                tool_call_ids = [tc['id'] for tc in cleaned[-1].tool_calls]
                if hasattr(msg, 'tool_call_id') and msg.tool_call_id in tool_call_ids:
                    # Valid ToolMessage, keep it
                    cleaned.append(msg)
                    logger.info(f"[ValidateMessages] Keeping valid ToolMessage with id {msg.tool_call_id}")
                else:
                    # Orphaned ToolMessage, skip it
                    logger.warning(f"[ValidateMessages] Removing orphaned ToolMessage with non-matching id {getattr(msg, 'tool_call_id', 'unknown')}")
            else:
                # ToolMessage without preceding AIMessage with tool_calls, skip it
                logger.warning(f"[ValidateMessages] Removing orphaned ToolMessage (no preceding AIMessage with tool_calls)")
                if hasattr(msg, 'tool_call_id'):
                    logger.warning(f"[ValidateMessages]   - Orphaned tool_call_id: {msg.tool_call_id}")
        else:
            # Not a ToolMessage, keep it
            cleaned.append(msg)
            logger.info(f"[ValidateMessages] Keeping {type(msg).__name__} at position {i}")

        i += 1

    # Remove trailing ToolMessages and their AIMessage
    # These would become orphaned when we add a new HumanMessage
    num_trailing_tools = 0
    while cleaned and isinstance(cleaned[-1], ToolMessage):
        cleaned.pop()
        num_trailing_tools += 1

    if num_trailing_tools > 0:
        logger.warning(f"[ValidateMessages] Removed {num_trailing_tools} trailing ToolMessages")
        # Also remove the AIMessage that requested these tools
        if (cleaned and
            isinstance(cleaned[-1], AIMessage) and
            hasattr(cleaned[-1], 'tool_calls') and
            cleaned[-1].tool_calls):
            cleaned.pop()
            logger.warning(f"[ValidateMessages] Also removed trailing AIMessage with tool_calls")

    logger.info(f"[ValidateMessages] Cleaned {len(messages)} messages to {len(cleaned)} messages")
    return cleaned


def create_agent_node(llm_client, tool_registry):
    """
    Create the agent node function.

    The agent node calls the LLM with the current conversation state.
    LLM decides whether to use tools or provide a final response.

    Args:
        llm_client: LangChain ChatModel instance
        tool_registry: ToolRegistry instance

    Returns:
        Callable agent node function
    """
    # Get tools in LangChain format
    tools = tool_registry.to_langchain_tools()
    llm_with_tools = llm_client.bind_tools(tools) if tools else llm_client

    async def agent_node(state: Dict[str, Any]) -> Dict[str, Any]:
        """Agent node: Call LLM with current conversation history"""
        # Log messages being sent to LLM
        logger.info(f"[AgentNode] Calling LLM with {len(state['messages'])} messages")
        for i, msg in enumerate(state["messages"]):
            logger.info(f"[AgentNode]   Message {i}: {type(msg).__name__}")
            if hasattr(msg, 'tool_calls') and msg.tool_calls:
                logger.info(f"[AgentNode]     - Has {len(msg.tool_calls)} tool_calls: {[tc['id'] for tc in msg.tool_calls]}")
            if hasattr(msg, 'tool_call_id'):
                logger.info(f"[AgentNode]     - Is ToolMessage with tool_call_id: {msg.tool_call_id}")

        # Call LLM with conversation history (already cleaned in process_async)
        response = await llm_with_tools.ainvoke(state["messages"])

        logger.info(f"[AgentNode] LLM response received: {type(response).__name__}")
        if hasattr(response, 'tool_calls') and response.tool_calls:
            logger.info(f"[AgentNode]   - Response has {len(response.tool_calls)} tool_calls")

        # Add AI message to conversation
        return {
            "messages": [response],
            "step_count": state["step_count"] + 1
        }

    return agent_node


def create_tools_node(tool_registry):
    """
    Create the tools node using LangGraph's ToolNode.

    ToolNode automatically handles tool execution based on tool_calls
    in the most recent AIMessage.

    Args:
        tool_registry: ToolRegistry instance

    Returns:
        ToolNode instance
    """
    # Get tools in LangChain format
    tools = tool_registry.to_langchain_tools()

    # Use LangGraph's ToolNode which handles tool execution automatically
    return ToolNode(tools)


def should_continue(state: Dict[str, Any]) -> Literal["tools", "end"]:
    """
    Routing function: Decide whether to call tools or end.

    Checks the last message in the conversation:
    - If it has tool_calls → route to "tools"
    - Otherwise → route to "end"

    Args:
        state: Current agent state

    Returns:
        "tools" to execute tools, "end" to finish
    """
    messages = state["messages"]
    last_message = messages[-1]

    # If LLM requested tool calls, route to tools
    if hasattr(last_message, "tool_calls") and last_message.tool_calls:
        return "tools"

    # Otherwise, end the conversation turn
    return "end"


def create_agent_graph(llm_client, tool_registry):
    """
    Create the LangGraph agent graph.

    Graph structure:
        START → agent → [routing] → tools → agent
                              ↓
                             END

    The graph loops between agent and tools until the agent
    provides a final response without tool calls.

    Args:
        llm_client: LangChain ChatModel instance
        tool_registry: ToolRegistry instance

    Returns:
        Compiled LangGraph graph
    """
    # Create nodes
    agent_node = create_agent_node(llm_client, tool_registry)
    tools_node = create_tools_node(tool_registry)

    # Build graph
    workflow = StateGraph(dict)

    # Add nodes
    workflow.add_node("agent", agent_node)
    workflow.add_node("tools", tools_node)

    # Set entry point
    workflow.set_entry_point("agent")

    # Add conditional routing from agent
    workflow.add_conditional_edges(
        "agent",
        should_continue,
        {
            "tools": "tools",
            "end": END
        }
    )

    # Add edge from tools back to agent
    workflow.add_edge("tools", "agent")

    # Compile the graph
    return workflow.compile()


async def run_conversation_loop(llm_client, tool_registry, state: Dict[str, Any]) -> Dict[str, Any]:
    """
    Run conversation loop using LangGraph.

    Flow:
    - Creates a LangGraph agent graph
    - Executes the graph with the initial state
    - Graph automatically handles agent → tools → agent looping
    - Returns when agent provides final response

    Args:
        llm_client: LangChain ChatModel instance
        tool_registry: ToolRegistry instance
        state: Agent state dict containing messages, step_count, max_steps, etc.

    Returns:
        Updated state dict with response and tool_results
    """
    # Create the agent graph
    graph = create_agent_graph(llm_client, tool_registry)

    # Run the graph
    result = await graph.ainvoke(state)

    # Extract the final response from the last message
    messages = result["messages"]
    last_message = messages[-1]

    if isinstance(last_message, AIMessage):
        result["response"] = last_message.content
    else:
        result["response"] = "I'm not sure how to respond to that."

    # Collect tool results from ToolMessages in the conversation
    tool_results = []
    for msg in messages:
        if isinstance(msg, ToolMessage):
            tool_results.append({
                "tool": msg.name if hasattr(msg, "name") else "unknown",
                "success": "Error" not in msg.content,
                "result": msg.content
            })

    result["tool_results"] = tool_results

    return result
