"""
DEPRECATED: This file is kept for reference only.

The node logic has been integrated into graph.py's run_conversation_loop().
The pure LangChain implementation doesn't use separate node functions anymore.

Original purpose (for reference):
- llm_agent_node: LLM decides whether to respond or call tools
- execute_tools_node: Execute tools requested by LLM
"""

from typing import Callable
from langchain_core.messages import HumanMessage, AIMessage, ToolMessage
from .state import AgentState


def llm_agent_node(llm_client, tool_registry) -> Callable:
    """
    Create LLM agent node.

    This node invokes the LLM with tools and conversation history.
    The LLM decides whether to:
    1. Respond directly (sets state["response"])
    2. Call tools (sets state["tool_calls"])

    Args:
        llm_client: LangChain ChatModel instance (e.g. ChatAnthropic)
        tool_registry: ToolRegistry instance

    Returns:
        Node function that processes agent state
    """
    def _agent(state: AgentState) -> AgentState:
        """
        LLM agent node implementation.

        Args:
            state: Current agent state

        Returns:
            Updated agent state
        """
        # Get tools in LangChain format
        tools = tool_registry.to_langchain_tools()

        # Bind tools to LLM (enables function calling)
        llm_with_tools = llm_client.bind_tools(tools)

        # Get conversation messages
        messages = state["messages"]

        # Invoke LLM with conversation history and tools
        response = llm_with_tools.invoke(messages)

        # Add AI message to conversation
        state["messages"].append(response)

        # Check if LLM wants to call tools
        if hasattr(response, "tool_calls") and response.tool_calls:
            # LLM requested tool calls
            state["tool_calls"] = response.tool_calls
            state["response"] = None
        else:
            # LLM provided final response
            state["response"] = response.content
            state["tool_calls"] = []

        return state

    return _agent


def execute_tools_node(tool_registry) -> Callable:
    """
    Create tool execution node.

    This node executes all tools requested by the LLM and adds
    results back to the conversation as ToolMessages.

    Args:
        tool_registry: ToolRegistry instance

    Returns:
        Node function that processes agent state
    """
    def _execute(state: AgentState) -> AgentState:
        """
        Tool execution node implementation.

        Args:
            state: Current agent state

        Returns:
            Updated agent state
        """
        tool_calls = state.get("tool_calls", [])
        results = []

        for call in tool_calls:
            try:
                # Extract tool name and arguments
                tool_name = call.get("name")
                tool_args = call.get("args", {})
                tool_id = call.get("id", "unknown")

                # Execute tool
                result = tool_registry.execute_tool(tool_name, **tool_args)

                # Record success
                results.append({
                    "tool": tool_name,
                    "success": True,
                    "result": result
                })

                # Add tool result to conversation
                state["messages"].append(
                    ToolMessage(
                        content=str(result),
                        tool_call_id=tool_id
                    )
                )

            except Exception as e:
                # Record failure
                results.append({
                    "tool": call.get("name", "unknown"),
                    "success": False,
                    "error": str(e)
                })

                # Add error to conversation
                state["messages"].append(
                    ToolMessage(
                        content=f"Error: {str(e)}",
                        tool_call_id=call.get("id", "unknown")
                    )
                )

        # Update state
        state["tool_results"] = results
        state["step_count"] = state.get("step_count", 0) + 1

        # Clear tool_calls so we don't re-execute
        state["tool_calls"] = []

        return state

    return _execute
