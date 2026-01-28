"""
DEPRECATED: This file is kept for reference only.

The routing logic has been integrated into graph.py's run_conversation_loop().
The pure LangChain implementation uses a simple while loop instead of conditional edges.

Original purpose (for reference):
Determined whether to execute tools, continue conversation, or end.
"""

from .state import AgentState


def should_execute_tools(state: AgentState) -> str:
    """
    Decide if we should execute tools or end conversation turn.

    Routing logic:
    1. If max steps reached → end (prevent infinite loops)
    2. If tool calls exist → execute tools
    3. If response exists → end (LLM provided final answer)
    4. Otherwise → end

    Args:
        state: Current agent state

    Returns:
        "tools" to execute tools, "end" to finish turn
    """
    # Max steps check (prevent infinite loops)
    if state.get("step_count", 0) >= state["max_steps"]:
        return "end"

    # If tool calls exist, execute them
    if state.get("tool_calls") and len(state["tool_calls"]) > 0:
        return "tools"

    # If response exists, we're done
    if state.get("response"):
        return "end"

    # Default: end
    return "end"
