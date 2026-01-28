"""
AgentState schema for conversation flow.

NOTE: Pure LangChain implementation uses plain dicts instead of TypedDict.
This file is kept for documentation of the state structure.

Defines the state structure used during multi-turn conversations with tool calling.
"""

from typing import TypedDict, List, Dict, Any, Optional
from langchain_core.messages import BaseMessage


class AgentState(TypedDict):
    """
    State schema for conversational agent.

    This state is passed between nodes in the LangGraph conversation flow.
    Each node can read and update different parts of the state.
    """

    # Input/Output
    user_input: str
    """Current user input text"""

    response: Optional[str]
    """Final response to return to user (set when conversation turn completes)"""

    # Conversation Context
    messages: List[BaseMessage]
    """Full conversation history as LangChain messages (HumanMessage, AIMessage, ToolMessage)"""

    # Tool Execution
    tool_calls: List[Dict[str, Any]]
    """Tool calls requested by LLM in current turn"""

    tool_results: List[Dict[str, Any]]
    """Results from executing tools in current turn"""

    # Metadata
    step_count: int
    """Number of tool execution steps in current turn (prevents infinite loops)"""

    max_steps: int
    """Maximum allowed steps before forcing conversation to end (default: 10)"""
