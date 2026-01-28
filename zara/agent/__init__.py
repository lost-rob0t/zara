"""
Conversational agent with pure LangChain and tool calling.

Main interface for conversation mode. Provides:
- AgentManager: Main class for managing conversations
- Integration with Prolog engine for hybrid command/conversation mode
- Tool calling with extensible registry
- Multi-turn dialogue with LLM
"""

import os
from typing import Any, Dict, Optional
from pathlib import Path
from langchain_core.messages import HumanMessage

from .conversation import ConversationManager
from .tools.registry import ToolRegistry
from .graph import run_conversation_loop, validate_and_clean_messages
from ..config import ZaraConfig, get_config


class AgentManager:
    """
    Manages conversational agent with pure LangChain and tool calling.

    This is the main interface for conversation mode. It:
    - Manages conversation state and history
    - Orchestrates LLM + tool calling via simple loop
    - Handles timeout and mode transitions
    - Loads and registers tools
    """

    def __init__(self, config: Optional[ZaraConfig] = None, prolog_engine=None):
        """
        Initialize agent manager.

        Args:
            config: ZaraConfig instance (uses global if None)
            prolog_engine: Optional PrologEngine instance for Prolog tools
        """
        self.config = config if config is not None else get_config()
        self.prolog_engine = prolog_engine

        # Get LLM config from system config
        llm_config = self.config.get_llm_config()

        # Create LangChain LLM client
        self.llm_client = self._create_llm_client(llm_config)

        # Create tool registry and load tools
        self.tool_registry = ToolRegistry(prolog_engine, self.config)
        self.tool_registry.load_builtin_tools()

        # Load user plugins from configured paths
        for plugin_dir in self.config.get_module_search_paths():
            self.tool_registry.load_user_tools(str(plugin_dir))

        # Create conversation manager with configured timeout
        agent_config = self.config.get_section("agent")
        timeout = agent_config.get("conversation_timeout", 60)
        self.conversation_manager = ConversationManager(timeout_seconds=timeout)

    def _create_llm_client(self, llm_config: Dict[str, Any]):
        """
        Create LangChain ChatModel from config.

        Args:
            llm_config: LLM configuration dict

        Returns:
            LangChain ChatModel instance

        Raises:
            ValueError: If provider not supported
        """
        provider = llm_config.get("provider", "ollama")
        model = llm_config.get("model")
        endpoint = llm_config.get("endpoint")

        if provider == "anthropic":
            from langchain_anthropic import ChatAnthropic
            api_key = llm_config.get("anthropic_api_key") or os.getenv("ANTHROPIC_API_KEY")
            return ChatAnthropic(model=model or "claude-3-5-sonnet-20241022", api_key=api_key)

        elif provider == "openai":
            from langchain_openai import ChatOpenAI
            api_key = llm_config.get("openai_api_key") or os.getenv("OPENAI_API_KEY")
            return ChatOpenAI(model=model or "gpt-4", api_key=api_key)

        elif provider == "ollama":
            from langchain_ollama import ChatOllama
            # Extract base URL from endpoint (remove /api/chat suffix)
            base_url = endpoint.replace("/api/chat", "") if endpoint else "http://localhost:11434"
            return ChatOllama(model=model or "llama3", base_url=base_url)

        else:
            raise ValueError(f"Unsupported LLM provider: {provider}")

    async def process_async(self, user_input: str) -> Dict[str, Any]:
        """
        Process user input in conversation mode.

        This is the main entry point for conversation turns.
        It runs the conversation loop with the user input and returns the response.

        Args:
            user_input: User's message text

        Returns:
            Dict with keys:
                - response: LLM's response text
                - tool_results: List of tool execution results
        """
        import logging
        logger = logging.getLogger(__name__)

        # Update activity timestamp
        self.conversation_manager.update_activity()

        # Get max steps from config
        agent_config = self.config.get_section("agent")
        max_steps = agent_config.get("max_steps", 10)

        # Clean and validate conversation history before using it (only if not empty)
        if len(self.conversation_manager.conversation_history) > 0:
            cleaned_history = validate_and_clean_messages(self.conversation_manager.conversation_history.copy())
            logger.info(f"[AgentManager] Cleaned conversation history from {len(self.conversation_manager.conversation_history)} to {len(cleaned_history)} messages")
        else:
            cleaned_history = []
            logger.info(f"[AgentManager] Conversation history is empty, starting fresh")

        # Build initial state
        state = {
            "user_input": user_input,
            "messages": cleaned_history,
            "tool_calls": [],
            "tool_results": [],
            "step_count": 0,
            "max_steps": max_steps,
            "response": None
        }

        # Add user message to state FIRST (before logging)
        state["messages"].append(HumanMessage(content=user_input))

        # Log the current conversation history
        logger.info(f"[AgentManager] Starting process_async with {len(state['messages'])} messages total")
        for i, msg in enumerate(state["messages"]):
            logger.info(f"[AgentManager]   Message {i}: {type(msg).__name__}")
            if hasattr(msg, 'tool_calls') and msg.tool_calls:
                logger.info(f"[AgentManager]     - Has {len(msg.tool_calls)} tool_calls")
            if hasattr(msg, 'tool_call_id'):
                logger.info(f"[AgentManager]     - Is ToolMessage with tool_call_id: {msg.tool_call_id}")

        # Run conversation loop
        result = await run_conversation_loop(self.llm_client, self.tool_registry, state)

        # Log the result
        logger.info(f"[AgentManager] Conversation loop completed with {len(result['messages'])} messages")

        # Update conversation history with all messages
        # Tool messages must be kept to maintain valid conversation context
        # (AIMessages with tool_calls require corresponding ToolMessages)
        self.conversation_manager.conversation_history = result["messages"]

        # Return response
        return {
            "response": result.get("response", "I'm not sure how to respond to that."),
            "tool_results": result.get("tool_results", [])
        }

    def should_exit_conversation(self) -> bool:
        """
        Check if conversation should end due to timeout.

        Returns:
            True if conversation has been inactive too long
        """
        return self.conversation_manager.should_exit_conversation()

    def exit_conversation(self):
        """Explicitly exit conversation mode and clear history."""
        self.conversation_manager.exit_conversation()


# Module exports
__all__ = [
    "AgentManager",
]
