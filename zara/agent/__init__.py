"""
Conversational agent with pure LangChain + LangGraph tool calling.

This file mainly orchestrates:
- conversation history
- tool registry
- running the LangGraph loop

Important behavior:
- We validate history to drop truly orphaned ToolMessages.
- We keep valid tool traces; Anthropic requires tool_result blocks to match a
  tool_use block in the immediately previous message.
"""

from __future__ import annotations

import os
from typing import Any, Dict, Optional

from langchain_core.messages import HumanMessage

from .conversation import ConversationManager
from .tools.registry import ToolRegistry
from .graph import run_conversation_loop, validate_and_clean_messages
from ..config import ZaraConfig, get_config


class AgentManager:
    """
    Manages conversational agent with tool calling.
    """

    def __init__(self, config: Optional[ZaraConfig] = None, prolog_engine=None):
        self.config = config if config is not None else get_config()
        self.prolog_engine = prolog_engine

        llm_config = self.config.get_llm_config()
        self.llm_client = self._create_llm_client(llm_config)

        self.tool_registry = ToolRegistry(prolog_engine, self.config)
        self.tool_registry.load_builtin_tools()

        for plugin_dir in self.config.get_module_search_paths():
            self.tool_registry.load_user_tools(str(plugin_dir))

        agent_config = self.config.get_section("agent")
        timeout = agent_config.get("conversation_timeout", 60)
        self.conversation_manager = ConversationManager(timeout_seconds=timeout)

    def _create_llm_client(self, llm_config: Dict[str, Any]):
        provider = llm_config.get("provider", "ollama")
        model = llm_config.get("model")
        endpoint = llm_config.get("endpoint")

        if provider == "anthropic":
            from langchain_anthropic import ChatAnthropic
            api_key = llm_config.get("anthropic_api_key") or os.getenv("ANTHROPIC_API_KEY")
            return ChatAnthropic(model=model or "claude-3-5-sonnet-20241022", api_key=api_key)

        if provider == "openai":
            from langchain_openai import ChatOpenAI
            api_key = llm_config.get("openai_api_key") or os.getenv("OPENAI_API_KEY")
            return ChatOpenAI(model=model or "gpt-4", api_key=api_key)

        if provider == "ollama":
            from langchain_ollama import ChatOllama
            base_url = endpoint.replace("/api/chat", "") if endpoint else "http://localhost:11434"
            return ChatOllama(model=model or "llama3", base_url=base_url)

        raise ValueError(f"Unsupported LLM provider: {provider}")

    async def process_async(self, user_input: str) -> Dict[str, Any]:
        import logging
        logger = logging.getLogger(__name__)

        self.conversation_manager.update_activity()

        agent_config = self.config.get_section("agent")
        max_steps = int(agent_config.get("max_steps", 10))

        history = self.conversation_manager.conversation_history or []
        if history:
            cleaned_history = validate_and_clean_messages(history.copy())
            logger.info(
                "[AgentManager] Cleaned history %d -> %d messages",
                len(history),
                len(cleaned_history),
            )
        else:
            cleaned_history = []
            logger.info("[AgentManager] History empty; starting fresh")

        state: Dict[str, Any] = {
            "user_input": user_input,
            "messages": cleaned_history,
            "tool_calls": [],
            "tool_results": [],
            "step_count": 0,
            "max_steps": max_steps,
            "response": None,
        }

        # Always append the new user message last.
        state["messages"].append(HumanMessage(content=user_input))

        result = await run_conversation_loop(self.llm_client, self.tool_registry, state)

        # Persist full message history (now correct because graph uses add_messages).
        self.conversation_manager.conversation_history = result.get("messages", [])

        return {
            "response": result.get("response", "I'm not sure how to respond to that."),
            "tool_results": result.get("tool_results", []),
        }

    def should_exit_conversation(self) -> bool:
        return self.conversation_manager.should_exit_conversation()

    def exit_conversation(self):
        self.conversation_manager.exit_conversation()


__all__ = ["AgentManager"]
