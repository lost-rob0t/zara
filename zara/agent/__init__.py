"""
Conversational agent with pure LangChain + LangGraph tool calling.

This file mainly orchestrates:
- conversation history
- tool registry
- running the configured agent loop

Important behavior:
- We validate history to drop truly orphaned ToolMessages.
- We keep valid tool traces; Anthropic requires tool_result blocks to match a
  tool_use block in the immediately previous message.
"""

from __future__ import annotations

import os
import uuid
from pathlib import Path
from typing import Any, Dict, Optional

from langchain_core.messages import HumanMessage, SystemMessage

from .approval import ToolApprovalController
from .conversation import ConversationManager
from .graph import run_conversation_loop, validate_and_clean_messages
from .hooks import AgentLoopAdviceRegistry
from .loops import AgentLoopRegistry
from .prompting import build_agent_system_prompt
from .tools.registry import ToolRegistry
from .user_hooks import UserHookLoader
from ..config import ZaraConfig, get_config
from ..memory import build_memory_manager, MemoryManager
from ..latency import LatencyTrace


class AgentManager:
    """Manages conversational agent with tool calling."""

    def __init__(
        self,
        config: Optional[ZaraConfig] = None,
        prolog_engine=None,
        memory_manager: Optional[MemoryManager] = None,
        principal=None,
    ):
        self.config = config if config is not None else get_config()
        self.prolog_engine = prolog_engine
        self.principal = principal

        llm_config = self.config.get_llm_config()
        self.llm_client = self._create_llm_client(llm_config)

        memory_config = self.config.get_section("memory")
        self.memory_manager = memory_manager or build_memory_manager(
            memory_config,
            principal=principal,
        )
        self.memory_context_limit = int(memory_config.get("max_chars", 1200))
        self.memory_top_k = int(memory_config.get("top_k", 5))

        self.tool_registry = ToolRegistry(prolog_engine, self.config)
        self.tool_registry.load_builtin_tools(self.memory_manager)

        for plugin_dir in self.config.get_module_search_paths():
            self.tool_registry.load_user_tools(str(plugin_dir))

        agent_config = self.config.get_section("agent")
        timeout = agent_config.get("conversation_timeout", 60)
        self.conversation_manager = ConversationManager(
            timeout_seconds=timeout,
            principal=principal,
        )
        approval_config = self.config.get_section("tool_approval")
        self.approval_controller = ToolApprovalController(
            timeout_seconds=float(approval_config.get("timeout_seconds", 300.0)),
            max_pending=int(approval_config.get("max_pending", 8)),
        )
        self.agent_loop_registry = AgentLoopRegistry()
        self.agent_loop_registry.register(
            "langgraph",
            "core:langgraph",
            run_conversation_loop,
        )
        get_hooks_config = getattr(self.config, "get_hooks_config", None)
        hooks_config = (
            get_hooks_config()
            if callable(get_hooks_config)
            else {"enabled": False, "allow_override": False}
        )
        self.agent_loop_advice = AgentLoopAdviceRegistry(
            enabled=hooks_config.get("enabled", False),
            allow_override=hooks_config.get("allow_override", False),
        )
        self.user_hook_loader = None
        config_dir = getattr(self.config, "config_dir", None)
        if config_dir is not None:
            self.user_hook_loader = UserHookLoader(
                config_dir=Path(config_dir),
                registry=self.agent_loop_advice,
            )
            self.user_hook_loader.load()

    def bind_event_publisher(self, publisher) -> None:
        self.approval_controller.bind_event_publisher(publisher)

    def register_agent_loop_backend(self, name: str, owner: str, callback) -> int:
        return self.agent_loop_registry.register(name, owner, callback)

    def unregister_agent_loop_backend(self, registration_id: int, *, owner: str) -> bool:
        return self.agent_loop_registry.unregister(registration_id, owner=owner)

    def _build_system_prompt(self):
        return build_agent_system_prompt(self.config)

    def _create_llm_client(self, llm_config: Dict[str, Any]):
        provider = llm_config.get("provider", "ollama")
        model = llm_config.get("model")
        endpoint = llm_config.get("endpoint")

        if provider == "anthropic":
            from langchain_anthropic import ChatAnthropic

            api_key = llm_config.get("anthropic_api_key") or os.getenv("ANTHROPIC_API_KEY")
            return ChatAnthropic(
                model=model or "claude-3-5-sonnet-20241022",
                api_key=api_key,
                timeout=60.0,
                max_retries=2,
            )

        if provider == "openai":
            from langchain_openai import ChatOpenAI

            api_key = llm_config.get("openai_api_key") or os.getenv("OPENAI_API_KEY")
            return ChatOpenAI(
                model=model or "gpt-4",
                api_key=api_key,
                timeout=60.0,
                max_retries=2,
            )

        if provider == "openrouter":
            from langchain_openai import ChatOpenAI

            api_key = llm_config.get("openrouter_api_key") or os.getenv("OPENROUTER_API_KEY")
            return ChatOpenAI(
                model=model or "openrouter/free",
                api_key=api_key,
                openai_api_base=endpoint or "https://openrouter.ai/api/v1",
                timeout=60.0,
                max_retries=2,
            )

        if provider == "ollama":
            from langchain_ollama import ChatOllama

            base_url = endpoint.replace("/api/chat", "") if endpoint else "http://localhost:11434"
            return ChatOllama(
                model=model or "llama3",
                base_url=base_url,
                timeout=60.0,
            )

        raise ValueError(f"Unsupported LLM provider: {provider}")

    def _get_agent_loop_advice(self) -> AgentLoopAdviceRegistry:
        registry = getattr(self, "agent_loop_advice", None)
        if registry is None:
            registry = AgentLoopAdviceRegistry(enabled=False, allow_override=False)
            self.agent_loop_advice = registry
        return registry

    def _get_agent_loop_registry(self) -> AgentLoopRegistry:
        registry = getattr(self, "agent_loop_registry", None)
        if registry is None:
            registry = AgentLoopRegistry()
            registry.register("langgraph", "core:langgraph", run_conversation_loop)
            self.agent_loop_registry = registry
        return registry

    async def process_async(
        self,
        user_input: str,
        latency_trace: Optional[LatencyTrace] = None,
        turn_id: Optional[str] = None,
        conversation_id: Optional[str] = None,
        stream_publisher=None,
        conversation_history: Optional[list] = None,
        extra_system_context: Optional[str] = None,
    ) -> Dict[str, Any]:
        import logging
        logger = logging.getLogger(__name__)

        agent_config = self.config.get_section("agent")
        self.conversation_manager.update_activity()
        max_steps = int(agent_config.get("max_steps", 10))
        backend_name = str(agent_config.get("backend", "langgraph"))
        backend = self._get_agent_loop_registry().resolve(backend_name)

        if turn_id is None:
            if latency_trace is not None:
                turn_id = latency_trace.trace_id
            else:
                turn_id = f"agent-{uuid.uuid4().hex}"

        logger.info("[AgentManager] turn_id=%s", turn_id)
        logger.info("[AgentManager] conversation_id=%s", conversation_id)
        logger.info("[AgentManager] backend=%s owner=%s", backend.name, backend.owner)
        logger.info("[AgentManager] user_input=%r", user_input)
        logger.info("[AgentManager] user_input_length=%d", len(user_input))

        provided_history = conversation_history is not None
        if provided_history:
            cleaned_history = list(conversation_history)
        else:
            history = self.conversation_manager.conversation_history or []
            if history:
                cleaned_history = validate_and_clean_messages(history.copy())
                logger.info(
                    "[AgentManager] Cleaned history %d -> %d messages",
                    len(history),
                    len(cleaned_history),
                )
                logger.info(
                    "[AgentManager] History preview=%s",
                    [type(m).__name__ for m in cleaned_history[-5:]],
                )
            else:
                cleaned_history = []
                logger.info("[AgentManager] History empty; starting fresh")

        state: Dict[str, Any] = {
            "turn_id": turn_id,
            "conversation_id": conversation_id,
            "user_input": user_input,
            "messages": cleaned_history,
            "tool_calls": [],
            "tool_results": [],
            "step_count": 0,
            "max_steps": max_steps,
            "response": None,
            "latency_trace": latency_trace,
        }

        system_prompt = self._build_system_prompt()

        if system_prompt:
            if not state["messages"] or not isinstance(state["messages"][0], SystemMessage):
                state["messages"].insert(0, SystemMessage(content=system_prompt))
                logger.info("[AgentManager] System prompt injected")
            else:
                logger.info("[AgentManager] System prompt already present")

        memory_context_message = None
        memory_context = self._build_memory_context(user_input)
        if memory_context:
            memory_context_message = SystemMessage(
                content=memory_context,
                id=f"memory-context-{uuid.uuid4()}",
            )
            state["messages"].insert(1, memory_context_message)

        if extra_system_context:
            state["messages"].insert(
                1,
                SystemMessage(
                    content=extra_system_context,
                    id=f"task-context-{uuid.uuid4()}",
                ),
            )

        state["messages"].append(HumanMessage(content=user_input))
        logger.info(
            "[AgentManager] Message types=%s",
            [type(m).__name__ for m in state["messages"][-6:]],
        )
        logger.info(
            "[AgentManager] Last user message=%r",
            user_input,
        )

        principal_id = getattr(self.principal, "principal_id", "local")
        result = await self._get_agent_loop_advice().invoke(
            backend.callback,
            self.llm_client,
            self.tool_registry,
            state,
            approval_controller=self.approval_controller,
            publisher=self.approval_controller.publisher,
            principal_id=principal_id,
            stream_publisher=stream_publisher,
        )

        result_messages = result.get("messages", [])
        if memory_context_message is not None:
            result_messages = [
                message
                for message in result_messages
                if getattr(message, "id", None) != memory_context_message.id
            ]
        if not provided_history:
            self.conversation_manager.conversation_history = result_messages

        return {
            "response": result.get("response", "I'm not sure how to respond to that."),
            "tool_results": result.get("tool_results", []),
            "turn_id": turn_id,
            "conversation_id": conversation_id,
        }

    def _build_memory_context(self, user_input: str) -> Optional[str]:
        if self.memory_manager is None:
            return None
        memories = self.memory_manager.retrieve(user_input, k=self.memory_top_k)
        if not memories:
            return None

        lines = []
        for entry in memories:
            text = entry.get("text") if isinstance(entry, dict) else str(entry)
            if not text:
                continue
            metadata = entry.get("metadata") if isinstance(entry, dict) else None
            kind = ""
            if isinstance(metadata, dict):
                kind = metadata.get("kind", "")
            prefix = f"[{kind}] " if kind else ""
            lines.append(f"- {prefix}{text}")

        if not lines:
            return None

        rendered = "Relevant memories:\n" + "\n".join(lines)
        if len(rendered) > self.memory_context_limit:
            rendered = rendered[: self.memory_context_limit].rstrip()
        return rendered

    def should_exit_conversation(self) -> bool:
        return self.conversation_manager.should_exit_conversation()

    def exit_conversation(self):
        self.conversation_manager.exit_conversation()

    async def approve_tool(self, tool_run_id: str) -> None:
        await self.approval_controller.approve(tool_run_id)

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        await self.approval_controller.reject(tool_run_id, reason)

    async def cancel_turn(self, turn_id: str) -> None:
        await self.approval_controller.cancel_turn(turn_id)

    async def shutdown_async(self) -> None:
        """Close dynamic providers and end the conversation cleanly."""
        try:
            await self.approval_controller.shutdown()
            self.exit_conversation()
        finally:
            await self.tool_registry.shutdown_async()


__all__ = ["AgentManager"]
