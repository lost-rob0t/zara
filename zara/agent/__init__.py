"""
Conversational agent with pure LangChain + LangGraph tool calling.

Context assembly is owned by ContextManager. Tool execution and dynamic MCP
capability lifecycle remain owned by ToolRegistry/MCPManager.
"""

from __future__ import annotations

import os
import sys
import uuid
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional

from langchain_core.messages import BaseMessage, HumanMessage, SystemMessage

from .approval import ToolApprovalController
from .context import ContextConfig, ContextManager, TransientContext
from .conversation import ConversationManager
from .graph import run_conversation_loop
from .skills import SkillRegistry
from .tools.registry import ToolRegistry
from ..config import ZaraConfig, get_config
from ..memory import build_memory_manager, MemoryManager
from ..latency import LatencyTrace


class _PreparedToolRegistry:
    def __init__(self, registry: ToolRegistry):
        self._registry = registry

    async def prepare_async(self) -> None:
        return None

    def dynamic_system_context(self):
        return None

    def __getattr__(self, name: str):
        return getattr(self._registry, name)


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

        context_values = self.config.get_section("context")
        context_config = ContextConfig(
            strategy=context_values.get("strategy", "truncate"),
            max_tokens=int(context_values.get("max_tokens", 32000)),
            preserve_recent_turns=int(context_values.get("preserve_recent_turns", 8)),
            summary_max_tokens=int(context_values.get("summary_max_tokens", 2000)),
            skill_max_tokens=int(context_values.get("skill_max_tokens", 6000)),
        )
        self.context_manager = ContextManager(
            system_prompt=self._build_system_prompt,
            config=context_config,
            summarizer=self._summarize_context,
        )

        skills_config = self.config.get_section("skills")
        self.skill_registry: SkillRegistry | None = None
        if skills_config.get("enabled", True):
            self.skill_registry = SkillRegistry(self._skill_roots(skills_config))
            self.skill_registry.discover()

        agent_config = self.config.get_section("agent")
        timeout = agent_config.get("conversation_timeout", 60)
        self.conversation_manager = ConversationManager(
            timeout_seconds=timeout,
            principal=principal,
            history_provider=lambda: self.context_manager.history,
            history_clear=self.context_manager.clear,
        )
        approval_config = self.config.get_section("tool_approval")
        self.approval_controller = ToolApprovalController(
            timeout_seconds=float(approval_config.get("timeout_seconds", 300.0)),
            max_pending=int(approval_config.get("max_pending", 8)),
        )

    def bind_event_publisher(self, publisher) -> None:
        self.approval_controller.bind_event_publisher(publisher)

    def _build_system_prompt(self):
        date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        base_prompt = self.config.get_agent_system_prompt() or """You are Zarathustra, an agentic large language model inside a voice assistant. Your primary goal is to be helpful, precise, and safe for the user.

        # Routing protocol — read this first

        The user's input falls into one of two categories. Pick the right path BEFORE reaching for any tool; this keeps latency low and avoids hijacking conversations.

        ## 0. Explicit service-tool capabilities

        Service plugins may add tools that are more specific than the legacy Prolog command router. When one of the following tools is present and the user's request explicitly matches it, use that tool directly instead of `query_prolog`:

        - `schedule_recurring_task`: use for recurring, repeating, periodic, interval-based, or autonomous background tasks. This is different from creating a one-time todo/reminder.
        - `list_recurring_tasks` / `cancel_recurring_task`: use to inspect or remove those recurring background tasks.
        - `speak`: use when the user explicitly asks Zara to speak, say something aloud, or produce TTS output.
        - `set_random_questions`: use when the user explicitly asks to enable or disable proactive/random questions.
        - `agent_mode_status`: use when the user asks about autonomous/agent-mode status.

        These specific capabilities win even when the request begins with words such as schedule, task, set, say, speak, or list. Do not first send them through Prolog and accidentally turn a recurring agent task into an ordinary todo.

        ## 1. Command utterances (starts with a command verb)

        If the user's first word is one of: open, launch, run, start, stop, end, pause, resume, play, next, skip, lock, unlock, text, message, dictate, dictation, voice, mic, enable, begin, activate, deactivate, search, find, lookup, navigate, goto, set, schedule, plan, add, note, remind, remember, reminder, todo, todos, task, tasks, list, show, edit, update, export, say, timer, alarm, weather, forecast, bye, goodbye, farewell, quit — treat it as a command unless the explicit service-tool rules above apply.

        For other commands, call the `query_prolog` tool ONCE with the goal `command_loop:handle_command(\"<exact user text>\")`. That path executes apps, media control, timers, todo capture, and dictation lifecycle in the existing Prolog pipeline. Relay the tool's result to the user in one short sentence. Do NOT call any other tool for a command unless the prolog tool explicitly failed or returned no match.

        ## 2. Conversational utterances (everything else)

        Questions, statements, chitchat, philosophy, explanations, and free-form chat are NOT commands. Answer directly in natural language. Do NOT call `query_prolog` for these. Do NOT call tools \"just in case\" — that adds latency and hijacks the conversation.

        Only use memory, calculator, file, or dynamically registered service tools when the user explicitly asks for the capability or the current task clearly requires it. Only set `forget.all_memories=true` and `confirm=true` when the user clearly asked to forget everything.

        # Style

        For ambiguous requests, ask ONE focused clarifying question before acting.

        Your style is wise, direct, strong, creative, and philosophical. Be helpful and insightful.

        # Output Format

        Respond in direct, clear, and concise natural language. Do not use JSON or list internal reasoning in the output. Use internal reasoning to inform a concise, user-facing final answer."""
        return base_prompt + f"\n # Current time \n {date}"

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

    async def process_async(
        self,
        user_input: str,
        latency_trace: Optional[LatencyTrace] = None,
        turn_id: Optional[str] = None,
        conversation_id: Optional[str] = None,
        stream_publisher=None,
    ) -> Dict[str, Any]:
        import logging
        logger = logging.getLogger(__name__)

        agent_config = self.config.get_section("agent")
        self.conversation_manager.update_activity()
        max_steps = int(agent_config.get("max_steps", 10))

        if turn_id is None:
            if latency_trace is not None:
                turn_id = latency_trace.trace_id
            else:
                turn_id = f"agent-{uuid.uuid4().hex}"

        logger.info("[AgentManager] turn_id=%s", turn_id)
        logger.info("[AgentManager] conversation_id=%s", conversation_id)
        logger.info("[AgentManager] user_input=%r", user_input)
        logger.info("[AgentManager] user_input_length=%d", len(user_input))

        lease = self.context_manager.begin_turn(turn_id)
        try:
            await self.tool_registry.prepare_async()
            transients = []
            dynamic_context = self.tool_registry.dynamic_system_context()
            if dynamic_context:
                transients.append(TransientContext("mcp", dynamic_context))
            memory_context = self._build_memory_context(user_input)
            if memory_context:
                transients.append(TransientContext("memory", memory_context))

            skill_context = None
            if self.skill_registry is not None:
                selection = self.skill_registry.select(
                    user_input,
                    max_tokens=self.context_manager.config.skill_max_tokens,
                )
                skill_context = self.skill_registry.render(selection)

            build = await self.context_manager.build_messages(
                lease,
                user_input,
                transients=transients,
                skill_context=skill_context,
            )
            state: Dict[str, Any] = {
                "turn_id": turn_id,
                "conversation_id": conversation_id,
                "user_input": user_input,
                "messages": list(build.messages),
                "tool_calls": [],
                "tool_results": [],
                "step_count": 0,
                "max_steps": max_steps,
                "response": None,
                "latency_trace": latency_trace,
            }
            logger.info(
                "[AgentManager] Context tokens=%d messages=%d skills=%s transients=%s",
                build.token_count,
                len(build.messages),
                build.audit.skill_context_included,
                build.audit.transient_kinds,
            )

            principal_id = getattr(self.principal, "principal_id", "local")
            result = await run_conversation_loop(
                self.llm_client,
                _PreparedToolRegistry(self.tool_registry),
                state,
                approval_controller=self.approval_controller,
                publisher=self.approval_controller.publisher,
                principal_id=principal_id,
                stream_publisher=stream_publisher,
            )
            self.context_manager.commit_result(lease, result.get("messages", []))
        except BaseException:
            self.context_manager.cancel_turn(turn_id)
            raise

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

    async def _summarize_context(
        self,
        messages: tuple[BaseMessage, ...],
        max_tokens: int,
    ) -> str:
        rendered = []
        for message in messages:
            content = getattr(message, "content", "")
            rendered.append(f"{type(message).__name__}: {content}")
        response = await self.llm_client.ainvoke(
            [
                SystemMessage(
                    content=(
                        "Compress the conversation prefix for continued context. Preserve decisions, "
                        "user constraints, unresolved tasks, named entities, and facts needed to continue. "
                        "Omit raw tool traces, repeated chatter, and superseded intermediate work. "
                        f"Keep the result within about {max_tokens} tokens."
                    )
                ),
                HumanMessage(content="\n".join(rendered)),
            ]
        )
        content = getattr(response, "content", "")
        if isinstance(content, str):
            return content
        if isinstance(content, list):
            return "".join(
                str(block.get("text", ""))
                for block in content
                if isinstance(block, dict) and block.get("type") == "text"
            )
        return str(content)

    def _skill_roots(self, skills_config: Dict[str, Any]) -> tuple[Path, ...]:
        repo_root = Path(__file__).resolve().parents[2]
        xdg = os.getenv("XDG_CONFIG_HOME")
        config_root = Path(xdg) if xdg else Path.home() / ".config"
        roots = [
            repo_root / "skills",
            Path(sys.prefix) / "share" / "zarathushtra" / "skills",
            config_root / "zarathushtra" / "skills",
        ]
        roots.extend(Path(path).expanduser() for path in skills_config.get("search_paths", []))
        result = []
        seen = set()
        for root in roots:
            key = str(root)
            if key in seen:
                continue
            seen.add(key)
            result.append(root)
        return tuple(result)

    def should_exit_conversation(self) -> bool:
        return self.conversation_manager.should_exit_conversation()

    def exit_conversation(self):
        self.conversation_manager.exit_conversation()

    async def approve_tool(self, tool_run_id: str) -> None:
        await self.approval_controller.approve(tool_run_id)

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        await self.approval_controller.reject(tool_run_id, reason)

    async def cancel_turn(self, turn_id: str) -> None:
        self.context_manager.cancel_turn(turn_id)
        await self.approval_controller.cancel_turn(turn_id)

    async def shutdown_async(self) -> None:
        try:
            await self.approval_controller.shutdown()
            self.exit_conversation()
        finally:
            await self.tool_registry.shutdown_async()


__all__ = ["AgentManager"]
