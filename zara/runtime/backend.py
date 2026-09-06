"""Application-service backends owned by :class:`zara.runtime.host.RuntimeHost`.

RuntimeHost owns lifecycle, threading, turn correlation, and cancellation.
Backends own the application services used to execute a turn. LangGraph is the
canonical conversational backend.
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from typing import Any, Callable, Optional

from .. import command_gate
from ..latency import LatencyTrace
from . import events

logger = logging.getLogger(__name__)

DETERMINISTIC_COMMAND_FAILED = (
    "I couldn't complete that deterministic command. I did not send it to the LLM."
)


class UnsupportedRuntimeCommand(RuntimeError):
    """Raised when a backend does not implement an optional capability."""


@dataclass(frozen=True)
class RuntimeTurnResult:
    response: str = ""
    tool_results: tuple[dict[str, Any], ...] = field(default_factory=tuple)
    metadata: dict[str, Any] = field(default_factory=dict)


class RuntimeBackend:
    """Async application-service contract used by RuntimeHost."""

    @property
    def principal_id(self) -> str:
        raise UnsupportedRuntimeCommand(
            "principal identity is not available in this runtime backend"
        )

    def bind_event_publisher(self, publisher) -> None:
        pass

    async def start(self) -> None:
        pass

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        system_context: Optional[str] = None,
        conversation_history: Optional[list] = None,
        latency_trace: Optional[LatencyTrace] = None,
    ) -> RuntimeTurnResult:
        raise NotImplementedError

    async def cancel_turn(self, turn_id: str) -> None:
        pass

    def register_tools(self, tools) -> None:
        raise UnsupportedRuntimeCommand(
            "tool registration is not available in this runtime backend"
        )

    def unregister_tools(self, names) -> None:
        pass

    def register_agent_loop_advice(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: Callable[..., Any],
    ) -> int:
        raise UnsupportedRuntimeCommand(
            "agent-loop advice is not available in this runtime backend"
        )

    def unregister_agent_loop_advice(self, registration_id: int) -> bool:
        return False

    def customization_diagnostics(self):
        raise UnsupportedRuntimeCommand(
            "customization diagnostics are not available in this runtime backend"
        )

    async def start_voice(self) -> None:
        raise UnsupportedRuntimeCommand("voice start is not available in this runtime backend")

    async def stop_voice(self) -> None:
        raise UnsupportedRuntimeCommand("voice stop is not available in this runtime backend")

    async def mute_speech(self, enabled: bool) -> None:
        raise UnsupportedRuntimeCommand("speech mute is not available in this runtime backend")

    async def approve_tool(self, tool_run_id: str) -> None:
        raise UnsupportedRuntimeCommand("tool approval is not available in this runtime backend")

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        raise UnsupportedRuntimeCommand("tool rejection is not available in this runtime backend")

    async def stop(self) -> None:
        pass


class LangGraphRuntimeBackend(RuntimeBackend):
    """Thin adapter over Zara's existing :class:`AgentManager`."""

    def __init__(
        self,
        manager_factory: Optional[Callable[[], Any]] = None,
        *,
        router=None,
    ) -> None:
        self._manager_factory = manager_factory
        self._manager = None
        self._publisher = None
        self._router = router
        self._memory_session: Optional[str] = None

    @property
    def principal_id(self) -> str:
        manager = self._manager
        if manager is None:
            raise RuntimeError("runtime backend is not started")
        principal = getattr(manager, "principal", None)
        principal_id = getattr(principal, "principal_id", None)
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise RuntimeError("runtime backend manager has no principal identity")
        return principal_id

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher
        if self._manager is not None:
            bind = getattr(self._manager, "bind_event_publisher", None)
            if bind is not None:
                bind(publisher)

    async def start(self) -> None:
        if self._manager is not None:
            return
        if self._manager_factory is None:
            from zara.agent import AgentManager

            self._manager_factory = AgentManager
        self._manager = self._manager_factory()
        if self._publisher is not None:
            bind = getattr(self._manager, "bind_event_publisher", None)
            if bind is not None:
                bind(self._publisher)

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        system_context: Optional[str] = None,
        conversation_history: Optional[list] = None,
        latency_trace: Optional[LatencyTrace] = None,
    ) -> RuntimeTurnResult:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not wired into the runtime backend yet"
            )

        task_turn = conversation_history is not None or system_context is not None
        command_like = command_gate.looks_like_command(text)

        if self._router is not None and not task_turn:
            conversation_manager = self._manager.conversation_manager
            in_conversation = bool(getattr(conversation_manager, "in_conversation", False))
            state = "conversation" if in_conversation else "passive"
            decision = await self._router.route(
                text,
                state=state,
                latency_trace=latency_trace,
                conversation_id=conversation_id,
            )
            if decision.action == "greeting":
                conversation_manager.enter_conversation()
                conversation_manager.conversation_history.clear()
                return RuntimeTurnResult(response=decision.response)
            if decision.action == "end_conversation":
                conversation_manager.exit_conversation()
                await self._rotate_memory_session()
                return RuntimeTurnResult(response=decision.response)
            if decision.action == "respond":
                await self._persist_turn(text, decision.response)
                return RuntimeTurnResult(response=decision.response)
            if decision.action == "delegate" and command_like:
                logger.error(
                    "Refusing LLM fallback after deterministic command routing "
                    "did not complete: %r",
                    text,
                )
                await self._persist_turn(text, DETERMINISTIC_COMMAND_FAILED)
                return RuntimeTurnResult(
                    response=DETERMINISTIC_COMMAND_FAILED,
                    metadata={"route": "deterministic_failed"},
                )
            if not in_conversation:
                conversation_manager.enter_conversation()
                conversation_manager.conversation_history.clear()

        result = await self._manager.process_async(
            text,
            turn_id=turn_id,
            conversation_id=conversation_id,
            latency_trace=latency_trace,
            stream_publisher=self._stream_publisher(turn_id, conversation_id),
            conversation_history=conversation_history,
            extra_system_context=system_context,
        )
        if not isinstance(result, dict):
            return RuntimeTurnResult(response=str(result))
        response = str(result.get("response", ""))
        tool_results = result.get("tool_results") or []
        normalized_tools = tuple(item for item in tool_results if isinstance(item, dict))
        return RuntimeTurnResult(response=response, tool_results=normalized_tools)

    async def cancel_turn(self, turn_id: str) -> None:
        if self._manager is None:
            return
        cancel = getattr(self._manager, "cancel_turn", None)
        if cancel is None:
            return
        result = cancel(turn_id)
        if asyncio.iscoroutine(result):
            await result

    def register_tools(self, tools) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        register = getattr(self._manager, "register_tools", None)
        if register is None:
            raise UnsupportedRuntimeCommand("runtime manager cannot register tools")
        register(tools)

    def unregister_tools(self, names) -> None:
        if self._manager is None:
            return
        unregister = getattr(self._manager, "unregister_tools", None)
        if unregister is not None:
            unregister(names)

    def register_agent_loop_advice(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: Callable[..., Any],
    ) -> int:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        register = getattr(self._manager, "register_agent_loop_advice", None)
        if register is None:
            raise UnsupportedRuntimeCommand(
                "runtime manager cannot register agent-loop advice"
            )
        return int(register(kind, owner, priority, callback))

    def unregister_agent_loop_advice(self, registration_id: int) -> bool:
        if self._manager is None:
            return False
        unregister = getattr(self._manager, "unregister_agent_loop_advice", None)
        if unregister is None:
            return False
        return bool(unregister(registration_id))

    def customization_diagnostics(self):
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        diagnostics = getattr(self._manager, "customization_diagnostics", None)
        if diagnostics is None:
            raise UnsupportedRuntimeCommand(
                "runtime manager cannot report customization diagnostics"
            )
        return diagnostics()

    async def start_voice(self) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        start = getattr(self._manager, "start_voice", None)
        if start is None:
            raise UnsupportedRuntimeCommand("voice start is not available")
        result = start()
        if asyncio.iscoroutine(result):
            await result

    async def stop_voice(self) -> None:
        if self._manager is None:
            return
        stop = getattr(self._manager, "stop_voice", None)
        if stop is None:
            return
        result = stop()
        if asyncio.iscoroutine(result):
            await result

    async def mute_speech(self, enabled: bool) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        mute = getattr(self._manager, "mute_speech", None)
        if mute is None:
            raise UnsupportedRuntimeCommand("speech mute is not available")
        result = mute(enabled)
        if asyncio.iscoroutine(result):
            await result

    async def approve_tool(self, tool_run_id: str) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        approve = getattr(self._manager, "approve_tool", None)
        if approve is None:
            raise UnsupportedRuntimeCommand("tool approval is not available")
        result = approve(tool_run_id)
        if asyncio.iscoroutine(result):
            await result

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        reject = getattr(self._manager, "reject_tool", None)
        if reject is None:
            raise UnsupportedRuntimeCommand("tool rejection is not available")
        result = reject(tool_run_id, reason)
        if asyncio.iscoroutine(result):
            await result

    async def stop(self) -> None:
        if self._manager is None:
            return
        manager = self._manager
        self._manager = None
        shutdown = getattr(manager, "shutdown_async", None)
        if shutdown is not None:
            await shutdown()

    def _stream_publisher(self, turn_id: str, conversation_id: Optional[str]):
        if self._publisher is None:
            return None

        def publish(delta: str) -> None:
            self._publisher(
                events.AssistantDelta(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    delta=str(delta),
                )
            )

        return publish

    async def _persist_turn(self, user_text: str, assistant_text: str) -> None:
        if self._manager is None:
            return
        memory = getattr(self._manager, "memory_manager", None)
        if memory is None:
            return
        try:
            session_id = await self._ensure_memory_session(memory)
            await memory.add_message(session_id, "user", user_text)
            await memory.add_message(session_id, "assistant", assistant_text)
        except Exception as error:
            logger.warning("Failed to persist deterministic turn: %s", error)

    async def _ensure_memory_session(self, memory) -> str:
        if self._memory_session:
            return self._memory_session
        session = await memory.create_session()
        self._memory_session = session.session_id
        return self._memory_session

    async def _rotate_memory_session(self) -> None:
        self._memory_session = None
