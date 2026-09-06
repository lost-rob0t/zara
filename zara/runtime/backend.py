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

DETERMINISTIC_COMMAND_UNAVAILABLE = (
    "I couldn't run that command because Zara's deterministic command router "
    "is unavailable."
)
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

        if not task_turn and self._router is None and command_like:
            logger.error(
                "Refusing LLM fallback for deterministic command because the "
                "semantic router is unavailable: %r",
                text,
            )
            return RuntimeTurnResult(
                response=DETERMINISTIC_COMMAND_UNAVAILABLE,
                metadata={"route": "deterministic_unavailable"},
            )

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
        raw_tool_results = result.get("tool_results", [])
        response = str(result.get("response", ""))
        if not task_turn:
            await self._persist_turn(text, response)
        return RuntimeTurnResult(
            response=response,
            tool_results=tuple(
                item if isinstance(item, dict) else {"result": item}
                for item in raw_tool_results
            ),
        )

    async def cancel_turn(self, turn_id: str) -> None:
        if self._manager is not None:
            cancel = getattr(self._manager, "cancel_turn", None)
            if cancel is not None:
                await cancel(turn_id)

    def _memory_manager(self):
        if self._manager is None:
            return None
        return getattr(self._manager, "memory_manager", None)

    async def _persist_turn(self, text: str, response: str) -> None:
        memory = self._memory_manager()
        if memory is None:
            return
        try:
            if self._memory_session is None:
                self._memory_session = await asyncio.to_thread(memory.start_session)
            await asyncio.to_thread(
                memory.add_message, self._memory_session, "user", text
            )
            if response:
                await asyncio.to_thread(
                    memory.add_message, self._memory_session, "assistant", response
                )
        except Exception:
            logger.warning("Memory persistence failed for daemon turn", exc_info=True)

    async def _rotate_memory_session(self) -> None:
        memory = self._memory_manager()
        if memory is None:
            return
        try:
            session = self._memory_session
            if session is not None:
                await asyncio.to_thread(memory.summarise_session, session)
            self._memory_session = await asyncio.to_thread(memory.start_session)
        except Exception:
            logger.warning(
                "Memory session rotation failed on conversation end", exc_info=True
            )

    def _stream_publisher(self, turn_id: str, conversation_id: Optional[str]):
        publisher = self._publisher
        if publisher is None:
            return None
        from ..agent import stream_events

        started = False

        def publish(event) -> None:
            nonlocal started
            try:
                if type(event) is stream_events.SentenceReady:
                    if not started:
                        started = True
                        publisher(
                            events.AssistantStarted(
                                turn_id=turn_id,
                                conversation_id=conversation_id,
                                label="agent",
                            )
                        )
                    publisher(
                        events.AssistantDelta(
                            turn_id=turn_id,
                            conversation_id=conversation_id,
                            label="agent",
                            text=event.text,
                        )
                    )
                elif type(event) is stream_events.Completed:
                    publisher(
                        events.AssistantComplete(
                            turn_id=turn_id,
                            conversation_id=conversation_id,
                            label="agent",
                            text=event.full_text,
                        )
                    )
            except Exception:
                logger.debug(
                    "Assistant stream event publication failed for turn %s",
                    turn_id,
                    exc_info=True,
                )

        return publish

    async def approve_tool(self, tool_run_id: str) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        approve = getattr(self._manager, "approve_tool", None)
        if approve is None:
            raise UnsupportedRuntimeCommand("tool approval is not available in this runtime backend")
        await approve(tool_run_id)

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        reject = getattr(self._manager, "reject_tool", None)
        if reject is None:
            raise UnsupportedRuntimeCommand("tool rejection is not available in this runtime backend")
        await reject(tool_run_id, reason)

    def register_tools(self, tools) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        self._manager.tool_registry.register_tools(list(tools))

    def unregister_tools(self, names) -> None:
        if self._manager is not None:
            self._manager.tool_registry.unregister_tools(list(names))

    def register_agent_loop_advice(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: Callable[..., Any],
    ) -> int:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        registry = getattr(self._manager, "agent_loop_advice", None)
        if registry is None:
            raise UnsupportedRuntimeCommand(
                "agent-loop advice is not available in this runtime backend"
            )
        return registry.register(
            kind,
            owner=owner,
            priority=priority,
            callback=callback,
        )

    def unregister_agent_loop_advice(self, registration_id: int) -> bool:
        if self._manager is None:
            return False
        registry = getattr(self._manager, "agent_loop_advice", None)
        if registry is None:
            return False
        return bool(registry.unregister(registration_id))

    def customization_diagnostics(self):
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        diagnostics = getattr(self._manager, "customization_diagnostics", None)
        if diagnostics is None:
            raise UnsupportedRuntimeCommand(
                "customization diagnostics are not available in this runtime backend"
            )
        return diagnostics()

    async def stop(self) -> None:
        manager = self._manager
        self._manager = None
        if manager is None:
            return
        shutdown = getattr(manager, "shutdown_async", None)
        if shutdown is not None:
            await shutdown()
        else:
            manager.exit_conversation()


def create_runtime_backend(config=None) -> RuntimeBackend:
    """Create Zara's canonical conversational backend."""

    if config is None:
        from zara.config import get_config

        config = get_config()

    backend_name = str(config.get("agent", "backend", "langgraph")).strip().lower()
    if backend_name != "langgraph":
        raise ValueError(
            f"Unsupported agent backend {backend_name!r}; choose 'langgraph'"
        )

    def manager_factory():
        from zara.agent import AgentManager

        return AgentManager(config=config)

    return LangGraphRuntimeBackend(manager_factory)


class AgentRuntimeBackend(RuntimeBackend):
    """Backward-compatible facade over Zara's canonical LangGraph backend.

    RuntimeHost historically constructed ``AgentRuntimeBackend`` directly. The
    facade preserves that API while keeping backend construction in one place.
    Supplying ``manager_factory`` explicitly remains supported for tests and
    embedders.
    """

    def __init__(
        self,
        manager_factory: Optional[Callable[[], Any]] = None,
        *,
        config=None,
        router=None,
    ) -> None:
        if manager_factory is not None:
            self._delegate: RuntimeBackend = LangGraphRuntimeBackend(
                manager_factory,
                router=router,
            )
        else:
            self._delegate = create_runtime_backend(config)

    @property
    def principal_id(self) -> str:
        return self._delegate.principal_id

    def bind_event_publisher(self, publisher) -> None:
        self._delegate.bind_event_publisher(publisher)

    async def start(self) -> None:
        await self._delegate.start()

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
        return await self._delegate.submit_turn(
            text,
            turn_id=turn_id,
            conversation_id=conversation_id,
            context_ids=context_ids,
            system_context=system_context,
            conversation_history=conversation_history,
            latency_trace=latency_trace,
        )

    async def cancel_turn(self, turn_id: str) -> None:
        await self._delegate.cancel_turn(turn_id)

    def register_tools(self, tools) -> None:
        self._delegate.register_tools(tools)

    def unregister_tools(self, names) -> None:
        self._delegate.unregister_tools(names)

    def register_agent_loop_advice(
        self,
        kind: str,
        owner: str,
        priority: int,
        callback: Callable[..., Any],
    ) -> int:
        return self._delegate.register_agent_loop_advice(
            kind,
            owner,
            priority,
            callback,
        )

    def unregister_agent_loop_advice(self, registration_id: int) -> bool:
        return self._delegate.unregister_agent_loop_advice(registration_id)

    def customization_diagnostics(self):
        return self._delegate.customization_diagnostics()

    async def start_voice(self) -> None:
        await self._delegate.start_voice()

    async def stop_voice(self) -> None:
        await self._delegate.stop_voice()

    async def mute_speech(self, enabled: bool) -> None:
        await self._delegate.mute_speech(enabled)

    async def approve_tool(self, tool_run_id: str) -> None:
        await self._delegate.approve_tool(tool_run_id)

    async def reject_tool(self, tool_run_id: str, reason: str = "") -> None:
        await self._delegate.reject_tool(tool_run_id, reason)

    async def stop(self) -> None:
        await self._delegate.stop()


__all__ = [
    "AgentRuntimeBackend",
    "LangGraphRuntimeBackend",
    "RuntimeBackend",
    "RuntimeTurnResult",
    "UnsupportedRuntimeCommand",
    "create_runtime_backend",
]
