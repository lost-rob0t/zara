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

from . import events


logger = logging.getLogger(__name__)


class UnsupportedRuntimeCommand(RuntimeError):
    """Raised when a backend does not implement an optional capability."""


@dataclass(frozen=True)
class RuntimeTurnResult:
    response: str = ""
    tool_results: tuple[dict[str, Any], ...] = field(default_factory=tuple)
    metadata: dict[str, Any] = field(default_factory=dict)


class RuntimeBackend:
    """Async application-service contract used by RuntimeHost."""

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

    def __init__(self, manager_factory: Optional[Callable[[], Any]] = None) -> None:
        self._manager_factory = manager_factory
        self._manager = None
        self._publisher = None
        self._timer_event_task: Optional[asyncio.Task] = None

    def bind_event_publisher(self, publisher) -> None:
        self._publisher = publisher

    async def start(self) -> None:
        if self._manager is not None:
            return
        if self._manager_factory is None:
            from zara.agent import AgentManager

            self._manager_factory = AgentManager
        self._manager = self._manager_factory()
        prolog_engine = getattr(self._manager, "prolog_engine", None)
        if callable(getattr(prolog_engine, "drain_timer_events", None)):
            self._timer_event_task = asyncio.create_task(
                self._pump_timer_events(),
                name="zara-timer-events",
            )

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
    ) -> RuntimeTurnResult:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not wired into the runtime backend yet"
            )

        prolog_engine = getattr(self._manager, "prolog_engine", None)
        if prolog_engine is not None:
            try:
                intent = await asyncio.to_thread(prolog_engine.resolve_intent, text)
                if (
                    intent is not None
                    and intent.kind == "prolog"
                    and intent.name != "ask"
                ):
                    response = await asyncio.to_thread(
                        prolog_engine.execute_intent_with_response,
                        intent.name,
                        intent.args,
                    )
                    if response is not None:
                        await self._publish_timer_events()
                        return RuntimeTurnResult(response=response.strip())
            except Exception:
                logger.warning("Prolog command path failed; using agent fallback", exc_info=True)

        result = await self._manager.process_async(
            text,
            turn_id=turn_id,
            conversation_id=conversation_id,
        )
        raw_tool_results = result.get("tool_results", [])
        return RuntimeTurnResult(
            response=str(result.get("response", "")),
            tool_results=tuple(
                item if isinstance(item, dict) else {"result": item}
                for item in raw_tool_results
            ),
        )

    async def cancel_turn(self, turn_id: str) -> None:
        return None

    def register_tools(self, tools) -> None:
        if self._manager is None:
            raise RuntimeError("runtime backend is not started")
        self._manager.tool_registry.register_tools(list(tools))

    def unregister_tools(self, names) -> None:
        if self._manager is not None:
            self._manager.tool_registry.unregister_tools(list(names))

    async def stop(self) -> None:
        timer_event_task = self._timer_event_task
        self._timer_event_task = None
        if timer_event_task is not None:
            timer_event_task.cancel()
            try:
                await timer_event_task
            except asyncio.CancelledError:
                pass
        manager = self._manager
        self._manager = None
        if manager is None:
            return
        shutdown = getattr(manager, "shutdown_async", None)
        if shutdown is not None:
            await shutdown()
        else:
            manager.exit_conversation()

    async def _pump_timer_events(self) -> None:
        while True:
            try:
                await self._publish_timer_events()
            except Exception:
                logger.warning("Timer event drain failed; retrying", exc_info=True)
            await asyncio.sleep(0.05)

    async def _publish_timer_events(self) -> None:
        manager = self._manager
        publisher = self._publisher
        if manager is None or publisher is None:
            return
        prolog_engine = getattr(manager, "prolog_engine", None)
        if prolog_engine is None:
            return
        records = await asyncio.to_thread(prolog_engine.drain_timer_events)
        for record in records:
            if record.kind == "scheduled":
                publisher(
                    events.TimerScheduled(
                        timer_id=record.timer_id,
                        name=record.name,
                        created_at_ns=record.created_at_ns,
                        due_at_ns=record.due_at_ns,
                        revision=record.revision,
                    )
                )
            elif record.kind == "fired":
                publisher(
                    events.TimerFired(
                        timer_id=record.timer_id,
                        name=record.name,
                        created_at_ns=record.created_at_ns,
                        due_at_ns=record.due_at_ns,
                        fired_at_ns=record.fired_at_ns,
                        revision=record.revision,
                        message=record.message,
                    )
                )


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
    ) -> None:
        if manager_factory is not None:
            self._delegate: RuntimeBackend = LangGraphRuntimeBackend(manager_factory)
        else:
            self._delegate = create_runtime_backend(config)

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
    ) -> RuntimeTurnResult:
        return await self._delegate.submit_turn(
            text,
            turn_id=turn_id,
            conversation_id=conversation_id,
            context_ids=context_ids,
        )

    async def cancel_turn(self, turn_id: str) -> None:
        await self._delegate.cancel_turn(turn_id)

    def register_tools(self, tools) -> None:
        self._delegate.register_tools(tools)

    def unregister_tools(self, names) -> None:
        self._delegate.unregister_tools(names)

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
