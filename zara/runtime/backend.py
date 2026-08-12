"""Application-service backend owned by :class:`zara.runtime.host.RuntimeHost`.

The host owns lifecycle, threading, turn correlation, and cancellation. A
backend owns the existing Zara application services used to execute a turn.
This keeps the host testable without inventing a second assistant runtime.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional


class UnsupportedRuntimeCommand(RuntimeError):
    """Raised when a backend does not implement an optional capability."""


@dataclass(frozen=True)
class RuntimeTurnResult:
    response: str = ""
    tool_results: tuple[dict[str, Any], ...] = field(default_factory=tuple)


class RuntimeBackend:
    """Async application-service contract used by RuntimeHost.

    Concrete backends execute exclusively on the RuntimeHost thread/asyncio
    loop unless they deliberately delegate work to an existing worker/actor.
    """

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


class AgentRuntimeBackend(RuntimeBackend):
    """Thin adapter over Zara's existing :class:`AgentManager`.

    The manager is constructed lazily inside ``start()``. RuntimeHost invokes
    ``start()`` on its dedicated worker thread, so provider/tool/memory setup is
    never performed synchronously by a Qt caller. A future desktop bootstrap
    may inject an AgentManager factory that also wires the existing Prolog
    engine; this adapter deliberately does not duplicate wake.py startup code.
    """

    def __init__(self, manager_factory: Optional[Callable[[], Any]] = None) -> None:
        self._manager_factory = manager_factory
        self._manager = None

    async def start(self) -> None:
        if self._manager is not None:
            return
        if self._manager_factory is None:
            from zara.agent import AgentManager

            self._manager_factory = AgentManager
        self._manager = self._manager_factory()

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
            # Context attachment resolution belongs to #88. Reject rather than
            # silently pretending the context was used.
            raise UnsupportedRuntimeCommand(
                "context attachments are not wired into the runtime backend yet"
            )

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
        # Cancelling the RuntimeHost asyncio task is the current concrete
        # cancellation path for buffered AgentManager calls. Provider/tool
        # specific cancellation hooks can be added behind this method without
        # changing the desktop command API.
        return None

    async def stop(self) -> None:
        if self._manager is not None:
            try:
                self._manager.exit_conversation()
            finally:
                self._manager = None
