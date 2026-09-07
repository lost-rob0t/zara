"""Core-owned context for one running agent tool invocation."""

from __future__ import annotations

import asyncio
from contextvars import ContextVar, Token
from dataclasses import dataclass, field
from typing import Optional


@dataclass(frozen=True)
class ToolInvocationContext:
    """Immutable invocation identity with a Core-owned cancellation signal."""

    principal_id: str
    turn_id: str
    conversation_id: Optional[str]
    tool_run_id: str
    _cancel_event: asyncio.Event = field(
        default_factory=asyncio.Event,
        repr=False,
        compare=False,
    )

    @property
    def cancelled(self) -> bool:
        return self._cancel_event.is_set()

    async def wait_cancelled(self) -> None:
        await self._cancel_event.wait()

    def _mark_cancelled(self) -> None:
        self._cancel_event.set()


_CURRENT_TOOL_INVOCATION: ContextVar[Optional[ToolInvocationContext]] = ContextVar(
    "zara_current_tool_invocation",
    default=None,
)


def current_tool_invocation() -> Optional[ToolInvocationContext]:
    """Return the current Core-owned tool invocation, if one is running."""

    return _CURRENT_TOOL_INVOCATION.get()


def _bind_tool_invocation(context: ToolInvocationContext) -> Token:
    return _CURRENT_TOOL_INVOCATION.set(context)


def _reset_tool_invocation(token: Token) -> None:
    _CURRENT_TOOL_INVOCATION.reset(token)


__all__ = ["ToolInvocationContext", "current_tool_invocation"]
