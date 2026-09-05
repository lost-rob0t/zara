"""Canonical invocation-scoped cooperative cancellation for Zara tools."""

from __future__ import annotations

import contextvars
import threading
from contextlib import contextmanager
from typing import Any, Iterator, Mapping, Optional

from langchain_core.runnables.config import ensure_config

_CONFIG_KEY = "_zara_tool_cancellation"


class ToolCancellation:
    """Read-only cancellation view exposed to the currently running tool."""

    __slots__ = ("__event",)

    def __init__(self, event: threading.Event) -> None:
        self.__event = event

    @property
    def cancelled(self) -> bool:
        return self.__event.is_set()

    def wait(self, timeout: Optional[float] = None) -> bool:
        return self.__event.wait(timeout=timeout)


class _ToolCancellationSignal:
    __slots__ = ("_event", "view")

    def __init__(self) -> None:
        self._event = threading.Event()
        self.view = ToolCancellation(self._event)

    def cancel(self) -> None:
        self._event.set()

    def invocation_config(
        self,
        config: Optional[Mapping[str, Any]],
    ) -> dict[str, Any]:
        """Return an invocation config carrying this exact read-only token."""
        invocation = dict(config or {})
        configurable = dict(invocation.get("configurable") or {})
        configurable[_CONFIG_KEY] = self.view
        invocation["configurable"] = configurable
        return invocation


_current_tool_cancellation: contextvars.ContextVar[Optional[ToolCancellation]] = (
    contextvars.ContextVar("zara_tool_cancellation", default=None)
)


def current_tool_cancellation() -> Optional[ToolCancellation]:
    """Return cancellation state for the exact current tool invocation."""
    cancellation = _current_tool_cancellation.get()
    if cancellation is not None:
        return cancellation

    try:
        configurable = ensure_config().get("configurable") or {}
    except Exception:
        return None
    candidate = configurable.get(_CONFIG_KEY)
    if isinstance(candidate, ToolCancellation):
        return candidate
    return None


@contextmanager
def tool_cancellation_scope() -> Iterator[_ToolCancellationSignal]:
    """Bind one private signal around Zara's canonical tool execution boundary."""
    signal = _ToolCancellationSignal()
    token = _current_tool_cancellation.set(signal.view)
    try:
        yield signal
    finally:
        _current_tool_cancellation.reset(token)


__all__ = ["ToolCancellation", "current_tool_cancellation", "tool_cancellation_scope"]
