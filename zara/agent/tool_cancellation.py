"""Canonical invocation-scoped cooperative cancellation for Zara tools."""

from __future__ import annotations

import threading
from types import MappingProxyType
from typing import Annotated, Mapping, Optional

from langgraph.prebuilt import InjectedState

_CANCELLATION_STATE_KEY = "__zara_tool_cancellation"


class ToolCancellation:
    """Read-only cancellation view for one exact tool invocation."""

    __slots__ = ("__event",)

    def __init__(self, event: threading.Event) -> None:
        self.__event = event

    @property
    def cancelled(self) -> bool:
        return self.__event.is_set()

    def wait(self, timeout: Optional[float] = None) -> bool:
        return self.__event.wait(timeout=timeout)


ToolCancellationArg = Annotated[
    ToolCancellation,
    InjectedState(_CANCELLATION_STATE_KEY),
]
"""Model-hidden cancellation dependency for cooperatively cancellable tools."""


class _ToolCancellationSignal:
    __slots__ = ("_event", "view", "_injected_state")

    def __init__(self) -> None:
        self._event = threading.Event()
        self.view = ToolCancellation(self._event)
        self._injected_state = MappingProxyType(
            {_CANCELLATION_STATE_KEY: self.view}
        )

    @property
    def injected_state(self) -> Mapping[str, ToolCancellation]:
        return self._injected_state

    def cancel(self) -> None:
        self._event.set()


def new_tool_cancellation_signal() -> _ToolCancellationSignal:
    """Create Core-owned state for one approved tool invocation."""
    return _ToolCancellationSignal()


__all__ = ["ToolCancellation", "ToolCancellationArg"]
