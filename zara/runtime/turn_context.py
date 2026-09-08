"""Core-owned turn context used to fence turn-scoped plugin composition."""

from __future__ import annotations

import contextvars
import threading
from contextlib import contextmanager
from typing import Iterator, Optional


class TurnCapabilityLease:
    """Opaque lifetime token for one RuntimeHost-owned turn."""

    def __init__(self, turn_id: str) -> None:
        if not isinstance(turn_id, str) or not turn_id:
            raise ValueError("turn_id must be a non-empty string")
        self.turn_id = turn_id
        self._active = True
        self._lock = threading.RLock()

    @property
    def active(self) -> bool:
        with self._lock:
            return self._active

    def invalidate(self) -> None:
        with self._lock:
            self._active = False

    @contextmanager
    def registration(self) -> Iterator[None]:
        """Serialize registration with invalidation and fail closed if stale."""
        with self._lock:
            if not self._active:
                raise RuntimeError("turn capability context was cancelled or became stale")
            yield


_CURRENT_TURN_CAPABILITY_LEASE: contextvars.ContextVar[Optional[TurnCapabilityLease]] = (
    contextvars.ContextVar("zara_current_turn_capability_lease", default=None)
)


@contextmanager
def bind_turn_capability_lease(lease: TurnCapabilityLease) -> Iterator[None]:
    if not isinstance(lease, TurnCapabilityLease):
        raise TypeError("lease must be a TurnCapabilityLease")
    token = _CURRENT_TURN_CAPABILITY_LEASE.set(lease)
    try:
        yield
    finally:
        _CURRENT_TURN_CAPABILITY_LEASE.reset(token)


def current_turn_capability_lease() -> Optional[TurnCapabilityLease]:
    return _CURRENT_TURN_CAPABILITY_LEASE.get()


__all__ = [
    "TurnCapabilityLease",
    "bind_turn_capability_lease",
    "current_turn_capability_lease",
]