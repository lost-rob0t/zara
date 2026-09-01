from __future__ import annotations

import threading
import time
import uuid
from dataclasses import dataclass


@dataclass(frozen=True)
class SpeechActivity:
    token: str
    source: str
    started_at: float


class SpeechActivityRegistry:
    """Process-wide playback marker used by voice VAD and barge-in paths."""

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._active: dict[str, SpeechActivity] = {}

    def begin(self, *, source: str) -> SpeechActivity:
        activity = SpeechActivity(
            token=uuid.uuid4().hex,
            source=str(source or "speech")[:128],
            started_at=time.monotonic(),
        )
        with self._lock:
            self._active[activity.token] = activity
        return activity

    def end(self, token: str) -> bool:
        with self._lock:
            return self._active.pop(str(token), None) is not None

    @property
    def active(self) -> bool:
        with self._lock:
            return bool(self._active)

    def snapshot(self) -> tuple[SpeechActivity, ...]:
        with self._lock:
            return tuple(self._active.values())

    def clear(self) -> None:
        with self._lock:
            self._active.clear()


speech_activity = SpeechActivityRegistry()


__all__ = ["SpeechActivity", "SpeechActivityRegistry", "speech_activity"]
