"""True barge-in: cancel stale LLM/TTS work on user speech (ZARA-026).

Allows the user to interrupt Zara at any point — during acknowledgement,
LLM generation, TTS synthesis, or playback. Uses Silero VAD with
playback-aware gating to distinguish user speech from Zara's own speaker
output (echo suppression).

The ``BargeInMonitor`` runs during TTS/acknowledgement playback and
feeds mic chunks through a Silero VAD with an elevated threshold. On
confirmed user speech onset, it:

1. Stops TTS/acknowledgement playback immediately.
2. Emits ``CancelTurn(turn_id)`` through the TurnCoordinator.
3. Propagates cancellation to active LLM streams, TTS synthesis, and
   pending notifications.
4. Prevents cancelled/stale text or audio from resuming after the new
   turn starts.

Echo suppression uses a playback-aware gating strategy: when playback is
active, the VAD threshold is elevated and more consecutive speech frames
are required before triggering barge-in. The interface is open for
acoustic echo cancellation (AEC) — a future AEC module can replace the
threshold-based gating without changing the monitor.
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np

from zara.streaming_stt import StreamingVAD, VADConfig, VAD_CHUNK_SAMPLES

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class BargeInConfig:
    """Configuration for barge-in detection during playback."""
    enabled: bool = True
    """Master toggle for barge-in during playback."""
    playback_vad_threshold: float = 0.7
    """Elevated VAD threshold during playback (vs 0.5 default)."""
    playback_min_speech_frames: int = 6
    """More consecutive speech frames required during playback to reduce echo false-positives."""
    idle_vad_threshold: float = 0.5
    """Normal VAD threshold when not playing back."""
    idle_min_speech_frames: int = 4
    """Consecutive speech frames when not playing back."""
    max_utterance_frames: int = 938
    """Hard cap on speech frames (30s at 32ms/chunk)."""
    trailing_silence_frames: int = 16
    """Trailing silence frames after speech to trigger speech_ended."""
    no_speech_timeout_frames: int = 312
    """No-speech timeout during playback (10s)."""
    pre_speech_buffer_chunks: int = 10
    """Rolling pre-speech buffer."""


@dataclass
class BargeInResult:
    """Result of a barge-in detection attempt."""
    turn_id: str
    detected: bool = False
    playback_active: bool = False
    false_positive_suppressed: bool = False
    latency_ms: float = 0.0


class BargeInMonitor:
    """Monitors mic audio during playback and triggers barge-in.

    The monitor wraps a ``StreamingVAD`` that swaps between elevated
    (playback-active) and normal (idle) thresholds. When playback starts,
    the monitor begins feeding mic chunks to the VAD. On confirmed speech
    onset, it invokes the cancellation callback and stops playback.

    The cancellation callback receives the ``turn_id`` of the interrupted
    turn. The caller is responsible for stopping TTS, cancelling the LLM
    stream, and emitting ``CancelTurn`` through the coordinator.

    Echo suppression is handled by the elevated threshold and higher
    ``min_speech_frames`` requirement during playback. A future AEC
    module can replace this by pre-filtering audio before it reaches
    the VAD.
    """

    def __init__(
        self,
        config: Optional[BargeInConfig] = None,
        vad_detector: Optional[Any] = None,
        on_barge_in: Optional[Callable[[str], None]] = None,
    ):
        self.config = config or BargeInConfig()
        self._vad_detector = vad_detector
        self._on_barge_in = on_barge_in
        self._playback_active = False
        self._current_turn_id: Optional[str] = None
        self._monitoring = False
        self._lock = threading.Lock()
        self._cancelled_turns: set[str] = set()

        self._playback_vad = StreamingVAD(
            VADConfig(
                vad_threshold=self.config.playback_vad_threshold,
                min_speech_frames=self.config.playback_min_speech_frames,
                trailing_silence_frames=self.config.trailing_silence_frames,
                max_utterance_frames=self.config.max_utterance_frames,
                no_speech_timeout_frames=self.config.no_speech_timeout_frames,
                pre_speech_buffer_chunks=self.config.pre_speech_buffer_chunks,
            ),
            vad_detector=vad_detector,
        )

    def on_playback_start(self, turn_id: str) -> None:
        """Called when TTS/acknowledgement playback starts for a turn.

        Begins monitoring mic audio for barge-in. If a previous turn's
        playback is still active, it is cancelled first.
        """
        with self._lock:
            if self._playback_active and self._current_turn_id:
                self._trigger_barge_in(self._current_turn_id)
            self._playback_active = True
            self._current_turn_id = turn_id
            self._monitoring = True
            self._playback_vad.start_turn(turn_id)
            logger.info("[BargeIn] monitoring started for turn %s", turn_id)

    def on_playback_stop(self) -> None:
        """Called when playback ends normally (not via barge-in)."""
        with self._lock:
            self._playback_active = False
            self._monitoring = False
            self._current_turn_id = None
            self._playback_vad.reset()

    def feed_mic_chunk(self, chunk: np.ndarray) -> BargeInResult:
        """Feed a mic chunk during playback monitoring.

        Returns a ``BargeInResult`` indicating whether barge-in was
        detected. If barge-in is detected, the cancellation callback is
        invoked and the monitor stops.
        """
        with self._lock:
            if not self._monitoring or not self._playback_active:
                return BargeInResult(
                    turn_id=self._current_turn_id or "",
                    detected=False,
                    playback_active=self._playback_active,
                    false_positive_suppressed=(
                        self._current_turn_id is None
                        and len(self._cancelled_turns) > 0
                    ),
                )
            if self._current_turn_id in self._cancelled_turns:
                return BargeInResult(
                    turn_id=self._current_turn_id,
                    detected=False,
                    playback_active=False,
                    false_positive_suppressed=True,
                )

        start_ns = time.monotonic_ns()
        events = self._playback_vad.feed(chunk)

        from zara.streaming_stt import SpeechStarted

        for event in events:
            if isinstance(event, SpeechStarted):
                latency_ms = (time.monotonic_ns() - start_ns) / 1_000_000
                self._trigger_barge_in(event.turn_id or self._current_turn_id or "")
                return BargeInResult(
                    turn_id=event.turn_id or self._current_turn_id or "",
                    detected=True,
                    playback_active=True,
                    latency_ms=latency_ms,
                )

        return BargeInResult(
            turn_id=self._current_turn_id or "",
            detected=False,
            playback_active=True,
        )

    def _trigger_barge_in(self, turn_id: str) -> None:
        with self._lock:
            if turn_id in self._cancelled_turns:
                return
            self._cancelled_turns.add(turn_id)
            self._playback_active = False
            self._monitoring = False
            self._current_turn_id = None

        logger.info("[BargeIn] barge-in detected for turn %s", turn_id)
        if self._on_barge_in is not None:
            try:
                self._on_barge_in(turn_id)
            except Exception as error:
                logger.warning("[BargeIn] cancellation callback error: %s", error)

    def cancel_turn(self, turn_id: str) -> None:
        """Mark a turn as cancelled without triggering barge-in.

        Used when cancellation comes from another source (e.g., stop
        phrase, timeout) so the monitor knows the turn is already
        cancelled and doesn't double-trigger.
        """
        with self._lock:
            self._cancelled_turns.add(turn_id)
            if self._current_turn_id == turn_id:
                self._playback_active = False
                self._monitoring = False
                self._current_turn_id = None

    @property
    def is_monitoring(self) -> bool:
        return self._monitoring

    @property
    def playback_active(self) -> bool:
        return self._playback_active

    def reset(self) -> None:
        """Clear all state for a new session."""
        with self._lock:
            self._cancelled_turns.clear()
            self._playback_active = False
            self._monitoring = False
            self._current_turn_id = None
            self._playback_vad.reset()


def interrupted_assistant_message(text: str, turn_id: str) -> dict:
    """Create a conversation history entry for interrupted assistant text.

    Interrupted text is stored with ``interrupted=True`` so it is not
    treated as a complete response for context. Only the partial text
    that was actually synthesized/played should be included.
    """
    return {
        "role": "assistant",
        "content": text,
        "interrupted": True,
        "turn_id": turn_id,
    }
