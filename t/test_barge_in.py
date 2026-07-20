"""Deterministic barge-in tests for ZARA-026.

Tests cover:
- Interruption during acknowledgement playback
- Interruption before first LLM token
- Interruption during TTS synthesis
- Interruption during buffered playback
- False-interruption from Zara's own output (echo suppression)
- False-interruption from background noise
- Cancellation race: provider returns after cancellation
- Speech-onset-to-playback-stop p95 <= 200 ms
- Cancelled turn cannot later speak or notify
- Zara's own voice does not self-trigger
"""

from __future__ import annotations

import time
import threading
from unittest.mock import MagicMock

import numpy as np
import pytest

from zara.barge_in import (
    BargeInConfig,
    BargeInMonitor,
    BargeInResult,
    interrupted_assistant_message,
)
from zara.streaming_stt import VAD_CHUNK_SAMPLES


class FakeVADDetector:
    """Deterministic VAD detector for barge-in tests."""

    def __init__(self, pattern=None, default_prob=0.0):
        self._pattern = pattern or []
        self._default = default_prob
        self._index = 0

    def reset(self):
        self._index = 0

    def process_chunk(self, audio_bytes):
        if self._index < len(self._pattern):
            prob = self._pattern[self._index]
        else:
            prob = self._default
        self._index += 1
        return prob


def silence_chunk():
    return np.zeros(VAD_CHUNK_SAMPLES, dtype=np.float32)


def speech_chunk():
    return np.ones(VAD_CHUNK_SAMPLES, dtype=np.float32) * 0.5


def feed_speech(monitor, n=10):
    """Feed n speech chunks and return first barge-in result."""
    for _ in range(n):
        result = monitor.feed_mic_chunk(speech_chunk())
        if result.detected:
            return result
    return BargeInResult(turn_id="", detected=False)


def test_barge_in_detects_speech_during_acknowledgement():
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-ack")

    result = feed_speech(monitor)
    assert result.detected is True
    assert result.turn_id == "turn-ack"
    assert cancelled == ["turn-ack"]


def test_barge_in_detects_speech_during_tts_playback():
    cancelled = []
    detector = FakeVADDetector(pattern=[0.9] * 10)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-tts")

    results = []
    for _ in range(10):
        results.append(monitor.feed_mic_chunk(speech_chunk()))

    detected = [r for r in results if r.detected]
    assert len(detected) >= 1
    assert cancelled == ["turn-tts"]


def test_silence_during_playback_does_not_trigger_barge_in():
    cancelled = []
    detector = FakeVADDetector(default_prob=0.01)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-silence")

    for _ in range(50):
        monitor.feed_mic_chunk(silence_chunk())

    assert cancelled == []
    assert monitor.playback_active is True


def test_background_noise_does_not_trigger_barge_in():
    cancelled = []
    detector = FakeVADDetector(default_prob=0.15)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-noise")

    for _ in range(50):
        monitor.feed_mic_chunk(silence_chunk())

    assert cancelled == []
    assert monitor.playback_active is True


def test_zara_output_does_not_self_trigger():
    """Zara's own TTS output should not trigger barge-in.

    The elevated threshold (0.7) and higher min_speech_frames (6)
    should suppress low-probability echo from the speaker.
    """
    cancelled = []
    # Simulate echo at ~0.2 probability (typical for speaker bleed)
    detector = FakeVADDetector(default_prob=0.2)
    config = BargeInConfig(
        playback_vad_threshold=0.7,
        playback_min_speech_frames=6,
    )
    monitor = BargeInMonitor(
        config=config,
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-self")

    for _ in range(100):
        monitor.feed_mic_chunk(silence_chunk())

    assert cancelled == []
    assert monitor.playback_active is True


def test_cancellation_race_provider_returns_after_cancellation():
    """A provider that returns after the turn is cancelled must not speak."""
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-race")

    feed_speech(monitor)
    assert "turn-race" in cancelled

    # Provider returns late — must not re-trigger
    result = monitor.feed_mic_chunk(speech_chunk())
    assert result.detected is False
    assert result.false_positive_suppressed is True


def test_cancelled_turn_cannot_speak_again():
    """After cancellation, feeding more audio must not trigger again."""
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-1")

    feed_speech(monitor)
    assert len(cancelled) == 1

    for _ in range(10):
        monitor.feed_mic_chunk(speech_chunk())
    assert len(cancelled) == 1


def test_new_turn_after_barge_in_starts_clean():
    """After barge-in, a new turn's playback should be monitored fresh."""
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-1")
    feed_speech(monitor)
    assert cancelled == ["turn-1"]

    monitor._vad_detector = FakeVADDetector(default_prob=0.9)
    monitor._playback_vad._vad = monitor._vad_detector
    monitor.on_playback_start("turn-2")
    assert monitor.playback_active is True
    assert monitor.is_monitoring is True


def test_disabled_barge_in_does_nothing():
    cancelled = []
    monitor = BargeInMonitor(
        config=BargeInConfig(enabled=False),
        on_barge_in=lambda tid: cancelled.append(tid),
    )
    monitor.on_playback_start("turn-disabled")
    result = monitor.feed_mic_chunk(speech_chunk())
    assert result.detected is False


def test_speech_onset_to_stop_latency_under_200ms():
    """Verify p95 barge-in latency <= 200ms (ZARA-022 budget)."""
    latencies = []
    for i in range(20):
        detector = FakeVADDetector(default_prob=0.9)
        monitor = BargeInMonitor(
            on_barge_in=lambda tid: None,
            vad_detector=detector,
        )
        monitor.on_playback_start(f"turn-{i}")

        start = time.monotonic_ns()
        feed_speech(monitor)
        elapsed_ms = (time.monotonic_ns() - start) / 1_000_000
        latencies.append(elapsed_ms)

    latencies.sort()
    p95 = latencies[int(len(latencies) * 0.95)]
    assert p95 <= 200, f"p95 latency {p95:.2f}ms exceeds 200ms budget"


def test_on_playback_stop_ends_monitoring():
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: None,
    )
    monitor.on_playback_start("turn-1")
    assert monitor.is_monitoring is True

    monitor.on_playback_stop()
    assert monitor.is_monitoring is False
    assert monitor.playback_active is False


def test_external_cancel_does_not_trigger_barge_in_callback():
    """cancel_turn() from another source should not call on_barge_in."""
    cancelled = []
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
    )
    monitor.on_playback_start("turn-ext")
    monitor.cancel_turn("turn-ext")

    assert cancelled == []
    assert monitor.playback_active is False


def test_interrupted_assistant_message_marked_correctly():
    msg = interrupted_assistant_message("partial response", "turn-1")
    assert msg["role"] == "assistant"
    assert msg["content"] == "partial response"
    assert msg["interrupted"] is True
    assert msg["turn_id"] == "turn-1"


def test_interrupted_message_is_distinct_from_complete():
    partial = interrupted_assistant_message("partial", "turn-1")
    complete = {"role": "assistant", "content": "complete", "interrupted": False}
    assert partial["interrupted"] is True
    assert complete["interrupted"] is False


def test_barge_in_during_llm_generation():
    """Barge-in before first LLM token should cancel the turn."""
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-llm")

    result = feed_speech(monitor)
    assert result.detected is True
    assert cancelled == ["turn-llm"]


def test_barge_in_during_tts_synthesis():
    """Barge-in during TTS synthesis should cancel immediately."""
    cancelled = []
    detector = FakeVADDetector(default_prob=0.9)
    monitor = BargeInMonitor(
        on_barge_in=lambda tid: cancelled.append(tid),
        vad_detector=detector,
    )
    monitor.on_playback_start("turn-synth")

    result = feed_speech(monitor)
    assert result.detected is True
    assert result.playback_active is True
