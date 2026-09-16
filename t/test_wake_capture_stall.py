"""Active-mode collection deadlines and capture-stall behavior (#881)."""

import asyncio
import queue
import threading

import numpy as np
import pytest

from zara.streaming_stt import StreamingVAD, VAD_CHUNK_SAMPLES, VADConfig
from zara.wake import WakeWordListener


class AutoClock:
    """Monotonic clock that advances a fixed step on every read."""

    def __init__(self, step=0.5, start=100.0):
        self.now = start
        self.step = step

    def __call__(self):
        value = self.now
        self.now += self.step
        return value


class FakeVADDetector:
    def __init__(self, pattern=None, default_prob=0.0):
        self.pattern = list(pattern or [])
        self.default_prob = default_prob
        self.index = 0

    def reset(self):
        self.index = 0

    def process_chunk(self, _audio_bytes):
        if self.index < len(self.pattern):
            probability = self.pattern[self.index]
        else:
            probability = self.default_prob
        self.index += 1
        return probability


def build_listener(pattern, default_prob=0.0, chunk_count=0, clock_step=0.5):
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.state = "PASSIVE"
    listener.audio_queue = queue.Queue(maxsize=64)
    listener.audio_ready = asyncio.Event()
    listener.stop_event = asyncio.Event()
    listener._shutdown_requested = threading.Event()
    listener._audio_notification_lock = threading.Lock()
    listener._audio_notification_pending = False
    listener._audio_epoch = 0
    listener.dropped_audio_chunks = 0
    listener.collection_status = "idle"
    listener.loop = asyncio.get_running_loop()
    listener._capture_stream = None
    listener.input_sample_rate = 16000
    listener.first_speech_timeout = 2.0
    listener.capture_stall_timeout = 1.0
    listener.max_utterance_duration = 1.0
    listener.silence_duration = 1.0
    listener.stop_on_interrupt = False
    listener.vad_config = VADConfig(
        min_speech_frames=1,
        trailing_silence_frames=2,
        max_utterance_frames=10**9,
    )
    listener._vad_factory = lambda config: StreamingVAD(
        config,
        FakeVADDetector(pattern, default_prob=default_prob),
    )
    listener._clock = AutoClock(step=clock_step)
    listener.logs: list[str] = []
    listener.log = listener.logs.append
    listener.ack_player = None
    listener.speaker = type("S", (), {"is_active": False, "_active_turns": set()})()
    listener.tts_task = None

    chunk = np.zeros((VAD_CHUNK_SAMPLES, 1), dtype=np.float32)
    for _ in range(chunk_count):
        try:
            listener.audio_queue.put_nowait((0, chunk.copy()))
        except queue.Full:
            break
    return listener


def test_first_speech_deadline_returns_with_zero_frames():
    async def scenario():
        listener = build_listener(pattern=[], default_prob=0.0, chunk_count=0)

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "first_speech_timeout"

    asyncio.run(scenario())


def test_mid_utterance_stall_returns_within_stall_bound():
    async def scenario():
        listener = build_listener(pattern=[0.9], default_prob=0.0, chunk_count=1)

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "capture_stall"
        assert any("stall" in message.lower() for message in listener.logs)

    asyncio.run(scenario())


def test_endless_speech_hits_utterance_wall_clock():
    async def scenario():
        listener = build_listener(
            pattern=[0.9],
            default_prob=0.9,
            chunk_count=200,
        )

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "max_utterance_timeout"
        assert any("utterance" in message.lower() for message in listener.logs)

    asyncio.run(scenario())


def test_utterance_cap_binds_when_stall_timeout_is_larger():
    """With a huge stall timeout the utterance wall-clock still returns."""
    async def scenario():
        listener = build_listener(pattern=[0.9], default_prob=0.0, chunk_count=1)
        listener.capture_stall_timeout = 3600.0

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "max_utterance_timeout"

    asyncio.run(scenario())
