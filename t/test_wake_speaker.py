"""PcmStreamSpeaker: client-owned daemon audio output playback (#244)."""

from __future__ import annotations

import logging
import queue
import time
import threading
from unittest.mock import patch

import numpy as np
import pytest

from zara.wake_daemon import PcmStreamSpeaker


def build_speaker() -> PcmStreamSpeaker:
    speaker = PcmStreamSpeaker()
    speaker._ensure_stream = lambda *args, **kwargs: None
    speaker._ensure_writer = lambda: None
    speaker._stream = FakeSinkStream()
    return speaker


def test_start_activates_turn_and_finish_deactivates():
    speaker = build_speaker()

    speaker.start(
        format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        conversation_id="conv-1",
        turn_id="turn-1",
        stream_id="tts-1",
    )
    assert speaker.is_active

    speaker.finish(turn_id="turn-1", stream_id="tts-1")
    assert not speaker.is_active


def test_cancel_drops_queued_audio_immediately():
    speaker = build_speaker()
    speaker.start(
        format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        conversation_id="conv-1",
        turn_id="turn-1",
        stream_id="tts-1",
    )

    speaker.chunk(b"\x01\x00" * 128, turn_id="turn-1", seq=0)
    speaker.chunk(b"\x02\x00" * 128, turn_id="turn-1", seq=1)
    speaker.cancel(turn_id="turn-1")

    assert not speaker.is_active
    assert speaker._queue.empty()
    assert speaker.cancelled_turns == ["turn-1"]
    assert speaker.chunks_played == 2


def test_chunks_for_cancelled_turn_are_dropped():
    speaker = build_speaker()
    speaker.start(
        format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        conversation_id="conv-1",
        turn_id="turn-1",
        stream_id="tts-1",
    )
    speaker.cancel(turn_id="turn-1")

    speaker.chunk(b"\x01\x00" * 64, turn_id="turn-1", seq=2)

    assert speaker._queue.empty()


def test_cancel_active_cancels_every_live_turn():
    speaker = build_speaker()
    speaker.start(
        format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        conversation_id="conv-1",
        turn_id="turn-1",
        stream_id="tts-1",
    )
    speaker.start(
        format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        conversation_id="conv-1",
        turn_id="turn-2",
        stream_id="tts-2",
    )
    speaker.chunk(b"\x00\x00", turn_id="turn-2", seq=0)

    speaker.cancel_active()

    assert not speaker.is_active
    assert speaker._queue.empty()
    assert sorted(speaker.cancelled_turns) == ["turn-1", "turn-2"]


def test_chunk_without_active_turn_is_ignored():
    speaker = build_speaker()

    speaker.chunk(b"\x01\x00" * 64, turn_id="turn-none", seq=0)

    assert speaker._queue.empty()
    assert speaker.chunks_played == 0


def test_utterance_frames_pad_partial_final_frame():
    import numpy as np

    from zara.wake_daemon import utterance_frames

    audio = np.zeros((600, 1), dtype=np.float32)

    frames = utterance_frames(audio)

    assert len(frames) == 2
    assert len(frames[1]) == 1024


class FakeSinkStream:
    def __init__(self):
        self.started = False
        self.writes: list[bytes] = []
        self.closed = False

    def start(self):
        self.started = True
        return self

    def write(self, block):
        self.writes.append(bytes(block))

    def close(self):
        self.closed = True


def test_open_timeout_is_bounded_and_degrades():
    def slow_factory(_sample_rate):
        time.sleep(0.5)
        return FakeSinkStream()

    speaker = PcmStreamSpeaker(stream_factory=slow_factory, open_timeout=0.1)
    began = time.monotonic()
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-slow")
    elapsed = time.monotonic() - began

    assert elapsed < 0.4
    assert speaker._stream is None
    assert speaker.open_failures == 1
    speaker.close()


def test_late_completed_stream_is_closed_and_not_adopted():
    release = threading.Event()
    created: list[FakeSinkStream] = []

    def slow_factory(_sample_rate):
        stream = FakeSinkStream()
        created.append(stream)
        release.wait(1.0)
        return stream

    speaker = PcmStreamSpeaker(stream_factory=slow_factory, open_timeout=0.1)
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-late")
    assert speaker._stream is None
    release.set()
    deadline = time.monotonic() + 2.0
    while time.monotonic() < deadline and not (created and created[0].closed):
        time.sleep(0.01)

    assert created and created[0].closed
    speaker.close()


def test_open_failure_is_logged_and_does_not_raise(caplog):
    def broken_factory(_sample_rate):
        raise RuntimeError("no Pulse sink reachable")

    speaker = PcmStreamSpeaker(stream_factory=broken_factory)
    with caplog.at_level(logging.WARNING, logger="zara.wake_daemon"):
        speaker.start(format={"sample_rate": 24000}, turn_id="turn-bad")

    assert speaker._stream is None
    assert speaker.open_failures == 1
    assert any("playback" in record.getMessage().lower() for record in caplog.records)
    speaker.finish(turn_id="turn-bad")
    speaker.close()


def test_chunks_queue_only_after_successful_open():
    state = {"fail": True}

    def factory(_sample_rate):
        if state["fail"]:
            raise RuntimeError("no sink yet")
        return FakeSinkStream()

    speaker = PcmStreamSpeaker(stream_factory=factory)
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-1")
    speaker.chunk(b"\x00\x00", turn_id="turn-1", seq=0)

    assert speaker._queue.empty()
    assert speaker.chunks_dropped == 1
    assert speaker.chunks_played == 0

    state["fail"] = False
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-2")
    assert speaker._stream is not None
    speaker.chunk(b"\x01\x00", turn_id="turn-2", seq=0)

    assert speaker.chunks_played == 1
    speaker.close()


def test_successful_open_resets_failure_streak():
    calls = {"count": 0}

    def flaky_factory(_sample_rate):
        calls["count"] += 1
        if calls["count"] == 1:
            raise RuntimeError("transient")
        return FakeSinkStream()

    speaker = PcmStreamSpeaker(stream_factory=flaky_factory)
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-1")
    speaker.finish(turn_id="turn-1")
    speaker.start(format={"sample_rate": 24000}, turn_id="turn-2")

    assert speaker._stream is not None
    assert speaker.open_failures == 0
    speaker.close()
