"""PcmStreamSpeaker: client-owned daemon audio output playback (#244)."""

from __future__ import annotations

import queue
import threading
from unittest.mock import patch

import numpy as np
import pytest

from zara.wake_daemon import PcmStreamSpeaker


def build_speaker() -> PcmStreamSpeaker:
    speaker = PcmStreamSpeaker()
    speaker._ensure_stream = lambda *args, **kwargs: None
    speaker._ensure_writer = lambda: None
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
