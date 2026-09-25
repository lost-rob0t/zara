from __future__ import annotations

import asyncio
import io
import subprocess
import wave
from types import SimpleNamespace

import pytest

from zara.runtime import events
from zara.runtime.bridge import RuntimeEventBus
from zara.runtime import tts_output
from zara.runtime.tts_output import TtsOutputBridge, _TurnAudio


def bridge_with(factory=lambda: object()):
    bus = RuntimeEventBus()
    published = []
    return TtsOutputBridge(
        subscription=bus.subscribe(),
        publish=published.append,
        engine_factory=factory,
        poll_interval=0,
    ), published


def wav(*, channels=1, width=2, rate=24000, frames=b"\x00\x00" * 8):
    out = io.BytesIO()
    with wave.open(out, "wb") as handle:
        handle.setnchannels(channels)
        handle.setsampwidth(width)
        handle.setframerate(rate)
        handle.writeframes(frames)
    return out.getvalue()


def test_engine_factory_failure_is_memoized_per_turn():
    calls = 0

    def fail():
        nonlocal calls
        calls += 1
        raise RuntimeError("offline")

    bridge, _ = bridge_with(fail)
    state = _TurnAudio("t", "c", "tts-t")
    assert bridge._get_engine(state) is None
    assert state.engine_failed is True
    assert bridge._get_engine(state) is None
    assert calls == 1


@pytest.mark.asyncio
async def test_cancel_and_failed_events_are_noops_without_live_turn():
    bridge, _ = bridge_with()
    await bridge.handle_event(events.TurnCancelled(turn_id="missing"))
    await bridge.handle_event(events.AgentFailed(turn_id="missing"))
    await bridge.cancel_all("done")
    assert bridge._turns == {}


@pytest.mark.asyncio
async def test_cancelled_state_never_schedules_synthesis():
    bridge, _ = bridge_with()
    state = _TurnAudio("t", "c", "tts-t", cancelled=True)
    bridge._ensure_task(state)
    assert state.task is None
    await bridge._wait_for_tasks(timeout=0)


@pytest.mark.asyncio
async def test_error_empty_and_unsupported_chunks_do_not_publish_audio(monkeypatch):
    class Engine:
        async def synthesize_stream(self, _text):
            yield SimpleNamespace(error="provider failed", audio=b"x", audio_format="pcm")
            yield SimpleNamespace(error=None, audio=b"", audio_format="pcm")
            yield SimpleNamespace(error=None, audio=b"x", audio_format="unknown")

    bridge, published = bridge_with(lambda: Engine())
    state = _TurnAudio("t", "c", "tts-t")
    await bridge._synthesize_phrase(state, "hello")
    assert published == []


@pytest.mark.asyncio
async def test_engine_stream_exception_is_contained_without_false_audio():
    class Engine:
        async def synthesize_stream(self, _text):
            raise RuntimeError("stream failed")
            yield

    bridge, published = bridge_with(lambda: Engine())
    state = _TurnAudio("t", "c", "tts-t")
    await bridge._synthesize_phrase(state, "hello")
    assert published == []


def test_decode_pcm_mp3_and_unknown_formats(monkeypatch):
    bridge, _ = bridge_with()
    assert bridge._decode_to_pcm(b"raw", "PCM") == b"raw"

    monkeypatch.setattr(tts_output, "_decode_with_ffmpeg", lambda _audio, _rate: b"")
    assert bridge._decode_to_pcm(b"mp3", "mp3") == b""
    assert bridge._mp3_warned is True
    assert bridge._decode_to_pcm(b"mp3", "mp3") == b""
    assert bridge._decode_to_pcm(b"x", "bogus") == b""


def test_decode_wav_rejects_corruption_and_unsupported_sample_width():
    assert tts_output._decode_wav(b"not-wave", 24000) == b""
    assert tts_output._decode_wav(wav(width=1, frames=b"\x00" * 8), 24000) == b""


def test_decode_wav_downmixes_stereo_and_resamples():
    frames = (b"\x01\x00\x03\x00") * 16
    pcm = tts_output._decode_wav(
        wav(channels=2, width=2, rate=48000, frames=frames),
        24000,
    )
    assert pcm
    assert len(pcm) % 2 == 0


def test_ffmpeg_decoder_handles_missing_failure_nonzero_and_success(monkeypatch):
    def missing(*_args, **_kwargs):
        raise FileNotFoundError

    monkeypatch.setattr(subprocess, "run", missing)
    assert tts_output._decode_with_ffmpeg(b"x", 24000) == b""

    def broken(*_args, **_kwargs):
        raise RuntimeError("boom")

    monkeypatch.setattr(subprocess, "run", broken)
    assert tts_output._decode_with_ffmpeg(b"x", 24000) == b""

    monkeypatch.setattr(
        subprocess,
        "run",
        lambda *_args, **_kwargs: SimpleNamespace(returncode=1, stdout=b"bad"),
    )
    assert tts_output._decode_with_ffmpeg(b"x", 24000) == b""

    monkeypatch.setattr(
        subprocess,
        "run",
        lambda *_args, **_kwargs: SimpleNamespace(returncode=0, stdout=b"pcm"),
    )
    assert tts_output._decode_with_ffmpeg(b"x", 24000) == b"pcm"


@pytest.mark.asyncio
async def test_completed_empty_turn_and_agent_failure_cleanup():
    class Engine:
        async def synthesize_stream(self, _text):
            yield SimpleNamespace(error=None, audio=b"pcm", audio_format="pcm")

    bridge, published = bridge_with(lambda: Engine())
    await bridge.handle_event(events.AssistantComplete(turn_id="empty", conversation_id="c", text=""))
    await bridge.wait_for_idle()
    assert any(isinstance(event, events.AudioOutputFinished) for event in published)

    state = bridge._state_for(events.AssistantDelta(turn_id="failed", conversation_id="c", text="x"))
    state.task = asyncio.create_task(asyncio.sleep(10))
    bridge._tasks.add(state.task)
    await bridge.handle_event(events.AgentFailed(turn_id="failed", reason="boom"))
    assert state.cancelled is True
    assert "failed" not in bridge._turns
