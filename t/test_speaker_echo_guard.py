"""Regression tests for microphone collection during Zara speaker playback."""

import asyncio
import queue
import threading
import time

import pytest

from zara.wake import WakeWordListener


class FakeVAD:
    last_probability = 0.0

    def start_turn(self, _turn_id):
        pass



def build_stopped_listener():
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.audio_queue = queue.Queue()
    listener._audio_epoch = 0
    listener._shutdown_requested = threading.Event()
    listener.stop_event = asyncio.Event()
    listener.stop_event.set()
    listener.first_speech_timeout = 5.0
    listener.collection_status = "idle"
    listener._clock = time.monotonic
    listener.log = lambda _message: None
    listener._ensure_turn_trace = lambda: None
    listener._new_streaming_vad = lambda: FakeVAD()
    return listener


@pytest.mark.asyncio
async def test_collection_waits_for_tts_and_discards_speaker_bleed():
    listener = build_stopped_listener()
    listener.audio_queue.put_nowait((0, object()))

    release_playback = asyncio.Event()

    async def playback():
        await release_playback.wait()

    listener.tts_task = asyncio.create_task(playback())
    collection = asyncio.create_task(listener.collect_audio_until_silence())

    await asyncio.sleep(0)
    assert not collection.done()

    release_playback.set()
    assert await collection is None
    assert listener.audio_queue.empty()
    assert listener._audio_epoch == 1


@pytest.mark.asyncio
async def test_collection_keeps_audio_when_no_tts_is_active():
    listener = build_stopped_listener()
    listener.tts_task = None
    listener.audio_queue.put_nowait((0, object()))

    assert await listener.collect_audio_until_silence() is None
    assert listener.audio_queue.qsize() == 1
    assert listener._audio_epoch == 0
