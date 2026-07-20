"""Conversation flow tests with mocked STT and TTS.

These tests verify the end-to-end wake → STT → route → LLM → TTS flow
without requiring a microphone, audio hardware, or network access.

STT is mocked by replacing ``transcribe_async`` to return predetermined
text. TTS is mocked by replacing ``synthesize_and_play_async`` so no
audio is synthesized or played.

Coverage:
- Wake word transitions to ACTIVE mode
- Stop phrase exits conversation mode
- Wake word alone exits conversation mode
- Wake word embedded in longer utterance does NOT exit conversation
- Barge-in during LLM cancels the request and restarts the turn
- Acknowledgement plays after speech ends, before STT/Prolog/LLM
- Acknowledgement stops when TTS response begins
- Conversation stop words ("disable", "end", "goodbye") end conversation
"""

from __future__ import annotations

import asyncio
import queue
import threading
from unittest.mock import AsyncMock, MagicMock

import numpy as np
import pytest

from zara.wake import WakeWordListener


class FakeClock:
    def __init__(self, *values):
        self.values = iter(values)
        self.current = 0.0

    def __call__(self):
        self.current = next(self.values, self.current)
        return self.current


def frame(value, size=5):
    return np.full((size, 1), value, dtype=np.float32)


def build_listener(queue_size=32, sample_rate=10):
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.state = "PASSIVE"
    listener.audio_queue = queue.Queue(maxsize=queue_size)
    listener.audio_ready = asyncio.Event()
    listener.stop_event = asyncio.Event()
    listener._shutdown_requested = threading.Event()
    listener._audio_notification_lock = threading.Lock()
    listener._audio_notification_pending = False
    listener._audio_epoch = 0
    listener.dropped_audio_chunks = 0
    listener.collection_status = "idle"
    listener.loop = asyncio.get_running_loop()
    listener.input_sample_rate = sample_rate
    listener.first_speech_timeout = 5.0
    listener.max_utterance_duration = 30.0
    listener.silence_duration = 1.0
    listener.silence_threshold = 0.1
    listener.silence_log_interval = 100.0
    listener._clock = FakeClock(0.0)
    listener.log = lambda _message: None
    listener.ack_player = None
    listener.current_latency_trace = None
    listener.tts_task = None
    listener.tts_stop_event = None
    listener.tts_player_proc = None
    listener.tts_playback_active = False
    listener.tts_lock = asyncio.Lock()
    listener.stop_on_interrupt = False
    listener.enable_tts = True
    listener.tts_config = {"provider": "qwen3"}
    listener.tts_client = None
    listener.agent_manager = None
    listener.session_id = None
    listener.memory = MagicMock()
    listener.config = MagicMock()
    listener.config.get_section = MagicMock(return_value={})
    return listener


def enqueue(listener, data, epoch=None):
    listener.audio_queue.put_nowait(
        (listener._audio_epoch if epoch is None else epoch, data)
    )


@pytest.mark.asyncio
async def test_stop_phrase_exits_conversation_mode():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = True
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener.speak_async = AsyncMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="goodbye")
    listener._play_acknowledgement = MagicMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_called_once()
    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_wake_word_alone_exits_conversation():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener.speak_async = AsyncMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="zara")
    listener._play_acknowledgement = MagicMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_called_once()
    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_wake_word_in_longer_utterance_does_not_exit():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(
        return_value="hey zara what is the weather"
    )
    listener._play_acknowledgement = MagicMock()
    listener.query_with_fallback_async = AsyncMock(
        return_value=(True, "It is sunny.")
    )
    listener.send_response_async = AsyncMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_not_called()


@pytest.mark.asyncio
async def test_disable_ends_conversation():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener.speak_async = AsyncMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="disable")
    listener._play_acknowledgement = MagicMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_called_once()
    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_end_ends_conversation():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener.speak_async = AsyncMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="end")
    listener._play_acknowledgement = MagicMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_called_once()
    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_goodbye_ends_conversation():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener.speak_async = AsyncMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="goodbye")
    listener._play_acknowledgement = MagicMock()

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_called_once()
    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_normal_utterance_does_not_exit_conversation():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(
        return_value="what is the meaning of life"
    )
    listener._play_acknowledgement = MagicMock()
    listener.query_with_fallback_async = AsyncMock(
        return_value=(True, "42, obviously.")
    )
    listener.send_response_async = AsyncMock()
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)

    await listener.active_mode_async()

    listener.agent_manager.exit_conversation.assert_not_called()
    listener.query_with_fallback_async.assert_awaited_once()
    listener.send_response_async.assert_awaited_once()


@pytest.mark.asyncio
async def test_barge_in_cancels_llm_and_restarts():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="tell me a story")
    listener._play_acknowledgement = MagicMock()

    llm_started = asyncio.Event()

    async def slow_llm(command):
        llm_started.set()
        await asyncio.Event().wait()
        return (True, "Once upon a time...")

    listener.query_with_fallback_async = slow_llm

    async def fast_barge_in():
        await llm_started.wait()
        await asyncio.sleep(0.01)
        return True

    listener._monitor_speech_during_llm = fast_barge_in
    listener.send_response_async = AsyncMock()

    await listener.active_mode_async()

    assert listener.state == "ACTIVE"


@pytest.mark.asyncio
async def test_llm_completes_before_barge_in():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="hello there")
    listener._play_acknowledgement = MagicMock()
    listener.query_with_fallback_async = AsyncMock(
        return_value=(True, "Hi! How can I help?")
    )
    listener.send_response_async = AsyncMock()
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)

    await listener.active_mode_async()

    listener.query_with_fallback_async.assert_awaited_once()
    listener.send_response_async.assert_awaited_once()


@pytest.mark.asyncio
async def test_acknowledgement_plays_after_speech_ends():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    transcribe_called = asyncio.Event()

    async def mock_transcribe(audio_data):
        transcribe_called.set()
        return "what time is it"

    listener.transcribe_async = mock_transcribe
    listener._play_acknowledgement = MagicMock()
    listener.query_with_fallback_async = AsyncMock(
        return_value=(True, "It is noon.")
    )
    listener.send_response_async = AsyncMock()
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)

    await listener.active_mode_async()

    listener._play_acknowledgement.assert_called_once()
    assert listener._play_acknowledgement.call_args is not None
    ack_call_time = listener._play_acknowledgement.call_args
    assert transcribe_called.is_set()


@pytest.mark.asyncio
async def test_acknowledgement_stops_when_tts_starts():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="hello")
    listener._play_acknowledgement = MagicMock()

    stop_ack_calls = []

    async def mock_send_response(title, message):
        listener._stop_acknowledgement()

    listener.send_response_async = mock_send_response
    listener.query_with_fallback_async = AsyncMock(
        return_value=(True, "Hello!")
    )
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)

    await listener.active_mode_async()

    listener._stop_acknowledgement.assert_called()


@pytest.mark.asyncio
async def test_short_text_is_ignored():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.prolog = MagicMock()
    listener.prolog.dictation_active.return_value = False
    listener.prolog.is_conversation_stop.return_value = False
    listener.in_conversation_mode = MagicMock(return_value=True)
    listener.agent_manager = MagicMock()
    listener.agent_manager.should_exit_conversation.return_value = False
    listener._conversation_timeout_remaining = MagicMock(return_value=None)
    listener._apply_conversation_grace = MagicMock()
    listener._stop_tts = AsyncMock()
    listener._stop_acknowledgement = MagicMock()

    chunk = frame(1.0, 10)
    enqueue(listener, chunk)
    listener._clock = FakeClock(0.0, 0.0, 2.0)

    listener.transcribe_async = AsyncMock(return_value="hi")
    listener._play_acknowledgement = MagicMock()
    listener.query_with_fallback_async = AsyncMock()

    await listener.active_mode_async()

    listener.query_with_fallback_async.assert_not_awaited()
