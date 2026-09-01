"""Wake conversation flow tests against the daemon voice client.

These tests verify the client-owned flow end to end without a microphone,
audio hardware, or a running daemon: wake spotting, utterance streaming,
daemon transcript events, stop phrases, acknowledgement playback, and
barge-in during daemon-side turns.

STT is only used for wake spotting locally; routing, agent turns, and
response audio come back as daemon events (issue #244).
"""

from __future__ import annotations

import asyncio
import queue
import threading
from unittest.mock import AsyncMock, MagicMock

import numpy as np
import pytest

from zara.runtime import events
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


class FakeDaemon:
    def __init__(self) -> None:
        self.stream_utterance = AsyncMock(return_value="stream-1")
        self.submit_cancel = MagicMock()
        self.ensure_connected = MagicMock()
        self.on_transcript_partial = []
        self.on_transcript_final = []
        self.on_assistant_delta = []
        self.on_assistant_complete = []
        self.on_turn_started = []
        self.on_turn_completed = []
        self.on_turn_cancelled = []

    def dispatch_final(self, text: str, *, stream_id="stream-1", trace_id="trace-1"):
        for handler in self.on_transcript_final:
            handler(
                events.VoiceTranscriptFinal(
                    turn_id="turn-1",
                    conversation_id="conv-1",
                    stream_id=stream_id,
                    trace_id=trace_id,
                    text=text,
                )
            )

    def dispatch_cancelled(self, turn_id="turn-1"):
        for handler in self.on_turn_cancelled:
            handler(events.TurnCancelled(turn_id=turn_id, reason="cancel command"))


class FakeSpeaker:
    def __init__(self) -> None:
        self._active_turns = set()
        self.cancel_active = MagicMock()
        self.cancel = MagicMock()
        self.on_playback_started = None

    @property
    def is_active(self) -> bool:
        return bool(self._active_turns)


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
    listener.wake_words = ["zara"]
    listener.stop_phrases = ["end conversation", "stop session"]
    listener.config = MagicMock()
    listener.config.get_section = MagicMock(return_value={})
    listener.speaker = FakeSpeaker()
    listener.daemon = FakeDaemon()
    listener._pending_wake_audio = None
    listener._conversation_last_activity = 0.0
    listener._stop_phrase_seen = threading.Event()
    listener._turn_finished = threading.Event()
    listener._active_daemon_turn_id = None
    listener._active_stream_id = None
    listener.response_timeout = 30.0
    listener.conversation_timeout = 60.0
    listener.collect_audio_until_silence = AsyncMock(return_value=frame(1.0, 10))
    return listener


@pytest.mark.asyncio
async def test_daemon_stop_phrase_transcript_exits_conversation_mode():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.collect_audio_until_silence = AsyncMock(return_value=frame(1.0, 10))
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)

    listener.daemon.on_transcript_final.append(
        lambda event: listener._on_transcript_final(event)
    )

    def stream_sets_stop(audio, *, trace_id):
        listener.daemon.dispatch_final("end conversation", trace_id=trace_id)
        return "stream-1"

    listener.daemon.stream_utterance = AsyncMock(side_effect=stream_sets_stop)

    await listener.active_mode_async()

    assert listener.state == "PASSIVE"
    listener.daemon.submit_cancel.assert_not_called()


@pytest.mark.asyncio
async def test_normal_daemon_transcript_keeps_conversation_active():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)
    listener.daemon.on_transcript_final.append(
        lambda event: listener._on_transcript_final(event)
    )

    def stream_sets_transcript(audio, *, trace_id):
        listener.daemon.dispatch_final("what time is it", trace_id=trace_id)
        return "stream-1"

    listener.daemon.stream_utterance = AsyncMock(side_effect=stream_sets_transcript)

    await listener.active_mode_async()

    assert listener.state == "ACTIVE"


@pytest.mark.asyncio
async def test_wake_word_alone_streams_pending_utterance_and_stays_active():
    listener = build_listener()
    wake_audio = frame(1.0, 10)
    listener.collect_audio_until_silence = AsyncMock(return_value=wake_audio)
    listener.transcribe_async = AsyncMock(return_value="Zara")

    with pytest.MonkeyPatch.context() as patcher:
        patcher.setattr(
            "zara.wake.send_notification_async", AsyncMock(return_value=True)
        )
        await listener.passive_mode_async()

    assert listener.state == "ACTIVE"
    assert listener._pending_wake_audio is wake_audio

    listener.collect_audio_until_silence = AsyncMock()
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)

    await listener.active_mode_async()

    listener.collect_audio_until_silence.assert_not_awaited()
    assert listener.daemon.stream_utterance.await_args.args[0] is wake_audio


@pytest.mark.asyncio
async def test_wake_in_longer_utterance_streams_whole_utterance():
    listener = build_listener()
    utterance = frame(1.0, 10)
    listener.collect_audio_until_silence = AsyncMock(return_value=utterance)
    listener.transcribe_async = AsyncMock(return_value="Zara what time is it")

    with pytest.MonkeyPatch.context() as patcher:
        patcher.setattr(
            "zara.wake.send_notification_async", AsyncMock(return_value=True)
        )
        await listener.passive_mode_async()

    assert listener.state == "ACTIVE"
    assert listener._pending_wake_audio is utterance


@pytest.mark.parametrize("stop_word", ["disable", "end", "goodbye"])
@pytest.mark.asyncio
async def test_conversation_stop_words_exit_via_daemon_transcript(stop_word):
    listener = build_listener()
    listener.state = "ACTIVE"
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)
    listener.daemon.on_transcript_final.append(
        lambda event: listener._on_transcript_final(event)
    )
    listener.daemon.stream_utterance = AsyncMock(
        side_effect=lambda audio, *, trace_id: (
            listener.daemon.dispatch_final(stop_word, trace_id=trace_id),
            "stream-1",
        )[1]
    )

    await listener.active_mode_async()

    assert listener.state == "PASSIVE"


@pytest.mark.asyncio
async def test_barge_in_cancels_daemon_turn_and_restarts():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener._monitor_speech_during_llm = AsyncMock(return_value=True)
    listener._stop_tts = AsyncMock()
    listener.ack_player = MagicMock()
    listener.daemon.on_turn_started.append(
        lambda event: listener._on_turn_started(event)
    )

    async def stream(audio, *, trace_id):
        listener._on_turn_started(
            events.TurnStarted(turn_id="turn-9", conversation_id="conv-1")
        )
        return "stream-1"

    listener.daemon.stream_utterance = AsyncMock(side_effect=stream)

    await listener.active_mode_async()

    listener.daemon.submit_cancel.assert_called_once_with("turn-9")
    listener.speaker.cancel_active.assert_called()
    assert listener.state == "ACTIVE"


@pytest.mark.asyncio
async def test_turn_completion_before_barge_in_completes_normally():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)

    await listener.active_mode_async()

    listener.daemon.submit_cancel.assert_not_called()
    assert listener.state == "ACTIVE"


@pytest.mark.asyncio
async def test_acknowledgement_plays_after_speech_ends_before_streaming():
    listener = build_listener()
    listener.state = "ACTIVE"
    order: list[str] = []
    listener._play_acknowledgement = MagicMock(
        side_effect=lambda turn_id: order.append("ack")
    )
    listener._monitor_speech_during_llm = AsyncMock(return_value=False)
    listener._wait_for_turn_completion = AsyncMock(return_value=True)

    async def stream(audio, *, trace_id):
        order.append("stream")
        return "stream-1"

    listener.daemon.stream_utterance = AsyncMock(side_effect=stream)

    await listener.active_mode_async()

    assert order == ["ack", "stream"]


@pytest.mark.asyncio
async def test_daemon_audio_playback_stops_acknowledgement():
    from unittest.mock import patch

    from zara.wake_daemon import PcmStreamSpeaker

    stopped = []
    with (
        patch.object(PcmStreamSpeaker, "_ensure_stream"),
        patch.object(PcmStreamSpeaker, "_ensure_writer"),
    ):
        speaker = PcmStreamSpeaker()
        speaker.on_playback_started = lambda: stopped.append("ack-stopped")
        speaker.start(
            format={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
            conversation_id="conv-1",
            turn_id="turn-5",
            stream_id="tts-5",
        )

    assert stopped == ["ack-stopped"]
    speaker.cancel(turn_id="turn-5")


@pytest.mark.asyncio
async def test_cancelled_daemon_turn_unblocks_wait():
    listener = build_listener()
    listener.state = "ACTIVE"
    listener.daemon.on_turn_cancelled.append(
        lambda event: listener._on_turn_cancelled(event)
    )
    listener._active_daemon_turn_id = "turn-7"

    listener.daemon.dispatch_cancelled("turn-7")

    completed = await listener._wait_for_turn_completion(listener._clock() + 1.0)

    assert completed is True
