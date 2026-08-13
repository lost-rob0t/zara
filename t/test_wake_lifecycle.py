import asyncio
import queue
import threading
from unittest.mock import AsyncMock, MagicMock, patch

import numpy as np
import pytest

from zara.streaming_stt import StreamingVAD, VADConfig
from zara.wake import WakeWordListener


class FakeClock:
    def __init__(self, *values):
        self.values = iter(values)
        self.current = 0.0

    def __call__(self):
        self.current = next(self.values, self.current)
        return self.current


class FakeVADDetector:
    def __init__(self, pattern=None, default_prob=0.0):
        self.pattern = pattern or []
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


def build_listener(queue_size=8, sample_rate=16000):
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
    listener._capture_stream = None
    listener.input_sample_rate = sample_rate
    listener.first_speech_timeout = 5.0
    listener.silence_duration = 1.0
    listener.vad_config = VADConfig(
        min_speech_frames=1,
        trailing_silence_frames=2,
        max_utterance_frames=100,
    )
    listener._vad_factory = lambda config: StreamingVAD(
        config,
        FakeVADDetector(),
    )
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
    return listener


def frame(value, size=512):
    return np.full((size, 1), value, dtype=np.float32)


def enqueue(listener, data, epoch=None):
    listener.audio_queue.put_nowait(
        (listener._audio_epoch if epoch is None else epoch, data)
    )


def set_vad(listener, pattern=None, default_prob=0.0):
    listener._vad_factory = lambda config: StreamingVAD(
        config,
        FakeVADDetector(pattern=pattern, default_prob=default_prob),
    )


def test_no_speech_deadline_is_deterministic():
    async def run():
        listener = build_listener()
        listener._clock = FakeClock(0.0, 6.0)

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "first_speech_timeout"

    asyncio.run(run())


def test_no_speech_after_wake_returns_to_passive():
    async def run():
        listener = build_listener()
        listener.state = "ACTIVE"
        listener._clock = FakeClock(0.0, 6.0)
        listener.prolog = MagicMock()
        listener.prolog.dictation_active.return_value = False
        listener.agent_manager = None
        listener.in_conversation_mode = MagicMock(return_value=False)

        await listener.active_mode_async()

        assert listener.state == "PASSIVE"
        assert listener.collection_status == "first_speech_timeout"

    asyncio.run(run())


def test_conversation_timeout_is_checked_after_collection_wait():
    async def run():
        listener = build_listener()
        listener.state = "ACTIVE"
        listener.prolog = MagicMock()
        listener.prolog.dictation_active.return_value = False
        listener.agent_manager = MagicMock()
        listener.agent_manager.should_exit_conversation.side_effect = [False, True]
        listener.in_conversation_mode = MagicMock(return_value=True)
        listener._conversation_timeout_remaining = MagicMock(return_value=12.0)
        listener.collect_audio_until_silence = AsyncMock(return_value=None)
        listener.collection_status = "first_speech_timeout"
        listener._end_timed_out_conversation = AsyncMock()

        await listener.active_mode_async()

        listener.collect_audio_until_silence.assert_awaited_once_with(12.0)
        listener._end_timed_out_conversation.assert_awaited_once_with()

    asyncio.run(run())


def test_continuous_noise_hits_first_speech_deadline():
    async def run():
        listener = build_listener(queue_size=16)
        listener._clock = FakeClock(0.0, 1.0, 2.0, 3.0, 4.0, 6.0)
        for _ in range(4):
            enqueue(listener, frame(0.01))

        result = await listener.collect_audio_until_silence()

        assert result is None
        assert listener.collection_status == "first_speech_timeout"

    asyncio.run(run())


def test_endless_speech_is_capped_by_audio_duration():
    async def run():
        listener = build_listener(queue_size=8)
        listener.vad_config.max_utterance_frames = 4
        set_vad(listener, default_prob=0.9)
        for _ in range(4):
            enqueue(listener, frame(1.0))

        result = await listener.collect_audio_until_silence()

        assert result.shape == (2048, 1)
        assert listener.collection_status == "max_utterance"

    asyncio.run(run())


def test_speech_ends_after_silence_deadline():
    async def run():
        listener = build_listener(queue_size=8)
        set_vad(listener, pattern=[0.9, 0.9, 0.0, 0.0])
        for _ in range(4):
            enqueue(listener, frame(1.0))

        result = await listener.collect_audio_until_silence()

        assert result.shape == (2048, 1)
        assert listener.collection_status == "silence"

    asyncio.run(run())


def test_callback_overflow_drops_oldest_and_stays_bounded():
    async def run():
        listener = build_listener(queue_size=3)
        for value in range(1000):
            listener.audio_callback(frame(value, 1), 1, None, None)

        assert listener.audio_queue.qsize() == 3
        assert listener.dropped_audio_chunks == 997
        retained = [listener.audio_queue.get_nowait()[1][0, 0] for _ in range(3)]
        assert retained == [997, 998, 999]

    asyncio.run(run())


def test_slow_consumer_cannot_grow_callback_buffer():
    async def run():
        listener = build_listener(queue_size=4)
        for _ in range(10000):
            listener.audio_callback(frame(1.0, 1), 1, None, None)

        assert listener.audio_queue.qsize() == 4
        assert listener.dropped_audio_chunks == 9996

    asyncio.run(run())


def test_shutdown_unblocks_passive_and_active_collectors():
    async def run():
        passive = build_listener()
        passive_task = asyncio.create_task(passive.collect_audio_until_silence())
        await asyncio.sleep(0)
        passive.request_stop()
        assert await asyncio.wait_for(passive_task, 0.2) is None
        assert passive.collection_status == "stopped"

        active = build_listener()
        active_task = asyncio.create_task(active.collect_audio_until_silence())
        await asyncio.sleep(0)
        active.request_stop()
        assert await asyncio.wait_for(active_task, 0.2) is None
        assert active.collection_status == "stopped"

        recording = build_listener()
        set_vad(recording, default_prob=0.9)
        enqueue(recording, frame(1.0))
        recording_task = asyncio.create_task(recording.collect_audio_until_silence())
        await asyncio.sleep(0)
        recording.request_stop()
        assert await asyncio.wait_for(recording_task, 0.2) is None
        assert recording.collection_status == "stopped"

    asyncio.run(run())


def test_state_transition_discards_stale_frames():
    async def run():
        listener = build_listener()
        enqueue(listener, frame(1.0))

        listener.transition_to("ACTIVE")
        enqueue(listener, frame(2.0), epoch=0)
        enqueue(listener, frame(3.0))

        result = await listener._next_audio()

        assert result[0, 0] == 3.0
        assert listener.audio_queue.empty()

    asyncio.run(run())


def test_capture_failure_aborts_audio_wait_with_actual_error():
    async def run():
        listener = build_listener()
        listener._capture_stream = MagicMock(
            last_error="parec exited with 7: Connection terminated"
        )

        with pytest.raises(
            RuntimeError,
            match=r"Audio capture failed: parec exited with 7: Connection terminated",
        ):
            await listener._next_audio()

    asyncio.run(run())


def test_wake_command_preserves_same_utterance_request():
    listener = WakeWordListener.__new__(WakeWordListener)

    assert listener._wake_command("Hey Zara, what time is it?") == "what time is it"
    assert listener._wake_command("Zarathustra open Firefox") == "open Firefox"
    assert listener._wake_command("ordinary conversation") is None


def test_passive_wake_queues_same_utterance_command():
    async def run():
        listener = build_listener()
        listener.prolog = MagicMock()
        listener.prolog.dictation_active.return_value = False
        listener.collect_audio_until_silence = AsyncMock(return_value=frame(1.0))
        listener.transcribe_async = AsyncMock(
            return_value="Hey Zara, what time is it?"
        )

        with patch(
            "zara.wake.send_notification_async",
            new_callable=AsyncMock,
        ):
            await listener.passive_mode_async()

        assert listener.state == "ACTIVE"
        assert listener._pending_wake_command == "what time is it"
        listener.transcribe_async.assert_awaited_once()

    asyncio.run(run())


def test_active_mode_routes_pending_wake_command_without_listening_again():
    async def run():
        listener = build_listener()
        listener.state = "ACTIVE"
        listener._pending_wake_command = "what time is it"
        listener.prolog = MagicMock()
        listener.prolog.dictation_active.return_value = False
        listener.prolog.is_conversation_stop.return_value = False
        listener.agent_manager = None
        listener.in_conversation_mode = MagicMock(return_value=False)
        listener.collect_audio_until_silence = AsyncMock()
        listener.query_with_fallback_async = AsyncMock(
            return_value=(False, "It is noon")
        )
        listener._monitor_speech_during_llm = AsyncMock(return_value=False)
        listener._play_acknowledgement = MagicMock()
        listener._stop_tts = AsyncMock()
        listener.send_response_async = AsyncMock()

        await listener.active_mode_async()

        listener.collect_audio_until_silence.assert_not_awaited()
        listener.query_with_fallback_async.assert_awaited_once_with(
            "what time is it"
        )
        listener.send_response_async.assert_awaited_once()

    asyncio.run(run())


def test_long_synthetic_input_keeps_queue_and_utterance_bounded():
    async def run():
        listener = build_listener(queue_size=5, sample_rate=100)
        for _ in range(100000):
            listener.audio_callback(frame(1.0, 10), 10, None, None)

        assert listener.audio_queue.qsize() == 5
        assert sum(item[1].nbytes for item in listener.audio_queue.queue) == 200

    asyncio.run(run())
