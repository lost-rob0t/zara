"""Daemon-side TTS output bridge producing ZARA/1 audio.output streams.

The bridge subscribes to a principal's runtime event bus, cuts assistant
output into phrase-sized synthesis units, decodes provider audio to raw
s16le mono PCM at the negotiated output rate, and publishes
``AudioOutputStarted/Chunk/Finished`` runtime events for the gateway to
carry to voice-capable clients. Clients own playback; the daemon owns
synthesis (issue #132 design). Phrase-level streaming upgrades, jitter
buffering, and provider pipeline hardening remain issue #29's scope.
"""

from __future__ import annotations

import asyncio
import io
import logging
import re
import threading
import wave
from dataclasses import dataclass, field
from typing import Callable, Optional

from zara.runtime import events

logger = logging.getLogger(__name__)

_PHRASE_END = re.compile(r"([^.\n!?…]*[.\n!?…]+)\s+")
_SENTINEL = object()


@dataclass
class _TurnAudio:
    turn_id: str
    conversation_id: Optional[str]
    stream_id: str
    buffer: str = ""
    queue: asyncio.Queue = field(default_factory=asyncio.Queue)
    task: Optional[asyncio.Task] = None
    cancelled: bool = False
    started: bool = False
    synthesizing: bool = False
    engine_failed: bool = False


class TtsOutputBridge:
    """Synthesize assistant output and publish raw PCM audio events."""

    def __init__(
        self,
        *,
        subscription,
        publish: Callable[[events.RuntimeEvent], object],
        engine_factory: Callable[[], object],
        sample_rate: int = 24000,
        poll_interval: float = 0.05,
    ) -> None:
        self._subscription = subscription
        self._publish = publish
        self._engine_factory = engine_factory
        self._engine: Optional[object] = None
        self._sample_rate = int(sample_rate)
        self._poll_interval = poll_interval
        self._turns: dict[str, _TurnAudio] = {}
        self._tasks: set[asyncio.Task] = set()
        self._stop_event = threading.Event()
        self._thread: Optional[threading.Thread] = None
        self._mp3_warned = False

    def _get_engine(self, state: _TurnAudio):
        if self._engine is not None:
            return self._engine
        if state.engine_failed:
            return None
        try:
            self._engine = self._engine_factory()
        except Exception as error:
            state.engine_failed = True
            logger.warning(
                "TTS engine unavailable for turn %s: %s", state.turn_id, error
            )
            return None
        return self._engine

    @property
    def sample_rate(self) -> int:
        return self._sample_rate

    def start(self) -> None:
        if self._thread is not None and self._thread.is_alive():
            return
        self._stop_event.clear()
        self._thread = threading.Thread(
            target=self._run_thread, name="zara-tts-output", daemon=True
        )
        self._thread.start()

    def stop(self, timeout: float = 5.0) -> None:
        self._stop_event.set()
        thread = self._thread
        if thread is not None:
            thread.join(timeout)
            self._thread = None

    def _run_thread(self) -> None:
        asyncio.run(self._main())

    async def _main(self) -> None:
        while not self._stop_event.is_set():
            drained = False
            for envelope in _drain(self._subscription, limit=32):
                drained = True
                try:
                    await self.handle_event(envelope.event)
                except Exception:
                    logger.warning(
                        "TTS output bridge failed to handle %s",
                        type(envelope.event).__name__,
                        exc_info=True,
                    )
            await self._wait_for_tasks(timeout=self._poll_interval if not drained else 0)
        await self.cancel_all(reason="bridge stopped")

    async def _wait_for_tasks(self, timeout: float) -> None:
        if not self._tasks:
            return
        done, pending = await asyncio.wait(
            list(self._tasks), timeout=timeout
        )
        for task in done:
            self._tasks.discard(task)
            error = task.exception()
            if error is not None and not isinstance(error, asyncio.CancelledError):
                logger.warning("TTS synthesis task failed: %s", error)

    async def wait_for_idle(self, timeout: float = 5.0) -> None:
        """Wait until every turn's phrase queue is drained and no
        synthesis is in flight. Tasks that merely wait for the next
        phrase do not block settlement."""

        async def _settled() -> None:
            while True:
                busy = [
                    state
                    for state in list(self._turns.values())
                    if not state.cancelled
                    and (
                        not state.queue.empty()
                        or (
                            state.task is not None
                            and not state.task.done()
                            and state.synthesizing
                        )
                    )
                ]
                if not busy:
                    return
                await asyncio.sleep(0.01)

        await asyncio.wait_for(_settled(), timeout)

    async def cancel_all(self, reason: str) -> None:
        for state in list(self._turns.values()):
            await self._cancel_turn_state(state)
        self._turns.clear()

    async def handle_event(self, event: events.RuntimeEvent) -> None:
        if type(event) is events.AssistantDelta:
            state = self._state_for(event)
            state.buffer += event.text
            for phrase in self._cut_phrases(state):
                await state.queue.put(phrase)
            self._ensure_task(state)
        elif type(event) is events.AssistantComplete:
            state = self._state_for(event)
            remainder = state.buffer.strip()
            state.buffer = ""
            if remainder:
                await state.queue.put(remainder)
            await state.queue.put(_SENTINEL)
            self._ensure_task(state)
        elif type(event) is events.TurnCancelled:
            state = self._turns.pop(event.turn_id, None)
            if state is not None:
                await self._cancel_turn_state(state)
        elif type(event) is events.AgentFailed:
            state = self._turns.pop(event.turn_id, None)
            if state is not None:
                await self._cancel_turn_state(state)

    def _state_for(self, event) -> _TurnAudio:
        state = self._turns.get(event.turn_id)
        if state is None:
            state = _TurnAudio(
                turn_id=event.turn_id,
                conversation_id=event.conversation_id,
                stream_id=f"tts-{event.turn_id}",
            )
            self._turns[event.turn_id] = state
        return state

    def _ensure_task(self, state: _TurnAudio) -> None:
        if state.task is None or state.task.done():
            if state.cancelled:
                return
            state.task = asyncio.create_task(self._synthesize_turn(state))
            self._tasks.add(state.task)

    @staticmethod
    def _cut_phrases(state: _TurnAudio) -> list[str]:
        phrases: list[str] = []
        while True:
            match = _PHRASE_END.match(state.buffer)
            if match is None:
                break
            phrase = match.group(1).strip()
            state.buffer = state.buffer[match.end():]
            if phrase:
                phrases.append(phrase)
        return phrases

    async def _synthesize_turn(self, state: _TurnAudio) -> None:
        try:
            while True:
                phrase = await state.queue.get()
                if phrase is _SENTINEL:
                    break
                if state.cancelled:
                    return
                state.synthesizing = True
                try:
                    await self._synthesize_phrase(state, phrase)
                finally:
                    state.synthesizing = False
            self._publish(
                events.AudioOutputFinished(
                    turn_id=state.turn_id,
                    conversation_id=state.conversation_id,
                    stream_id=state.stream_id,
                )
            )
        except asyncio.CancelledError:
            raise
        finally:
            if self._turns.get(state.turn_id) is state:
                self._turns.pop(state.turn_id, None)

    async def _synthesize_phrase(self, state: _TurnAudio, phrase: str) -> None:
        engine = self._get_engine(state)
        if engine is None:
            return
        try:
            stream = engine.synthesize_stream(phrase)
            async for chunk in stream:
                if state.cancelled:
                    return
                if getattr(chunk, "error", None) or not chunk.audio:
                    if getattr(chunk, "error", None):
                        logger.warning(
                            "TTS synthesis error for turn %s: %s",
                            state.turn_id,
                            chunk.error,
                        )
                    continue
                pcm = await asyncio.to_thread(
                    self._decode_to_pcm, chunk.audio, chunk.audio_format
                )
                if not pcm or state.cancelled:
                    continue
                if not state.started:
                    state.started = True
                    self._publish(
                        events.AudioOutputStarted(
                            turn_id=state.turn_id,
                            conversation_id=state.conversation_id,
                            stream_id=state.stream_id,
                            sample_rate=self._sample_rate,
                            channels=1,
                        )
                    )
                self._publish(
                    events.AudioOutputChunk(
                        turn_id=state.turn_id,
                        conversation_id=state.conversation_id,
                        stream_id=state.stream_id,
                        pcm=pcm,
                    )
                )
        except asyncio.CancelledError:
            raise
        except Exception as error:
            logger.warning(
                "TTS synthesis failed for turn %s: %s", state.turn_id, error
            )

    async def _cancel_turn_state(self, state: _TurnAudio) -> None:
        state.cancelled = True
        if state.task is not None and not state.task.done():
            state.task.cancel()
            try:
                await state.task
            except asyncio.CancelledError:
                pass
            self._tasks.discard(state.task)

    def _decode_to_pcm(self, audio: bytes, audio_format: str) -> bytes:
        audio_format = (audio_format or "").lower()
        if audio_format == "wav":
            return _decode_wav(audio, self._sample_rate)
        if audio_format == "mp3":
            pcm = _decode_with_ffmpeg(audio, self._sample_rate)
            if not pcm and not self._mp3_warned:
                self._mp3_warned = True
                logger.warning(
                    "MP3 decoding unavailable (ffmpeg missing or failed); "
                    "audio output for this provider is dropped"
                )
            return pcm
        logger.warning("Unsupported TTS audio format: %s", audio_format)
        return b""


def _drain(subscription, limit: int):
    drained = 0
    while drained < limit:
        try:
            yield subscription.get_nowait()
        except Exception:
            return
        drained += 1


def _decode_wav(audio: bytes, target_rate: int) -> bytes:
    try:
        with wave.open(io.BytesIO(audio), "rb") as handle:
            channels = handle.getnchannels()
            sampwidth = handle.getsampwidth()
            rate = handle.getframerate()
            frames = handle.readframes(handle.getnframes())
    except Exception as error:
        logger.warning("WAV decoding failed: %s", error)
        return b""
    if sampwidth != 2:
        logger.warning("Unsupported WAV sample width: %s", sampwidth)
        return b""
    import numpy as np

    samples = np.frombuffer(frames, dtype="<i2").astype(np.float32)
    if channels > 1:
        samples = samples.reshape(-1, channels).mean(axis=1)
    if rate != target_rate and samples.size:
        duration = samples.size / float(rate)
        target_length = max(1, int(duration * target_rate))
        positions = np.linspace(0, samples.size - 1, target_length)
        samples = np.interp(positions, np.arange(samples.size), samples)
    return np.clip(samples, -32768, 32767).astype("<i2").tobytes()


def _decode_with_ffmpeg(audio: bytes, target_rate: int) -> bytes:
    try:
        import subprocess

        result = subprocess.run(
            [
                "ffmpeg",
                "-loglevel",
                "error",
                "-i",
                "pipe:0",
                "-f",
                "s16le",
                "-acodec",
                "pcm_s16le",
                "-ac",
                "1",
                "-ar",
                str(target_rate),
                "pipe:1",
            ],
            input=audio,
            capture_output=True,
            timeout=30,
        )
    except FileNotFoundError:
        return b""
    except Exception as error:
        logger.warning("ffmpeg decode failed: %s", error)
        return b""
    if result.returncode != 0:
        return b""
    return result.stdout
