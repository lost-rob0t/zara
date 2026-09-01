"""Thin daemon voice client for the wake listener.

Wake keeps only physically client-owned responsibilities (wake word
spotting, VAD endpointing, microphone capture, barge-in capture, local
playback). Transcript synthesis, Prolog-first routing, agent turns,
memory, conversation persistence, and TTS synthesis happen daemon-side
over the ZARA/1 ``audio.input.*`` / transcript / assistant / audio output
contract (issue #244). Daemon unavailability fails closed; there is no
private in-process runtime fallback.
"""

from __future__ import annotations

import logging
import queue
import threading
import uuid
from typing import Callable, Optional

import numpy as np

from zara.runtime import events
from zara.runtime.bridge import RuntimeEventSubscription
from zara.runtime.commands import CancelTurn
from zara.zmq_transport import ZmqZaraClient

logger = logging.getLogger(__name__)

WAKE_AUDIO_OUTPUT_FORMATS = (
    {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
    {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1},
    {"codec": "pcm_s16le", "sample_rate": 48000, "channels": 1},
)
WAKE_FRAME_BYTES = 1024
CONNECT_TIMEOUT = 10.0


class WakeDaemonUnavailable(RuntimeError):
    """The Zara daemon is unreachable; wake must fail closed."""


class PcmStreamSpeaker:
    """Client-owned playback sink for daemon audio.output streams.

    One persistent ``sd.OutputStream`` renders raw s16le mono PCM delivered
    by ``audio.output.*`` events. ``cancel`` drops queued audio immediately
    so barge-in stops playback within the interruption budget.
    """

    def __init__(self) -> None:
        self._stream = None
        self._queue: "queue.Queue[Optional[np.ndarray]]" = queue.Queue()
        self._writer: Optional[threading.Thread] = None
        self._stop = threading.Event()
        self._drain = threading.Event()
        self._drained = threading.Event()
        self._lock = threading.Lock()
        self._sample_rate: Optional[int] = None
        self._active_turns: set[str] = set()
        self.on_playback_started: Optional[Callable] = None
        self.on_playback_finished: Optional[Callable] = None
        self.cancelled_turns: list[str] = []
        self.started_turns: list[str] = []
        self.finished_turns: list[str] = []
        self.chunks_played: int = 0

    @property
    def is_active(self) -> bool:
        return bool(self._active_turns)

    def _ensure_writer(self) -> None:
        if self._writer is None or not self._writer.is_alive():
            self._stop.clear()
            self._writer = threading.Thread(
                target=self._write_loop, name="wake-speaker", daemon=True
            )
            self._writer.start()

    def _ensure_stream(self, sample_rate: int) -> None:
        with self._lock:
            if self._stream is not None and self._sample_rate == sample_rate:
                return
            if self._stream is not None:
                try:
                    self._stream.close()
                except Exception:
                    logger.warning("Speaker stream replace failed", exc_info=True)
                self._stream = None
            import sounddevice as sd

            self._stream = sd.OutputStream(
                samplerate=int(sample_rate),
                channels=1,
                dtype="int16",
            )
            self._stream.start()
            self._sample_rate = int(sample_rate)

    def _write_loop(self) -> None:
        while not self._stop.is_set():
            try:
                block = self._queue.get(timeout=0.1)
            except queue.Empty:
                continue
            if block is None:
                self._drained.set()
                continue
            self._drained.clear()
            stream = self._stream
            if stream is None or self._stop.is_set():
                continue
            try:
                stream.write(block)
            except Exception:
                logger.warning("Speaker write failed", exc_info=True)

    def start(self, **kwargs) -> None:
        turn_id = str(kwargs.get("turn_id") or "")
        self._active_turns.add(turn_id)
        self.started_turns.append(turn_id)
        callback = self.on_playback_started
        if callback is not None:
            try:
                callback()
            except Exception:
                logger.warning("Playback start callback failed", exc_info=True)
        fmt = kwargs.get("format") or {}
        sample_rate = int(fmt.get("sample_rate", 24000))
        self._ensure_stream(sample_rate)
        self._ensure_writer()

    def chunk(self, payload=b"", *, turn_id=None, **kwargs) -> None:
        if turn_id is not None and str(turn_id) not in self._active_turns:
            return
        block = np.frombuffer(bytes(payload), dtype="<i2").reshape(-1, 1)
        self._queue.put(block)
        self.chunks_played += 1

    def finish(self, *, turn_id=None, **kwargs) -> None:
        if turn_id is not None:
            self._active_turns.discard(str(turn_id))
            self.finished_turns.append(str(turn_id))
        self._queue.put(None)
        self._drained.wait(timeout=5.0)
        callback = self.on_playback_finished
        if callback is not None:
            try:
                callback()
            except Exception:
                logger.warning("Playback finish callback failed", exc_info=True)

    def cancel(self, *, turn_id=None, **kwargs) -> None:
        if turn_id is not None:
            self.cancelled_turns.append(str(turn_id))
            self._active_turns.discard(str(turn_id))
        while True:
            try:
                self._queue.get_nowait()
            except queue.Empty:
                return

    def cancel_active(self) -> None:
        for turn_id in list(self._active_turns):
            self.cancel(turn_id=turn_id)

    def close(self) -> None:
        self._stop.set()
        self._queue.put(None)
        writer = self._writer
        if writer is not None:
            writer.join(timeout=2.0)
            self._writer = None
        with self._lock:
            if self._stream is not None:
                try:
                    self._stream.close()
                except Exception:
                    pass
                self._stream = None


def utterance_frames(audio: np.ndarray) -> list[bytes]:
    """Convert float32 mono audio into exact s16le protocol frames."""
    mono = audio if audio.ndim == 1 else audio[:, 0]
    mono = np.asarray(mono, dtype=np.float32)
    scaled = np.clip(mono, -1.0, 1.0) * 32767.0
    pcm = scaled.astype("<i2").tobytes()
    frames: list[bytes] = []
    for offset in range(0, len(pcm), WAKE_FRAME_BYTES):
        block = pcm[offset : offset + WAKE_FRAME_BYTES]
        if len(block) < WAKE_FRAME_BYTES:
            block = block + b"\x00" * (WAKE_FRAME_BYTES - len(block))
        frames.append(block)
    return frames


class WakeDaemonClient:
    """Client-owned adapter over :class:`ZmqZaraClient` for wake."""

    def __init__(
        self,
        *,
        client: Optional[ZmqZaraClient] = None,
        client_factory: Optional[Callable[[], ZmqZaraClient]] = None,
        endpoint: Optional[str] = None,
        curve_client=None,
        voice_output=None,
        audio_output_formats=WAKE_AUDIO_OUTPUT_FORMATS,
        connect_timeout: float = CONNECT_TIMEOUT,
    ) -> None:
        self._client = client
        self._client_factory = client_factory
        self._endpoint = endpoint
        self._curve_client = curve_client
        self._voice_output = voice_output
        self._audio_output_formats = audio_output_formats
        self._connect_timeout = connect_timeout
        self._subscription: Optional[RuntimeEventSubscription] = None
        self._pump_thread: Optional[threading.Thread] = None
        self._pump_stop = threading.Event()
        self.on_transcript_partial: list[Callable] = []
        self.on_transcript_final: list[Callable] = []
        self.on_assistant_delta: list[Callable] = []
        self.on_assistant_complete: list[Callable] = []
        self.on_turn_started: list[Callable] = []
        self.on_turn_completed: list[Callable] = []
        self.on_turn_cancelled: list[Callable] = []

    @property
    def client(self) -> ZmqZaraClient:
        if self._client is None:
            raise WakeDaemonUnavailable("wake daemon client is not connected")
        return self._client

    @property
    def audio_output_format(self):
        return self.client.negotiated_audio_output_format

    def connect(self) -> None:
        try:
            if self._client is None:
                if self._client_factory is not None:
                    self._client = self._client_factory()
                else:
                    self._client = ZmqZaraClient(
                        self._endpoint,
                        curve_client=self._curve_client,
                        voice_output=self._voice_output,
                        audio_output_formats=list(self._audio_output_formats),
                    )
            self._client.start().result(timeout=self._connect_timeout)
        except WakeDaemonUnavailable:
            raise
        except BaseException as error:
            self._client = None
            raise WakeDaemonUnavailable(
                f"Could not reach the Zara daemon: {error}"
            ) from error
        try:
            self.client.open_conversation()
            self._subscription = self.client.subscribe(maxsize=256)
        except BaseException as error:
            self._client = None
            raise WakeDaemonUnavailable(
                f"Could not open a Zara daemon conversation: {error}"
            ) from error

    def ensure_connected(self, *, max_attempts: int = 4) -> None:
        if self._client is not None and self._client.state == "READY":
            return
        try:
            self.client.reconnect_with_backoff(max_attempts=max_attempts)
        except BaseException as error:
            raise WakeDaemonUnavailable(
                f"Lost the Zara daemon and could not reconnect: {error}"
            ) from error
        self._subscription = self.client.subscribe(maxsize=256)

    def start_pump(self) -> None:
        if self._pump_thread is not None:
            return
        self._pump_stop.clear()
        self._pump_thread = threading.Thread(
            target=self._pump_loop, name="wake-daemon-events", daemon=True
        )
        self._pump_thread.start()

    def stop_pump(self, timeout: float = 2.0) -> None:
        self._pump_stop.set()
        thread = self._pump_thread
        if thread is not None:
            thread.join(timeout)
            self._pump_thread = None

    def _pump_loop(self) -> None:
        while not self._pump_stop.is_set():
            try:
                self._drain_once()
            except Exception:
                logger.warning("Wake daemon event dispatch failed", exc_info=True)
            self._pump_stop.wait(0.05)

    def _drain_once(self, limit: int = 32) -> None:
        if self._subscription is None:
            if self._client is None:
                return
            try:
                self._subscription = self._client.subscribe(maxsize=256)
            except Exception:
                logger.warning("Wake daemon subscription failed", exc_info=True)
                return
        subscription = self._subscription
        for _ in range(limit):
            try:
                envelope = subscription.get_nowait()
            except queue.Empty:
                return
            self._dispatch(envelope.event)

    async def dispatch_events(self, limit: int = 32) -> None:
        self._drain_once(limit=limit)

    def _dispatch(self, event: events.RuntimeEvent) -> None:
        try:
            if type(event) is events.VoiceTranscriptPartial:
                for handler in self.on_transcript_partial:
                    handler(event)
            elif type(event) is events.VoiceTranscriptFinal:
                for handler in self.on_transcript_final:
                    handler(event)
            elif type(event) is events.AssistantDelta:
                for handler in self.on_assistant_delta:
                    handler(event)
            elif type(event) is events.AssistantComplete:
                for handler in self.on_assistant_complete:
                    handler(event)
            elif type(event) is events.TurnStarted:
                for handler in self.on_turn_started:
                    handler(event)
            elif type(event) is events.AgentCompleted:
                for handler in self.on_turn_completed:
                    handler(event)
            elif type(event) is events.TurnCancelled:
                for handler in self.on_turn_cancelled:
                    handler(event)
        except Exception:
            logger.warning(
                "Wake event handler failed for %s", type(event).__name__, exc_info=True
            )

    def submit_cancel(self, turn_id: str) -> None:
        """Send canonical turn cancellation for a daemon-side turn."""
        if not turn_id:
            return
        try:
            self.client.submit(CancelTurn(turn_id=turn_id))
        except Exception:
            logger.warning("Failed to submit turn cancellation", exc_info=True)

    async def stream_utterance(self, audio: np.ndarray, *, trace_id: str) -> str:
        """Stream one collected utterance to the daemon and commit it."""
        stream_id = uuid.uuid4().hex
        client = self.client
        client.start_audio_input(stream_id, trace_id=trace_id)
        for seq, frame in enumerate(utterance_frames(audio)):
            client.send_audio_input(stream_id, seq=seq, pcm=frame, trace_id=trace_id)
        client.commit_audio_input(stream_id, trace_id=trace_id)
        return stream_id

    def cancel_utterance(self, stream_id: str, *, trace_id: Optional[str] = None) -> None:
        try:
            self.client.cancel_audio_input(stream_id, trace_id=trace_id)
        except Exception:
            logger.warning("Failed to cancel wake audio stream", exc_info=True)

    def close(self) -> None:
        self.stop_pump()
        client = self._client
        self._client = None
        self._subscription = None
        if client is not None:
            try:
                client.close(timeout=2.0)
            except Exception:
                logger.warning("Wake daemon client close failed", exc_info=True)
