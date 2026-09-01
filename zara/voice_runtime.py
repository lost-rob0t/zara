"""Daemon-owned live voice ingress for ZARA/1.

The transport owns framing and backpressure. This adapter owns only the
bounded handoff from validated PCM frames into Zara's existing streaming STT
pipeline and canonical RuntimeSupervisor command path.
"""

from __future__ import annotations

import queue
import threading
import uuid
from dataclasses import dataclass
from typing import Callable, Optional

import numpy as np

from zara.barge_in import BargeInConfig
from zara.principals import PrincipalContext
from zara.runtime import events
from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalMismatch
from zara.speech_activity import speech_activity
from zara.streaming_stt import (
    FinalTranscript,
    PartialTranscript,
    SpeechEnded,
    SpeechStarted,
    StreamingTranscriber,
    make_faster_whisper_transcriber,
)


@dataclass
class _VoiceStream:
    principal: PrincipalContext
    conversation_id: Optional[str]
    stream_id: str
    trace_id: Optional[str]
    transcriber: object
    committed: bool = False
    submitted: bool = False
    cancel_requested: bool = False
    inflight_events: int = 0


@dataclass(frozen=True)
class _AudioWork:
    stream_id: str
    pcm: bytes


@dataclass(frozen=True)
class _CommitWork:
    stream_id: str


_STOP = object()


class RuntimeVoiceIngress:
    """Bounded actor-style PCM -> streaming STT -> RuntimeSupervisor bridge."""

    def __init__(
        self,
        supervisor,
        *,
        principal: Optional[PrincipalContext] = None,
        transcriber_factory: Optional[Callable[..., object]] = None,
        queue_size: int = 32,
    ) -> None:
        if principal is not None and not isinstance(principal, PrincipalContext):
            raise TypeError("principal must be a PrincipalContext")
        if type(queue_size) is not int or queue_size <= 0:
            raise ValueError("queue_size must be a positive integer")
        self.supervisor = supervisor
        self.principal = principal
        self._transcriber_factory = transcriber_factory or self._default_transcriber_factory
        self._queue: queue.Queue[object] = queue.Queue(maxsize=queue_size)
        self._streams: dict[str, _VoiceStream] = {}
        self._lock = threading.RLock()
        self._event_condition = threading.Condition(self._lock)
        self._stop = threading.Event()
        self._thread: Optional[threading.Thread] = None
        self._default_model = None
        self._barge_in_config = BargeInConfig()

    def _check_principal(self, principal: PrincipalContext) -> None:
        if not isinstance(principal, PrincipalContext):
            raise TypeError("voice ingress requires PrincipalContext")
        if self.principal is not None and principal != self.principal:
            raise PrincipalMismatch(principal.principal_id)

    def _ensure_worker(self) -> None:
        with self._lock:
            if self._thread is not None and self._thread.is_alive():
                return
            if self._stop.is_set():
                raise RuntimeError("voice ingress is closed")
            self._thread = threading.Thread(
                target=self._run,
                name="zara-voice-ingress",
                daemon=True,
            )
            self._thread.start()

    def _default_transcriber_factory(self, **_context):
        from zara.config import get_config
        from zara.transcription import Transcriber

        settings = get_config().get_section("stt")
        model_name = str(settings.get("model", "small"))
        device = str(settings.get("device", "cpu"))
        threads = settings.get("threads")
        if self._default_model is None:
            loader = Transcriber(model=model_name, device=device, threads=threads)
            self._default_model = loader.model
        return StreamingTranscriber(make_faster_whisper_transcriber(self._default_model))

    def start(
        self,
        *,
        principal: PrincipalContext,
        conversation_id: Optional[str],
        stream_id: str,
        trace_id: Optional[str],
    ) -> None:
        self._check_principal(principal)
        if not isinstance(stream_id, str) or not stream_id:
            raise ValueError("stream_id must be a non-empty string")
        with self._lock:
            if stream_id in self._streams:
                raise KeyError(stream_id)
            transcriber = self._transcriber_factory(
                principal=principal,
                conversation_id=conversation_id,
                stream_id=stream_id,
                trace_id=trace_id,
            )
            transcriber.start_turn(stream_id)
            self._streams[stream_id] = _VoiceStream(
                principal=principal,
                conversation_id=conversation_id,
                stream_id=stream_id,
                trace_id=trace_id,
                transcriber=transcriber,
            )
        self._ensure_worker()

    def chunk(
        self,
        pcm: bytes,
        *,
        principal: PrincipalContext,
        conversation_id: Optional[str],
        stream_id: str,
        trace_id: Optional[str],
        seq: int,
    ) -> None:
        del seq
        self._check_principal(principal)
        with self._lock:
            stream = self._streams.get(stream_id)
            if stream is None or stream.committed:
                raise KeyError(stream_id)
            self._check_context(stream, conversation_id, trace_id)
        self._queue.put_nowait(_AudioWork(stream_id=stream_id, pcm=bytes(pcm)))

    def commit(
        self,
        *,
        principal: PrincipalContext,
        conversation_id: Optional[str],
        stream_id: str,
        trace_id: Optional[str],
    ) -> None:
        self._check_principal(principal)
        with self._lock:
            stream = self._require_stream(stream_id)
            self._check_context(stream, conversation_id, trace_id)
            if stream.committed:
                raise KeyError(stream_id)
            stream.committed = True
        try:
            self._queue.put_nowait(_CommitWork(stream_id=stream_id))
        except queue.Full:
            with self._lock:
                current = self._streams.get(stream_id)
                if current is stream:
                    stream.committed = False
            raise

    def cancel(
        self,
        *,
        principal: PrincipalContext,
        conversation_id: Optional[str],
        stream_id: str,
        trace_id: Optional[str],
    ) -> None:
        self._check_principal(principal)
        with self._event_condition:
            stream = self._require_stream(stream_id)
            self._check_context(stream, conversation_id, trace_id)
            stream.cancel_requested = True
            self._streams.pop(stream_id, None)
            while stream.inflight_events:
                self._event_condition.wait()
        stream.transcriber.cancel(stream_id)

    def _require_stream(self, stream_id: str) -> _VoiceStream:
        stream = self._streams.get(stream_id)
        if stream is None:
            raise KeyError(stream_id)
        return stream

    @staticmethod
    def _check_context(
        stream: _VoiceStream,
        conversation_id: Optional[str],
        trace_id: Optional[str],
    ) -> None:
        if stream.conversation_id != conversation_id or stream.trace_id != trace_id:
            raise ValueError("voice stream correlation changed")

    @staticmethod
    def _pcm_float32(pcm: bytes) -> np.ndarray:
        return np.frombuffer(pcm, dtype="<i2").astype(np.float32) / 32768.0

    def _publish(self, stream: _VoiceStream, event: events.RuntimeEvent) -> None:
        publisher = getattr(self.supervisor, "publish", None)
        if callable(publisher):
            publisher(stream.principal, event)

    def _publish_if_current(
        self,
        stream: _VoiceStream,
        event: events.RuntimeEvent,
    ) -> bool:
        with self._event_condition:
            if (
                self._streams.get(stream.stream_id) is not stream
                or stream.cancel_requested
            ):
                return False
            stream.inflight_events += 1
        try:
            self._publish(stream, event)
            return True
        finally:
            with self._event_condition:
                stream.inflight_events -= 1
                if not stream.inflight_events:
                    self._event_condition.notify_all()

    def _handle_stt_event(self, stream: _VoiceStream, event: object) -> None:
        common = {
            "conversation_id": stream.conversation_id,
            "stream_id": stream.stream_id,
            "trace_id": stream.trace_id,
        }
        if isinstance(event, SpeechStarted):
            visible = events.VoiceSpeechStarted(
                pre_speech_samples=event.pre_speech_samples,
                **common,
            )
            self._publish_if_current(stream, visible)
            return
        if isinstance(event, PartialTranscript):
            visible = events.VoiceTranscriptPartial(text=event.text, **common)
            self._publish_if_current(stream, visible)
            return
        if isinstance(event, SpeechEnded):
            visible = events.VoiceSpeechEnded(reason=event.reason, **common)
            self._publish_if_current(stream, visible)
            return
        if isinstance(event, FinalTranscript):
            self._submit_final(stream, event.text, provider=event.provider)

    def _set_playback_vad(self, transcriber: object, playback_active: bool) -> None:
        vad = getattr(transcriber, "_vad", None)
        config = getattr(vad, "config", None)
        if config is None:
            return

        idle = getattr(transcriber, "_zara_idle_vad", None)
        if idle is None:
            idle = (float(config.vad_threshold), int(config.min_speech_frames))
            setattr(transcriber, "_zara_idle_vad", idle)

        current = bool(getattr(transcriber, "_zara_playback_active", False))
        if current == playback_active:
            return

        if playback_active:
            config.vad_threshold = max(
                idle[0],
                float(self._barge_in_config.playback_vad_threshold),
            )
            config.min_speech_frames = max(
                idle[1],
                int(self._barge_in_config.playback_min_speech_frames),
            )
        else:
            config.vad_threshold, config.min_speech_frames = idle
        setattr(transcriber, "_zara_playback_active", playback_active)

    def _run(self) -> None:
        while not self._stop.is_set():
            try:
                item = self._queue.get(timeout=0.1)
            except queue.Empty:
                continue
            if item is _STOP:
                return
            if not isinstance(item, (_AudioWork, _CommitWork)):
                continue
            with self._lock:
                stream = self._streams.get(item.stream_id)
            if stream is None:
                continue
            try:
                if isinstance(item, _AudioWork):
                    self._set_playback_vad(stream.transcriber, speech_activity.active)
                    emitted = stream.transcriber.feed(self._pcm_float32(item.pcm))
                else:
                    emitted = stream.transcriber.commit(stream.stream_id)
            except Exception:
                continue
            for event in emitted:
                self._handle_stt_event(stream, event)
            if isinstance(item, _CommitWork):
                with self._lock:
                    current = self._streams.get(item.stream_id)
                    if current is stream:
                        self._streams.pop(item.stream_id, None)

    def _submit_final(self, stream: _VoiceStream, text: str, *, provider: str = "") -> None:
        clean = str(text or "").strip()
        if not clean:
            return
        with self._event_condition:
            current = self._streams.get(stream.stream_id)
            if current is not stream or stream.submitted or stream.cancel_requested:
                return
            stream.submitted = True
            stream.inflight_events += 1
        try:
            self._publish(
                stream,
                events.VoiceTranscriptFinal(
                    conversation_id=stream.conversation_id,
                    stream_id=stream.stream_id,
                    trace_id=stream.trace_id,
                    text=clean,
                    provider=str(provider or ""),
                ),
            )
            request_id = stream.trace_id or uuid.uuid4().hex
            self.supervisor.submit(
                stream.principal,
                SubmitTurn(
                    text=clean,
                    conversation_id=stream.conversation_id,
                    request_id=request_id,
                ),
            )
        finally:
            with self._event_condition:
                stream.inflight_events -= 1
                if not stream.inflight_events:
                    self._event_condition.notify_all()

    def close(self, timeout: float = 5.0) -> None:
        self._stop.set()
        with self._lock:
            streams = tuple(self._streams.values())
            self._streams.clear()
        for stream in streams:
            try:
                stream.transcriber.cancel(stream.stream_id)
            except Exception:
                pass
        try:
            self._queue.put_nowait(_STOP)
        except queue.Full:
            pass
        thread = self._thread
        if thread is not None:
            thread.join(max(0.0, float(timeout)))
            if thread.is_alive():
                raise TimeoutError("voice ingress worker did not stop")


__all__ = ["RuntimeVoiceIngress"]
