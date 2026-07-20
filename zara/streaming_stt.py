"""Streaming VAD and incremental STT pipeline (ZARA-024).

Replaces fixed multi-second audio chunks with a continuous streaming
pipeline built on Silero VAD (via the ``pysilero-vad`` C extension) and
faster-whisper. The VAD processes 512-sample (32 ms at 16 kHz) chunks in
real time and emits typed events for speech start, partial transcript,
speech end, and final transcript.

A bounded rolling pre-speech buffer ensures the first phoneme is not
clipped. Adaptive endpointing uses separate minimum speech, trailing
silence, maximum utterance, and no-speech deadlines.

The ``TranscriberInterface`` is stable so a genuinely streaming backend
can replace the batch faster-whisper call without changing the
coordinator or any caller.
"""

from __future__ import annotations

import logging
import threading
from collections import deque
from dataclasses import dataclass, field
from typing import Any, Callable, List, Optional, Protocol, Sequence

import numpy as np

logger = logging.getLogger(__name__)

VAD_CHUNK_SAMPLES = 512  # Silero VAD fixed window size
VAD_SAMPLE_RATE = 16000  # Silero VAD requires 16 kHz


# ---------------------------------------------------------------------------
# Typed STT events
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class STTEvent:
    """Base event emitted by the streaming transcriber."""
    turn_id: Optional[str] = None


@dataclass(frozen=True)
class SpeechStarted(STTEvent):
    pre_speech_samples: int = 0


@dataclass(frozen=True)
class PartialTranscript(STTEvent):
    text: str = ""
    text_length: int = 0


@dataclass(frozen=True)
class SpeechEnded(STTEvent):
    reason: str = "silence"


@dataclass(frozen=True)
class FinalTranscript(STTEvent):
    text: str = ""
    text_length: int = 0
    provider: str = ""


# ---------------------------------------------------------------------------
# Streaming VAD (Silero-backed)
# ---------------------------------------------------------------------------


@dataclass
class VADConfig:
    """Adaptive endpointing parameters."""
    vad_threshold: float = 0.5
    """Speech probability threshold for speech/silence classification."""
    min_speech_frames: int = 4
    """Minimum consecutive speech chunks before speech_started fires."""
    trailing_silence_frames: int = 16
    """Consecutive silence chunks after speech to trigger speech_ended."""
    max_utterance_frames: int = 938
    """Hard cap on total speech chunks (30 s at 32 ms/chunk)."""
    no_speech_timeout_frames: int = 156
    """Return to idle if no speech within this many chunks (5 s)."""
    pre_speech_buffer_chunks: int = 10
    """Rolling buffer of pre-speech chunks to avoid clipping onset."""
    partial_interval_frames: int = 31
    """Run partial transcription every N chunks (~1 s at 32 ms/chunk)."""


class StreamingVAD:
    """Silero VAD wrapper with adaptive endpointing and pre-speech buffer.

    Processes audio in fixed 512-sample (32 ms at 16 kHz) chunks.
    Maintains a rolling pre-speech buffer so the first phoneme is not
    clipped. The state machine transitions::

        idle → maybe_speech → speaking → ended

    Each ``feed()`` call returns zero or more ``STTEvent`` values.

    The ``vad_detector`` parameter allows injecting a fake detector for
    deterministic testing. In production, a ``SileroVoiceActivityDetector``
    is created lazily on first use.
    """

    def __init__(
        self,
        config: Optional[VADConfig] = None,
        vad_detector: Optional[Any] = None,
    ):
        self.config = config or VADConfig()
        self._vad: Optional[Any] = vad_detector
        self._owns_vad = vad_detector is None
        self._state = "idle"
        self._speech_frame_count = 0
        self._silence_frame_count = 0
        self._total_frames = 0
        self._pre_speech: deque = deque(
            maxlen=self.config.pre_speech_buffer_chunks
        )
        self._speech_audio: List[np.ndarray] = []
        self._turn_id: Optional[str] = None
        self._lock = threading.Lock()
        self._cancelled = False

    def _ensure_vad(self):
        if self._vad is None:
            from pysilero_vad import SileroVoiceActivityDetector
            self._vad = SileroVoiceActivityDetector()

    def reset(self) -> None:
        """Reset to idle state for a new turn."""
        with self._lock:
            self._state = "idle"
            self._speech_frame_count = 0
            self._silence_frame_count = 0
            self._total_frames = 0
            self._pre_speech.clear()
            self._speech_audio = []
            self._turn_id = None
            self._cancelled = False
            if self._vad is not None:
                self._vad.reset()

    def cancel(self, turn_id: Optional[str] = None) -> None:
        """Mark the current turn as cancelled. No further events will emit."""
        with self._lock:
            self._cancelled = True
            self._turn_id = None
            self._state = "idle"

    def start_turn(self, turn_id: str) -> None:
        """Begin a new turn with the given ID."""
        self.reset()
        self._turn_id = turn_id

    @property
    def state(self) -> str:
        return self._state

    @property
    def speech_audio(self) -> np.ndarray:
        """All speech audio accumulated so far (including pre-speech)."""
        if not self._speech_audio:
            return np.zeros(0, dtype=np.float32)
        return np.concatenate(self._speech_audio)

    @property
    def pre_speech_audio(self) -> np.ndarray:
        if not self._pre_speech:
            return np.zeros(0, dtype=np.float32)
        return np.concatenate(list(self._pre_speech))

    def feed(self, chunk: np.ndarray) -> List[STTEvent]:
        """Process one 512-sample float32 chunk. Returns emitted events.

        The chunk must be 512 float32 samples at 16 kHz mono. If the
        input is stereo, only the first channel is used.
        """
        if len(chunk) != VAD_CHUNK_SAMPLES:
            raise ValueError(
                f"Chunk must be exactly {VAD_CHUNK_SAMPLES} samples, got {len(chunk)}"
            )

        events: List[STTEvent] = []

        with self._lock:
            if self._cancelled:
                return events

            self._total_frames += 1
            self._ensure_vad()

            mono = chunk[:, 0] if chunk.ndim > 1 else chunk
            mono_int16 = np.clip(mono * 32767, -32768, 32767).astype(np.int16)
            audio_bytes = mono_int16.tobytes()

            try:
                prob = self._vad.process_chunk(audio_bytes)
            except Exception as error:
                logger.warning("[StreamingVAD] VAD error: %s", error)
                return events

            is_speech = prob >= self.config.vad_threshold

            if self._state == "idle":
                self._pre_speech.append(mono)
                if is_speech:
                    self._speech_frame_count = 1
                    self._silence_frame_count = 0
                    if self._speech_frame_count >= self.config.min_speech_frames:
                        self._state = "speaking"
                        self._speech_audio = list(self._pre_speech)
                        events.append(SpeechStarted(
                            turn_id=self._turn_id,
                            pre_speech_samples=len(self.pre_speech_audio),
                        ))
                    else:
                        self._state = "maybe_speech"
                elif self._total_frames >= self.config.no_speech_timeout_frames:
                    self._pre_speech.clear()

            elif self._state == "maybe_speech":
                if is_speech:
                    self._speech_frame_count += 1
                    self._pre_speech.append(mono)
                    if self._speech_frame_count >= self.config.min_speech_frames:
                        self._state = "speaking"
                        self._speech_audio = list(self._pre_speech)
                        events.append(SpeechStarted(
                            turn_id=self._turn_id,
                            pre_speech_samples=len(self.pre_speech_audio),
                        ))
                else:
                    self._speech_frame_count = 0
                    self._state = "idle"

            elif self._state == "speaking":
                self._speech_audio.append(mono)
                self._speech_frame_count += 1

                if is_speech:
                    self._silence_frame_count = 0
                else:
                    self._silence_frame_count += 1

                if self._speech_frame_count >= self.config.max_utterance_frames:
                    self._state = "ended"
                    events.append(SpeechEnded(
                        turn_id=self._turn_id,
                        reason="max_utterance",
                    ))

                elif self._silence_frame_count >= self.config.trailing_silence_frames:
                    self._state = "ended"
                    events.append(SpeechEnded(
                        turn_id=self._turn_id,
                        reason="silence",
                    ))

        return events


# ---------------------------------------------------------------------------
# Transcriber interface
# ---------------------------------------------------------------------------


class TranscriberInterface(Protocol):
    """Stable interface for streaming transcription.

    A genuinely streaming backend can implement this interface without
    changing the coordinator or any caller.
    """

    def feed(self, chunk: np.ndarray) -> List[STTEvent]: ...

    def cancel(self, turn_id: Optional[str] = None) -> None: ...

    def reset(self) -> None: ...

    def start_turn(self, turn_id: str) -> None: ...


# ---------------------------------------------------------------------------
# Streaming transcriber (faster-whisper + Silero VAD)
# ---------------------------------------------------------------------------


class StreamingTranscriber:
    """Streaming VAD + incremental faster-whisper transcription.

    Feeds audio through ``StreamingVAD`` and runs faster-whisper on
    accumulated speech audio to produce partial and final transcripts.

    Partial transcripts are emitted every ``partial_interval_frames``
    chunks during active speech. The final transcript is emitted when
    speech ends. Only the final transcript should trigger irreversible
    command execution.
    """

    def __init__(
        self,
        transcribe_fn: Callable[[np.ndarray], str],
        config: Optional[VADConfig] = None,
        vad_detector: Optional[Any] = None,
    ):
        self._vad = StreamingVAD(config or VADConfig(), vad_detector=vad_detector)
        self._transcribe_fn = transcribe_fn
        self._frames_since_partial = 0
        self._last_partial_text = ""

    def reset(self) -> None:
        self._vad.reset()
        self._frames_since_partial = 0
        self._last_partial_text = ""

    def cancel(self, turn_id: Optional[str] = None) -> None:
        self._vad.cancel(turn_id)

    def start_turn(self, turn_id: str) -> None:
        self.reset()
        self._vad.start_turn(turn_id)

    def feed(self, chunk: np.ndarray) -> List[STTEvent]:
        events = self._vad.feed(chunk)
        result: List[STTEvent] = []

        for event in events:
            if isinstance(event, SpeechStarted):
                result.append(event)
            elif isinstance(event, SpeechEnded):
                final_text = self._transcribe_fn(self._vad.speech_audio)
                result.append(event)
                if not self._vad._cancelled:
                    result.append(FinalTranscript(
                        turn_id=event.turn_id,
                        text=final_text,
                        text_length=len(final_text),
                        provider="faster-whisper",
                    ))

        if self._vad.state == "speaking":
            self._frames_since_partial += 1
            if self._frames_since_partial >= self._vad.config.partial_interval_frames:
                self._frames_since_partial = 0
                audio = self._vad.speech_audio
                if len(audio) >= VAD_CHUNK_SAMPLES:
                    partial_text = self._transcribe_fn(audio)
                    if partial_text and partial_text != self._last_partial_text:
                        self._last_partial_text = partial_text
                        result.append(PartialTranscript(
                            turn_id=self._vad._turn_id,
                            text=partial_text,
                            text_length=len(partial_text),
                        ))

        return result

    @property
    def state(self) -> str:
        return self._vad.state


def make_faster_whisper_transcriber(
    model: Any,
    sample_rate: int = VAD_SAMPLE_RATE,
    language: str = "en",
) -> Callable[[np.ndarray], str]:
    """Create a transcribe function from a faster-whisper WhisperModel."""

    def transcribe(audio: np.ndarray) -> str:
        if len(audio) == 0:
            return ""
        audio_float = audio.astype(np.float32)
        if sample_rate != VAD_SAMPLE_RATE:
            from zara.audio import resample_audio
            audio_float = resample_audio(audio_float, sample_rate, VAD_SAMPLE_RATE)
        try:
            segments, _ = model.transcribe(
                audio_float,
                beam_size=1,
                vad_filter=True,
                language=language,
                no_speech_threshold=0.5,
            )
            return " ".join(seg.text.strip() for seg in segments).strip()
        except Exception as error:
            logger.warning("[StreamingSTT] transcription error: %s", error)
            return ""

    return transcribe


def make_fixture_transcriber(
    mapping: Optional[dict] = None,
    default_text: str = "fixture transcript",
) -> Callable[[np.ndarray], str]:
    """Create a deterministic transcribe function for tests/benchmarks.

    Maps audio length ranges to predetermined text so tests are
    deterministic without loading a real Whisper model.
    """
    mapping = mapping or {}

    def transcribe(audio: np.ndarray) -> str:
        length = len(audio)
        for (min_len, max_len), text in sorted(mapping.items()):
            if min_len <= length < max_len:
                return text
        return default_text

    return transcribe
