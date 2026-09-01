#!/usr/bin/env python3
"""
Wake Word Listener for Zarathushtra - thin daemon voice client.

Client-owned: wake word spotting, Silero VAD endpointing, microphone
capture, barge-in capture, acknowledgement playback. Everything else
(STT synthesis, Prolog-first routing, agent turns, memory, conversation
persistence, TTS synthesis) happens daemon-side over the ZARA/1 voice
contract. Daemon unavailability fails closed (issue #244).
"""

import os
import math
import logging
import sys
import time
import asyncio
import pathlib
import queue
import threading
from typing import Any, Optional, Tuple
import numpy as np
import sounddevice as sd
import faster_whisper
from concurrent.futures import ThreadPoolExecutor

from .acknowledgement import AcknowledgementConfig, AcknowledgementPlayer
from .streaming_stt import (
    SpeechEnded,
    SpeechStarted,
    StreamingVAD,
    VADConfig,
    VAD_CHUNK_SAMPLES,
    VAD_SAMPLE_RATE,
)
from .config import get_config
from .notifications import send_notification_async
from .audio import resolve_input_sample_rate, resample_audio
from .latency import JSONLMetricsSink, LatencyTrace, metrics_path
from .runtime.events import RuntimeEvent
from .wake_daemon import PcmStreamSpeaker, WakeDaemonClient, WakeDaemonUnavailable
from .runtime.clarification import (
    DialogueTemplate,
    OPEN_APP_TEMPLATE,
    SCHEDULE_TODO_TEMPLATE,
    TEXT_MESSAGE_TEMPLATE,
)
from .wake_words import (
    WAKE_TOKEN_STRIP,
    WAKE_WORDS,
    _normalize_wake_words,
    edit_distance,
    find_wake_span,
    resolve_wake_words,
    wake_distance_threshold,
)

DEFAULT_SAMPLE_RATE = 16000
CHANNELS = 1
DEFAULT_SILENCE_DURATION = 5.0
MAX_RECORDING_DURATION = 30.0
DEFAULT_AUDIO_QUEUE_CHUNKS = 32
TIMEOUT_ACTIVE = 5
DEFAULT_RESPONSE_TIMEOUT = 30.0

CLARIFICATION_PRINCIPAL = "local"
CLARIFICATION_CONVERSATION = "voice"

PENDING_DIALOGUE_TEMPLATES: dict[str, DialogueTemplate] = {
    "open": OPEN_APP_TEMPLATE,
    "text": TEXT_MESSAGE_TEMPLATE,
    "python(schedule_todo)": SCHEDULE_TODO_TEMPLATE,
}

PIDFILE = "/tmp/zara_wakeword.pid"
LOGFILE = "/tmp/zara_wakeword.log"


def load_tts_config():
    """Load TTS configuration from config system"""
    config = get_config()
    tts_cfg = config.get_section("tts")

    provider = tts_cfg.get("provider", "qwen3")
    qwen3_url = tts_cfg.get("endpoint", os.getenv("QWEN3_TTS_URL", "http://localhost:7860"))
    qwen3_voice = tts_cfg.get("voice", os.getenv("QWEN3_VOICE", "demo_speaker0"))

    return {
        "provider": provider,
        "qwen3_url": qwen3_url,
        "qwen3_voice": qwen3_voice,
    }


class WakeWordListener:
    def __init__(self, model="base.en", device="cpu", prolog_main_path=None, enable_tts=True):
        self.state = "PASSIVE"
        self.audio_queue: Optional[queue.Queue] = None
        self.audio_ready: Optional[asyncio.Event] = None
        self.stop_event: Optional[asyncio.Event] = None
        self._shutdown_requested = threading.Event()
        self._audio_notification_lock = threading.Lock()
        self._audio_notification_pending = False
        self._audio_epoch = 0
        self.dropped_audio_chunks = 0
        self.collection_status = "idle"
        self._clock = time.monotonic
        self._latency_cold_start = True
        self.current_latency_trace: Optional[LatencyTrace] = None
        self.enable_tts = enable_tts
        self.loop: Optional[asyncio.AbstractEventLoop] = None
        self._capture_stream = None

        self.executor = ThreadPoolExecutor(max_workers=4)
        self.config = get_config()
        latency_config = self.config.get_latency_config()
        self.latency_enabled = latency_config["enabled"]
        self.latency_sink = (
            JSONLMetricsSink(metrics_path(latency_config))
            if self.latency_enabled
            else None
        )

        wake_cfg = self.config.get_section("wake")
        stt_cfg = self.config.get_section("stt")
        daemon_cfg = self.config.get_section("daemon") or {}
        raw_stop_phrases = wake_cfg.get("stop_phrases") or []
        if isinstance(raw_stop_phrases, str):
            raw_stop_phrases = [raw_stop_phrases]
        self.stop_phrases = [
            " ".join(str(phrase).lower().split())
            for phrase in raw_stop_phrases
            if str(phrase).strip()
        ]
        self.stt_threads = self._positive_int(stt_cfg.get("threads", 4), 4)
        self.wake_beam_size = self._positive_int(stt_cfg.get("wake_beam_size", 2), 2)
        self.stt_language = str(stt_cfg.get("language", "en") or "en").strip() or "en"
        self.stop_on_interrupt = bool(wake_cfg.get("stop_tts_on_input", True))
        self.silence_duration = self._parse_float(
            wake_cfg.get("silence_duration", DEFAULT_SILENCE_DURATION),
            DEFAULT_SILENCE_DURATION,
        )
        self.first_speech_timeout = self._positive_float(
            wake_cfg.get("first_speech_timeout", TIMEOUT_ACTIVE),
            TIMEOUT_ACTIVE,
        )
        self.max_utterance_duration = self._positive_float(
            wake_cfg.get("max_utterance_duration", MAX_RECORDING_DURATION),
            MAX_RECORDING_DURATION,
        )
        self.audio_queue_chunks = self._positive_int(
            wake_cfg.get("audio_queue_chunks", DEFAULT_AUDIO_QUEUE_CHUNKS),
            DEFAULT_AUDIO_QUEUE_CHUNKS,
        )
        self.vad_config = VADConfig(
            vad_threshold=self._positive_float(stt_cfg.get("vad_threshold", 0.5), 0.5),
            min_speech_frames=self._vad_frames(stt_cfg.get("min_speech_ms", 128), 128),
            trailing_silence_frames=self._vad_frames(
                stt_cfg.get("trailing_silence_ms", 320), 320
            ),
            max_utterance_frames=self._vad_frames(
                stt_cfg.get(
                    "max_utterance_ms",
                    self.max_utterance_duration * 1000,
                ),
                MAX_RECORDING_DURATION * 1000,
            ),
            no_speech_timeout_frames=self._vad_frames(
                stt_cfg.get("no_speech_timeout_ms", 5000), 5000
            ),
            pre_speech_buffer_chunks=self._positive_int(
                stt_cfg.get("pre_speech_buffer_chunks", 10), 10
            ),
            partial_interval_frames=self._vad_frames(
                stt_cfg.get("partial_transcript_ms", 1000), 1000
            ),
        )
        self._pending_wake_audio: Optional[np.ndarray] = None
        self._last_audio_warning = 0.0

        self.response_timeout = self._positive_float(
            daemon_cfg.get("response_timeout", DEFAULT_RESPONSE_TIMEOUT),
            DEFAULT_RESPONSE_TIMEOUT,
        )
        self.conversation_timeout = self._positive_float(
            (self.config.get_section("agent") or {}).get(
                "conversation_timeout", 60
            ),
            60,
        )
        self._conversation_last_activity = 0.0
        self._stop_phrase_seen = threading.Event()
        self._turn_finished = threading.Event()
        self._active_daemon_turn_id: Optional[str] = None
        self._active_stream_id: Optional[str] = None
        self.speaker = PcmStreamSpeaker()
        self.daemon = WakeDaemonClient(
            endpoint=daemon_cfg.get("endpoint") or None,
            curve_client=self._build_curve_client(daemon_cfg),
            voice_output=self.speaker,
        )

        self.target_sample_rate = self._parse_float(
            wake_cfg.get("sample_rate", DEFAULT_SAMPLE_RATE),
            DEFAULT_SAMPLE_RATE,
        )
        self.input_sample_rate, rate_note = resolve_input_sample_rate(
            self.target_sample_rate,
            channels=CHANNELS,
        )
        if rate_note:
            self.log(rate_note)

        self.ack_player: Optional[AcknowledgementPlayer] = None
        self._init_ack_player(wake_cfg)

        self.wake_words = resolve_wake_words(self.config)
        self.wake_prompt = (
            ". ".join(word.capitalize() for word in self.wake_words) + "."
        )

        load_started = self._clock()
        self.log(
            f"Loading Whisper {model} "
            f"(device={device}, cpu_threads={self.stt_threads}, workers=1)"
        )
        if self.enable_tts:
            self.log("Acknowledgement playback enabled")

        self.model = faster_whisper.WhisperModel(
            model,
            device=device,
            compute_type="int8",
            cpu_threads=self.stt_threads,
            num_workers=1,
        )
        self.log(
            f"Whisper model ready: {model} on {device} "
            f"({self._clock() - load_started:.2f}s)"
        )
        self.log(
            "Silero VAD configured "
            f"(threshold={self.vad_config.vad_threshold:.2f}, "
            f"min_speech_frames={self.vad_config.min_speech_frames}, "
            f"trailing_silence_frames={self.vad_config.trailing_silence_frames})"
        )
        self.log(
            "Wake spotting configured "
            f"(language={self.stt_language}, wake_beam={self.wake_beam_size})"
        )

    @staticmethod
    def _build_curve_client(daemon_cfg: dict):
        secret = daemon_cfg.get("curve_secret_key")
        server_public = daemon_cfg.get("curve_server_public_key")
        if not secret or not server_public:
            return None
        from zara.security_transport import CurveClientConfig

        return CurveClientConfig(
            public_key=str(daemon_cfg.get("curve_public_key", "")),
            secret_key=str(secret),
            server_public_key=str(server_public),
        )

    def _init_ack_player(self, wake_cfg: dict) -> None:
        """Build the AcknowledgementPlayer from config and pre-warm clips."""
        ack_cfg = wake_cfg.get("acknowledgement", {}) or {}
        if not self.enable_tts or not ack_cfg.get("enabled", True):
            self.ack_player = AcknowledgementPlayer(
                config=AcknowledgementConfig(enabled=False)
            )
            return

        phrases_raw = ack_cfg.get("phrases", []) or []
        phrases = tuple(dict.fromkeys(str(p) for p in phrases_raw if p))
        provider = ack_cfg.get("provider", self.tts_config.get("provider", "edge"))
        voice = ack_cfg.get("voice", "")
        if not voice:
            tts_section = self.config.get_section("tts")
            voice = (
                tts_section.get("elevenlabs_voice_id")
                if provider == "11labs"
                else tts_section.get("edge_voice", "en-US-AriaNeural")
            )
        volume = self._parse_float(ack_cfg.get("volume", 1.0), 1.0)

        config = AcknowledgementConfig(
            enabled=True,
            phrase=ack_cfg.get("phrase", "Okay"),
            phrases=phrases,
            provider=provider,
            voice=voice,
            volume=volume,
        )

        tts_engine_for_ack: Optional[TTSEngine] = None
        if provider not in ("edge",):
            try:
                config_dict = (
                    self.config._config if hasattr(self.config, "_config") else {}
                )
                tts_engine_for_ack = TTSEngine(provider=provider, config=config_dict)
            except Exception as error:
                self.log(f"Ack TTS engine init failed, using edge fallback: {error}")

        self.ack_player = AcknowledgementPlayer(config=config, tts_engine=tts_engine_for_ack)
        try:
            self.ack_player.initialize()
            if self.ack_player.has_audio:
                self.log(f"Ack player ready: source={self.ack_player.source}")
            else:
                self.log("Ack player initialized without audio (will be silent)")
        except Exception as error:
            self.log(f"Ack player init failed: {error}")
            self.ack_player = AcknowledgementPlayer(config=AcknowledgementConfig(enabled=False))

    def log(self, msg):
        ts = time.strftime("%H:%M:%S")
        with open(LOGFILE, "a") as f:
            f.write(f"[{ts}] {msg}\n")
        print(f"[{ts}] {msg}", flush=True)

    def _parse_float(self, value: Any, default: float) -> float:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default

    def _positive_float(self, value: Any, default: float) -> float:
        parsed = self._parse_float(value, default)
        return parsed if parsed > 0 else default

    def _positive_int(self, value: Any, default: int) -> int:
        try:
            parsed = int(value)
        except (TypeError, ValueError):
            return default
        return parsed if parsed > 0 else default

    def _vad_frames(self, milliseconds: Any, default: float) -> int:
        duration = self._positive_float(milliseconds, default)
        chunk_ms = VAD_CHUNK_SAMPLES * 1000 / VAD_SAMPLE_RATE
        return max(1, math.ceil(duration / chunk_ms))

    def _new_streaming_vad(self) -> StreamingVAD:
        factory = getattr(self, "_vad_factory", StreamingVAD)
        return factory(self.vad_config)

    def _wake_command(self, text: str) -> Optional[str]:
        raw_text = text or ""
        span = find_wake_span(raw_text, getattr(self, "wake_words", None) or WAKE_WORDS)
        if span is None:
            return None
        start, end = span
        command = f"{raw_text[:start]} {raw_text[end:]}"
        command = " ".join(command.split())
        return command.strip(" \t\r\n,.:;!?-")

    def _condition_stt_audio(self, audio: np.ndarray) -> np.ndarray:
        """Sanitize microphone PCM and remove DC bias before VAD/Whisper."""
        conditioned = np.asarray(audio, dtype=np.float32).copy()
        if conditioned.size == 0:
            return conditioned
        np.nan_to_num(conditioned, copy=False, nan=0.0, posinf=1.0, neginf=-1.0)
        dc_offset = float(np.mean(conditioned, dtype=np.float64))
        if abs(dc_offset) > 1e-4:
            conditioned -= dc_offset
        return np.clip(conditioned, -1.0, 1.0)

    def _log_no_speech(self, peak: float, probability: float) -> None:
        now = self._clock()
        if now - getattr(self, "_last_audio_warning", 0.0) < 30.0:
            return
        self._last_audio_warning = now
        if peak <= 1e-5:
            self.log("Audio input is silent; check the selected microphone and hardware gain")
            return
        if peak >= 0.995 and probability < 0.1:
            self.log(
                "Audio input is saturated but Silero sees no speech; "
                "check laptop microphone gain/boost and the selected input source "
                f"(peak={peak:.4f}, max_probability={probability:.3f})"
            )
            return
        self.log(
            "No speech detected by Silero VAD "
            f"(peak={peak:.4f}, max_probability={probability:.3f})"
        )

    def _is_conversation_stop(self, text: str) -> bool:
        text_lower = (text or "").lower().strip()
        if not text_lower:
            return False
        for phrase in getattr(self, "stop_phrases", []) or []:
            normalized = " ".join(str(phrase).lower().split())
            if normalized and text_lower == normalized:
                return True
        words = text_lower.split()
        if len(words) <= 2:
            for stop in ("disable", "end", "goodbye", "bye", "stop", "quit"):
                if text_lower == stop:
                    return True
                if (
                    len(words) == 2
                    and words[0] in getattr(self, "wake_words", WAKE_WORDS)
                    and words[1] == stop
                ):
                    return True
        return False

    def _new_latency_trace(self, consume_cold: bool = True) -> Optional[LatencyTrace]:
        if not getattr(self, "latency_enabled", False):
            return None
        run_kind = "cold" if self._latency_cold_start else "warm"
        if consume_cold:
            self._latency_cold_start = False
        trace = LatencyTrace(run_kind=run_kind, sink=self.latency_sink)
        self.current_latency_trace = trace
        return trace

    def _ensure_turn_trace(self) -> Optional[LatencyTrace]:
        trace = getattr(self, "current_latency_trace", None)
        if trace is None or trace.has_event("final_transcript"):
            trace = self._new_latency_trace()
        return trace

    def _play_acknowledgement(self, turn_id: str) -> None:
        ack_player = getattr(self, "ack_player", None)
        if ack_player is None:
            return
        if getattr(self, "tts_playback_active", False):
            self.log("Ack skipped: TTS playback still active")
            return
        try:
            result = ack_player.play(turn_id)
            if result.played:
                trace = getattr(self, "current_latency_trace", None)
                if trace is not None:
                    trace.record(
                        "ack_first_audio",
                        provider=ack_player.source or "unknown",
                        observable_proxy=True,
                    )
                    trace.flush()
                self.log(f"Playing acknowledgement: '{result.phrase}' (source={result.source})")
            elif result.suppressed:
                self.log(f"Ack suppressed for turn {turn_id}")
            elif result.error:
                self.log(f"Ack skipped: {result.error}")
        except Exception as error:
            self.log(f"Ack playback error: {error}")

    def _stop_acknowledgement(self) -> None:
        ack_player = getattr(self, "ack_player", None)
        if ack_player is None:
            return
        try:
            ack_player.stop()
        except Exception as error:
            self.log(f"Ack stop error: {error}")

    async def _stop_tts(self):
        self._stop_acknowledgement()
        self.speaker.cancel_active()

    async def _interrupt_tts(self, trace: Optional[LatencyTrace]) -> None:
        self._stop_acknowledgement()
        if not self.speaker._active_turns and not getattr(
            self, "tts_playback_active", False
        ):
            return
        if trace is not None:
            trace.record("interruption_detected")
        self.speaker.cancel_active()
        if trace is not None:
            trace.record("cancellation_completed")
            trace.flush()

    def audio_callback(self, indata, _frames, _time_info, status):
        trace = getattr(self, "current_latency_trace", None)
        if trace is not None and not trace.has_event("audio_frame_received"):
            trace.record("audio_frame_received", frames=int(len(indata)))
        if status:
            self.log(f"Audio: {status}")

        if self._shutdown_requested.is_set() or self.audio_queue is None:
            return

        item = (self._audio_epoch, indata.copy())
        try:
            self.audio_queue.put_nowait(item)
        except queue.Full:
            try:
                self.audio_queue.get_nowait()
            except queue.Empty:
                pass
            try:
                self.audio_queue.put_nowait(item)
            except queue.Full:
                return
            self.dropped_audio_chunks += 1

        if self.loop is None:
            return
        with self._audio_notification_lock:
            if self._audio_notification_pending:
                return
            self._audio_notification_pending = True
        self.loop.call_soon_threadsafe(self._signal_audio_ready)

    def _signal_audio_ready(self):
        with self._audio_notification_lock:
            self._audio_notification_pending = False
        if self.audio_ready is not None:
            self.audio_ready.set()

    def request_stop(self):
        self._shutdown_requested.set()
        if self.loop is not None:
            self.loop.call_soon_threadsafe(self._signal_stop)

    def _signal_stop(self):
        if self.stop_event is not None:
            self.stop_event.set()
        if self.audio_ready is not None:
            self.audio_ready.set()

    def _stopping(self) -> bool:
        return self._shutdown_requested.is_set() or (
            self.stop_event is not None and self.stop_event.is_set()
        )

    def _raise_if_capture_failed(self) -> None:
        stream = getattr(self, "_capture_stream", None)
        error = getattr(stream, "last_error", None) if stream is not None else None
        if error:
            raise RuntimeError(f"Audio capture failed: {error}")

    async def _next_audio(self, deadline: Optional[float] = None):
        while not self._stopping():
            self._raise_if_capture_failed()
            if self.audio_ready is not None:
                self.audio_ready.clear()
            try:
                epoch, data = self.audio_queue.get_nowait()
            except queue.Empty:
                if deadline is not None and self._clock() >= deadline:
                    return None
                if self.audio_ready is None:
                    await asyncio.sleep(0)
                    continue
                timeout = 0.1
                if deadline is not None:
                    timeout = min(timeout, max(0.0, deadline - self._clock()))
                    if timeout == 0:
                        return None
                try:
                    await asyncio.wait_for(self.audio_ready.wait(), timeout=timeout)
                except asyncio.TimeoutError:
                    pass
                continue
            if epoch == self._audio_epoch:
                return data
        return None

    async def transcribe_async(self, audio_data, *, wake_mode: bool = False):
        """Run Whisper transcription in the worker pool."""
        loop = asyncio.get_event_loop()

        def _transcribe():
            if audio_data.ndim > 1:
                audio_data_mono = audio_data[:, 0]
            else:
                audio_data_mono = audio_data

            if self.input_sample_rate != self.target_sample_rate:
                audio_data_mono = resample_audio(
                    audio_data_mono,
                    self.input_sample_rate,
                    self.target_sample_rate,
                )

            audio_float = self._condition_stt_audio(audio_data_mono)
            beam_size = self.wake_beam_size if wake_mode else self.stt_beam_size
            segments, _ = self.model.transcribe(
                audio_float,
                beam_size=beam_size,
                vad_filter=False,
                language=self.stt_language,
                initial_prompt=getattr(
                    self, "wake_prompt", "Zara. Hey Zara. Zarathushtra."
                ) if wake_mode else None,
                condition_on_previous_text=False,
                no_speech_threshold=0.8 if wake_mode else 0.5,
            )

            return " ".join(seg.text.strip() for seg in segments).strip()

        return await loop.run_in_executor(self.executor, _transcribe)

    async def collect_audio_until_silence(self, first_speech_timeout=None):
        """Collect one utterance using Silero VAD endpointing."""
        tts_task = getattr(self, "tts_task", None)
        speaker = getattr(self, "speaker", None)
        speaker_active = speaker is not None and speaker.is_active
        if (tts_task is not None and not tts_task.done()) or speaker_active:
            self.log("Waiting for active playback before microphone collection")
            if tts_task is not None and not tts_task.done():
                await asyncio.gather(tts_task, return_exceptions=True)
            # Drop speaker bleed captured while Zara was talking. Bump the
            # epoch as well so any concurrent callback frame from the old
            # playback window cannot become the first frame of the next turn.
            self._audio_epoch += 1
            self.clear_queue()

        trace = self._ensure_turn_trace()
        if first_speech_timeout is None:
            first_speech_timeout = self.first_speech_timeout
        else:
            first_speech_timeout = max(
                0.0,
                self._parse_float(first_speech_timeout, self.first_speech_timeout),
            )
        deadline = self._clock() + first_speech_timeout
        turn_id = trace.trace_id if trace is not None else f"turn-{time.monotonic_ns()}"
        vad = self._new_streaming_vad()
        vad.start_turn(turn_id)
        pending = np.zeros(0, dtype=np.float32)
        speech_detected = False
        max_peak = 0.0
        max_probability = 0.0
        self.collection_status = "waiting_for_speech"

        while not self._stopping():
            data = await self._next_audio(None if speech_detected else deadline)
            if data is None:
                if self._stopping():
                    self.collection_status = "stopped"
                    return None
                self.collection_status = "first_speech_timeout"
                self._log_no_speech(max_peak, max_probability)
                return None

            mono = data[:, 0] if data.ndim > 1 else data
            mono = mono.astype(np.float32, copy=False)
            if self.input_sample_rate != VAD_SAMPLE_RATE:
                mono = resample_audio(mono, self.input_sample_rate, VAD_SAMPLE_RATE)
            if len(mono):
                max_peak = max(max_peak, float(np.max(np.abs(mono))))
            pending = np.concatenate((pending, mono))

            while len(pending) >= VAD_CHUNK_SAMPLES:
                chunk = pending[:VAD_CHUNK_SAMPLES]
                pending = pending[VAD_CHUNK_SAMPLES:]
                chunk = self._condition_stt_audio(chunk)
                events = vad.feed(chunk)
                max_probability = max(max_probability, vad.last_probability)
                for event in events:
                    if isinstance(event, SpeechStarted):
                        speech_detected = True
                        self.collection_status = "recording"
                        deadline = None
                        self.log("Speech detected (Silero VAD)")
                        if trace is not None:
                            trace.record("speech_start")
                        if getattr(self, "stop_on_interrupt", False):
                            await self._interrupt_tts(trace)
                    elif isinstance(event, SpeechEnded):
                        self.collection_status = event.reason
                        audio = vad.speech_audio
                        if self.input_sample_rate != VAD_SAMPLE_RATE:
                            audio = resample_audio(
                                audio,
                                VAD_SAMPLE_RATE,
                                self.input_sample_rate,
                            )
                        if trace is not None:
                            trace.record("speech_end", reason=event.reason)
                            trace.flush()
                        return audio.reshape(-1, CHANNELS)

            if not speech_detected and self._clock() >= deadline:
                self.collection_status = "first_speech_timeout"
                self._log_no_speech(max_peak, max_probability)
                return None

        self.collection_status = "stopped"
        return None

    def clear_queue(self):
        if self.audio_queue is None:
            return
        while True:
            try:
                self.audio_queue.get_nowait()
            except queue.Empty:
                break

    def transition_to(self, state: str):
        if state == self.state:
            return
        self._audio_epoch += 1
        self.clear_queue()
        self.state = state

    def check_wake_word(self, text):
        return self._wake_command(text) is not None

    def in_conversation_mode(self) -> bool:
        return self.state == "ACTIVE"

    async def _monitor_speech_during_llm(self) -> bool:
        if self.audio_queue is None:
            await asyncio.sleep(0.1)
            return False

        vad = StreamingVAD(
            VADConfig(
                vad_threshold=0.7,
                min_speech_frames=6,
                trailing_silence_frames=16,
                max_utterance_frames=938,
                no_speech_timeout_frames=312,
                pre_speech_buffer_chunks=10,
            )
        )
        vad.start_turn("barge-in")

        chunk_buffer: list[np.ndarray] = []
        buffer_target = VAD_CHUNK_SAMPLES

        while not self._stopping():
            deadline = self._clock() + 0.2
            data = await self._next_audio(deadline=deadline)
            if data is None:
                continue

            mono = data[:, 0] if data.ndim > 1 else data

            if self.input_sample_rate != VAD_SAMPLE_RATE:
                mono = resample_audio(mono, self.input_sample_rate, VAD_SAMPLE_RATE)

            chunk_buffer.append(mono)
            available = sum(len(c) for c in chunk_buffer)

            while available >= buffer_target:
                combined = np.concatenate(chunk_buffer) if len(chunk_buffer) > 1 else chunk_buffer[0]
                vad_chunk = self._condition_stt_audio(combined[:buffer_target])
                remainder = combined[buffer_target:]
                chunk_buffer = [remainder] if len(remainder) > 0 else []

                events = vad.feed(vad_chunk)
                for event in events:
                    if isinstance(event, SpeechStarted):
                        self.log("🔊 Barge-in during LLM (VAD speech detected)")
                        return True

                available = sum(len(c) for c in chunk_buffer)

        return False

    async def run_async(self):
        self.loop = asyncio.get_running_loop()
        self._shutdown_requested.clear()
        self.audio_queue = queue.Queue(maxsize=self.audio_queue_chunks)
        self.audio_ready = asyncio.Event()
        self.stop_event = asyncio.Event()

        self.log("🔥 Starting Wake Word Listener (daemon voice client)")
        self.log(
            f"Wake words: {', '.join(getattr(self, 'wake_words', None) or WAKE_WORDS)}"
        )

        self._connect_daemon()

        with open(PIDFILE, "w") as f:
            f.write(str(os.getpid()))

        try:
            capture_stream = sd.InputStream(
                samplerate=self.input_sample_rate,
                channels=CHANNELS,
                callback=self.audio_callback,
            )
            self._capture_stream = capture_stream
            with capture_stream:
                source = getattr(capture_stream, "source", None)
                source_note = f" source='{source}'" if source else ""
                self.log(
                    "✅ Wake listener ready; say "
                    f"'{(getattr(self, 'wake_words', None) or WAKE_WORDS)[-1].capitalize()}'."
                    f"{source_note}"
                )
                while not self._stopping():
                    self._raise_if_capture_failed()
                    self.daemon.ensure_connected()
                    if self.state == "PASSIVE":
                        await self.passive_mode_async()
                    elif self.state == "ACTIVE":
                        await self.active_mode_async()
        finally:
            self._capture_stream = None
            self.request_stop()
            self.speaker.close()
            self.daemon.close()
            pathlib.Path(PIDFILE).unlink(missing_ok=True)
            self.log("Stopped")

    def _connect_daemon(self) -> None:
        self.daemon.connect()
        self._wire_daemon_handlers()
        self.daemon.start_pump()
        self.speaker.on_playback_started = self._on_playback_started
        self.speaker.on_playback_finished = self._on_playback_finished
        self.log(
            "Connected to Zara daemon "
            f"(conversation ready, audio output {self.daemon.audio_output_format})"
        )

    def _on_playback_started(self) -> None:
        self._stop_acknowledgement()
        trace = getattr(self, "current_latency_trace", None)
        if trace is not None:
            trace.record("tts_first_playback", observable_proxy=True)
            trace.flush()

    def _on_playback_finished(self) -> None:
        trace = getattr(self, "current_latency_trace", None)
        if trace is not None:
            trace.record("tts_final_playback")
            trace.flush()

    def _wire_daemon_handlers(self) -> None:
        self.daemon.on_transcript_partial.append(self._on_transcript_partial)
        self.daemon.on_transcript_final.append(self._on_transcript_final)
        self.daemon.on_assistant_delta.append(self._on_assistant_delta)
        self.daemon.on_assistant_complete.append(self._on_assistant_complete)
        self.daemon.on_turn_started.append(self._on_turn_started)
        self.daemon.on_turn_completed.append(self._on_turn_completed)
        self.daemon.on_turn_cancelled.append(self._on_turn_cancelled)

    def _on_transcript_partial(self, event) -> None:
        self.log(f"… {event.text}")

    def _on_transcript_final(self, event) -> None:
        trace = getattr(self, "current_latency_trace", None)
        if trace is not None and event.trace_id == trace.trace_id:
            trace.record("final_transcript", text_length=len(event.text))
            trace.flush()
        if self._is_conversation_stop(event.text):
            self._stop_phrase_seen.set()
            self._turn_finished.set()

    def _on_assistant_delta(self, event) -> None:
        try:
            from .pets import runtime_bridge

            runtime_bridge.response_text(event.text, label="assistant")
        except Exception:
            pass

    def _on_assistant_complete(self, event) -> None:
        try:
            from .pets import runtime_bridge

            runtime_bridge.response_text(event.text, label="assistant")
        except Exception:
            pass
        text = (event.text or "").strip()
        if text:
            loop = self.loop
            if loop is not None:
                loop.call_soon_threadsafe(
                    lambda: loop.create_task(
                        send_notification_async("Zara", text, urgency="normal")
                    )
                )

    def _on_turn_started(self, event) -> None:
        self._active_daemon_turn_id = event.turn_id

    def _on_turn_completed(self, event) -> None:
        if event.turn_id == self._active_daemon_turn_id:
            self._turn_finished.set()
            self._conversation_last_activity = self._clock()

    def _on_turn_cancelled(self, event) -> None:
        if event.turn_id == self._active_daemon_turn_id:
            trace = getattr(self, "current_latency_trace", None)
            if trace is not None:
                trace.record("cancellation_completed")
                trace.flush()
            self._turn_finished.set()

    async def passive_mode_async(self):
        trace = self._new_latency_trace(consume_cold=False)
        chunk = await self.collect_audio_until_silence(self.first_speech_timeout)
        if chunk is None:
            self.current_latency_trace = None
            return

        text = await self.transcribe_async(chunk, wake_mode=True)
        self.log(f"Passive transcript: {text!r}")
        command = self._wake_command(text)

        if command is not None:
            self._latency_cold_start = False
            self._pending_wake_audio = chunk
            if trace is not None:
                trace.record("wake_detected")
                trace.record("ack_requested", channel="notification")
            await send_notification_async("Zara", "Listening", "normal", 1000)
            self.log("🔥 Wake word detected")
            if trace is not None:
                trace.flush()
            self.transition_to("ACTIVE")
            self._conversation_last_activity = self._clock()
        else:
            self.current_latency_trace = None

    def _conversation_timed_out(self) -> bool:
        if self.state != "ACTIVE":
            return False
        last = getattr(self, "_conversation_last_activity", 0.0)
        if last <= 0.0:
            return False
        return (self._clock() - last) > self.conversation_timeout

    def _cancel_daemon_turn(self) -> None:
        turn_id = self._active_daemon_turn_id
        if not turn_id:
            return
        self.daemon.submit_cancel(turn_id)
        self.speaker.cancel_active()

    async def _wait_for_turn_completion(self, deadline: float) -> bool:
        while self._clock() < deadline:
            if self._stopping():
                return False
            if self._turn_finished.is_set():
                return True
            await asyncio.sleep(0.05)
        self.log("Daemon turn timed out; continuing to listen")
        return False

    async def active_mode_async(self):
        trace = self._ensure_turn_trace()
        if self._conversation_timed_out():
            self.log("Conversation timeout, exiting conversation mode")
            self.transition_to("PASSIVE")
            self._conversation_last_activity = 0.0
            return

        pending_audio = self._pending_wake_audio
        self._pending_wake_audio = None
        if pending_audio is not None:
            utterance = pending_audio
        else:
            first_speech_timeout = self.first_speech_timeout
            if self._conversation_last_activity > 0.0:
                remaining = self.conversation_timeout - (
                    self._clock() - self._conversation_last_activity
                )
                if remaining <= 0:
                    self.transition_to("PASSIVE")
                    self._conversation_last_activity = 0.0
                    return
                first_speech_timeout = max(first_speech_timeout, remaining)
            utterance = await self.collect_audio_until_silence(first_speech_timeout)
            if utterance is None:
                if self.collection_status == "stopped":
                    return
                self.log("⏸️ No speech - returning to passive")
                self.transition_to("PASSIVE")
                self._conversation_last_activity = 0.0
                return

        if self.stop_on_interrupt:
            await self._stop_tts()

        turn_id = (
            trace.trace_id
            if trace is not None
            else f"turn-{int(time.time() * 1000)}"
        )
        self._play_acknowledgement(turn_id)

        self._turn_finished.clear()
        self._stop_phrase_seen.clear()
        self._active_daemon_turn_id = None
        try:
            stream_id = await self.daemon.stream_utterance(
                utterance, trace_id=turn_id
            )
        except WakeDaemonUnavailable as error:
            raise
        self._active_stream_id = stream_id
        if trace is not None:
            trace.record("route_selected", route="daemon")

        monitor_task = asyncio.create_task(self._monitor_speech_during_llm())
        wait_task = asyncio.create_task(
            self._wait_for_turn_completion(self._clock() + self.response_timeout)
        )
        done, _pending = await asyncio.wait(
            {monitor_task, wait_task},
            return_when=asyncio.FIRST_COMPLETED,
        )

        if monitor_task in done and monitor_task.result():
            wait_task.cancel()
            try:
                await wait_task
            except (asyncio.CancelledError, Exception):
                pass
            self.log("🔄 Barge-in: cancelling daemon turn, restarting listening")
            self._stop_acknowledgement()
            await self._stop_tts()
            ack_player = getattr(self, "ack_player", None)
            if ack_player is not None:
                ack_player.reset()
            self._cancel_daemon_turn()
            self.transition_to("ACTIVE")
            return

        monitor_task.cancel()
        try:
            await monitor_task
        except (asyncio.CancelledError, Exception):
            pass
        completed = await wait_task

        if self._stop_phrase_seen.is_set():
            self.log("Stop phrase detected; returning to passive mode")
            self._stop_phrase_seen.clear()
            self.transition_to("PASSIVE")
            self._conversation_last_activity = 0.0
            return

        if completed:
            self._conversation_last_activity = self._clock()

def run_wake_listener(model="small", device="cpu", enable_tts=True,
                      with_pets=False) -> int:
    """Run the wake listener; return a process exit code (fails closed)."""
    pet_proc = None
    if with_pets:
        pet_proc = _launch_pet_overlay()
        from .pets import runtime_bridge
        runtime_bridge._ensure_publisher()
        import time
        time.sleep(0.3)

    listener = None
    try:
        listener = WakeWordListener(
            model=model,
            device=device,
            enable_tts=enable_tts,
        )

        async def run_with_cleanup():
            try:
                await listener.run_async()
            except KeyboardInterrupt:
                listener.request_stop()
                listener.log("Interrupted")
            finally:
                listener.request_stop()
                if listener.executor is not None:
                    listener.executor.shutdown(wait=True)

        asyncio.run(run_with_cleanup())
    except WakeDaemonUnavailable as error:
        print(f"Error: {error}", file=sys.stderr)
        print(
            "Start the Zara daemon with: nix run .#zara-server "
            "(or systemd zara-server.service)",
            file=sys.stderr,
        )
        return 2
    except KeyboardInterrupt:
        pass
    finally:
        if pet_proc is not None:
            pet_proc.terminate()
            try:
                pet_proc.wait(timeout=3)
            except Exception:
                pet_proc.kill()

    return 0


def main(model="small", device="cpu", prolog_main_path=None, enable_tts=True,
         with_pets=False):
    return run_wake_listener(
        model=model,
        device=device,
        enable_tts=enable_tts,
        with_pets=with_pets,
    )


def _launch_pet_overlay():
    import subprocess
    import sys
    return subprocess.Popen(
        [sys.executable, "-m", "zara", "--pets"],
        stdout=subprocess.DEVNULL,
        stderr=None,
    )
