"""Offline speaker analysis for Zara's Prolog Voice Expert.

Audio is first gated by Zara's existing Silero VAD, then speaker labels are
produced by sherpa-onnx diarization. Only the intersection is retained, so
speaker facts describe speech rather than arbitrary model windows.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Optional

import numpy as np
import soundfile as sf

from zara.prolog_engine import _prolog_string
from zara.streaming_stt import (
    SpeechEnded,
    SpeechStarted,
    StreamingVAD,
    VADConfig,
    VAD_CHUNK_SAMPLES,
    VAD_SAMPLE_RATE,
)


MAX_ANALYSIS_SEGMENTS = 512
DEFAULT_MAX_SOURCE_SECONDS = 1800.0
MIN_INTERSECTION_SECONDS = 0.12


class VoiceAnalysisError(RuntimeError):
    """Speaker analysis could not produce trustworthy bounded output."""


@dataclass(frozen=True)
class SpeakerSegment:
    index: int
    speaker: str
    start: float
    end: float

    @property
    def duration(self) -> float:
        return max(0.0, self.end - self.start)

    def to_dict(self) -> dict[str, Any]:
        return asdict(self) | {"duration": self.duration}


class VoiceAnalyzer:
    """Run VAD-gated offline diarization and project results into Prolog."""

    def __init__(self, *, config: Any, prolog_engine: Any) -> None:
        if prolog_engine is None:
            raise ValueError("VoiceAnalyzer requires Zara's live Prolog engine")
        self.config = config
        self.prolog_engine = prolog_engine

    def analyze_wav(
        self,
        wav_path: str | Path,
        *,
        source_id: str,
        num_speakers: Optional[int] = None,
        persist_to_kb: bool = True,
    ) -> list[SpeakerSegment]:
        path = Path(wav_path)
        if not path.is_file():
            raise FileNotFoundError(f"analysis WAV not found: {path}")
        source_id = str(source_id).strip()
        if not source_id:
            raise ValueError("source_id must not be empty")

        audio, sample_rate = sf.read(
            path,
            dtype="float32",
            always_2d=True,
        )
        mono = np.asarray(audio[:, 0], dtype=np.float32)
        if sample_rate != VAD_SAMPLE_RATE:
            raise VoiceAnalysisError(
                f"analysis audio must be {VAD_SAMPLE_RATE} Hz; got {sample_rate}"
            )
        max_samples = int(self._max_source_seconds() * VAD_SAMPLE_RATE)
        if len(mono) > max_samples:
            mono = mono[:max_samples]
        if len(mono) < VAD_CHUNK_SAMPLES:
            raise VoiceAnalysisError("analysis audio is too short")

        speech_intervals = self._vad_intervals(mono)
        if not speech_intervals:
            if persist_to_kb:
                self._store_segments(source_id, [])
            return []

        diarized = self._diarize(mono, num_speakers=num_speakers)
        segments = self._intersect(speech_intervals, diarized)
        if persist_to_kb:
            self._store_segments(source_id, segments)
        return segments

    def _vad_intervals(self, audio: np.ndarray) -> list[tuple[float, float]]:
        stt = self._section("stt")
        config = VADConfig(
            vad_threshold=self._bounded_float(
                stt.get("vad_threshold", 0.5), 0.5, 0.0, 1.0
            ),
            min_speech_frames=self._ms_frames(stt.get("min_speech_ms", 128), 128),
            trailing_silence_frames=self._ms_frames(
                stt.get("trailing_silence_ms", 320), 320
            ),
            max_utterance_frames=self._ms_frames(
                stt.get("max_utterance_ms", 30000), 30000
            ),
            no_speech_timeout_frames=self._ms_frames(
                stt.get("no_speech_timeout_ms", 5000), 5000
            ),
            pre_speech_buffer_chunks=self._bounded_int(
                stt.get("pre_speech_buffer_chunks", 10), 10, 0, 128
            ),
            partial_interval_frames=self._ms_frames(
                stt.get("partial_transcript_ms", 1000), 1000
            ),
        )
        vad = StreamingVAD(config)
        turn_number = 0
        vad.start_turn(f"voice-analysis-{turn_number}")
        current_start: Optional[int] = None
        intervals: list[tuple[float, float]] = []

        full_samples = len(audio) - (len(audio) % VAD_CHUNK_SAMPLES)
        for offset in range(0, full_samples, VAD_CHUNK_SAMPLES):
            chunk = audio[offset : offset + VAD_CHUNK_SAMPLES]
            chunk_end = offset + VAD_CHUNK_SAMPLES
            events = vad.feed(chunk)
            for event in events:
                if isinstance(event, SpeechStarted):
                    current_start = max(
                        0,
                        chunk_end - int(event.pre_speech_samples),
                    )
                elif isinstance(event, SpeechEnded):
                    start = (
                        current_start
                        if current_start is not None
                        else max(0, chunk_end - VAD_CHUNK_SAMPLES)
                    )
                    if chunk_end > start:
                        intervals.append(
                            (start / VAD_SAMPLE_RATE, chunk_end / VAD_SAMPLE_RATE)
                        )
                    turn_number += 1
                    vad.start_turn(f"voice-analysis-{turn_number}")
                    current_start = None

        if vad.state == "speaking":
            committed = vad.commit(f"voice-analysis-{turn_number}")
            if any(isinstance(event, SpeechEnded) for event in committed):
                start = current_start if current_start is not None else full_samples
                end = len(audio)
                if end > start:
                    intervals.append(
                        (start / VAD_SAMPLE_RATE, end / VAD_SAMPLE_RATE)
                    )

        return self._merge_intervals(intervals)

    def _diarize(
        self,
        audio: np.ndarray,
        *,
        num_speakers: Optional[int],
    ) -> list[tuple[str, float, float]]:
        voice = self._section("voice_expert")
        segmentation_model = self._model_path(
            voice.get("diarization_segmentation_model", ""),
            "voice_expert.diarization_segmentation_model",
        )
        embedding_model = self._model_path(
            voice.get("diarization_embedding_model", ""),
            "voice_expert.diarization_embedding_model",
        )

        try:
            import sherpa_onnx
        except ImportError as error:
            raise VoiceAnalysisError(
                "speaker diarization requires sherpa-onnx>=1.10.28"
            ) from error

        configured_speakers = self._bounded_int(
            voice.get("diarization_num_speakers", -1),
            -1,
            -1,
            64,
        )
        actual_num_speakers = configured_speakers
        if num_speakers is not None:
            actual_num_speakers = self._bounded_int(
                num_speakers,
                -1,
                1,
                64,
            )
        threshold = self._bounded_float(
            voice.get("diarization_cluster_threshold", 0.5),
            0.5,
            0.01,
            2.0,
        )

        config = sherpa_onnx.OfflineSpeakerDiarizationConfig(
            segmentation=sherpa_onnx.OfflineSpeakerSegmentationModelConfig(
                pyannote=sherpa_onnx.OfflineSpeakerSegmentationPyannoteModelConfig(
                    model=str(segmentation_model),
                    window_shift_ratio=0.1,
                ),
            ),
            embedding=sherpa_onnx.SpeakerEmbeddingExtractorConfig(
                model=str(embedding_model),
            ),
            clustering=sherpa_onnx.FastClusteringConfig(
                num_clusters=actual_num_speakers,
                threshold=threshold,
            ),
            min_duration_on=self._bounded_float(
                voice.get("diarization_min_duration_on", 0.3),
                0.3,
                0.05,
                5.0,
            ),
            min_duration_off=self._bounded_float(
                voice.get("diarization_min_duration_off", 0.5),
                0.5,
                0.05,
                5.0,
            ),
        )
        if not config.validate():
            raise VoiceAnalysisError(
                "invalid sherpa-onnx speaker diarization configuration"
            )
        diarizer = sherpa_onnx.OfflineSpeakerDiarization(config)
        if int(diarizer.sample_rate) != VAD_SAMPLE_RATE:
            raise VoiceAnalysisError(
                "configured speaker diarization models must use 16 kHz audio"
            )

        result = diarizer.process(audio).sort_by_start_time()
        rows: list[tuple[str, float, float]] = []
        for row in result:
            start = max(0.0, float(row.start))
            end = max(start, float(row.end))
            if end <= start:
                continue
            rows.append((f"speaker_{int(row.speaker):02d}", start, end))
        return rows

    def _intersect(
        self,
        speech_intervals: list[tuple[float, float]],
        diarized: list[tuple[str, float, float]],
    ) -> list[SpeakerSegment]:
        raw: list[tuple[str, float, float]] = []
        for speaker, speaker_start, speaker_end in diarized:
            for speech_start, speech_end in speech_intervals:
                start = max(speaker_start, speech_start)
                end = min(speaker_end, speech_end)
                if end - start >= MIN_INTERSECTION_SECONDS:
                    raw.append((speaker, start, end))

        raw.sort(key=lambda row: (row[1], row[2], row[0]))
        merged: list[tuple[str, float, float]] = []
        for speaker, start, end in raw:
            if (
                merged
                and merged[-1][0] == speaker
                and start - merged[-1][2] <= 0.15
            ):
                prior_speaker, prior_start, prior_end = merged[-1]
                merged[-1] = (
                    prior_speaker,
                    prior_start,
                    max(prior_end, end),
                )
            else:
                merged.append((speaker, start, end))

        bounded = merged[:MAX_ANALYSIS_SEGMENTS]
        return [
            SpeakerSegment(
                index=index,
                speaker=speaker,
                start=round(start, 3),
                end=round(end, 3),
            )
            for index, (speaker, start, end) in enumerate(bounded)
        ]

    def _store_segments(
        self,
        source_id: str,
        segments: list[SpeakerSegment],
    ) -> None:
        terms = ",".join(
            "segment("
            f"{segment.index},"
            f"{_prolog_string(segment.speaker)},"
            f"{int(round(segment.start * 1000))},"
            f"{int(round(segment.end * 1000))}"
            ")"
            for segment in segments
        )
        goal = (
            "kb_voice_expert:replace_speaker_segments("
            f"{_prolog_string(source_id)},[{terms}])"
        )
        self.prolog_engine.query_once(goal)

    def _section(self, name: str) -> dict[str, Any]:
        if hasattr(self.config, "get_section"):
            return dict(self.config.get_section(name) or {})
        if isinstance(self.config, dict):
            return dict(self.config.get(name, {}) or {})
        return {}

    def _max_source_seconds(self) -> float:
        voice = self._section("voice_expert")
        return self._bounded_float(
            voice.get("max_source_seconds", DEFAULT_MAX_SOURCE_SECONDS),
            DEFAULT_MAX_SOURCE_SECONDS,
            5.0,
            14_400.0,
        )

    @staticmethod
    def _model_path(value: object, field: str) -> Path:
        text = str(value or "").strip()
        if not text:
            raise VoiceAnalysisError(
                f"{field} is not configured"
            )
        path = Path(text).expanduser()
        if not path.is_file():
            raise VoiceAnalysisError(f"{field} does not exist: {path}")
        return path

    @staticmethod
    def _ms_frames(value: object, default: int) -> int:
        try:
            milliseconds = float(value)
        except (TypeError, ValueError):
            milliseconds = float(default)
        if not math.isfinite(milliseconds) or milliseconds <= 0:
            milliseconds = float(default)
        return max(1, int(math.ceil(milliseconds / 32.0)))

    @staticmethod
    def _bounded_int(
        value: object,
        default: int,
        minimum: int,
        maximum: int,
    ) -> int:
        if isinstance(value, bool):
            return default
        try:
            parsed = int(value)
        except (TypeError, ValueError):
            return default
        return parsed if minimum <= parsed <= maximum else default

    @staticmethod
    def _bounded_float(
        value: object,
        default: float,
        minimum: float,
        maximum: float,
    ) -> float:
        if isinstance(value, bool):
            return default
        try:
            parsed = float(value)
        except (TypeError, ValueError):
            return default
        if not math.isfinite(parsed) or not minimum <= parsed <= maximum:
            return default
        return parsed

    @staticmethod
    def _merge_intervals(
        intervals: list[tuple[float, float]],
    ) -> list[tuple[float, float]]:
        if not intervals:
            return []
        ordered = sorted(intervals)
        merged = [ordered[0]]
        for start, end in ordered[1:]:
            prior_start, prior_end = merged[-1]
            if start <= prior_end + 0.032:
                merged[-1] = (prior_start, max(prior_end, end))
            else:
                merged.append((start, end))
        return merged


__all__ = [
    "SpeakerSegment",
    "VoiceAnalysisError",
    "VoiceAnalyzer",
]
