"""Prolog-authoritative voice planning and bounded voice/media tools.

The language model may propose semantic speaker labels, roles, or a preferred
voice. It never owns the final selection: every new speaker is resolved by
kb_voice_expert:resolve_voice/5 and Python only executes that result.
"""

from __future__ import annotations

import asyncio
import json
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Literal, Optional
from urllib.parse import urlsplit

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from zara.prolog_engine import _prolog_string
from zara.speech_activity import speech_activity
from zara.tts import Qwen3TTSClient, TTSEngine


MAX_YOUTUBE_RESULTS = 10
MAX_QUERY_CHARS = 300
MAX_SEGMENTS = 64
MAX_TOTAL_NARRATION_CHARS = 50_000
MAX_SEGMENT_CHARS = 8_000
MAX_VOICE_NAME_CHARS = 80
MAX_SPEAKER_CHARS = 80
DOWNLOAD_TIMEOUT_SECONDS = 180
SEARCH_TIMEOUT_SECONDS = 45
PLAYBACK_TIMEOUT_SECONDS = 600


class VoiceSegment(BaseModel):
    text: str = Field(..., min_length=1, max_length=MAX_SEGMENT_CHARS)
    speaker: str = Field("narrator", min_length=1, max_length=MAX_SPEAKER_CHARS)
    role: str = Field("narrator", min_length=1, max_length=MAX_SPEAKER_CHARS)
    requested_voice: Optional[str] = Field(default=None, max_length=MAX_VOICE_NAME_CHARS)


class VoicePlanArgs(BaseModel):
    segments: list[VoiceSegment] = Field(..., min_length=1, max_length=MAX_SEGMENTS)
    mode: Literal["smart", "single", "multi"] = "smart"


class VoiceSpeakArgs(BaseModel):
    text: str = Field(..., min_length=1, max_length=MAX_SEGMENT_CHARS)
    speaker: str = Field("narrator", min_length=1, max_length=MAX_SPEAKER_CHARS)
    role: str = Field("narrator", min_length=1, max_length=MAX_SPEAKER_CHARS)
    requested_voice: Optional[str] = Field(default=None, max_length=MAX_VOICE_NAME_CHARS)


class YouTubeSearchArgs(BaseModel):
    query: str = Field(..., min_length=1, max_length=MAX_QUERY_CHARS)
    limit: int = Field(5, ge=1, le=MAX_YOUTUBE_RESULTS)


class VoiceCloneYouTubeArgs(BaseModel):
    url: str = Field(..., min_length=1, max_length=2048)
    voice_name: str = Field(..., min_length=1, max_length=MAX_VOICE_NAME_CHARS)
    start_seconds: float = Field(0.0, ge=0.0, le=86_400.0)
    duration_seconds: float = Field(15.0, ge=3.0, le=45.0)
    reference_text: str = Field("", max_length=2000)
    rights_basis: Literal[
        "self",
        "explicit_permission",
        "licensed",
        "public_domain",
        "synthetic",
    ]
    subject_is_public_figure: bool = Field(
        False,
        description="Must be false. This tool does not support public-figure voice cloning.",
    )


class VoiceDeleteArgs(BaseModel):
    voice_name: str = Field(..., min_length=1, max_length=MAX_VOICE_NAME_CHARS)


class VoiceExpert:
    """Execute voice operations while keeping Prolog authoritative."""

    def __init__(self, prolog_engine: Any, config: Any) -> None:
        if prolog_engine is None:
            raise ValueError("Voice Expert requires Zara's live Prolog engine")
        self.prolog_engine = prolog_engine
        self.config = config

    def youtube_search(self, query: str, limit: int = 5) -> str:
        query = " ".join(str(query).split())
        if not query:
            raise ValueError("YouTube query must not be empty")
        if len(query) > MAX_QUERY_CHARS:
            raise ValueError(f"YouTube query exceeds {MAX_QUERY_CHARS} characters")
        limit = max(1, min(int(limit), MAX_YOUTUBE_RESULTS))
        ytdlp = self._require_binary("yt-dlp")
        command = [
            ytdlp,
            "--dump-single-json",
            "--skip-download",
            "--flat-playlist",
            "--no-warnings",
            "--quiet",
            "--playlist-end",
            str(limit),
            "--sleep-requests",
            "1",
            "--sleep-interval",
            "1",
            "--max-sleep-interval",
            "3",
            f"ytsearch{limit}:{query}",
        ]
        completed = self._run(command, timeout=SEARCH_TIMEOUT_SECONDS)
        payload = json.loads(completed.stdout or "{}")
        rows = []
        for entry in list(payload.get("entries") or [])[:limit]:
            if not isinstance(entry, dict):
                continue
            video_id = str(entry.get("id") or "")
            webpage_url = str(entry.get("webpage_url") or entry.get("url") or "")
            if video_id and not webpage_url.startswith(("http://", "https://")):
                webpage_url = f"https://www.youtube.com/watch?v={video_id}"
            rows.append(
                {
                    "id": video_id,
                    "title": str(entry.get("title") or ""),
                    "channel": str(
                        entry.get("channel")
                        or entry.get("uploader")
                        or entry.get("channel_name")
                        or ""
                    ),
                    "duration": entry.get("duration"),
                    "url": webpage_url,
                }
            )
        return json.dumps({"query": query, "results": rows}, ensure_ascii=False)

    def list_voices(self) -> str:
        return json.dumps(
            {"provider": self._provider(), "voices": self._available_voices()},
            ensure_ascii=False,
        )

    def plan(
        self,
        segments: list[VoiceSegment | dict[str, Any]],
        mode: str = "smart",
    ) -> list[dict[str, str]]:
        normalized = [
            segment
            if isinstance(segment, VoiceSegment)
            else VoiceSegment.model_validate(segment)
            for segment in segments
        ]
        if not normalized or len(normalized) > MAX_SEGMENTS:
            raise ValueError(f"voice plan requires 1 to {MAX_SEGMENTS} segments")
        total_chars = sum(len(segment.text) for segment in normalized)
        if total_chars > MAX_TOTAL_NARRATION_CHARS:
            raise ValueError(
                f"narration exceeds {MAX_TOTAL_NARRATION_CHARS} total characters"
            )
        if mode not in {"smart", "single", "multi"}:
            raise ValueError("mode must be smart, single, or multi")

        available = self._available_voices()
        if not available:
            raise RuntimeError("configured TTS provider has no available voices")

        speaker_voices: dict[str, str] = {}
        used: list[str] = []
        plan: list[dict[str, str]] = []
        for index, segment in enumerate(normalized):
            speaker = self._speaker(segment.speaker)
            requested = str(segment.requested_voice or "").strip()
            role = self._role(segment.role)
            if mode == "single":
                speaker = "narrator"
                role = "narrator"
                requested = requested if index == 0 else ""

            if speaker in speaker_voices and not requested:
                voice = speaker_voices[speaker]
            else:
                voice = self._resolve_voice(
                    role=role,
                    requested_voice=requested,
                    available=available,
                    used=used if mode != "single" else [],
                )
                speaker_voices[speaker] = voice
                if voice not in used:
                    used.append(voice)

            plan.append(
                {
                    "text": segment.text,
                    "speaker": speaker,
                    "role": role,
                    "voice": voice,
                }
            )
        return plan

    def plan_json(self, segments: list[VoiceSegment], mode: str = "smart") -> str:
        return json.dumps(
            {"provider": self._provider(), "segments": self.plan(segments, mode)},
            ensure_ascii=False,
        )

    def speak(
        self,
        text: str,
        speaker: str = "narrator",
        role: str = "narrator",
        requested_voice: Optional[str] = None,
    ) -> str:
        return self.narrate(
            [
                VoiceSegment(
                    text=text,
                    speaker=speaker,
                    role=role,
                    requested_voice=requested_voice,
                )
            ],
            mode="smart",
        )

    def narrate(self, segments: list[VoiceSegment], mode: str = "smart") -> str:
        plan = self.plan(segments, mode)
        mpv = self._require_binary("mpv")
        paths: list[str] = []
        with tempfile.TemporaryDirectory(prefix="zara-voice-") as temp_dir:
            root = Path(temp_dir)
            for index, segment in enumerate(plan):
                result = self._synthesize(segment["text"], segment["voice"])
                if not result.success:
                    raise RuntimeError(result.error or "TTS synthesis failed")
                suffix = ".mp3" if result.audio_format == "mp3" else ".wav"
                path = root / f"{index:04d}{suffix}"
                path.write_bytes(result.audio)
                paths.append(str(path))

            activity = speech_activity.begin(source="voice-expert")
            try:
                self._run(
                    [
                        mpv,
                        "--no-video",
                        "--audio-display=no",
                        "--really-quiet",
                        "--no-terminal",
                        "--gapless-audio=yes",
                        *paths,
                    ],
                    timeout=PLAYBACK_TIMEOUT_SECONDS,
                )
            finally:
                speech_activity.end(activity.token)
        return json.dumps(
            {
                "spoken": True,
                "provider": self._provider(),
                "segments": [
                    {
                        "speaker": item["speaker"],
                        "role": item["role"],
                        "voice": item["voice"],
                    }
                    for item in plan
                ],
            },
            ensure_ascii=False,
        )

    def clone_from_youtube(
        self,
        url: str,
        voice_name: str,
        start_seconds: float = 0.0,
        duration_seconds: float = 15.0,
        reference_text: str = "",
        rights_basis: str = "self",
        subject_is_public_figure: bool = False,
    ) -> str:
        self._validate_clone_rights(rights_basis, subject_is_public_figure)
        self._validate_voice_name(voice_name)
        self._youtube_url(url)
        ytdlp = self._require_binary("yt-dlp")
        ffmpeg = self._require_binary("ffmpeg")

        with tempfile.TemporaryDirectory(prefix="zara-voice-ref-") as temp_dir:
            root = Path(temp_dir)
            output_template = str(root / "source.%(ext)s")
            self._run(
                [
                    ytdlp,
                    "--no-playlist",
                    "--no-warnings",
                    "--quiet",
                    "--sleep-requests",
                    "1",
                    "--sleep-interval",
                    "1",
                    "--max-sleep-interval",
                    "3",
                    "-f",
                    "bestaudio/best",
                    "-o",
                    output_template,
                    url,
                ],
                timeout=DOWNLOAD_TIMEOUT_SECONDS,
            )
            candidates = sorted(path for path in root.glob("source.*") if path.is_file())
            if len(candidates) != 1:
                raise RuntimeError("yt-dlp did not produce exactly one reference file")
            source = candidates[0]
            wav_path = root / "reference.wav"
            self._run(
                [
                    ffmpeg,
                    "-nostdin",
                    "-hide_banner",
                    "-loglevel",
                    "error",
                    "-ss",
                    f"{float(start_seconds):.3f}",
                    "-i",
                    str(source),
                    "-t",
                    f"{float(duration_seconds):.3f}",
                    "-vn",
                    "-ac",
                    "1",
                    "-ar",
                    "24000",
                    "-c:a",
                    "pcm_s16le",
                    "-y",
                    str(wav_path),
                ],
                timeout=90,
            )
            if not wav_path.is_file() or wav_path.stat().st_size < 1024:
                raise RuntimeError("reference extraction produced no usable WAV audio")
            result = asyncio.run(
                self._qwen_client().register_voice(
                    voice_name,
                    str(wav_path),
                    reference_text=str(reference_text).strip(),
                )
            )
        return json.dumps(
            {
                "registered": True,
                "provider": "qwen3",
                "voice": voice_name,
                "rights_basis": rights_basis,
                "provider_result": result,
            },
            ensure_ascii=False,
        )

    def delete_voice(self, voice_name: str) -> str:
        self._validate_voice_name(voice_name)
        result = asyncio.run(self._qwen_client().delete_voice(voice_name))
        return json.dumps(
            {
                "deleted": True,
                "provider": "qwen3",
                "voice": voice_name,
                "provider_result": result,
            },
            ensure_ascii=False,
        )

    def _resolve_voice(
        self,
        *,
        role: str,
        requested_voice: str,
        available: list[str],
        used: list[str],
    ) -> str:
        goal = (
            "kb_voice_expert:resolve_voice("
            f"{_prolog_string(role)},"
            f"{_prolog_string(requested_voice)},"
            f"{self._prolog_string_list(available)},"
            f"{self._prolog_string_list(used)},"
            "Voice)"
        )
        result = self.prolog_engine.query_once(goal)
        if not result or "Voice" not in result:
            raise RuntimeError("Voice Expert Prolog policy returned no voice")
        voice = str(result["Voice"])
        if voice not in available:
            raise RuntimeError("Voice Expert selected a voice outside provider inventory")
        return voice

    def _available_voices(self) -> list[str]:
        provider = self._provider()
        tts = self._tts_config()
        if provider == "qwen3":
            voices = asyncio.run(self._qwen_client().list_voices())
            configured = str(tts.get("voice") or os.getenv("QWEN3_VOICE", "zara"))
            normalized = [str(voice) for voice in voices if str(voice)]
            if configured and configured not in normalized:
                normalized.insert(0, configured)
            return normalized
        if provider == "edge":
            return [str(tts.get("edge_voice", "en-US-GuyNeural"))]
        if provider == "11labs":
            voice = str(tts.get("elevenlabs_voice_id", "")).strip()
            return [voice] if voice else []
        return ["local"]

    def _synthesize(self, text: str, voice: str):
        tts = self._tts_config()
        provider = self._provider()
        if provider == "qwen3":
            tts["voice"] = voice
        elif provider == "edge":
            tts["edge_voice"] = voice
        elif provider == "11labs":
            tts["elevenlabs_voice_id"] = voice
        engine = TTSEngine(provider, {"tts": tts})
        try:
            return asyncio.run(engine.synthesize_async(text))
        finally:
            asyncio.run(engine.close())

    def _qwen_client(self) -> Qwen3TTSClient:
        tts = self._tts_config()
        endpoint = str(
            tts.get("endpoint")
            or os.getenv("QWEN3_TTS_URL", "http://localhost:7860")
        )
        return Qwen3TTSClient(
            endpoint,
            total_timeout=float(tts.get("total_timeout", 30.0)),
            connect_timeout=float(tts.get("connect_timeout", 5.0)),
            read_timeout=float(tts.get("read_timeout", 20.0)),
        )

    def _provider(self) -> str:
        provider = str(self._tts_config().get("provider", "qwen3"))
        return "qwen3" if provider == "qwen" else provider

    def _tts_config(self) -> dict[str, Any]:
        if hasattr(self.config, "get_section"):
            return dict(self.config.get_section("tts") or {})
        if isinstance(self.config, dict):
            return dict(self.config.get("tts", {}) or {})
        return {}

    @staticmethod
    def _speaker(value: str) -> str:
        value = " ".join(str(value).split()).strip().lower()
        if not value or len(value) > MAX_SPEAKER_CHARS:
            raise ValueError("speaker label is invalid")
        return value

    @staticmethod
    def _role(value: str) -> str:
        value = re.sub(r"[^a-z0-9_]+", "_", str(value).strip().lower()).strip("_")
        if not value or len(value) > MAX_SPEAKER_CHARS:
            raise ValueError("voice role is invalid")
        return value

    @staticmethod
    def _validate_voice_name(value: str) -> None:
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,79}", str(value)):
            raise ValueError(
                "voice name must contain only letters, digits, dot, underscore, or hyphen"
            )

    @staticmethod
    def _validate_clone_rights(
        rights_basis: str,
        subject_is_public_figure: bool,
    ) -> None:
        allowed = {
            "self",
            "explicit_permission",
            "licensed",
            "public_domain",
            "synthetic",
        }
        if rights_basis not in allowed:
            raise PermissionError(
                "voice reference requires an explicit supported rights basis"
            )
        if subject_is_public_figure:
            raise PermissionError(
                "public-figure voice cloning is not supported by this tool"
            )

    @staticmethod
    def _youtube_url(url: str) -> str:
        parsed = urlsplit(str(url).strip())
        host = (parsed.hostname or "").lower().rstrip(".")
        if parsed.scheme not in {"http", "https"}:
            raise ValueError("YouTube URL must use http or https")
        if not (
            host == "youtu.be"
            or host == "youtube.com"
            or host.endswith(".youtube.com")
        ):
            raise ValueError("voice reference URL must be a YouTube URL")
        return str(url)

    @staticmethod
    def _prolog_string_list(values: list[str]) -> str:
        return "[" + ",".join(_prolog_string(str(value)) for value in values) + "]"

    @staticmethod
    def _require_binary(name: str) -> str:
        path = shutil.which(name)
        if path is None:
            raise RuntimeError(f"{name} is not installed or not on PATH")
        return path

    @staticmethod
    def _run(
        command: list[str],
        *,
        timeout: float,
    ) -> subprocess.CompletedProcess[str]:
        try:
            return subprocess.run(
                command,
                check=True,
                capture_output=True,
                text=True,
                timeout=float(timeout),
                stdin=subprocess.DEVNULL,
            )
        except subprocess.TimeoutExpired as error:
            raise RuntimeError(
                f"command timed out after {timeout:g} seconds"
            ) from error
        except subprocess.CalledProcessError as error:
            detail = " ".join((error.stderr or error.stdout or "").split())[:500]
            raise RuntimeError(
                detail or f"command exited with {error.returncode}"
            ) from error


def build_voice_tools(prolog_engine: Any, config: Any) -> list[StructuredTool]:
    """Build core Voice Expert tools bound to Zara's live Prolog engine."""

    expert = VoiceExpert(prolog_engine, config)

    return [
        StructuredTool.from_function(
            expert.youtube_search,
            name="youtube_search",
            description=(
                "Search YouTube through packaged yt-dlp and return bounded metadata only. "
                "This does not download media."
            ),
            args_schema=YouTubeSearchArgs,
        ),
        StructuredTool.from_function(
            expert.list_voices,
            name="voice_list",
            description="List voices available to Zara's configured TTS provider.",
        ),
        StructuredTool.from_function(
            expert.plan_json,
            name="voice_plan",
            description=(
                "Build a speaker-aware voice plan. The model supplies speaker/role hints, "
                "but Prolog is authoritative for every final voice selection. Smart mode "
                "stays single-voice unless role policy requires another voice."
            ),
            args_schema=VoicePlanArgs,
        ),
        StructuredTool.from_function(
            expert.speak,
            name="voice_speak",
            description=(
                "Speak text with Zara's configured TTS. Voice selection is resolved by "
                "the Prolog Voice Expert; the configured/default voice is used normally."
            ),
            args_schema=VoiceSpeakArgs,
        ),
        StructuredTool.from_function(
            expert.narrate,
            name="voice_narrate",
            description=(
                "Speak multiple narration/dialogue segments. The LLM may tag speakers "
                "and roles; Prolog resolves voices and only uses multiple voices when "
                "policy or an explicit request calls for it."
            ),
            args_schema=VoicePlanArgs,
        ),
        StructuredTool.from_function(
            expert.clone_from_youtube,
            name="voice_clone_from_youtube",
            description=(
                "Create a Qwen3-TTS reference voice from a short authorized YouTube clip. "
                "Requires an explicit rights basis and does not support public-figure cloning."
            ),
            args_schema=VoiceCloneYouTubeArgs,
            metadata={"zara_requires_approval": True},
        ),
        StructuredTool.from_function(
            expert.delete_voice,
            name="voice_delete",
            description="Delete a registered Qwen3-TTS voice by name.",
            args_schema=VoiceDeleteArgs,
            metadata={"zara_requires_approval": True},
        ),
    ]


__all__ = ["VoiceExpert", "VoiceSegment", "build_voice_tools"]
