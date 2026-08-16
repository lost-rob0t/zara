"""Speech-to-text backend adapters for Zara.

Zara's wake and dictation paths historically instantiate
``faster_whisper.WhisperModel`` directly.  This module provides a narrow
compatibility boundary so those callers can select other engines without
forking the audio/VAD pipeline.

Local backends:
- faster-whisper (existing CTranslate2 path)
- whisper.cpp (persistent local server; Vulkan-capable)
- OpenAI Whisper reference implementation
- sherpa-onnx Moonshine v1/v2, Zipformer transducer, and SenseVoice models

Remote backends:
- OpenAI audio transcription API
- Groq OpenAI-compatible audio transcription API
"""

from __future__ import annotations

import io
import os
from contextlib import contextmanager
from pathlib import Path
from types import SimpleNamespace
from typing import Iterator

import httpx
import numpy as np
import soundfile as sf


PROVIDER_ALIASES = {
    "faster_whisper": "faster-whisper",
    "fasterwhisper": "faster-whisper",
    "whisper_cpp": "whisper-cpp",
    "whispercpp": "whisper-cpp",
    "whisper": "openai-whisper",
    "openai_whisper": "openai-whisper",
    "sherpa": "sherpa-onnx",
    "sherpa_onnx": "sherpa-onnx",
    "moonshine_v2": "moonshine-v2",
    "sensevoice": "sense-voice",
    "sense_voice": "sense-voice",
}

STT_PROVIDERS = (
    "faster-whisper",
    "whisper-cpp",
    "whisper",
    "openai-whisper",
    "sherpa-onnx",
    "moonshine",
    "moonshine-v2",
    "zipformer",
    "sense-voice",
    "openai",
    "groq",
)

WHISPER_SIZE_NAMES = {
    "tiny",
    "tiny.en",
    "base",
    "base.en",
    "small",
    "small.en",
    "medium",
    "medium.en",
    "large",
    "large-v1",
    "large-v2",
    "large-v3",
    "large-v3-turbo",
    "turbo",
}


def normalize_provider(provider: str) -> str:
    value = str(provider or "faster-whisper").strip().lower()
    value = PROVIDER_ALIASES.get(value, value)
    canonical = {normalize_provider_name(p) for p in STT_PROVIDERS}
    if value not in canonical:
        choices = ", ".join(STT_PROVIDERS)
        raise ValueError(f"Unsupported STT provider {provider!r}; choose one of: {choices}")
    return value


def normalize_provider_name(provider: str) -> str:
    value = str(provider).strip().lower()
    return PROVIDER_ALIASES.get(value, value)


def resolve_model_for_provider(provider: str, model: str) -> str:
    provider = normalize_provider(provider)
    model = str(model).strip()

    if provider == "openai" and model in WHISPER_SIZE_NAMES:
        return "gpt-4o-mini-transcribe"
    if provider == "groq" and model in WHISPER_SIZE_NAMES:
        return "whisper-large-v3-turbo"
    return model


def needs_faster_whisper_files(provider: str) -> bool:
    return normalize_provider(provider) == "faster-whisper"


def needs_whisper_cpp_files(provider: str) -> bool:
    return normalize_provider(provider) == "whisper-cpp"


def _segment_result(text: str):
    text = str(text or "").strip()
    segments = [SimpleNamespace(text=text)] if text else []
    return segments, SimpleNamespace(language=None)


def _mono_float32(audio) -> np.ndarray:
    value = np.asarray(audio, dtype=np.float32)
    if value.ndim > 1:
        value = value[:, 0]
    value = np.nan_to_num(value, nan=0.0, posinf=1.0, neginf=-1.0)
    return np.clip(value, -1.0, 1.0)


def _wav_bytes(audio: np.ndarray, sample_rate: int = 16000) -> bytes:
    buffer = io.BytesIO()
    sf.write(buffer, _mono_float32(audio), sample_rate, format="WAV", subtype="PCM_16")
    return buffer.getvalue()


def _transcription_url(base_url: str) -> str:
    base = str(base_url).rstrip("/")
    if base.endswith("/audio/transcriptions"):
        return base
    return f"{base}/audio/transcriptions"


class OpenAIWhisperModel:
    """Compatibility adapter for the reference ``openai-whisper`` package."""

    def __init__(
        self,
        model: str,
        *,
        device: str = "cpu",
        cpu_threads: int = 4,
        **_kwargs,
    ):
        try:
            import torch
            import whisper
        except ImportError as error:
            raise RuntimeError(
                "openai-whisper STT requires the Nix/Python openai-whisper package"
            ) from error

        if cpu_threads > 0:
            torch.set_num_threads(cpu_threads)

        self._model = whisper.load_model(model, device=device)

    def transcribe(self, audio, **kwargs):
        options = {
            "language": kwargs.get("language"),
            "condition_on_previous_text": kwargs.get("condition_on_previous_text", False),
            "initial_prompt": kwargs.get("initial_prompt"),
        }
        beam_size = kwargs.get("beam_size")
        if beam_size is not None and beam_size > 1:
            options["beam_size"] = beam_size

        result = self._model.transcribe(_mono_float32(audio), **options)
        return _segment_result(result.get("text", ""))


class HTTPTranscriptionModel:
    """OpenAI-compatible ``/audio/transcriptions`` adapter."""

    def __init__(
        self,
        model: str,
        *,
        endpoint: str,
        api_key_env: str,
        timeout: float = 30.0,
        **_kwargs,
    ):
        key = os.getenv(api_key_env)
        if not key:
            raise RuntimeError(f"{api_key_env} is required for remote STT")

        self.model = model
        self.endpoint = _transcription_url(endpoint)
        self.api_key = key
        self.timeout = timeout

    def transcribe(self, audio, **kwargs):
        data = {"model": self.model}
        language = kwargs.get("language")
        if language:
            data["language"] = language

        response = httpx.post(
            self.endpoint,
            headers={"Authorization": f"Bearer {self.api_key}"},
            files={"file": ("audio.wav", _wav_bytes(audio), "audio/wav")},
            data=data,
            timeout=self.timeout,
        )
        response.raise_for_status()
        payload = response.json()
        return _segment_result(payload.get("text", ""))


class OpenAITranscriptionModel(HTTPTranscriptionModel):
    def __init__(self, model: str, **kwargs):
        super().__init__(
            model,
            endpoint=os.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1"),
            api_key_env="OPENAI_API_KEY",
            **kwargs,
        )


class GroqTranscriptionModel(HTTPTranscriptionModel):
    def __init__(self, model: str, **kwargs):
        super().__init__(
            model,
            endpoint=os.getenv("GROQ_BASE_URL", "https://api.groq.com/openai/v1"),
            api_key_env="GROQ_API_KEY",
            **kwargs,
        )


def _files(root: Path, pattern: str) -> list[Path]:
    return sorted(p for p in root.rglob(pattern) if p.is_file())


def _require_file(root: Path, description: str, *patterns: str) -> Path:
    for pattern in patterns:
        matches = _files(root, pattern)
        if matches:
            return matches[0]
    wanted = " or ".join(patterns)
    raise ValueError(f"Missing {description} in {root}; expected {wanted}")


def _moonshine_files(root: Path) -> dict[str, Path]:
    preprocessor = _require_file(root, "Moonshine preprocessor", "*preprocess*.onnx")
    encoder = _require_file(root, "Moonshine encoder", "*encoder*.onnx", "*encode*.onnx")
    uncached = _require_file(root, "Moonshine uncached decoder", "*uncached*decode*.onnx")

    cached_candidates = [
        p
        for p in _files(root, "*cached*decode*.onnx")
        if "uncached" not in p.name.lower()
    ]
    if not cached_candidates:
        raise ValueError(f"Missing Moonshine cached decoder in {root}")

    return {
        "preprocessor": preprocessor,
        "encoder": encoder,
        "uncached_decoder": uncached,
        "cached_decoder": cached_candidates[0],
        "tokens": _require_file(root, "tokens", "tokens.txt", "*tokens*.txt"),
    }


def _moonshine_v2_files(root: Path) -> dict[str, Path]:
    return {
        "encoder": _require_file(
            root,
            "Moonshine v2 encoder",
            "encoder_model.ort",
            "*encoder*.ort",
        ),
        "decoder": _require_file(
            root,
            "Moonshine v2 merged decoder",
            "decoder_model_merged.ort",
            "*merged*decoder*.ort",
            "*decoder*merged*.ort",
        ),
        "tokens": _require_file(root, "tokens", "tokens.txt", "*tokens*.txt"),
    }


def _zipformer_files(root: Path) -> dict[str, Path]:
    return {
        "encoder": _require_file(root, "Zipformer encoder", "*encoder*.onnx"),
        "decoder": _require_file(root, "Zipformer decoder", "*decoder*.onnx"),
        "joiner": _require_file(root, "Zipformer joiner", "*joiner*.onnx"),
        "tokens": _require_file(root, "tokens", "tokens.txt", "*tokens*.txt"),
    }


def _sense_voice_files(root: Path) -> dict[str, Path]:
    return {
        "model": _require_file(root, "SenseVoice model", "model*.onnx", "*sense*voice*.onnx"),
        "tokens": _require_file(root, "tokens", "tokens.txt", "*tokens*.txt"),
    }


def detect_sherpa_family(model_dir: str) -> str:
    root = Path(model_dir).expanduser()
    if not root.is_dir():
        raise ValueError(
            f"sherpa-onnx STT needs a local model directory, got {model_dir!r}"
        )

    onnx_names = [p.name.lower() for p in _files(root, "*.onnx")]
    ort_names = [p.name.lower() for p in _files(root, "*.ort")]
    if any("encoder" in name for name in ort_names) and any(
        "decoder" in name and "merged" in name for name in ort_names
    ):
        return "moonshine-v2"
    if any("uncached" in name for name in onnx_names) and any(
        "cached" in name for name in onnx_names
    ):
        return "moonshine"
    if any("joiner" in name for name in onnx_names):
        return "zipformer"
    if any(name.startswith("model") or "sense" in name for name in onnx_names):
        return "sense-voice"
    raise ValueError(
        f"Could not detect sherpa-onnx model family in {root}; "
        "supported layouts are Moonshine v1/v2, Zipformer transducer, and SenseVoice"
    )


class SherpaOnnxModel:
    """Compatibility adapter for sherpa-onnx local ASR model directories."""

    def __init__(
        self,
        model: str,
        *,
        family: str | None = None,
        device: str = "cpu",
        cpu_threads: int = 4,
        **_kwargs,
    ):
        try:
            import sherpa_onnx
        except ImportError as error:
            raise RuntimeError(
                "sherpa-onnx STT requires the Nix/Python sherpa-onnx package"
            ) from error

        root = Path(model).expanduser().resolve()
        self.family = family or detect_sherpa_family(str(root))
        provider = "cuda" if device == "cuda" else "cpu"
        threads = max(1, int(cpu_threads or 1))

        if self.family == "moonshine":
            files = _moonshine_files(root)
            self._recognizer = sherpa_onnx.OfflineRecognizer.from_moonshine(
                preprocessor=str(files["preprocessor"]),
                encoder=str(files["encoder"]),
                uncached_decoder=str(files["uncached_decoder"]),
                cached_decoder=str(files["cached_decoder"]),
                tokens=str(files["tokens"]),
                num_threads=threads,
                decoding_method="greedy_search",
                provider=provider,
            )
        elif self.family == "moonshine-v2":
            files = _moonshine_v2_files(root)
            self._recognizer = sherpa_onnx.OfflineRecognizer.from_moonshine_v2(
                encoder=str(files["encoder"]),
                decoder=str(files["decoder"]),
                tokens=str(files["tokens"]),
                num_threads=threads,
                decoding_method="greedy_search",
                provider=provider,
            )
        elif self.family == "zipformer":
            files = _zipformer_files(root)
            self._recognizer = sherpa_onnx.OfflineRecognizer.from_transducer(
                encoder=str(files["encoder"]),
                decoder=str(files["decoder"]),
                joiner=str(files["joiner"]),
                tokens=str(files["tokens"]),
                num_threads=threads,
                decoding_method="greedy_search",
                provider=provider,
            )
        elif self.family == "sense-voice":
            files = _sense_voice_files(root)
            self._recognizer = sherpa_onnx.OfflineRecognizer.from_sense_voice(
                model=str(files["model"]),
                tokens=str(files["tokens"]),
                num_threads=threads,
                provider=provider,
                language="en",
                use_itn=True,
            )
        else:
            raise ValueError(f"Unsupported sherpa-onnx family: {self.family}")

    def transcribe(self, audio, **_kwargs):
        stream = self._recognizer.create_stream()
        stream.accept_waveform(16000, _mono_float32(audio))
        self._recognizer.decode_stream(stream)
        return _segment_result(stream.result.text)


def model_class_for_provider(provider: str):
    provider = normalize_provider(provider)
    if provider == "whisper-cpp":
        from .whisper_cpp import WhisperCppModel

        return WhisperCppModel
    if provider == "openai-whisper":
        return OpenAIWhisperModel
    if provider == "openai":
        return OpenAITranscriptionModel
    if provider == "groq":
        return GroqTranscriptionModel
    if provider in {
        "sherpa-onnx",
        "moonshine",
        "moonshine-v2",
        "zipformer",
        "sense-voice",
    }:
        family = None if provider == "sherpa-onnx" else provider

        class BoundSherpaModel(SherpaOnnxModel):
            def __init__(self, model: str, **kwargs):
                super().__init__(model, family=family, **kwargs)

        BoundSherpaModel.__name__ = f"SherpaOnnx_{provider.replace('-', '_')}"
        return BoundSherpaModel
    raise ValueError(f"Provider {provider!r} does not need a compatibility model")


@contextmanager
def backend_compat(provider: str) -> Iterator[None]:
    """Temporarily expose a selected backend as ``WhisperModel``.

    This keeps the existing wake/dictation audio pipelines stable while their
    historical direct constructor dependency is migrated behind a permanent
    backend interface.
    """
    provider = normalize_provider(provider)
    if provider == "faster-whisper":
        yield
        return

    import faster_whisper

    original = faster_whisper.WhisperModel
    faster_whisper.WhisperModel = model_class_for_provider(provider)
    try:
        yield
    finally:
        faster_whisper.WhisperModel = original
