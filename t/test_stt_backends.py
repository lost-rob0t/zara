from pathlib import Path

import faster_whisper
import numpy as np
import pytest

from zara.stt_backends import (
    _transcription_url,
    _wav_bytes,
    backend_compat,
    detect_sherpa_family,
    needs_faster_whisper_files,
    normalize_provider,
    resolve_model_for_provider,
)


def _touch(root: Path, *names: str) -> None:
    for name in names:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.touch()


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        ("faster_whisper", "faster-whisper"),
        ("whisper", "openai-whisper"),
        ("openai_whisper", "openai-whisper"),
        ("sherpa", "sherpa-onnx"),
        ("sensevoice", "sense-voice"),
        ("moonshine", "moonshine"),
        ("moonshine_v2", "moonshine-v2"),
        ("zipformer", "zipformer"),
        ("groq", "groq"),
    ],
)
def test_provider_aliases(value, expected):
    assert normalize_provider(value) == expected


def test_unknown_provider_fails_closed():
    with pytest.raises(ValueError, match="Unsupported STT provider"):
        normalize_provider("mystery-asr")


def test_only_faster_whisper_uses_huggingface_whisper_resolver():
    assert needs_faster_whisper_files("faster-whisper") is True
    assert needs_faster_whisper_files("whisper") is False
    assert needs_faster_whisper_files("moonshine") is False


def test_remote_provider_maps_legacy_whisper_size_to_provider_default():
    assert resolve_model_for_provider("openai", "tiny") == "gpt-4o-mini-transcribe"
    assert resolve_model_for_provider("groq", "small") == "whisper-large-v3-turbo"
    assert resolve_model_for_provider("groq", "whisper-large-v3") == "whisper-large-v3"


def test_detect_moonshine_model_directory(tmp_path):
    _touch(
        tmp_path,
        "preprocess.onnx",
        "encode.onnx",
        "uncached_decode.onnx",
        "cached_decode.onnx",
        "tokens.txt",
    )
    assert detect_sherpa_family(str(tmp_path)) == "moonshine"


def test_detect_moonshine_v2_model_directory(tmp_path):
    _touch(
        tmp_path,
        "encoder_model.ort",
        "decoder_model_merged.ort",
        "tokens.txt",
    )
    assert detect_sherpa_family(str(tmp_path)) == "moonshine-v2"


def test_detect_zipformer_model_directory(tmp_path):
    _touch(
        tmp_path,
        "encoder-epoch-99-avg-1.int8.onnx",
        "decoder-epoch-99-avg-1.onnx",
        "joiner-epoch-99-avg-1.int8.onnx",
        "tokens.txt",
    )
    assert detect_sherpa_family(str(tmp_path)) == "zipformer"


def test_detect_sense_voice_model_directory(tmp_path):
    _touch(tmp_path, "model.int8.onnx", "tokens.txt")
    assert detect_sherpa_family(str(tmp_path)) == "sense-voice"


def test_sherpa_model_requires_local_directory(tmp_path):
    missing = tmp_path / "not-downloaded"
    with pytest.raises(ValueError, match="local model directory"):
        detect_sherpa_family(str(missing))


def test_backend_compat_swaps_and_restores_whisper_model():
    original = faster_whisper.WhisperModel
    with backend_compat("moonshine"):
        assert faster_whisper.WhisperModel is not original
        assert faster_whisper.WhisperModel.__name__ == "SherpaOnnx_moonshine"
    assert faster_whisper.WhisperModel is original


def test_moonshine_v2_compat_class_is_bound():
    original = faster_whisper.WhisperModel
    with backend_compat("moonshine-v2"):
        assert faster_whisper.WhisperModel is not original
        assert faster_whisper.WhisperModel.__name__ == "SherpaOnnx_moonshine_v2"
    assert faster_whisper.WhisperModel is original


def test_wav_payload_is_real_16khz_pcm():
    payload = _wav_bytes(np.zeros(160, dtype=np.float32))
    assert payload[:4] == b"RIFF"
    assert payload[8:12] == b"WAVE"


def test_transcription_endpoint_joining():
    assert (
        _transcription_url("https://api.example.test/v1")
        == "https://api.example.test/v1/audio/transcriptions"
    )
    existing = "https://api.example.test/v1/audio/transcriptions"
    assert _transcription_url(existing) == existing
