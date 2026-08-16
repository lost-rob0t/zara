from pathlib import Path

import pytest

import zara.__main__ as cli
from zara.stt_backends import (
    model_class_for_provider,
    needs_whisper_cpp_files,
    normalize_provider,
)
from zara.whisper_cpp import WhisperCppModel, resolve_whisper_cpp_model


def test_whisper_cpp_provider_aliases():
    assert normalize_provider("whisper-cpp") == "whisper-cpp"
    assert normalize_provider("whisper_cpp") == "whisper-cpp"
    assert normalize_provider("whispercpp") == "whisper-cpp"
    assert needs_whisper_cpp_files("whisper-cpp") is True
    assert needs_whisper_cpp_files("faster-whisper") is False


@pytest.mark.parametrize("alias", ["vulkan", "amd", "rocm", "hip"])
def test_whisper_cpp_amd_aliases_select_vulkan(alias):
    assert cli.normalize_stt_device(alias, provider="whisper-cpp") == "vulkan"


def test_whisper_cpp_cpu_is_preserved():
    assert cli.normalize_stt_device("cpu", provider="whisper-cpp") == "cpu"


def test_whisper_cpp_rejects_cuda_device():
    with pytest.raises(ValueError, match="uses Vulkan"):
        cli.normalize_stt_device("cuda", provider="whisper-cpp")


def test_legacy_amd_alias_stays_cuda_without_whisper_cpp():
    assert cli.normalize_stt_device("amd") == "cuda"
    assert cli.normalize_stt_device("rocm", provider="faster-whisper") == "cuda"


def test_whisper_cpp_backend_class_is_registered():
    assert model_class_for_provider("whisper-cpp") is WhisperCppModel


def test_local_ggml_model_is_used_without_download(tmp_path: Path):
    model = tmp_path / "ggml-small.bin"
    model.write_bytes(b"ggml-test")

    assert resolve_whisper_cpp_model(str(model)) == str(model.resolve())


def _bare_model(*, device: str) -> WhisperCppModel:
    model = WhisperCppModel.__new__(WhisperCppModel)
    model.binary = "/nix/store/test/bin/whisper-server"
    model.model = "/models/ggml-small.bin"
    model.port = 18234
    model.cpu_threads = 4
    model.device = device
    return model


def test_vulkan_server_command_keeps_gpu_enabled():
    command = _bare_model(device="vulkan")._command()

    assert "--model" in command
    assert "/models/ggml-small.bin" in command
    assert "--no-gpu" not in command
    assert "--no-flash-attn" in command


def test_cpu_server_command_disables_gpu():
    command = _bare_model(device="cpu")._command()

    assert "--no-gpu" in command
