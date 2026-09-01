from pathlib import Path

import pytest

import zara.__main__ as cli
from zara.stt_backends import (
    model_class_for_provider,
    needs_whisper_cpp_files,
    normalize_provider,
)
from zara.whisper_cpp import (
    WhisperCppModel,
    _normalize_backend_device,
    resolve_whisper_cpp_model,
)


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


def test_whisper_cpp_rejects_user_facing_cuda_device():
    with pytest.raises(ValueError, match="uses Vulkan"):
        cli.normalize_stt_device("cuda", provider="whisper-cpp")


@pytest.mark.parametrize("token", ["vulkan", "amd", "rocm", "hip"])
@pytest.mark.parametrize("provider", [None, "faster-whisper", "whisper-cpp"])
def test_amd_device_tokens_select_vulkan_for_any_provider(token, provider):
    assert cli.normalize_stt_device(token, provider=provider) == "vulkan"


def test_cuda_is_preserved_for_nvidia_backends():
    assert cli.normalize_stt_device("cuda") == "cuda"
    assert cli.normalize_stt_device("cuda", provider="faster-whisper") == "cuda"
    assert cli.normalize_stt_device("cpu", provider="faster-whisper") == "cpu"


def test_unknown_device_still_lists_choices():
    with pytest.raises(ValueError, match="cpu, cuda, vulkan, rocm, hip, amd"):
        cli.normalize_stt_device("tpu")


def test_amd_routing_sends_local_providers_to_whisper_cpp_vulkan():
    for provider in ["faster-whisper", "openai-whisper", "sherpa-onnx"]:
        routed, notice = cli.route_stt_provider_for_amd_device(provider, "vulkan")
        assert routed == "whisper-cpp"
        assert "whisper.cpp Vulkan backend" in notice


def test_amd_routing_leaves_whisper_cpp_and_remote_providers_alone():
    assert cli.route_stt_provider_for_amd_device("whisper-cpp", "vulkan") == (
        "whisper-cpp",
        None,
    )
    assert cli.route_stt_provider_for_amd_device("groq", "vulkan") == ("groq", None)
    assert cli.route_stt_provider_for_amd_device("openai", "vulkan") == ("openai", None)


def test_amd_routing_ignores_non_vulkan_devices():
    assert cli.route_stt_provider_for_amd_device("faster-whisper", "cuda") == (
        "faster-whisper",
        None,
    )
    assert cli.route_stt_provider_for_amd_device("faster-whisper", "cpu") == (
        "faster-whisper",
        None,
    )


def test_legacy_dictation_cuda_token_maps_back_to_vulkan():
    assert _normalize_backend_device("cuda") == "vulkan"
    assert _normalize_backend_device("vulkan") == "vulkan"
    assert _normalize_backend_device("cpu") == "cpu"


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
