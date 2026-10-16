from pathlib import Path
import subprocess
import threading
from unittest.mock import Mock

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
    _worker_identity,
    _worker_state_path,
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


def test_start_refuses_prior_live_worker(monkeypatch, tmp_path):
    import json
    import os
    import zara.whisper_cpp as whisper_cpp

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    state = _worker_state_path()
    state.parent.mkdir(parents=True, exist_ok=True)
    state.write_text(json.dumps({"pid": os.getpid(), "identity": _worker_identity(os.getpid())}))
    model = _bare_model(device="cpu")
    model._client = Mock()
    model._log = Mock()
    model._process = None
    popen = Mock()
    monkeypatch.setattr(whisper_cpp.subprocess, "Popen", popen)

    with pytest.raises(RuntimeError, match="remains alive"):
        model._start_server()
    popen.assert_not_called()


def test_unresponsive_shutdown_is_bounded_and_keeps_worker_reference(monkeypatch):
    import zara.whisper_cpp as whisper_cpp

    model = _bare_model(device="vulkan")
    process = Mock(pid=12345)
    process.poll.return_value = None
    process.wait.side_effect = subprocess.TimeoutExpired("whisper-server", 2)
    model._process = process
    model._client = Mock()
    model._log = Mock()
    monkeypatch.setattr(whisper_cpp, "_worker_identity", lambda pid: "test-worker")

    with pytest.raises(RuntimeError, match="remains alive"):
        model.close()
    assert model._process is process
    process.terminate.assert_called_once()
    process.kill.assert_called_once()
    assert process.wait.call_count == 2


def test_startup_timeout_stops_child_and_reports_backend(monkeypatch, tmp_path):
    import zara.whisper_cpp as whisper_cpp

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    monkeypatch.setenv("ZARA_WHISPER_CPP_START_TIMEOUT", "1")
    model = _bare_model(device="vulkan")
    model.endpoint = "http://127.0.0.1:18234"
    model._client = Mock()
    model._client.get.side_effect = whisper_cpp.httpx.ConnectError("not ready")
    model._log = tmp_path.joinpath("worker.log").open("w+")
    process = Mock(pid=12345)
    process.poll.return_value = None
    process.wait.return_value = 0
    monkeypatch.setattr(whisper_cpp.subprocess, "Popen", Mock(return_value=process))
    monkeypatch.setattr(whisper_cpp, "_worker_identity", lambda pid: "test-worker")
    ticks = iter([0, 0, 2])
    monkeypatch.setattr(whisper_cpp.time, "monotonic", lambda: next(ticks))
    monkeypatch.setattr(whisper_cpp.time, "sleep", lambda seconds: None)

    with pytest.raises(RuntimeError, match="vulkan server failed to start"):
        model._start_server()
    process.terminate.assert_called_once()
    assert model._process is None


def test_device_loss_disables_later_vulkan_start_but_allows_cpu(monkeypatch, tmp_path):
    import json
    import zara.whisper_cpp as whisper_cpp

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    model = _bare_model(device="vulkan")
    model._record_device_loss("VK_ERROR_DEVICE_LOST")
    assert json.loads(_worker_state_path().read_text())["gpu_disabled"] is True
    model._client = Mock()
    model._log = Mock()
    model._process = None
    popen = Mock()
    monkeypatch.setattr(whisper_cpp.subprocess, "Popen", popen)
    with pytest.raises(RuntimeError, match="GPU worker disabled"):
        model._start_server()
    popen.assert_not_called()


def test_stale_worker_record_does_not_block_restart(monkeypatch, tmp_path):
    import json
    import zara.whisper_cpp as whisper_cpp

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    state = _worker_state_path()
    state.parent.mkdir(parents=True, exist_ok=True)
    state.write_text(json.dumps({"pid": 99999999, "identity": "old"}))
    model = _bare_model(device="cpu")
    model.endpoint = "http://127.0.0.1:18234"
    model._client = Mock()
    model._client.get.return_value.status_code = 200
    model._log = Mock()
    process = Mock(pid=12345)
    process.poll.return_value = None
    monkeypatch.setattr(whisper_cpp.subprocess, "Popen", Mock(return_value=process))
    monkeypatch.setattr(whisper_cpp, "_worker_identity", lambda pid: "new" if pid == 12345 else None)

    model._start_server()
    assert json.loads(state.read_text()) == {"pid": 12345, "identity": "new"}
    model._client.get.assert_called_once_with(f"{model.endpoint}/", timeout=2.0)


def test_failed_startup_closes_client_and_log(monkeypatch, tmp_path):
    import zara.whisper_cpp as whisper_cpp

    model_path = tmp_path / "ggml-small.bin"
    model_path.write_bytes(b"model")
    client = Mock()
    log = Mock()
    monkeypatch.setattr(whisper_cpp, "_server_binary", lambda: "whisper-server")
    monkeypatch.setattr(whisper_cpp, "_free_local_port", lambda: 18234)
    monkeypatch.setattr(whisper_cpp.httpx, "Client", Mock(return_value=client))
    monkeypatch.setattr(whisper_cpp.tempfile, "TemporaryFile", Mock(return_value=log))
    monkeypatch.setattr(
        WhisperCppModel, "_start_server", Mock(side_effect=RuntimeError("startup failed"))
    )

    with pytest.raises(RuntimeError, match="startup failed"):
        WhisperCppModel(str(model_path))
    client.close.assert_called_once()
    log.close.assert_called_once()


def test_device_loss_rejects_later_requests_on_existing_model(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    model = _bare_model(device="vulkan")
    model.endpoint = "http://127.0.0.1:18234"
    model._request_lock = threading.Lock()
    model._client = Mock()
    model._record_device_loss("VK_ERROR_DEVICE_LOST")

    with pytest.raises(RuntimeError, match="GPU worker disabled"):
        model.transcribe([0.0])
    model._client.post.assert_not_called()


def test_corrupt_worker_state_fails_closed(monkeypatch, tmp_path):
    import zara.whisper_cpp as whisper_cpp

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    state = _worker_state_path()
    state.parent.mkdir(parents=True, exist_ok=True)
    state.write_text('{"pid":')
    model = _bare_model(device="cpu")
    model._client = Mock()
    model._log = Mock()
    model._process = None
    popen = Mock()
    monkeypatch.setattr(whisper_cpp.subprocess, "Popen", popen)

    with pytest.raises(RuntimeError, match="worker state unreadable"):
        model._start_server()
    popen.assert_not_called()


def test_shutdown_handles_child_exit_between_poll_and_signal(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    model = _bare_model(device="cpu")
    process = Mock(pid=12345)
    process.poll.return_value = None
    process.terminate.side_effect = ProcessLookupError
    model._process = process
    model._client = Mock()
    model._log = Mock()

    model.close()
    assert model._process is None
    model._client.close.assert_called_once()
    model._log.close.assert_called_once()
