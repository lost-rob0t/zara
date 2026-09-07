from threading import Event

import pytest

import zara.dictate as dictate
import zara.whisper_cpp as whisper_cpp


def test_historical_gpu_aliases_remain_legacy_cuda_tokens():
    for alias in ("amd", "rocm", "hip"):
        assert dictate._normalize_device(alias) == "cuda"


def test_explicit_vulkan_is_a_supported_dictation_device():
    assert dictate._normalize_device("vulkan") == "vulkan"


def test_ggml_model_on_legacy_cuda_selects_whisper_cpp_vulkan():
    assert dictate._select_model_backend(
        "/tmp/ggml-small.en.bin",
        "cuda",
    ) == ("whisper_cpp", "vulkan", "vulkan")


def test_non_ggml_cuda_stays_on_faster_whisper():
    assert dictate._select_model_backend(
        "small",
        "cuda",
    ) == ("faster_whisper", "cuda", "float16")


def test_vulkan_model_load_does_not_require_faster_whisper(monkeypatch):
    sentinel = object()
    calls = {}

    def fake_whisper_cpp(*args, **kwargs):
        calls["args"] = args
        calls["kwargs"] = kwargs
        return sentinel

    monkeypatch.setattr(dictate, "WhisperModel", None, raising=False)
    monkeypatch.setattr(whisper_cpp, "WhisperCppModel", fake_whisper_cpp)

    result = dictate._load_whisper_model(
        "/tmp/ggml-small.en.bin",
        "vulkan",
        "vulkan",
        cpu_threads=8,
        workers=2,
    )

    assert result is sentinel
    assert calls == {
        "args": ("/tmp/ggml-small.en.bin",),
        "kwargs": {
            "device": "vulkan",
            "cpu_threads": 8,
            "num_workers": 2,
        },
    }


def test_missing_faster_whisper_fails_only_when_that_backend_is_selected(monkeypatch):
    monkeypatch.setattr(dictate, "WhisperModel", None, raising=False)
    monkeypatch.setattr(
        dictate,
        "_FASTER_WHISPER_IMPORT_ERROR",
        ImportError("fixture missing faster-whisper"),
        raising=False,
    )

    with pytest.raises(RuntimeError, match="faster-whisper is required"):
        dictate._load_whisper_model(
            "small",
            "cuda",
            "float16",
            cpu_threads=8,
            workers=2,
        )


def test_main_reports_actual_whisper_cpp_vulkan_backend(monkeypatch, tmp_path):
    logs = []
    loads = []
    run_stop = Event()

    monkeypatch.setattr(dictate, "PIDFILE", str(tmp_path / "dictation.pid"))
    monkeypatch.setattr(dictate, "LOGFILE", str(tmp_path / "dictation.log"))
    monkeypatch.setattr(dictate, "log", logs.append)
    monkeypatch.setattr(dictate, "_get_input_sample_rate", lambda: 16000.0)
    monkeypatch.setattr(dictate, "_resolve_stop_phrases", lambda value: ["stop voice"])

    def fake_reset():
        nonlocal run_stop
        run_stop = Event()
        return run_stop, dictate.queue.Queue(), dictate.queue.Queue()

    def fake_load(model_name, device, compute_type, cpu_threads, workers):
        loads.append((model_name, device, compute_type, cpu_threads, workers))
        return object()

    class FakeThread:
        def __init__(self, target, args, daemon):
            self.run_stop_event = args[-1]

        def start(self):
            self.run_stop_event.set()

        def join(self):
            pass

    class FakeExecutor:
        def __init__(self, max_workers):
            self.max_workers = max_workers

        def shutdown(self, wait, cancel_futures):
            pass

    monkeypatch.setattr(dictate, "_reset_runtime_state", fake_reset)
    monkeypatch.setattr(dictate, "_load_whisper_model", fake_load)
    monkeypatch.setattr(dictate, "Thread", FakeThread)
    monkeypatch.setattr(dictate, "ThreadPoolExecutor", FakeExecutor)

    assert dictate.main(
        model_name="/tmp/ggml-small.en.bin",
        device="amd",
        threads=8,
        workers=2,
    ) == 0

    assert loads == [
        ("/tmp/ggml-small.en.bin", "vulkan", "vulkan", 8, 2),
    ]
    assert any("Using whisper.cpp Vulkan STT" in message for message in logs)
    assert any("on vulkan" in message for message in logs)
