from __future__ import annotations

import sys
from types import SimpleNamespace

import pytest

import zara.dictate as dictate


def test_explicit_vulkan_is_a_first_class_dictation_device():
    assert dictate._normalize_device("vulkan") == "vulkan"


def test_ggml_model_uses_vulkan_behind_legacy_cuda_token():
    assert dictate._select_model_device("/models/ggml-large-v3.bin", "cuda") == "vulkan"


def test_non_ggml_cuda_model_stays_on_cuda():
    assert dictate._select_model_device("large-v3", "cuda") == "cuda"


def test_vulkan_loader_uses_whisper_cpp_without_faster_whisper(monkeypatch):
    calls = {}
    sentinel = object()

    class FakeWhisperCppModel:
        def __init__(self, *args, **kwargs):
            calls["args"] = args
            calls["kwargs"] = kwargs

        def __new__(cls, *args, **kwargs):
            instance = super().__new__(cls)
            return instance

    def fake_model(*args, **kwargs):
        calls["args"] = args
        calls["kwargs"] = kwargs
        return sentinel

    fake_module = SimpleNamespace(WhisperCppModel=fake_model)
    monkeypatch.setitem(sys.modules, "zara.whisper_cpp", fake_module)
    monkeypatch.setattr(dictate, "WhisperModel", None)

    result = dictate._load_whisper_model(
        "/models/ggml-large-v3.bin",
        "vulkan",
        "vulkan",
        cpu_threads=8,
        workers=2,
    )

    assert result is sentinel
    assert calls == {
        "args": ("/models/ggml-large-v3.bin",),
        "kwargs": {
            "device": "vulkan",
            "cpu_threads": 8,
            "num_workers": 2,
        },
    }


def test_cpu_loader_fails_explicitly_when_faster_whisper_is_unavailable(monkeypatch):
    monkeypatch.setattr(dictate, "WhisperModel", None)
    monkeypatch.setattr(dictate, "_FASTER_WHISPER_IMPORT_ERROR", ImportError("missing"), raising=False)

    with pytest.raises(RuntimeError, match="faster-whisper is required"):
        dictate._load_whisper_model(
            "small",
            "cpu",
            "int8",
            cpu_threads=4,
            workers=1,
        )
