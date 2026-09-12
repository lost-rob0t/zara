from __future__ import annotations

import concurrent.futures
import importlib
import threading

import numpy as np
import pytest

from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalContext, ZaraServer
from zara.streaming_stt import FinalTranscript


PCM_FRAME = (np.arange(512, dtype=np.int16) - 256).astype("<i2").tobytes()


class RecordingTranscriber:
    def __init__(self) -> None:
        self.started = []
        self.cancelled = []
        self.frames = []

    def start_turn(self, turn_id: str) -> None:
        self.started.append(turn_id)

    def feed(self, chunk: np.ndarray):
        self.frames.append(chunk.copy())
        return [
            FinalTranscript(
                turn_id="stt-stream-1",
                text="hello from the daemon microphone stream",
                text_length=39,
                provider="fixture",
            )
        ]

    def cancel(self, turn_id=None) -> None:
        self.cancelled.append(turn_id)


class RecordingSupervisor:
    def __init__(self) -> None:
        self.submissions = []
        self.submitted = threading.Event()

    def submit(self, principal, command):
        self.submissions.append((principal, command))
        self.submitted.set()
        future = concurrent.futures.Future()
        future.set_result(None)
        return future


@pytest.mark.parametrize("device", ["vulkan", "amd", "rocm", "hip"])
def test_default_daemon_transcriber_routes_amd_devices_to_whisper_cpp(
    monkeypatch,
    device,
):
    voice_runtime = importlib.import_module("zara.voice_runtime")
    config = importlib.import_module("zara.config")
    transcription = importlib.import_module("zara.transcription")
    whisper_cpp = importlib.import_module("zara.whisper_cpp")
    calls = {}

    class FakeConfig:
        def get_section(self, name):
            assert name == "stt"
            return {
                "provider": "faster-whisper",
                "model": "base.en",
                "device": device,
                "threads": 6,
            }

    class FakeWhisperCppModel:
        def __init__(self, model, **kwargs):
            calls["model"] = model
            calls["kwargs"] = kwargs

        def transcribe(self, _audio, **_kwargs):
            return [], object()

    def reject_faster_whisper(**_kwargs):
        raise AssertionError("Vulkan must not construct faster-whisper")

    monkeypatch.setattr(config, "get_config", lambda: FakeConfig())
    monkeypatch.setattr(transcription, "Transcriber", reject_faster_whisper)
    monkeypatch.setattr(
        whisper_cpp,
        "resolve_whisper_cpp_model",
        lambda model: f"/cache/ggml-{model}.bin",
    )
    monkeypatch.setattr(whisper_cpp, "WhisperCppModel", FakeWhisperCppModel)

    ingress = voice_runtime.RuntimeVoiceIngress(object())
    transcriber = ingress._default_transcriber_factory()

    assert calls == {
        "model": "/cache/ggml-base.en.bin",
        "kwargs": {
            "device": "vulkan",
            "cpu_threads": 6,
            "num_workers": 1,
        },
    }
    assert isinstance(ingress._default_model, FakeWhisperCppModel)
    assert transcriber._transcribe_fn(np.zeros(512, dtype=np.float32)) == ""


def test_default_daemon_transcriber_does_not_fallback_when_vulkan_model_resolution_fails(
    monkeypatch,
):
    voice_runtime = importlib.import_module("zara.voice_runtime")
    config = importlib.import_module("zara.config")
    transcription = importlib.import_module("zara.transcription")
    whisper_cpp = importlib.import_module("zara.whisper_cpp")

    class FakeConfig:
        def get_section(self, name):
            assert name == "stt"
            return {
                "provider": "faster-whisper",
                "model": "base.en",
                "device": "vulkan",
                "threads": 4,
            }

    def reject_faster_whisper(**_kwargs):
        raise AssertionError("Vulkan failure must not fall back to faster-whisper")

    def fail_resolution(_model):
        raise RuntimeError("GGML model unavailable")

    monkeypatch.setattr(config, "get_config", lambda: FakeConfig())
    monkeypatch.setattr(transcription, "Transcriber", reject_faster_whisper)
    monkeypatch.setattr(whisper_cpp, "resolve_whisper_cpp_model", fail_resolution)

    ingress = voice_runtime.RuntimeVoiceIngress(object())
    with pytest.raises(RuntimeError, match="GGML model unavailable"):
        ingress._default_transcriber_factory()

    assert ingress._default_model is None


def test_runtime_voice_ingress_converts_wire_pcm_and_submits_final_transcript_to_runtime():
    voice_runtime = importlib.import_module("zara.voice_runtime")
    transcriber = RecordingTranscriber()
    supervisor = RecordingSupervisor()
    principal = PrincipalContext("user:alice")
    ingress = voice_runtime.RuntimeVoiceIngress(
        supervisor,
        transcriber_factory=lambda **_kwargs: transcriber,
        queue_size=4,
    )

    try:
        ingress.start(
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-voice-1",
        )
        ingress.chunk(
            PCM_FRAME,
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-voice-1",
            seq=0,
        )

        assert supervisor.submitted.wait(1.0)
        assert transcriber.started == ["mic-1"]
        assert len(transcriber.frames) == 1
        frame = transcriber.frames[0]
        assert frame.dtype == np.float32
        assert frame.shape == (512,)
        expected = np.frombuffer(PCM_FRAME, dtype="<i2").astype(np.float32) / 32768.0
        np.testing.assert_allclose(frame, expected)

        assert len(supervisor.submissions) == 1
        submitted_principal, command = supervisor.submissions[0]
        assert submitted_principal == principal
        assert isinstance(command, SubmitTurn)
        assert command.text == "hello from the daemon microphone stream"
        assert command.conversation_id == "conversation-a"
        assert command.request_id == "trace-voice-1"
    finally:
        ingress.close(timeout=1.0)


def test_runtime_voice_ingress_cancel_is_terminal_and_does_not_submit_stale_transcript():
    voice_runtime = importlib.import_module("zara.voice_runtime")
    transcriber = RecordingTranscriber()
    supervisor = RecordingSupervisor()
    principal = PrincipalContext("user:alice")
    ingress = voice_runtime.RuntimeVoiceIngress(
        supervisor,
        transcriber_factory=lambda **_kwargs: transcriber,
        queue_size=4,
    )

    try:
        common = {
            "principal": principal,
            "conversation_id": "conversation-a",
            "stream_id": "mic-1",
            "trace_id": "trace-voice-1",
        }
        ingress.start(**common)
        ingress.cancel(**common)
        with pytest.raises(KeyError, match="mic-1"):
            ingress.chunk(PCM_FRAME, **common, seq=0)
        assert transcriber.cancelled == ["mic-1"]
        assert supervisor.submissions == []
    finally:
        ingress.close(timeout=1.0)


def test_default_server_gateway_injects_daemon_voice_runtime(monkeypatch):
    captured = {}

    class RecordingGateway:
        def __init__(
            self,
            endpoint,
            *,
            supervisor,
            principal,
            voice_ingress,
            audio_output_format=None,
        ):
            captured.update(
                endpoint=endpoint,
                supervisor=supervisor,
                principal=principal,
                voice_ingress=voice_ingress,
                audio_output_format=audio_output_format,
            )

    import zara.zmq_transport as zmq_transport

    monkeypatch.setattr(zmq_transport, "ZaraZmqGateway", RecordingGateway)
    principal = PrincipalContext("user:alice")
    supervisor = object()
    server = ZaraServer(principal=principal)

    gateway = server._build_default_gateway(
        "ipc:///tmp/zara-test.sock",
        supervisor=supervisor,
        principal=principal,
    )

    assert gateway is not None
    assert captured["supervisor"] is supervisor
    assert captured["principal"] == principal
    assert captured["voice_ingress"] is not None
    assert captured["voice_ingress"].principal == principal
