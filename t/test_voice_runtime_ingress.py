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
        def __init__(self, endpoint, *, supervisor, principal, voice_ingress):
            captured.update(
                endpoint=endpoint,
                supervisor=supervisor,
                principal=principal,
                voice_ingress=voice_ingress,
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
