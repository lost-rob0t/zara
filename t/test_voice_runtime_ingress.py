from __future__ import annotations

import concurrent.futures
import importlib
import queue
import threading
from types import SimpleNamespace

import numpy as np
import pytest

from zara.runtime import bridge, events
from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalContext, ZaraServer
from zara.streaming_stt import FinalTranscript, PartialTranscript


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
            PartialTranscript(
                turn_id="untrusted-stt-stream-id",
                text="hello from the daemon",
                text_length=21,
            ),
            FinalTranscript(
                turn_id="untrusted-stt-stream-id",
                text="hello from the daemon microphone stream",
                text_length=39,
                provider="fixture-provider-must-not-leak",
            ),
        ]

    def cancel(self, turn_id=None) -> None:
        self.cancelled.append(turn_id)


class BlockingTranscriber(RecordingTranscriber):
    def __init__(self) -> None:
        super().__init__()
        self.entered = threading.Event()
        self.release = threading.Event()

    def feed(self, chunk: np.ndarray):
        self.frames.append(chunk.copy())
        self.entered.set()
        assert self.release.wait(1.0)
        return [
            PartialTranscript(
                turn_id="stale-stt-id",
                text="stale partial",
                text_length=13,
            ),
            FinalTranscript(
                turn_id="stale-stt-id",
                text="stale final",
                text_length=11,
                provider="fixture",
            ),
        ]


class RecordingSupervisor:
    def __init__(self) -> None:
        self.submissions = []
        self.submitted = threading.Event()
        self.bus = bridge.RuntimeEventBus()
        self.runtime_principals = []

    def runtime(self, principal):
        self.runtime_principals.append(principal)
        return SimpleNamespace(bus=self.bus)

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


def test_runtime_voice_ingress_publishes_partial_and_final_with_server_owned_correlation():
    voice_runtime = importlib.import_module("zara.voice_runtime")
    transcriber = RecordingTranscriber()
    supervisor = RecordingSupervisor()
    subscription = supervisor.bus.subscribe()
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
        partial = subscription.get(timeout=1.0).event
        final = subscription.get(timeout=1.0).event
        partial_type = getattr(events, "TranscriptPartial", None)
        assert partial_type is not None
        assert isinstance(partial, partial_type)
        assert partial.text == "hello from the daemon"
        assert partial.conversation_id == "conversation-a"
        assert partial.turn_id is None
        assert partial.stream_id == "mic-1"
        assert partial.trace_id == "trace-voice-1"
        assert isinstance(final, events.TranscriptReady)
        assert final.text == "hello from the daemon microphone stream"
        assert final.conversation_id == "conversation-a"
        assert final.turn_id is None
        assert final.stream_id == "mic-1"
        assert final.trace_id == "trace-voice-1"
        assert supervisor.runtime_principals == [principal, principal]
        assert "fixture-provider-must-not-leak" not in repr(partial)
        assert "fixture-provider-must-not-leak" not in repr(final)
    finally:
        subscription.close()
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


def test_cancelled_stream_drops_transcript_returned_by_inflight_stt_work():
    voice_runtime = importlib.import_module("zara.voice_runtime")
    transcriber = BlockingTranscriber()
    supervisor = RecordingSupervisor()
    subscription = supervisor.bus.subscribe()
    principal = PrincipalContext("user:alice")
    ingress = voice_runtime.RuntimeVoiceIngress(
        supervisor,
        transcriber_factory=lambda **_kwargs: transcriber,
        queue_size=4,
    )
    common = {
        "principal": principal,
        "conversation_id": "conversation-a",
        "stream_id": "mic-race",
        "trace_id": "trace-race",
    }

    try:
        ingress.start(**common)
        ingress.chunk(PCM_FRAME, **common, seq=0)
        assert transcriber.entered.wait(1.0)
        ingress.cancel(**common)
        transcriber.release.set()
        with pytest.raises(queue.Empty):
            subscription.get(timeout=0.2)
        assert supervisor.submissions == []
    finally:
        transcriber.release.set()
        subscription.close()
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
