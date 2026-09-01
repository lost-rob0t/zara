"""trace_id continuity from wake audio ingress through daemon events (AC5)."""

from __future__ import annotations

import concurrent.futures
import time

import pytest

from zara.latency import LatencyTrace
from zara.runtime import bridge, events
from zara.runtime.host import RuntimeHost
from zara.server import PrincipalContext, ServerState
from zara.streaming_stt import (
    FinalTranscript,
    PartialTranscript,
    SpeechEnded,
    SpeechStarted,
)
from zara.voice_runtime import RuntimeVoiceIngress
from zara.wake_daemon import WakeDaemonClient
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, ZmqZaraClient

PCM_FRAME = b"\x00\x00" * 512


class RecordingSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()
        self.submitted = []

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def publish(self, principal, event):
        assert isinstance(principal, PrincipalContext)
        return self.bus.publish(event)

    def submit(self, principal, command):
        self.submitted.append((principal, command))
        future = concurrent.futures.Future()
        future.set_exception(RuntimeError("turns not executed in this harness"))
        return future


class ContinuityTranscriber:
    def start_turn(self, _turn_id):
        pass

    def feed(self, _chunk):
        return [
            SpeechStarted(turn_id="mic-1", pre_speech_samples=512),
            PartialTranscript(turn_id="mic-1", text="open fire", text_length=9),
            FinalTranscript(
                turn_id="mic-1",
                text="open firefox",
                text_length=12,
                provider="fixture-provider",
            ),
        ]

    def commit(self, _turn_id=None):
        return [SpeechEnded(turn_id="mic-1", reason="commit")]

    def cancel(self, _turn_id=None):
        pass


@pytest.fixture
def zmq_context():
    import zmq

    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def transport_config():
    return TransportConfig(
        sndhwm=16,
        rcvhwm=16,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
    )


def build_stack(zmq_context):
    address = f"inproc://wake-latency-{time.time_ns()}"
    supervisor = RecordingSupervisor()
    principal = PrincipalContext("user:latency")
    voice_ingress = RuntimeVoiceIngress(
        supervisor,
        principal=principal,
        transcriber_factory=lambda **_kwargs: ContinuityTranscriber(),
        queue_size=4,
    )
    from zara.zmq_transport import ZaraZmqGateway

    gateway = ZaraZmqGateway(
        address,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config(),
        voice_ingress=voice_ingress,
    )
    gateway.start().result(timeout=1.0)
    daemon = WakeDaemonClient(
        client=ZmqZaraClient(address, context=zmq_context, config=transport_config())
    )
    daemon.connect()
    return supervisor, gateway, voice_ingress, daemon


@pytest.mark.asyncio
async def test_client_trace_id_reaches_voice_events_and_submit_turn(zmq_context):
    supervisor, gateway, voice_ingress, daemon = build_stack(zmq_context)
    received: list[events.RuntimeEvent] = []
    daemon.on_transcript_final.append(received.append)
    daemon.start_pump()
    try:
        import numpy as np

        audio = np.zeros((512, 1), dtype=np.float32)
        await daemon.stream_utterance(audio, trace_id="trace-e2e-1")

        deadline = time.monotonic() + 2.0
        while not received and time.monotonic() < deadline:
            time.sleep(0.02)

        assert received, "final transcript event did not reach the wake client"
        final = received[0]
        assert final.trace_id == "trace-e2e-1"
        assert final.text == "open firefox"
        assert supervisor.submitted, "final transcript did not submit a turn"
        principal, command = supervisor.submitted[0]
        assert command.request_id == "trace-e2e-1"
        assert command.text == "open firefox"
    finally:
        daemon.close()
        gateway.close(timeout=1.0)
        voice_ingress.close(timeout=1.0)


def test_host_turn_trace_uses_submit_turn_request_id():
    from zara.runtime.commands import SubmitTurn

    host = RuntimeHost(
        backend_factory=lambda: None,
        publisher=lambda event: None,
        subscriber=lambda **kwargs: None,
    )
    command = SubmitTurn(text="open firefox", request_id="trace-e2e-2")
    trace = host._build_turn_latency_trace(command)
    assert trace is not None
    assert trace.trace_id == "trace-e2e-2"
