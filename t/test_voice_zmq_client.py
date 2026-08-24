from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.runtime import bridge, events
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, ZmqZaraClient


PCM_FRAME = b"\x01\x00" * 512


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        future.set_exception(AssertionError(f"unexpected runtime command: {command!r}"))
        return future


class RecordingVoiceIngress:
    def __init__(self) -> None:
        self.calls = []

    def start(self, **kwargs):
        self.calls.append(("start", kwargs))

    def chunk(self, payload: bytes, **kwargs):
        self.calls.append(("chunk", payload, kwargs))

    def commit(self, **kwargs):
        self.calls.append(("commit", kwargs))

    def cancel(self, **kwargs):
        self.calls.append(("cancel", kwargs))


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


@pytest.fixture
def transport_config():
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
    )


def endpoint(name: str) -> str:
    return f"inproc://voice-client-{name}-{time.time_ns()}"


def test_zara_client_streams_binary_pcm_with_trace_and_conversation_correlation(
    zmq_context,
    transport_config,
):
    address = endpoint("roundtrip")
    ingress = RecordingVoiceIngress()
    principal = PrincipalContext("user:voice-client")
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=principal,
        context=zmq_context,
        config=transport_config,
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(address, context=zmq_context, config=transport_config)

    try:
        client.start().result(timeout=1.0)
        conversation_id = client.open_conversation("conversation-voice").result(timeout=1.0)

        started = client.start_audio_input("mic-1", trace_id="trace-voice").result(timeout=1.0)
        accepted = client.send_audio_input(
            "mic-1",
            seq=0,
            pcm=PCM_FRAME,
            trace_id="trace-voice",
        ).result(timeout=1.0)
        committed = client.commit_audio_input("mic-1", trace_id="trace-voice").result(timeout=1.0)

        assert started.type == "audio.input.started"
        assert accepted.type == "audio.input.accepted"
        assert committed.type == "audio.input.committed"
        assert ingress.calls == [
            (
                "start",
                {
                    "principal": principal,
                    "conversation_id": conversation_id,
                    "stream_id": "mic-1",
                    "trace_id": "trace-voice",
                },
            ),
            (
                "chunk",
                PCM_FRAME,
                {
                    "principal": principal,
                    "conversation_id": conversation_id,
                    "stream_id": "mic-1",
                    "trace_id": "trace-voice",
                    "seq": 0,
                },
            ),
            (
                "commit",
                {
                    "principal": principal,
                    "conversation_id": conversation_id,
                    "stream_id": "mic-1",
                    "trace_id": "trace-voice",
                },
            ),
        ]
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_zara_client_subscription_receives_typed_visible_stt_events(
    zmq_context,
    transport_config,
):
    address = endpoint("visible-stt")
    supervisor = FakeSupervisor()
    principal = PrincipalContext("user:voice-client")
    gateway = ZaraZmqGateway(
        address,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config,
        voice_ingress=RecordingVoiceIngress(),
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(address, context=zmq_context, config=transport_config)

    try:
        client.start().result(timeout=1.0)
        conversation_id = client.open_conversation("conversation-stt").result(timeout=1.0)
        subscription = client.subscribe()
        try:
            supervisor.bus.publish(
                events.VoiceSpeechStarted(
                    conversation_id=conversation_id,
                    stream_id="mic-1",
                    trace_id="trace-voice",
                    pre_speech_samples=512,
                )
            )
            supervisor.bus.publish(
                events.VoiceTranscriptPartial(
                    conversation_id=conversation_id,
                    stream_id="mic-1",
                    trace_id="trace-voice",
                    text="hello wor",
                )
            )
            supervisor.bus.publish(
                events.VoiceSpeechEnded(
                    conversation_id=conversation_id,
                    stream_id="mic-1",
                    trace_id="trace-voice",
                    reason="silence",
                )
            )
            supervisor.bus.publish(
                events.VoiceTranscriptFinal(
                    conversation_id=conversation_id,
                    stream_id="mic-1",
                    trace_id="trace-voice",
                    text="hello world",
                    provider="provider-secret-must-not-cross-wire",
                )
            )

            received = [subscription.get(timeout=1.0).event for _ in range(4)]
            assert [type(event) for event in received] == [
                events.VoiceSpeechStarted,
                events.VoiceTranscriptPartial,
                events.VoiceSpeechEnded,
                events.VoiceTranscriptFinal,
            ]
            assert [event.conversation_id for event in received] == [conversation_id] * 4
            assert [event.stream_id for event in received] == ["mic-1"] * 4
            assert [event.trace_id for event in received] == ["trace-voice"] * 4
            assert received[0].pre_speech_samples == 512
            assert received[1].text == "hello wor"
            assert received[2].reason == "silence"
            assert received[3].text == "hello world"
            assert received[3].provider == ""
        finally:
            subscription.close()
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)
