"""Daemon TTS output bridge and ZARA/1 audio.output mapping (#244)."""

from __future__ import annotations

import asyncio
import io
import threading
import time
import wave

import pytest

from zara.protocol_runtime import runtime_event_to_message
from zara.runtime import events
from zara.runtime.bridge import EventEnvelope, RuntimeEventBus
from zara.runtime.tts_output import TtsOutputBridge
from zara.tts.engine import StreamChunk


def wav_bytes(sample_rate: int = 24000, samples: int = 480) -> bytes:
    buffer = io.BytesIO()
    with wave.open(buffer, "wb") as handle:
        handle.setnchannels(1)
        handle.setsampwidth(2)
        handle.setframerate(sample_rate)
        handle.writeframes(b"\x00\x01" * samples)
    return buffer.getvalue()


class FakeEngine:
    def __init__(self, audio: bytes, delay_event: asyncio.Event | None = None) -> None:
        self.audio = audio
        self.calls: list[str] = []
        self.delay_event = delay_event

    async def synthesize_stream(self, text: str):
        self.calls.append(text)
        if self.delay_event is not None:
            await self.delay_event.wait()
        yield StreamChunk(
            provider="fake",
            audio=self.audio,
            audio_format="wav",
            first_chunk=True,
        )


def build_bridge(engine, **kwargs) -> TtsOutputBridge:
    bus = RuntimeEventBus()
    published: list[events.RuntimeEvent] = []
    subscription = bus.subscribe()
    bridge = TtsOutputBridge(
        subscription=subscription,
        publish=published.append,
        engine_factory=lambda: engine,
        **kwargs,
    )
    bridge.published = published
    return bridge


def envelope_of(event) -> EventEnvelope:
    bus = RuntimeEventBus()
    return bus.publish(event)


@pytest.mark.asyncio
async def test_punctuated_delta_synthesizes_phrase():
    engine = FakeEngine(wav_bytes())
    bridge = build_bridge(engine, sample_rate=24000)

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="turn-0001", conversation_id="conv-1", text="Hello there. "
        )
    )
    await bridge.wait_for_idle()

    started = [event for event in bridge.published if isinstance(event, events.AudioOutputStarted)]
    chunks = [event for event in bridge.published if isinstance(event, events.AudioOutputChunk)]
    assert len(started) == 1
    assert started[0].sample_rate == 24000
    assert started[0].stream_id
    assert chunks and len(chunks[0].pcm) > 0
    assert engine.calls == ["Hello there."]


@pytest.mark.asyncio
async def test_unpunctuated_text_flushes_on_completion():
    engine = FakeEngine(wav_bytes())
    bridge = build_bridge(engine, sample_rate=24000)

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="turn-0002", conversation_id="conv-1", text="hello there"
        )
    )
    await bridge.wait_for_idle()
    assert engine.calls == []

    await bridge.handle_event(
        events.AssistantComplete(turn_id="turn-0002", conversation_id="conv-1", text="hello there")
    )
    await bridge.wait_for_idle()

    assert engine.calls == ["hello there"]
    finished = [event for event in bridge.published if isinstance(event, events.AudioOutputFinished)]
    assert len(finished) == 1
    assert finished[0].turn_id == "turn-0002"


@pytest.mark.asyncio
async def test_multiple_phrases_synthesize_in_order():
    engine = FakeEngine(wav_bytes())
    bridge = build_bridge(engine, sample_rate=24000)

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="turn-0003",
            conversation_id="conv-1",
            text="First phrase. Second phrase. Third",
        )
    )
    await bridge.handle_event(
        events.AssistantComplete(turn_id="turn-0003", conversation_id="conv-1", text="")
    )
    await bridge.wait_for_idle()

    assert engine.calls == ["First phrase.", "Second phrase.", "Third"]


@pytest.mark.asyncio
async def test_turn_cancellation_stops_synthesis():
    gate = asyncio.Event()
    engine = FakeEngine(wav_bytes(), delay_event=gate)
    bridge = build_bridge(engine, sample_rate=24000)

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="turn-0004", conversation_id="conv-1", text="Hello there. "
        )
    )
    await bridge.handle_event(
        events.TurnCancelled(turn_id="turn-0004", reason="cancel command")
    )
    gate.set()
    await asyncio.sleep(0.05)
    await bridge.wait_for_idle()

    chunks = [event for event in bridge.published if isinstance(event, events.AudioOutputChunk)]
    assert chunks == []
    finished = [event for event in bridge.published if isinstance(event, events.AudioOutputFinished)]
    assert finished == []


@pytest.mark.asyncio
async def test_wav_decode_produces_s16le_mono_at_target_rate():
    engine = FakeEngine(wav_bytes(sample_rate=48000, samples=480))
    bridge = build_bridge(engine, sample_rate=24000)

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="turn-0005", conversation_id="conv-1", text="Okay. "
        )
    )
    await bridge.wait_for_idle()

    chunks = [event for event in bridge.published if isinstance(event, events.AudioOutputChunk)]
    assert chunks
    pcm = chunks[0].pcm
    assert len(pcm) % 2 == 0
    import struct

    values = struct.unpack(f"<{len(pcm) // 2}h", pcm)
    assert max(values) <= 32767 and min(values) >= -32768


def test_audio_output_started_maps_to_wire_start():
    envelope = envelope_of(
        events.AudioOutputStarted(
            turn_id="turn-0006", conversation_id="conv-1", stream_id="tts-1", sample_rate=24000
        )
    )
    message = runtime_event_to_message(envelope, message_id="m1", timestamp_ns=1)

    assert message.type == "audio.output.start"
    assert message.turn_id == "turn-0006"
    assert message.stream_id == "tts-1"
    assert message.body == {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}


def test_audio_output_chunk_maps_to_wire_chunk_with_payload():
    envelope = envelope_of(
        events.AudioOutputChunk(
            turn_id="turn-0007", conversation_id="conv-1", stream_id="tts-1", pcm=b"\x01\x00\x02\x00"
        )
    )
    message = runtime_event_to_message(envelope, message_id="m2", timestamp_ns=1)

    assert message.type == "audio.output.chunk"
    assert message.payload_count == 1
    assert message.content_type == "audio/pcm;codec=pcm_s16le"
    assert message.stream_id == "tts-1"


def test_audio_output_finished_maps_to_wire_done():
    envelope = envelope_of(
        events.AudioOutputFinished(
            turn_id="turn-0008", conversation_id="conv-1", stream_id="tts-1"
        )
    )
    message = runtime_event_to_message(envelope, message_id="m3", timestamp_ns=1)

    assert message.type == "audio.output.done"
    assert message.turn_id == "turn-0008"
    assert message.stream_id == "tts-1"


# ---------------------------------------------------------------------------
# Gateway delivery over a real ZARA/1 gateway + client


class FakeSupervisor:
    def __init__(self) -> None:
        from zara.runtime import bridge
        from zara.server import ServerState

        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):
        raise AssertionError(f"unexpected runtime command: {command!r}")


class RecordingSink:
    def __init__(self) -> None:
        self.calls: list[tuple] = []
        self.lock = threading.Lock()

    def start(self, **kwargs) -> None:
        with self.lock:
            self.calls.append(("start", kwargs))

    def chunk(self, payload, **kwargs) -> None:
        with self.lock:
            self.calls.append(("chunk", payload, kwargs))

    def cancel(self, **kwargs) -> None:
        with self.lock:
            self.calls.append(("cancel", kwargs))

    def finish(self, **kwargs) -> None:
        with self.lock:
            self.calls.append(("finish", kwargs))

    def kinds(self) -> list[str]:
        with self.lock:
            return [call[0] for call in self.calls]


def _wait_for(predicate, timeout: float = 2.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.01)
    return predicate()


@pytest.fixture
def zmq_ctx():
    import zmq

    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


OUTPUT_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}


def _start_gateway_with_client(zmq_ctx, *, offer_formats):
    from zara.server import PrincipalContext
    from zara.zmq_transport import TransportConfig, ZaraZmqGateway

    config = TransportConfig(
        sndhwm=16,
        rcvhwm=16,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
    )
    address = f"inproc://tts-gateway-{time.time_ns()}"
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        address,
        supervisor=supervisor,
        principal=PrincipalContext("user:voice"),
        context=zmq_ctx,
        config=config,
    )
    gateway.start().result(timeout=1.0)

    from zara.zmq_transport import ZmqZaraClient

    sink = RecordingSink()
    kwargs = {}
    if offer_formats:
        kwargs["audio_output_formats"] = [OUTPUT_FORMAT]
    client = ZmqZaraClient(
        address,
        context=zmq_ctx,
        config=config,
        voice_output=sink,
        **kwargs,
    )
    client.start().result(timeout=1.0)
    client.open_conversation("conv-9").result(timeout=1.0)
    return supervisor, gateway, client, sink


def test_gateway_delivers_audio_output_to_negotiated_client(zmq_ctx):
    supervisor, gateway, client, sink = _start_gateway_with_client(
        zmq_ctx, offer_formats=True
    )
    try:
        supervisor.bus.publish(
            events.AudioOutputStarted(
                turn_id="turn-31",
                conversation_id="conv-9",
                stream_id="tts-31",
                sample_rate=24000,
                channels=1,
            )
        )
        supervisor.bus.publish(
            events.AudioOutputChunk(
                turn_id="turn-31",
                conversation_id="conv-9",
                stream_id="tts-31",
                pcm=b"\x01\x00" * 64,
            )
        )
        supervisor.bus.publish(
            events.AudioOutputFinished(
                turn_id="turn-31", conversation_id="conv-9", stream_id="tts-31"
            )
        )

        assert _wait_for(lambda: sink.kinds() == ["start", "chunk", "finish"])
        start_call = sink.calls[0][1]
        assert start_call["turn_id"] == "turn-31"
        assert start_call["stream_id"] == "tts-31"
        assert start_call["format"]["sample_rate"] == 24000
        assert sink.calls[1][1] == b"\x01\x00" * 64
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_gateway_drops_audio_for_non_negotiated_route(zmq_ctx):
    import zmq

    from zara.protocol import ProtocolMessage, decode_message, encode_message
    from zara.server import PrincipalContext
    from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options

    config = TransportConfig(
        sndhwm=16,
        rcvhwm=16,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
    )
    address = f"inproc://tts-legacy-{time.time_ns()}"
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        address,
        supervisor=supervisor,
        principal=PrincipalContext("user:voice"),
        context=zmq_ctx,
        config=config,
    )
    gateway.start().result(timeout=1.0)
    dealer = zmq_ctx.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    dealer.connect(address)
    try:
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-legacy",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        poller = zmq.Poller()
        poller.register(dealer, zmq.POLLIN)
        assert dict(poller.poll(1000)).get(dealer) == zmq.POLLIN
        opened = decode_message(dealer.recv_multipart()).message
        assert opened.type == "hello.ok"
        assert "audio_output_format" not in (opened.body or {})

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="conversation.open",
                    id="open-legacy",
                    timestamp_ns=2,
                    payload_count=0,
                    body={"conversation_id": "conv-9"},
                )
            )
        )
        assert dict(poller.poll(1000)).get(dealer) == zmq.POLLIN
        opened = decode_message(dealer.recv_multipart()).message
        assert opened.type == "conversation.opened"

        supervisor.bus.publish(
            events.AudioOutputChunk(
                turn_id="turn-32",
                conversation_id="conv-9",
                stream_id="tts-32",
                pcm=b"\x01\x00" * 8,
            )
        )
        assert dict(poller.poll(300)).get(dealer) is None
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
