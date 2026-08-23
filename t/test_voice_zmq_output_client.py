from __future__ import annotations

import threading
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import events
from zara.zmq_transport import TransportConfig, ZmqZaraClient


OUTPUT_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}
OUTPUT_CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"
PCM_OUTPUT = b"\x01\x00" * 240


class RecordingVoiceOutput:
    def __init__(self) -> None:
        self.calls = []
        self.done = threading.Event()
        self.turn2_done = threading.Event()

    def start(self, **kwargs) -> None:
        self.calls.append(("start", kwargs))

    def chunk(self, payload: bytes, **kwargs) -> None:
        self.calls.append(("chunk", payload, kwargs))

    def cancel(self, **kwargs) -> None:
        self.calls.append(("cancel", kwargs))

    def finish(self, **kwargs) -> None:
        self.calls.append(("finish", kwargs))
        self.done.set()
        if kwargs.get("turn_id") == "turn-2":
            self.turn2_done.set()


def endpoint(name: str) -> str:
    return f"inproc://voice-output-client-{name}-{time.time_ns()}"


def transport_config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
    )


def hello_ok(reply_to: str) -> ProtocolMessage:
    return ProtocolMessage(
        type="hello.ok",
        id="hello-ok",
        reply_to=reply_to,
        session_id="session-1",
        timestamp_ns=1,
        payload_count=0,
        body={
            "version": 1,
            "max_payload_frames": 16,
            "max_payload_frame_bytes": 1024 * 1024,
            "max_payload_bytes": 4 * 1024 * 1024,
            "audio_output_format": OUTPUT_FORMAT,
        },
    )


def output_start(turn_id: str, stream_id: str, *, timestamp_ns: int) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.start",
        id=f"{stream_id}-start",
        timestamp_ns=timestamp_ns,
        payload_count=0,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        trace_id=f"trace-{turn_id}",
        body=OUTPUT_FORMAT,
    )


def output_chunk(
    turn_id: str,
    stream_id: str,
    seq: int,
    *,
    timestamp_ns: int,
) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.chunk",
        id=f"{stream_id}-chunk-{seq}",
        timestamp_ns=timestamp_ns,
        payload_count=1,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        seq=seq,
        trace_id=f"trace-{turn_id}",
        content_type=OUTPUT_CONTENT_TYPE,
    )


def output_done(turn_id: str, stream_id: str, *, timestamp_ns: int) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.done",
        id=f"{stream_id}-done",
        timestamp_ns=timestamp_ns,
        payload_count=0,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        trace_id=f"trace-{turn_id}",
    )


def receive_hello(socket: zmq.Socket) -> tuple[bytes, ProtocolMessage]:
    frames = socket.recv_multipart()
    route, app_frames = frames[0], frames[1:]
    hello = decode_message(app_frames).message
    assert hello.body["audio_output_formats"] == [OUTPUT_FORMAT]
    return route, hello


def run_scripted_server(context: zmq.Context, address: str, ready: threading.Event) -> None:
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.bind(address)
    ready.set()
    try:
        route, hello = receive_hello(socket)
        socket.send_multipart([route, *encode_message(hello_ok(hello.id))])
        messages = (
            (output_start("turn-1", "speaker-1", timestamp_ns=2), ()),
            (output_chunk("turn-1", "speaker-1", 0, timestamp_ns=3), (PCM_OUTPUT,)),
            (output_done("turn-1", "speaker-1", timestamp_ns=4), ()),
        )
        for message, payloads in messages:
            socket.send_multipart([route, *encode_message(message, payloads=payloads)])
    finally:
        socket.close(0)


def run_cancelled_turn_server(
    context: zmq.Context,
    address: str,
    ready: threading.Event,
) -> None:
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.bind(address)
    ready.set()
    try:
        route, hello = receive_hello(socket)
        socket.send_multipart([route, *encode_message(hello_ok(hello.id))])
        messages = (
            (output_start("turn-1", "speaker-1", timestamp_ns=2), ()),
            (output_chunk("turn-1", "speaker-1", 0, timestamp_ns=3), (PCM_OUTPUT,)),
            (
                ProtocolMessage(
                    type="turn.cancelled",
                    id="turn-1-cancelled",
                    timestamp_ns=4,
                    payload_count=0,
                    conversation_id="conversation-1",
                    turn_id="turn-1",
                    body={"reason": "barge-in"},
                ),
                (),
            ),
            (output_chunk("turn-1", "speaker-1", 1, timestamp_ns=5), (PCM_OUTPUT,)),
            (output_done("turn-1", "speaker-1", timestamp_ns=6), ()),
            (output_start("turn-2", "speaker-2", timestamp_ns=7), ()),
            (output_chunk("turn-2", "speaker-2", 0, timestamp_ns=8), (PCM_OUTPUT,)),
            (output_done("turn-2", "speaker-2", timestamp_ns=9), ()),
        )
        for message, payloads in messages:
            socket.send_multipart([route, *encode_message(message, payloads=payloads)])
    finally:
        socket.close(0)


def test_zmq_client_hands_binary_output_to_client_owned_playback_boundary():
    context = zmq.Context()
    address = endpoint("handoff")
    ready = threading.Event()
    output = RecordingVoiceOutput()
    server = threading.Thread(
        target=run_scripted_server,
        args=(context, address, ready),
        daemon=True,
    )
    server.start()
    assert ready.wait(1.0)

    client = ZmqZaraClient(
        address,
        context=context,
        config=transport_config(),
        voice_output=output,
    )
    try:
        client.start().result(timeout=1.0)
        assert output.done.wait(1.0)
        assert output.calls == [
            (
                "start",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                    "format": OUTPUT_FORMAT,
                },
            ),
            (
                "chunk",
                PCM_OUTPUT,
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                    "seq": 0,
                },
            ),
            (
                "finish",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                },
            ),
        ]
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()


def test_turn_cancel_stops_playback_and_drops_late_audio_without_affecting_next_turn():
    context = zmq.Context()
    address = endpoint("cancel-stale")
    ready = threading.Event()
    output = RecordingVoiceOutput()
    server = threading.Thread(
        target=run_cancelled_turn_server,
        args=(context, address, ready),
        daemon=True,
    )
    server.start()
    assert ready.wait(1.0)

    client = ZmqZaraClient(
        address,
        context=context,
        config=transport_config(),
        voice_output=output,
    )
    subscription = client.subscribe()
    try:
        client.start().result(timeout=1.0)
        assert output.turn2_done.wait(1.0)
        envelope = subscription.get(timeout=1.0)
        assert isinstance(envelope.event, events.TurnCancelled)
        assert envelope.event.turn_id == "turn-1"
        assert envelope.event.reason == "barge-in"
        assert output.calls == [
            (
                "start",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                    "format": OUTPUT_FORMAT,
                },
            ),
            (
                "chunk",
                PCM_OUTPUT,
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                    "seq": 0,
                },
            ),
            (
                "cancel",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-turn-1",
                },
            ),
            (
                "start",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-2",
                    "stream_id": "speaker-2",
                    "trace_id": "trace-turn-2",
                    "format": OUTPUT_FORMAT,
                },
            ),
            (
                "chunk",
                PCM_OUTPUT,
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-2",
                    "stream_id": "speaker-2",
                    "trace_id": "trace-turn-2",
                    "seq": 0,
                },
            ),
            (
                "finish",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-2",
                    "stream_id": "speaker-2",
                    "trace_id": "trace-turn-2",
                },
            ),
        ]
    finally:
        subscription.close()
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()
