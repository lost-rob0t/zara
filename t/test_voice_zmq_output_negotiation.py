from __future__ import annotations

import threading
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.zmq_transport import TransportConfig, ZmqZaraClient


NEGOTIATED_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}
MISMATCHED_FORMAT = {"codec": "pcm_s16le", "sample_rate": 48000, "channels": 1}
CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"
PCM_OUTPUT = b"\x01\x00" * 240


class RecordingVoiceOutput:
    def __init__(self) -> None:
        self.calls = []
        self.valid_done = threading.Event()

    def start(self, **kwargs) -> None:
        self.calls.append(("start", kwargs))

    def chunk(self, payload: bytes, **kwargs) -> None:
        self.calls.append(("chunk", payload, kwargs))

    def finish(self, **kwargs) -> None:
        self.calls.append(("finish", kwargs))
        if kwargs.get("turn_id") == "turn-valid":
            self.valid_done.set()

    def cancel(self, **kwargs) -> None:
        self.calls.append(("cancel", kwargs))


def endpoint() -> str:
    return f"inproc://voice-output-negotiation-{time.time_ns()}"


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


def output_start(turn_id: str, stream_id: str, format_spec: dict[str, object]) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.start",
        id=f"{stream_id}-start",
        timestamp_ns=2,
        payload_count=0,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        trace_id=f"trace-{turn_id}",
        body=format_spec,
    )


def output_chunk(turn_id: str, stream_id: str) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.chunk",
        id=f"{stream_id}-chunk-0",
        timestamp_ns=3,
        payload_count=1,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        seq=0,
        trace_id=f"trace-{turn_id}",
        content_type=CONTENT_TYPE,
    )


def output_done(turn_id: str, stream_id: str) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.done",
        id=f"{stream_id}-done",
        timestamp_ns=4,
        payload_count=0,
        conversation_id="conversation-1",
        turn_id=turn_id,
        stream_id=stream_id,
        trace_id=f"trace-{turn_id}",
    )


def run_server(context: zmq.Context, address: str, ready: threading.Event) -> None:
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.bind(address)
    ready.set()
    try:
        frames = socket.recv_multipart()
        route, hello = frames[0], decode_message(frames[1:]).message
        assert hello.body["audio_output_formats"] == [NEGOTIATED_FORMAT]
        hello_ok = ProtocolMessage(
            type="hello.ok",
            id="hello-ok",
            reply_to=hello.id,
            session_id="session-1",
            timestamp_ns=1,
            payload_count=0,
            body={
                "version": 1,
                "max_payload_frames": 16,
                "max_payload_frame_bytes": 1024 * 1024,
                "max_payload_bytes": 4 * 1024 * 1024,
                "audio_output_format": NEGOTIATED_FORMAT,
            },
        )
        socket.send_multipart([route, *encode_message(hello_ok)])

        messages = (
            (output_start("turn-mismatch", "speaker-bad", MISMATCHED_FORMAT), ()),
            (output_start("turn-valid", "speaker-good", NEGOTIATED_FORMAT), ()),
            (output_chunk("turn-valid", "speaker-good"), (PCM_OUTPUT,)),
            (output_done("turn-valid", "speaker-good"), ()),
        )
        for message, payloads in messages:
            socket.send_multipart([route, *encode_message(message, payloads=payloads)])
    finally:
        socket.close(0)


def test_output_start_must_match_negotiated_format_before_playback() -> None:
    context = zmq.Context()
    address = endpoint()
    ready = threading.Event()
    output = RecordingVoiceOutput()
    server = threading.Thread(target=run_server, args=(context, address, ready), daemon=True)
    server.start()
    assert ready.wait(1.0)

    client = ZmqZaraClient(
        address,
        context=context,
        config=transport_config(),
        voice_output=output,
        audio_output_formats=[NEGOTIATED_FORMAT],
    )
    try:
        client.start().result(timeout=1.0)
        assert client.audio_output_format == NEGOTIATED_FORMAT
        assert output.valid_done.wait(1.0)
        assert [
            call for call in output.calls if call[1].get("turn_id") == "turn-mismatch"
        ] == []
        assert [call[0] for call in output.calls] == ["start", "chunk", "finish"]
        assert output.calls[0][1]["format"] == NEGOTIATED_FORMAT
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()
