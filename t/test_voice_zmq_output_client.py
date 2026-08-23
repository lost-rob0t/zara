from __future__ import annotations

import threading
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.zmq_transport import TransportConfig, ZmqZaraClient


OUTPUT_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}
OUTPUT_CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"
PCM_OUTPUT = b"\x01\x00" * 240


class RecordingVoiceOutput:
    def __init__(self) -> None:
        self.calls = []
        self.done = threading.Event()

    def start(self, **kwargs) -> None:
        self.calls.append(("start", kwargs))

    def chunk(self, payload: bytes, **kwargs) -> None:
        self.calls.append(("chunk", payload, kwargs))

    def finish(self, **kwargs) -> None:
        self.calls.append(("finish", kwargs))
        self.done.set()


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


def run_scripted_server(context: zmq.Context, address: str, ready: threading.Event) -> None:
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.bind(address)
    ready.set()
    try:
        frames = socket.recv_multipart()
        route, app_frames = frames[0], frames[1:]
        hello = decode_message(app_frames).message
        socket.send_multipart(
            [
                route,
                *encode_message(
                    ProtocolMessage(
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
                        },
                    )
                ),
            ]
        )
        messages = (
            (
                ProtocolMessage(
                    type="audio.output.start",
                    id="output-start",
                    timestamp_ns=2,
                    payload_count=0,
                    conversation_id="conversation-1",
                    turn_id="turn-1",
                    stream_id="speaker-1",
                    trace_id="trace-1",
                    body=OUTPUT_FORMAT,
                ),
                (),
            ),
            (
                ProtocolMessage(
                    type="audio.output.chunk",
                    id="output-chunk",
                    timestamp_ns=3,
                    payload_count=1,
                    conversation_id="conversation-1",
                    turn_id="turn-1",
                    stream_id="speaker-1",
                    seq=0,
                    trace_id="trace-1",
                    content_type=OUTPUT_CONTENT_TYPE,
                ),
                (PCM_OUTPUT,),
            ),
            (
                ProtocolMessage(
                    type="audio.output.done",
                    id="output-done",
                    timestamp_ns=4,
                    payload_count=0,
                    conversation_id="conversation-1",
                    turn_id="turn-1",
                    stream_id="speaker-1",
                    trace_id="trace-1",
                ),
                (),
            ),
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
                    "trace_id": "trace-1",
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
                    "trace_id": "trace-1",
                    "seq": 0,
                },
            ),
            (
                "finish",
                {
                    "conversation_id": "conversation-1",
                    "turn_id": "turn-1",
                    "stream_id": "speaker-1",
                    "trace_id": "trace-1",
                },
            ),
        ]
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()
