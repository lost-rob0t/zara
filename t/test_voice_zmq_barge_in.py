from __future__ import annotations

import threading
import time

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.zmq_transport import TransportConfig, ZmqZaraClient


OUTPUT_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}


class RecordingVoiceOutput:
    def __init__(self) -> None:
        self.calls = []
        self.started = threading.Event()

    def start(self, **kwargs) -> None:
        self.calls.append(("start", kwargs))
        self.started.set()

    def chunk(self, payload: bytes, **kwargs) -> None:
        self.calls.append(("chunk", payload, kwargs))

    def cancel(self, **kwargs) -> None:
        self.calls.append(("cancel", kwargs))

    def finish(self, **kwargs) -> None:
        self.calls.append(("finish", kwargs))


def endpoint(name: str) -> str:
    return f"inproc://voice-barge-in-{name}-{time.time_ns()}"


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
        },
    )


def output_start() -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.output.start",
        id="speaker-start",
        timestamp_ns=2,
        payload_count=0,
        conversation_id="conversation-1",
        turn_id="turn-active",
        stream_id="speaker-active",
        trace_id="trace-active",
        body=OUTPUT_FORMAT,
    )


def run_barge_in_server(
    context: zmq.Context,
    address: str,
    ready: threading.Event,
    received: list[ProtocolMessage],
    received_one: threading.Event,
) -> None:
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.setsockopt(zmq.RCVTIMEO, 1000)
    socket.bind(address)
    ready.set()
    try:
        frames = socket.recv_multipart()
        route, app_frames = frames[0], frames[1:]
        hello = decode_message(app_frames).message
        socket.send_multipart([route, *encode_message(hello_ok(hello.id))])
        socket.send_multipart([route, *encode_message(output_start())])

        first_frames = socket.recv_multipart()
        first = decode_message(first_frames[1:]).message
        received.append(first)
        received_one.set()

        if first.type == "turn.cancel":
            socket.send_multipart(
                [
                    route,
                    *encode_message(
                        ProtocolMessage(
                            type="turn.cancel.accepted",
                            id="cancel-ok",
                            reply_to=first.id,
                            turn_id=first.turn_id,
                            timestamp_ns=3,
                            payload_count=0,
                        )
                    ),
                ]
            )
            second_frames = socket.recv_multipart()
            second = decode_message(second_frames[1:]).message
            received.append(second)
            if second.type == "audio.input.start":
                socket.send_multipart(
                    [
                        route,
                        *encode_message(
                            ProtocolMessage(
                                type="audio.input.started",
                                id="audio-started",
                                reply_to=second.id,
                                stream_id=second.stream_id,
                                timestamp_ns=4,
                                payload_count=0,
                            )
                        ),
                    ]
                )
        elif first.type == "audio.input.start":
            socket.send_multipart(
                [
                    route,
                    *encode_message(
                        ProtocolMessage(
                            type="audio.input.started",
                            id="audio-started",
                            reply_to=first.id,
                            stream_id=first.stream_id,
                            timestamp_ns=3,
                            payload_count=0,
                        )
                    ),
                ]
            )
    finally:
        socket.close(0)


def test_new_speech_stops_active_client_playback_before_network_ack():
    context = zmq.Context()
    address = endpoint("local-stop")
    ready = threading.Event()
    received_one = threading.Event()
    received = []
    output = RecordingVoiceOutput()
    server = threading.Thread(
        target=run_barge_in_server,
        args=(context, address, ready, received, received_one),
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
        assert output.started.wait(1.0)

        audio_future = client.start_audio_input("mic-new", trace_id="trace-new")

        assert [call[0] for call in output.calls] == ["start", "cancel"]
        assert output.calls[-1][1] == {
            "conversation_id": "conversation-1",
            "turn_id": "turn-active",
            "stream_id": "speaker-active",
            "trace_id": "trace-active",
        }
        audio_future.result(timeout=1.0)
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()


def test_new_speech_sends_canonical_turn_cancel_before_audio_stream_start():
    context = zmq.Context()
    address = endpoint("cancel-order")
    ready = threading.Event()
    received_one = threading.Event()
    received = []
    output = RecordingVoiceOutput()
    server = threading.Thread(
        target=run_barge_in_server,
        args=(context, address, ready, received, received_one),
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
        assert output.started.wait(1.0)

        audio_future = client.start_audio_input("mic-new", trace_id="trace-new")
        assert received_one.wait(1.0)

        assert received[0].type == "turn.cancel"
        assert received[0].turn_id == "turn-active"
        audio_future.result(timeout=1.0)
        assert [message.type for message in received] == [
            "turn.cancel",
            "audio.input.start",
        ]
        assert received[1].stream_id == "mic-new"
        assert received[1].trace_id == "trace-new"
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()
