from __future__ import annotations

import concurrent.futures
import queue
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


PCM_FRAME = b"\x01\x02" * 512
CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"


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


class BackpressuredVoiceIngress:
    def __init__(self) -> None:
        self.reject_next_chunk = True
        self.accepted = []

    def start(self, **_kwargs):
        return None

    def chunk(self, payload: bytes, **kwargs):
        if self.reject_next_chunk:
            self.reject_next_chunk = False
            raise queue.Full
        self.accepted.append((payload, kwargs))

    def commit(self, **_kwargs):
        return None

    def cancel(self, **_kwargs):
        return None


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def transport_config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
    )


def receive(socket: zmq.Socket) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(1000)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage, payloads=()) -> ProtocolMessage:
    socket.send_multipart(encode_message(message, payloads=payloads))
    return receive(socket)


def test_ingress_queue_full_is_explicit_and_retry_does_not_skip_sequence(zmq_context):
    ingress = BackpressuredVoiceIngress()
    address = f"inproc://voice-backpressure-{time.time_ns()}"
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=zmq_context,
        config=transport_config(),
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    dealer = zmq_context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config(), router=False)
    dealer.connect(address)
    try:
        hello = send(
            dealer,
            ProtocolMessage(
                type="hello",
                id="hello",
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            ),
        )
        assert hello.type == "hello.ok"

        opened = send(
            dealer,
            ProtocolMessage(
                type="conversation.open",
                id="open",
                timestamp_ns=1,
                payload_count=0,
                conversation_id="conversation-a",
            ),
        )
        assert opened.type == "conversation.opened"

        started = send(
            dealer,
            ProtocolMessage(
                type="audio.input.start",
                id="start",
                timestamp_ns=1,
                payload_count=0,
                stream_id="mic-1",
                trace_id="trace-a",
                body={
                    "codec": "pcm_s16le",
                    "sample_rate": 16000,
                    "channels": 1,
                    "frame_samples": 512,
                },
            ),
        )
        assert started.type == "audio.input.started"

        chunk = ProtocolMessage(
            type="audio.input.chunk",
            id="chunk-0-a",
            timestamp_ns=1,
            payload_count=1,
            stream_id="mic-1",
            seq=0,
            trace_id="trace-a",
            content_type=CONTENT_TYPE,
        )
        rejected = send(dealer, chunk, (PCM_FRAME,))
        assert rejected.type == "protocol.error"
        assert rejected.body == {
            "code": "audio_backpressure",
            "message": "audio input is temporarily backpressured",
            "retryable": True,
        }
        assert PCM_FRAME.hex() not in repr(rejected.body)

        retry = ProtocolMessage(
            type="audio.input.chunk",
            id="chunk-0-b",
            timestamp_ns=2,
            payload_count=1,
            stream_id="mic-1",
            seq=0,
            trace_id="trace-a",
            content_type=CONTENT_TYPE,
        )
        accepted = send(dealer, retry, (PCM_FRAME,))
        assert accepted.type == "audio.input.accepted"
        assert accepted.seq == 0
        assert ingress.accepted == [
            (
                PCM_FRAME,
                {
                    "principal": PrincipalContext("user:voice"),
                    "conversation_id": "conversation-a",
                    "stream_id": "mic-1",
                    "trace_id": "trace-a",
                    "seq": 0,
                },
            )
        ]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
