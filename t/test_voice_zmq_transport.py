from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


PCM_FRAME = b"\x00\x00" * 512
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
    return f"inproc://voice-{name}-{time.time_ns()}"


def receive(socket: zmq.Socket) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(1000)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage, payloads=()) -> ProtocolMessage:
    socket.send_multipart(encode_message(message, payloads=payloads))
    return receive(socket)


def hello(socket: zmq.Socket) -> None:
    response = send(
        socket,
        ProtocolMessage(type="hello", id="hello", timestamp_ns=1, payload_count=0, body={"versions": [1]}),
    )
    assert response.type == "hello.ok"


def voice(message_type: str, *, request_id: str, stream_id: str = "mic-1", seq=None, payload_count=0, content_type=None, body=None):
    return ProtocolMessage(
        type=message_type,
        id=request_id,
        timestamp_ns=1,
        payload_count=payload_count,
        stream_id=stream_id,
        seq=seq,
        content_type=content_type,
        body=body,
    )


def start_message(request_id="start", stream_id="mic-1"):
    return voice(
        "audio.input.start",
        request_id=request_id,
        stream_id=stream_id,
        body={"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1, "frame_samples": 512},
    )


def chunk_message(seq: int, request_id=None, stream_id="mic-1"):
    return voice(
        "audio.input.chunk",
        request_id=request_id or f"chunk-{seq}",
        stream_id=stream_id,
        seq=seq,
        payload_count=1,
        content_type=CONTENT_TYPE,
    )


def make_gateway(zmq_context, transport_config, name):
    address = endpoint(name)
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    dealer = zmq_context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(address)
    hello(dealer)
    return gateway, dealer


def test_gateway_accepts_start_contiguous_chunks_and_commit(zmq_context, transport_config):
    gateway, dealer = make_gateway(zmq_context, transport_config, "happy")
    try:
        assert send(dealer, start_message()).type == "audio.input.started"
        assert send(dealer, chunk_message(0), (PCM_FRAME,)).type == "audio.input.accepted"
        assert send(dealer, chunk_message(1), (PCM_FRAME,)).type == "audio.input.accepted"
        committed = send(dealer, voice("audio.input.commit", request_id="commit"))
        assert committed.type == "audio.input.committed"
        assert committed.stream_id == "mic-1"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_gateway_rejects_chunk_without_start_and_sequence_gap(zmq_context, transport_config):
    gateway, dealer = make_gateway(zmq_context, transport_config, "sequence")
    try:
        missing = send(dealer, chunk_message(0), (PCM_FRAME,))
        assert missing.type == "protocol.error"
        assert missing.body["code"] == "audio_stream_not_open"

        assert send(dealer, start_message()).type == "audio.input.started"
        gap = send(dealer, chunk_message(1), (PCM_FRAME,))
        assert gap.type == "protocol.error"
        assert gap.body["code"] == "audio_sequence_error"
        assert send(dealer, chunk_message(0), (PCM_FRAME,)).type == "audio.input.accepted"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_gateway_rejects_duplicate_start_and_late_chunk_after_commit(zmq_context, transport_config):
    gateway, dealer = make_gateway(zmq_context, transport_config, "terminal")
    try:
        assert send(dealer, start_message()).type == "audio.input.started"
        duplicate = send(dealer, start_message(request_id="start-again"))
        assert duplicate.type == "protocol.error"
        assert duplicate.body["code"] == "audio_stream_already_open"
        assert send(dealer, voice("audio.input.commit", request_id="commit")).type == "audio.input.committed"
        late = send(dealer, chunk_message(0, request_id="late"), (PCM_FRAME,))
        assert late.type == "protocol.error"
        assert late.body["code"] == "audio_stream_not_open"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_audio_stream_ids_are_route_scoped(zmq_context, transport_config):
    address = endpoint("routes")
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    first = zmq_context.socket(zmq.DEALER)
    second = zmq_context.socket(zmq.DEALER)
    for dealer in (first, second):
        apply_socket_options(dealer, transport_config, router=False)
        dealer.connect(address)
        hello(dealer)
    try:
        assert send(first, start_message(request_id="start-a")).type == "audio.input.started"
        assert send(second, start_message(request_id="start-b")).type == "audio.input.started"
        assert send(first, chunk_message(0, request_id="a0"), (PCM_FRAME,)).type == "audio.input.accepted"
        assert send(second, chunk_message(0, request_id="b0"), (PCM_FRAME,)).type == "audio.input.accepted"
    finally:
        first.close(0)
        second.close(0)
        gateway.close(timeout=1.0)
