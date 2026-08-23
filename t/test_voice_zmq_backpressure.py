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


class FailingVoiceIngress:
    def __init__(self, operation: str) -> None:
        self.operation = operation

    def start(self, **_kwargs):
        if self.operation == "start":
            raise RuntimeError("ingress unavailable")

    def chunk(self, _payload: bytes, **_kwargs):
        if self.operation == "chunk":
            raise RuntimeError("ingress unavailable")

    def commit(self, **_kwargs):
        if self.operation == "commit":
            raise RuntimeError("ingress unavailable")

    def cancel(self, **_kwargs):
        if self.operation == "cancel":
            raise RuntimeError("ingress unavailable")


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


def start_message(request_id: str = "start") -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.input.start",
        id=request_id,
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
    )


def chunk_message(request_id: str = "chunk-0", seq: int = 0) -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.input.chunk",
        id=request_id,
        timestamp_ns=1,
        payload_count=1,
        stream_id="mic-1",
        seq=seq,
        trace_id="trace-a",
        content_type=CONTENT_TYPE,
    )


def terminal_message(message_type: str, request_id: str) -> ProtocolMessage:
    return ProtocolMessage(
        type=message_type,
        id=request_id,
        timestamp_ns=1,
        payload_count=0,
        stream_id="mic-1",
        trace_id="trace-a",
    )


def connected_gateway(zmq_context, ingress):
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
    return gateway, dealer


def test_ingress_queue_full_is_explicit_and_retry_does_not_skip_sequence(zmq_context):
    ingress = BackpressuredVoiceIngress()
    gateway, dealer = connected_gateway(zmq_context, ingress)
    try:
        started = send(dealer, start_message())
        assert started.type == "audio.input.started"

        rejected = send(dealer, chunk_message("chunk-0-a"), (PCM_FRAME,))
        assert rejected.type == "protocol.error"
        assert rejected.body == {
            "code": "audio_backpressure",
            "message": "audio input is temporarily backpressured",
            "retryable": True,
        }
        assert PCM_FRAME.hex() not in repr(rejected.body)

        accepted = send(dealer, chunk_message("chunk-0-b"), (PCM_FRAME,))
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


def test_ingress_start_failure_is_explicit_and_gateway_survives(zmq_context):
    gateway, dealer = connected_gateway(zmq_context, FailingVoiceIngress("start"))
    try:
        response = send(dealer, start_message("start-fails"))
        assert response.type == "protocol.error"
        assert response.body == {
            "code": "audio_ingress_error",
            "message": "audio input runtime is unavailable",
            "retryable": True,
        }
        assert gateway.is_alive
        assert send(
            dealer,
            ProtocolMessage(
                type="ping",
                id="ping-after-start-failure",
                timestamp_ns=2,
                payload_count=0,
            ),
        ).type == "pong"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_ingress_chunk_failure_is_explicit_does_not_advance_sequence_and_gateway_survives(zmq_context):
    ingress = FailingVoiceIngress("chunk")
    gateway, dealer = connected_gateway(zmq_context, ingress)
    try:
        assert send(dealer, start_message()).type == "audio.input.started"
        response = send(dealer, chunk_message("chunk-fails"), (PCM_FRAME,))
        assert response.type == "protocol.error"
        assert response.body == {
            "code": "audio_ingress_error",
            "message": "audio input runtime is unavailable",
            "retryable": True,
        }
        assert gateway.is_alive

        ingress.operation = "none"
        retry = send(dealer, chunk_message("chunk-retry"), (PCM_FRAME,))
        assert retry.type == "audio.input.accepted"
        assert retry.seq == 0
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


@pytest.mark.parametrize(
    ("operation", "message_type", "success_type"),
    [
        ("commit", "audio.input.commit", "audio.input.committed"),
        ("cancel", "audio.input.cancel", "audio.input.cancelled"),
    ],
)
def test_terminal_ingress_failure_is_retryable_and_does_not_close_stream(
    zmq_context,
    operation,
    message_type,
    success_type,
):
    ingress = FailingVoiceIngress(operation)
    gateway, dealer = connected_gateway(zmq_context, ingress)
    try:
        assert send(dealer, start_message()).type == "audio.input.started"

        failed = send(dealer, terminal_message(message_type, f"{operation}-fails"))
        assert failed.type == "protocol.error"
        assert failed.body == {
            "code": "audio_ingress_error",
            "message": "audio input runtime is unavailable",
            "retryable": True,
        }
        assert gateway.is_alive

        ingress.operation = "none"
        retry = send(dealer, terminal_message(message_type, f"{operation}-retry"))
        assert retry.type == success_type
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
