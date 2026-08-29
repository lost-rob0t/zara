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
    return f"inproc://voice-cancel-{name}-{time.time_ns()}"


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
        ProtocolMessage(
            type="hello",
            id="hello",
            timestamp_ns=1,
            payload_count=0,
            body={"versions": [1]},
        ),
    )
    assert response.type == "hello.ok"


def voice(message_type: str, *, request_id: str, stream_id: str = "mic-1", seq=None, payload_count=0, content_type=None, trace_id=None):
    return ProtocolMessage(
        type=message_type,
        id=request_id,
        timestamp_ns=1,
        payload_count=payload_count,
        stream_id=stream_id,
        seq=seq,
        content_type=content_type,
        trace_id=trace_id,
    )


def start_message(*, request_id="start", stream_id="mic-1", trace_id="trace-cancel"):
    return ProtocolMessage(
        type="audio.input.start",
        id=request_id,
        timestamp_ns=1,
        payload_count=0,
        stream_id=stream_id,
        trace_id=trace_id,
        body={
            "codec": "pcm_s16le",
            "sample_rate": 16000,
            "channels": 1,
            "frame_samples": 512,
        },
    )


def chunk_message(*, request_id="chunk", stream_id="mic-1", seq=0, trace_id="trace-cancel"):
    return voice(
        "audio.input.chunk",
        request_id=request_id,
        stream_id=stream_id,
        seq=seq,
        payload_count=1,
        content_type=CONTENT_TYPE,
        trace_id=trace_id,
    )


def make_gateway(zmq_context, transport_config, name, ingress):
    address = endpoint(name)
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=zmq_context,
        config=transport_config,
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    dealer = zmq_context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(address)
    hello(dealer)
    return gateway, dealer


def test_cancel_is_terminal_and_notifies_ingress(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    gateway, dealer = make_gateway(zmq_context, transport_config, "terminal", ingress)
    try:
        opened = send(
            dealer,
            ProtocolMessage(
                type="conversation.open",
                id="open",
                timestamp_ns=1,
                payload_count=0,
                conversation_id="conversation-cancel",
            ),
        )
        assert opened.type == "conversation.opened"
        assert send(dealer, start_message()).type == "audio.input.started"
        assert send(dealer, chunk_message(), (PCM_FRAME,)).type == "audio.input.accepted"

        cancelled = send(
            dealer,
            voice(
                "audio.input.cancel",
                request_id="cancel",
                trace_id="trace-cancel",
            ),
        )
        assert cancelled.type == "audio.input.cancelled"
        assert cancelled.stream_id == "mic-1"

        common = {
            "principal": PrincipalContext("user:voice"),
            "conversation_id": "conversation-cancel",
            "stream_id": "mic-1",
            "trace_id": "trace-cancel",
        }
        assert ingress.calls == [
            ("start", common),
            ("chunk", PCM_FRAME, {**common, "seq": 0}),
            ("cancel", common),
        ]

        late = send(
            dealer,
            chunk_message(request_id="late", seq=1),
            (PCM_FRAME,),
        )
        assert late.type == "protocol.error"
        assert late.body["code"] == "audio_stream_not_open"
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_cancel_without_open_stream_is_indistinguishable_from_other_missing_terminal_state(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    gateway, dealer = make_gateway(zmq_context, transport_config, "missing", ingress)
    try:
        response = send(
            dealer,
            voice("audio.input.cancel", request_id="cancel-missing"),
        )
        assert response.type == "protocol.error"
        assert response.body["code"] == "audio_stream_not_open"
        assert ingress.calls == []
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_cancelling_same_stream_id_on_one_route_does_not_close_other_route(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    address = endpoint("routes")
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=zmq_context,
        config=transport_config,
        voice_ingress=ingress,
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
        assert send(first, voice("audio.input.cancel", request_id="cancel-a")).type == "audio.input.cancelled"
        assert send(second, chunk_message(request_id="b0"), (PCM_FRAME,)).type == "audio.input.accepted"
    finally:
        first.close(0)
        second.close(0)
        gateway.close(timeout=1.0)
