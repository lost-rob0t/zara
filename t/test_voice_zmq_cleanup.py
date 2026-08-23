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
    return f"inproc://voice-cleanup-{name}-{time.time_ns()}"


def receive(socket: zmq.Socket) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(1000)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage, payloads=()) -> ProtocolMessage:
    socket.send_multipart(encode_message(message, payloads=payloads))
    return receive(socket)


def hello(socket: zmq.Socket, request_id: str = "hello") -> None:
    response = send(
        socket,
        ProtocolMessage(
            type="hello",
            id=request_id,
            timestamp_ns=1,
            payload_count=0,
            body={"versions": [1]},
        ),
    )
    assert response.type == "hello.ok"


def open_conversation(socket: zmq.Socket, conversation_id: str, request_id: str) -> None:
    response = send(
        socket,
        ProtocolMessage(
            type="conversation.open",
            id=request_id,
            timestamp_ns=1,
            payload_count=0,
            conversation_id=conversation_id,
        ),
    )
    assert response.type == "conversation.opened"


def start_message(*, request_id="start", stream_id="mic-1", trace_id="trace-a"):
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


def chunk_message(*, request_id="chunk", stream_id="mic-1", seq=0, trace_id="trace-a"):
    return ProtocolMessage(
        type="audio.input.chunk",
        id=request_id,
        timestamp_ns=1,
        payload_count=1,
        stream_id=stream_id,
        seq=seq,
        trace_id=trace_id,
        content_type=CONTENT_TYPE,
    )


def make_gateway(zmq_context, transport_config, name, ingress, *, identity=None):
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
    if identity is not None:
        dealer.setsockopt(zmq.IDENTITY, identity)
    dealer.connect(address)
    hello(dealer)
    return gateway, dealer


def test_stream_correlation_is_bound_at_start(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    gateway, dealer = make_gateway(zmq_context, transport_config, "correlation", ingress)
    try:
        open_conversation(dealer, "conversation-a", "open-a")
        assert send(dealer, start_message(trace_id="trace-a")).type == "audio.input.started"

        open_conversation(dealer, "conversation-b", "open-b")
        accepted = send(
            dealer,
            chunk_message(trace_id="trace-b"),
            (PCM_FRAME,),
        )
        assert accepted.type == "audio.input.accepted"

        common = {
            "principal": PrincipalContext("user:voice"),
            "conversation_id": "conversation-a",
            "stream_id": "mic-1",
            "trace_id": "trace-a",
        }
        assert ingress.calls == [
            ("start", common),
            ("chunk", PCM_FRAME, {**common, "seq": 0}),
        ]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_route_drop_cancels_open_stream_with_bound_context(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    identity = b"voice-route-cleanup"
    gateway, dealer = make_gateway(
        zmq_context,
        transport_config,
        "route-drop",
        ingress,
        identity=identity,
    )
    try:
        open_conversation(dealer, "conversation-a", "open")
        assert send(dealer, start_message(trace_id="trace-a")).type == "audio.input.started"

        gateway._drop_route(identity)

        assert ingress.calls[-1] == (
            "cancel",
            {
                "principal": PrincipalContext("user:voice"),
                "conversation_id": "conversation-a",
                "stream_id": "mic-1",
                "trace_id": "trace-a",
            },
        )
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_rehello_cancels_open_stream_before_replacing_session(zmq_context, transport_config):
    ingress = RecordingVoiceIngress()
    gateway, dealer = make_gateway(zmq_context, transport_config, "rehello", ingress)
    try:
        open_conversation(dealer, "conversation-a", "open")
        assert send(dealer, start_message(trace_id="trace-a")).type == "audio.input.started"

        hello(dealer, request_id="hello-again")

        assert ingress.calls[-1] == (
            "cancel",
            {
                "principal": PrincipalContext("user:voice"),
                "conversation_id": "conversation-a",
                "stream_id": "mic-1",
                "trace_id": "trace-a",
            },
        )
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
