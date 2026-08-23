from __future__ import annotations

import concurrent.futures
import socket as net_socket

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.security import Capability, SecurityLimits, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import (
    CurveClientConfig,
    CurveServerConfig,
    configure_curve_client_socket,
)
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, apply_socket_options


PCM_FRAME = b"\x00\x00" * 512
CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
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
        poll_interval_ms=5,
        event_queue_size=8,
        pending_request_limit=8,
    )


def keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    return f"tcp://127.0.0.1:{port}"


def receive(socket: zmq.Socket) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(1500)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def send(socket: zmq.Socket, message: ProtocolMessage, payloads=()) -> ProtocolMessage:
    socket.send_multipart(encode_message(message, payloads=payloads))
    return receive(socket)


def secure_dealer(
    context: zmq.Context,
    endpoint: str,
    transport_config: TransportConfig,
    *,
    client_public: str,
    client_secret: str,
    server_public: str,
) -> zmq.Socket:
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
    )
    dealer.connect(endpoint)
    return dealer


def hello(socket: zmq.Socket) -> ProtocolMessage:
    response = send(
        socket,
        ProtocolMessage(
            type="hello",
            id="hello-secure-voice",
            timestamp_ns=1,
            payload_count=0,
            body={"versions": [1]},
        ),
    )
    assert response.type == "hello.ok"
    return response


def start_message(request_id: str = "voice-start") -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.input.start",
        id=request_id,
        timestamp_ns=2,
        payload_count=0,
        stream_id="mic-1",
        trace_id="trace-secure-voice",
        body={
            "codec": "pcm_s16le",
            "sample_rate": 16000,
            "channels": 1,
            "frame_samples": 512,
        },
    )


def chunk_message() -> ProtocolMessage:
    return ProtocolMessage(
        type="audio.input.chunk",
        id="voice-chunk-0",
        timestamp_ns=3,
        payload_count=1,
        stream_id="mic-1",
        seq=0,
        trace_id="trace-secure-voice",
        content_type=CONTENT_TYPE,
    )


def terminal_message(message_type: str, request_id: str) -> ProtocolMessage:
    return ProtocolMessage(
        type=message_type,
        id=request_id,
        timestamp_ns=4,
        payload_count=0,
        stream_id="mic-1",
        trace_id="trace-secure-voice",
    )


def make_gateway(
    context: zmq.Context,
    endpoint: str,
    transport_config: TransportConfig,
    *,
    registry: SecurityRegistry,
    server_public: str,
    server_secret: str,
    voice_ingress: RecordingVoiceIngress,
) -> SecureZaraZmqGateway:
    return SecureZaraZmqGateway(
        endpoint,
        supervisor=FakeSupervisor(),
        context=context,
        config=transport_config,
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        security_limits=SecurityLimits(),
        voice_ingress=voice_ingress,
    )


@pytest.mark.parametrize(
    ("message_type", "capability"),
    [
        ("audio.input.start", Capability.TURN_SUBMIT),
        ("audio.input.chunk", Capability.TURN_SUBMIT),
        ("audio.input.commit", Capability.TURN_SUBMIT),
        ("audio.input.cancel", Capability.TURN_CANCEL),
    ],
)
def test_secure_voice_messages_reuse_existing_turn_capabilities(message_type, capability):
    assert SecureZaraZmqGateway._capability_for(message_type) is capability


def test_authorized_secure_voice_ingress_uses_authenticated_principal(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=principal,
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    ingress = RecordingVoiceIngress()
    gateway = make_gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello(dealer)
        opened = send(
            dealer,
            ProtocolMessage(
                type="conversation.open",
                id="voice-open",
                timestamp_ns=2,
                payload_count=0,
                conversation_id="conversation-secure",
            ),
        )
        assert opened.type == "conversation.opened"
        assert send(dealer, start_message()).type == "audio.input.started"
        assert send(dealer, chunk_message(), (PCM_FRAME,)).type == "audio.input.accepted"
        assert send(
            dealer,
            terminal_message("audio.input.commit", "voice-commit"),
        ).type == "audio.input.committed"

        common = {
            "principal": principal,
            "conversation_id": "conversation-secure",
            "stream_id": "mic-1",
            "trace_id": "trace-secure-voice",
        }
        assert ingress.calls == [
            ("start", common),
            ("chunk", PCM_FRAME, {**common, "seq": 0}),
            ("commit", common),
        ]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_missing_submit_capability_rejects_voice_before_ingress(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:limited", kind="authenticated"),
        device_id="limited-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    ingress = RecordingVoiceIngress()
    gateway = make_gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello(dealer)
        denied = send(dealer, start_message())
        assert denied.type == "protocol.error"
        assert denied.body == {
            "code": "authorization_denied",
            "message": "request is not authorized",
            "retryable": False,
        }
        assert ingress.calls == []
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)


def test_voice_cancel_requires_turn_cancel_capability_before_ingress(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    client_public, client_secret = keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:no-cancel", kind="authenticated"),
        device_id="no-cancel-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    ingress = RecordingVoiceIngress()
    gateway = make_gateway(
        zmq_context,
        endpoint,
        transport_config,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        voice_ingress=ingress,
    )
    gateway.start().result(timeout=1.0)
    dealer = secure_dealer(
        zmq_context,
        endpoint,
        transport_config,
        client_public=client_public,
        client_secret=client_secret,
        server_public=server_public,
    )
    try:
        hello(dealer)
        assert send(dealer, start_message()).type == "audio.input.started"
        denied = send(
            dealer,
            terminal_message("audio.input.cancel", "voice-cancel"),
        )
        assert denied.type == "protocol.error"
        assert denied.body["code"] == "authorization_denied"
        assert [call[0] for call in ingress.calls] == ["start"]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
