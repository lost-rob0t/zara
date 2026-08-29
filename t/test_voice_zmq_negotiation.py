from __future__ import annotations

import concurrent.futures
import threading
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import (
    ProtocolRemoteError,
    TransportConfig,
    ZaraZmqGateway,
    ZmqZaraClient,
    apply_socket_options,
)


OUTPUT_FORMAT = {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1}
OTHER_FORMAT = {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1}


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


def config() -> TransportConfig:
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


def endpoint(name: str) -> str:
    return f"inproc://voice-negotiation-{name}-{time.time_ns()}"


def receive(socket: zmq.Socket) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(1000)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def test_gateway_selects_declared_output_pcm_format_in_hello_ok():
    context = zmq.Context()
    address = endpoint("gateway")
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=context,
        config=config(),
        audio_output_format=OUTPUT_FORMAT,
    )
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config(), router=False)
    dealer.connect(address)
    try:
        gateway.start().result(timeout=1.0)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello",
                    timestamp_ns=1,
                    payload_count=0,
                    body={
                        "versions": [1],
                        "audio_output_formats": [OTHER_FORMAT, OUTPUT_FORMAT],
                    },
                )
            )
        )
        response = receive(dealer)
        assert response.type == "hello.ok"
        assert response.body["audio_output_format"] == OUTPUT_FORMAT
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.term()


def test_gateway_rejects_hello_without_compatible_output_pcm_format():
    context = zmq.Context()
    address = endpoint("incompatible")
    gateway = ZaraZmqGateway(
        address,
        supervisor=FakeSupervisor(),
        principal=PrincipalContext("user:voice"),
        context=context,
        config=config(),
        audio_output_format=OUTPUT_FORMAT,
    )
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config(), router=False)
    dealer.connect(address)
    try:
        gateway.start().result(timeout=1.0)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1], "audio_output_formats": [OTHER_FORMAT]},
                )
            )
        )
        response = receive(dealer)
        assert response.type == "protocol.error"
        assert response.body == {
            "code": "unsupported_audio_output",
            "message": "no compatible audio output format",
            "retryable": False,
        }
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
        context.term()


def run_handshake_server(context, address, ready, observed):
    socket = context.socket(zmq.ROUTER)
    socket.setsockopt(zmq.LINGER, 0)
    socket.bind(address)
    ready.set()
    try:
        frames = socket.recv_multipart()
        route, app_frames = frames[0], frames[1:]
        hello = decode_message(app_frames).message
        observed.append(hello)
        socket.send_multipart(
            [
                route,
                *encode_message(
                    ProtocolMessage(
                        type="hello.ok",
                        id="hello-ok",
                        reply_to=hello.id,
                        session_id="session-1",
                        timestamp_ns=2,
                        payload_count=0,
                        body={
                            "version": 1,
                            "max_payload_frames": 16,
                            "max_payload_frame_bytes": 1024 * 1024,
                            "max_payload_bytes": 4 * 1024 * 1024,
                            "audio_output_format": OUTPUT_FORMAT,
                        },
                    )
                ),
            ]
        )
    finally:
        socket.close(0)


def test_zara_client_advertises_and_records_negotiated_output_format():
    context = zmq.Context()
    address = endpoint("client")
    ready = threading.Event()
    observed = []
    server = threading.Thread(
        target=run_handshake_server,
        args=(context, address, ready, observed),
        daemon=True,
    )
    server.start()
    assert ready.wait(1.0)
    client = ZmqZaraClient(
        address,
        context=context,
        config=config(),
        audio_output_formats=[OUTPUT_FORMAT],
    )
    try:
        client.start().result(timeout=1.0)
        assert observed[0].body["audio_output_formats"] == [OUTPUT_FORMAT]
        assert client.audio_output_format == OUTPUT_FORMAT
    finally:
        client.close(timeout=1.0)
        server.join(timeout=1.0)
        context.term()
