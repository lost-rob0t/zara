from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class ImmediateSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(request_id=command.request_id, turn_id=f"turn-{command.request_id}")
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)


class StalledCompletionGateway(ZaraZmqGateway):
    def _drain_outbound(self, socket):
        return None


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def _config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        event_queue_size=2,
        pending_request_limit=8,
    )


def _dealer(context: zmq.Context, endpoint: str, config: TransportConfig, identity: bytes):
    socket = context.socket(zmq.DEALER)
    socket.setsockopt(zmq.IDENTITY, identity)
    apply_socket_options(socket, config, router=False)
    socket.connect(endpoint)
    return socket


def _receive(socket: zmq.Socket, timeout_ms: int = 1000) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def _hello(socket: zmq.Socket, request_id: str) -> None:
    socket.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id=request_id,
                timestamp_ns=time.time_ns(),
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    response = _receive(socket)
    assert response.type == "hello.ok"
    assert response.reply_to == request_id


def _submit(socket: zmq.Socket, request_id: str) -> None:
    socket.send_multipart(
        encode_message(
            ProtocolMessage(
                type="turn.submit",
                id=request_id,
                timestamp_ns=time.time_ns(),
                payload_count=0,
                body={"text": request_id},
            )
        )
    )


def test_stalled_route_completion_queue_cannot_evict_healthy_route(zmq_context):
    config = _config()
    endpoint = f"inproc://route-outbound-isolation-{time.time_ns()}"
    supervisor = ImmediateSupervisor()
    gateway = StalledCompletionGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=config,
    )
    gateway.start().result(timeout=1.0)

    stalled = _dealer(zmq_context, endpoint, config, b"stalled-route")
    healthy = _dealer(zmq_context, endpoint, config, b"healthy-route")
    try:
        _hello(stalled, "hello-stalled")
        _hello(healthy, "hello-healthy")

        _submit(stalled, "stalled-1")
        _submit(stalled, "stalled-2")
        _submit(healthy, "healthy-submit")

        deadline = time.monotonic() + 1.0
        while len(supervisor.commands) < 3 and time.monotonic() < deadline:
            time.sleep(0.005)
        submitted = [
            command.request_id
            for _, command in supervisor.commands
            if isinstance(command, SubmitTurn)
        ]
        assert submitted == ["stalled-1", "stalled-2", "healthy-submit"]

        healthy.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="ping",
                    id="healthy-ping",
                    timestamp_ns=time.time_ns(),
                    payload_count=0,
                )
            )
        )
        response = _receive(healthy)
        assert response.type == "pong"
        assert response.reply_to == "healthy-ping"
    finally:
        stalled.close(0)
        healthy.close(0)
        gateway.close(timeout=1.0)
