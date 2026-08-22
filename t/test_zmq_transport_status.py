from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class StatusSupervisor:
    def __init__(self, state: ServerState) -> None:
        self.state = state
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        future.set_exception(AssertionError("runtime.status must not dispatch a runtime command"))
        return future


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
        event_queue_size=8,
        pending_request_limit=8,
    )


def _receive(socket: zmq.Socket, timeout_ms: int = 1000) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def test_runtime_status_reports_supervisor_state_without_dispatch(zmq_context):
    endpoint = f"inproc://runtime-status-{time.time_ns()}"
    config = _config()
    supervisor = StatusSupervisor(ServerState.DEGRADED)
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=config,
    )
    gateway.start().result(timeout=1.0)
    client = zmq_context.socket(zmq.DEALER)
    apply_socket_options(client, config, router=False)
    client.connect(endpoint)
    try:
        client.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-status",
                    timestamp_ns=time.time_ns(),
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = _receive(client)
        assert hello.type == "hello.ok"
        assert hello.session_id

        client.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="runtime.status",
                    id="status-1",
                    session_id=hello.session_id,
                    timestamp_ns=time.time_ns(),
                    payload_count=0,
                )
            )
        )
        response = _receive(client)

        assert response.type == "runtime.status.ok"
        assert response.reply_to == "status-1"
        assert response.session_id == hello.session_id
        assert response.body == {"state": "degraded"}
    finally:
        client.close(0)
        gateway.close(timeout=1.0)
