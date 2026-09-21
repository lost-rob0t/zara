from __future__ import annotations

import concurrent.futures
import time

import pytest
import zmq

from zara.runtime import bridge, events
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.server import ServerState
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server import PrincipalContext
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, apply_socket_options


class PublishBeforeAcceptSupervisor:
    """Supervisor whose runtime publishes TurnStarted before the submit future resolves.

    Mirrors RuntimeHost._submit_turn: the TurnStarted event is published while the
    turn is merely accepted, before the transport's completion callback can send
    the turn.accepted reply.
    """

    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        if isinstance(command, SubmitTurn):
            self.bus.publish(
                events.TurnStarted(
                    turn_id="turn-canonical",
                    conversation_id=command.conversation_id,
                    label="runtime-host",
                )
            )
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(
                request_id=command.request_id,
                turn_id="turn-canonical",
                detail="turn accepted",
            )
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


def unique_endpoint(prefix: str) -> str:
    return f"inproc://{prefix}-{time.time_ns()}"


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


def receive_message(socket: zmq.Socket, *, timeout_ms: int = 1500):
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN, "no frame arrived"
    return decode_message(socket.recv_multipart()).message


def test_turn_events_arrive_after_turn_accepted_and_are_never_dropped(
    zmq_context,
    transport_config,
):
    endpoint = unique_endpoint("accept-ordering")
    supervisor = PublishBeforeAcceptSupervisor()
    principal = PrincipalContext("local-owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
    try:
        apply_socket_options(dealer, transport_config, router=False)
        dealer.connect(endpoint)
        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="hello",
                    id="hello-1",
                    timestamp_ns=1,
                    payload_count=0,
                    body={"versions": [1]},
                )
            )
        )
        hello = receive_message(dealer)
        assert hello.type == "hello.ok"
        session_id = hello.session_id

        dealer.send_multipart(
            encode_message(
                ProtocolMessage(
                    type="turn.submit",
                    id="submit-1",
                    session_id=session_id,
                    timestamp_ns=2,
                    payload_count=0,
                    body={"text": "hello"},
                )
            )
        )

        first = receive_message(dealer)
        assert first.type == "turn.accepted", (
            f"expected accepted first, got {first.type}"
        )

        second = receive_message(dealer)
        assert second.type == "turn.started", (
            f"runtime event must follow accepted, got {second.type} (dropped?)"
        )
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
