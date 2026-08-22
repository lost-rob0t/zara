from __future__ import annotations

import concurrent.futures
import queue
import time

import pytest
import zmq
import zmq.asyncio

from zara.client import ZaraClientState
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge, events
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import (
    ClientNotReady,
    TransportConfig,
    ZaraZmqGateway,
    ZmqZaraClient,
    apply_socket_options,
)


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        if isinstance(command, SubmitTurn):
            future.set_result(CommandReceipt(request_id=command.request_id, turn_id="turn-canonical"))
        elif isinstance(command, CancelTurn):
            future.set_result(CommandReceipt(request_id=command.request_id, turn_id=command.turn_id))
        else:
            future.set_exception(AssertionError(f"unexpected command: {command!r}"))
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)


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


def unique_endpoint(prefix: str) -> str:
    return f"inproc://{prefix}-{time.time_ns()}"


def test_socket_options_are_finite_and_router_mandatory_is_server_only(zmq_context, transport_config):
    router = zmq_context.socket(zmq.ROUTER)
    dealer = zmq_context.socket(zmq.DEALER)
    try:
        apply_socket_options(router, transport_config, router=True)
        apply_socket_options(dealer, transport_config, router=False)

        assert router.getsockopt(zmq.SNDHWM) == 8
        assert router.getsockopt(zmq.RCVHWM) == 8
        assert router.getsockopt(zmq.MAXMSGSIZE) == 1024 * 1024
        assert router.getsockopt(zmq.ROUTER_MANDATORY) == 1
        assert router.getsockopt(zmq.LINGER) == 0

        assert dealer.getsockopt(zmq.SNDHWM) == 8
        assert dealer.getsockopt(zmq.RCVHWM) == 8
        assert dealer.getsockopt(zmq.MAXMSGSIZE) == 1024 * 1024
        assert dealer.getsockopt(zmq.LINGER) == 0
    finally:
        router.close(0)
        dealer.close(0)


def test_client_requires_handshake_before_submit(zmq_context, transport_config):
    client = ZmqZaraClient(
        unique_endpoint("not-bound"),
        context=zmq_context,
        config=transport_config,
    )
    with pytest.raises(ClientNotReady):
        client.submit(SubmitTurn(text="no handshake"))
    assert client.state is ZaraClientState.NEW
    client.close(timeout=0.1)
    assert client.state is ZaraClientState.STOPPED


def test_gateway_rejects_command_before_hello(zmq_context, transport_config):
    endpoint = unique_endpoint("handshake-required")
    supervisor = FakeSupervisor()
    principal = PrincipalContext("local-test")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(endpoint)
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="ping",
                id="ping-before-hello",
                timestamp_ns=1,
                payload_count=0,
            )
        )
    )
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(1000)).get(dealer) == zmq.POLLIN
    decoded = decode_message(dealer.recv_multipart()).message
    assert decoded.type == "protocol.error"
    assert decoded.reply_to == "ping-before-hello"
    assert decoded.body == {
        "code": "handshake_required",
        "message": "handshake required",
        "retryable": True,
    }
    assert supervisor.commands == []

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_client_handshake_ping_conversation_submit_cancel_and_runtime_event(
    zmq_context,
    transport_config,
):
    endpoint = unique_endpoint("roundtrip")
    supervisor = FakeSupervisor()
    principal = PrincipalContext("local-owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    client = ZmqZaraClient(
        endpoint,
        context=zmq_context,
        config=transport_config,
    )
    subscription = client.subscribe(maxsize=8)
    client.start().result(timeout=1.0)
    assert client.state is ZaraClientState.READY
    assert client.session_id

    pong = client.ping().result(timeout=1.0)
    assert pong.type == "pong"

    conversation_id = client.open_conversation("conversation-a").result(timeout=1.0)
    assert conversation_id == "conversation-a"

    receipt = client.submit(
        SubmitTurn(
            request_id="request-submit",
            text="hello",
            conversation_id=conversation_id,
            context_ids=("ctx-a",),
        )
    ).result(timeout=1.0)
    assert receipt == CommandReceipt(request_id="request-submit", turn_id="turn-canonical")
    submitted = supervisor.commands[-1]
    assert submitted[0] == principal
    assert submitted[1] == SubmitTurn(
        request_id="request-submit",
        text="hello",
        conversation_id="conversation-a",
        context_ids=("ctx-a",),
    )

    cancel_receipt = client.submit(
        CancelTurn(request_id="request-cancel", turn_id="turn-canonical")
    ).result(timeout=1.0)
    assert cancel_receipt == CommandReceipt(request_id="request-cancel", turn_id="turn-canonical")
    assert supervisor.commands[-1][1] == CancelTurn(
        request_id="request-cancel",
        turn_id="turn-canonical",
    )

    supervisor.bus.publish(
        events.ResponseText(
            turn_id="turn-canonical",
            conversation_id="conversation-a",
            text="world",
            truncated=False,
        )
    )
    envelope = subscription.get(timeout=1.0)
    assert envelope.event == events.ResponseText(
        turn_id="turn-canonical",
        conversation_id="conversation-a",
        text="world",
        truncated=False,
    )

    client.close(timeout=1.0)
    gateway.close(timeout=1.0)
    assert client.state is ZaraClientState.STOPPED


def test_two_routes_do_not_receive_each_others_runtime_events(zmq_context, transport_config):
    endpoint = unique_endpoint("route-isolation")
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    first = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    second = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    first_events = first.subscribe(maxsize=8)
    second_events = second.subscribe(maxsize=8)
    first.start().result(timeout=1.0)
    second.start().result(timeout=1.0)

    first.open_conversation("conversation-first").result(timeout=1.0)
    second.open_conversation("conversation-second").result(timeout=1.0)

    supervisor.bus.publish(
        events.ResponseText(
            turn_id="turn-first",
            conversation_id="conversation-first",
            text="only first",
            truncated=False,
        )
    )
    assert first_events.get(timeout=1.0).event.text == "only first"
    with pytest.raises(queue.Empty):
        second_events.get(timeout=0.1)

    first.close(timeout=1.0)
    second.close(timeout=1.0)
    gateway.close(timeout=1.0)
