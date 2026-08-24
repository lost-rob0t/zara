from __future__ import annotations

import concurrent.futures
import queue
import time

import pytest
import zmq
import zmq.asyncio

from zara import zmq_transport as transport
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


class BlockingSupervisor(FakeSupervisor):
    def __init__(self) -> None:
        super().__init__()
        self.blocked = []

    def submit(self, principal, command):
        assert isinstance(principal, PrincipalContext)
        self.commands.append((principal, command))
        if isinstance(command, SubmitTurn):
            future = concurrent.futures.Future()
            self.blocked.append(future)
            return future
        return super().submit(principal, command)


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


def receive_message(socket: zmq.Socket, *, timeout_ms: int = 1000):
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(socket) == zmq.POLLIN
    return decode_message(socket.recv_multipart()).message


def test_socket_options_are_finite_and_router_mandatory_is_server_only(zmq_context, transport_config):
    router = zmq_context.socket(zmq.ROUTER)
    dealer = zmq_context.socket(zmq.DEALER)
    try:
        apply_socket_options(router, transport_config, router=True)
        apply_socket_options(dealer, transport_config, router=False)

        assert router.getsockopt(zmq.SNDHWM) == 8
        assert router.getsockopt(zmq.RCVHWM) == 8
        assert router.getsockopt(zmq.MAXMSGSIZE) == 1024 * 1024
        assert router.getsockopt(zmq.LINGER) == 0

        assert dealer.getsockopt(zmq.SNDHWM) == 8
        assert dealer.getsockopt(zmq.RCVHWM) == 8
        assert dealer.getsockopt(zmq.MAXMSGSIZE) == 1024 * 1024
        assert dealer.getsockopt(zmq.LINGER) == 0

        router.bind(unique_endpoint("mandatory-option"))
        with pytest.raises(zmq.ZMQError) as error:
            router.send_multipart([b"missing-route", b"payload"], flags=zmq.NOBLOCK)
        assert error.value.errno == zmq.EHOSTUNREACH
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
    decoded = receive_message(dealer)
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

    scheduled = events.TimerScheduled(
        timer_id="timer-shared",
        name="tea",
        created_at_ns=100,
        due_at_ns=200,
        revision=1,
    )
    supervisor.bus.publish(scheduled)
    assert first_events.get(timeout=1.0).event == scheduled
    assert second_events.get(timeout=1.0).event == scheduled

    first.close(timeout=1.0)
    fired = events.TimerFired(
        timer_id="timer-shared",
        name="tea",
        created_at_ns=100,
        due_at_ns=200,
        fired_at_ns=220,
        revision=2,
        message='Timer "tea" finished.',
    )
    supervisor.bus.publish(fired)
    assert second_events.get(timeout=1.0).event == fired

    second.close(timeout=1.0)
    gateway.close(timeout=1.0)


def test_gateway_restart_forgets_route_handshake_state(zmq_context, transport_config):
    endpoint = unique_endpoint("gateway-reset")
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
    dealer.setsockopt(zmq.IDENTITY, b"stable-route")
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(endpoint)
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id="hello-before-restart",
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    assert receive_message(dealer).type == "hello.ok"
    dealer.close(0)

    gateway.close(timeout=1.0)
    gateway.start().result(timeout=1.0)

    dealer = zmq_context.socket(zmq.DEALER)
    dealer.setsockopt(zmq.IDENTITY, b"stable-route")
    apply_socket_options(dealer, transport_config, router=False)
    dealer.connect(endpoint)
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="ping",
                id="ping-after-restart",
                timestamp_ns=2,
                payload_count=0,
            )
        )
    )
    response = receive_message(dealer)
    assert response.type == "protocol.error"
    assert response.body["code"] == "handshake_required"

    dealer.close(0)
    gateway.close(timeout=1.0)


def test_duplicate_stable_submit_id_replays_receipt_without_second_side_effect(
    zmq_context,
    transport_config,
):
    endpoint = unique_endpoint("duplicate-submit")
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    client.start().result(timeout=1.0)

    command = SubmitTurn(request_id="stable-request", text="do once")
    first = client.submit(command).result(timeout=1.0)
    second = client.submit(command).result(timeout=1.0)

    assert first == second
    submitted = [item for _, item in supervisor.commands if isinstance(item, SubmitTurn)]
    assert submitted == [command]

    client.close(timeout=1.0)
    gateway.close(timeout=1.0)


def test_pending_request_cap_fails_before_unbounded_future_growth(zmq_context):
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        pending_request_limit=2,
    )
    endpoint = unique_endpoint("pending-cap")
    supervisor = BlockingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=config,
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(endpoint, context=zmq_context, config=config)
    client.start().result(timeout=1.0)

    client.submit(SubmitTurn(request_id="pending-1", text="one"))
    client.submit(SubmitTurn(request_id="pending-2", text="two"))
    with pytest.raises(transport.ClientBackpressureError):
        client.submit(SubmitTurn(request_id="pending-3", text="three"))

    deadline = time.monotonic() + 1.0
    while len(supervisor.blocked) < 2 and time.monotonic() < deadline:
        time.sleep(0.005)
    assert len(supervisor.blocked) == 2
    submitted_ids = [
        command.request_id
        for _, command in supervisor.commands
        if isinstance(command, SubmitTurn)
    ]
    assert submitted_ids == ["pending-1", "pending-2"]

    for future in supervisor.blocked:
        future.set_result(CommandReceipt(request_id="released", turn_id="turn-released"))
    client.close(timeout=1.0)
    gateway.close(timeout=1.0)


def test_blocked_runtime_future_does_not_starve_other_client_control(zmq_context, transport_config):
    endpoint = unique_endpoint("fairness")
    supervisor = BlockingSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    blocked = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    healthy = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    blocked.start().result(timeout=1.0)
    healthy.start().result(timeout=1.0)

    blocked.submit(SubmitTurn(request_id="blocked-turn", text="wait"))
    pong = healthy.ping().result(timeout=1.0)
    assert pong.type == "pong"

    for future in supervisor.blocked:
        future.set_result(CommandReceipt(request_id="blocked-turn", turn_id="turn-blocked"))
    blocked.close(timeout=1.0)
    healthy.close(timeout=1.0)
    gateway.close(timeout=1.0)


def test_client_rehandshakes_after_gateway_restart_with_new_session(zmq_context, transport_config):
    endpoint = unique_endpoint("rehandshake")
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    client.start().result(timeout=1.0)
    first_session = client.session_id

    gateway.close(timeout=1.0)
    gateway.start().result(timeout=1.0)
    client.reconnect().result(timeout=1.0)

    assert client.state is ZaraClientState.READY
    assert client.session_id
    assert client.session_id != first_session
    assert client.ping().result(timeout=1.0).type == "pong"

    client.close(timeout=1.0)
    gateway.close(timeout=1.0)


def test_owner_threads_are_dead_after_close_and_objects_can_restart(zmq_context, transport_config):
    endpoint = unique_endpoint("restart-owners")
    supervisor = FakeSupervisor()
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-owner"),
        context=zmq_context,
        config=transport_config,
    )
    client = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)

    gateway.start().result(timeout=1.0)
    client.start().result(timeout=1.0)
    first_session = client.session_id
    client.close(timeout=1.0)
    gateway.close(timeout=1.0)
    assert not client.is_alive
    assert not gateway.is_alive

    gateway.start().result(timeout=1.0)
    client.start().result(timeout=1.0)
    assert client.session_id != first_session
    assert client.ping().result(timeout=1.0).type == "pong"
    client.close(timeout=1.0)
    gateway.close(timeout=1.0)
    assert not client.is_alive
    assert not gateway.is_alive
