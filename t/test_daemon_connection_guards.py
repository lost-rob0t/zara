from __future__ import annotations

import concurrent.futures
import queue
import socket as net_socket
import time
from pathlib import Path

import pytest
import zmq

from zara.client import ZaraClientState
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_state import PersistentSecurityState
from zara.security_transport import CurveClientConfig, CurveServerConfig
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import ClientDisconnected, TransportConfig, ZmqZaraClient

HANDSHAKE_BUDGET_SECONDS = 12.0


class FakeSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands: list[tuple[PrincipalContext, object]] = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(
                request_id=command.request_id,
                turn_id=f"turn-{command.request_id}",
            )
        )
        return future

    def subscribe(self, principal, *, maxsize=0):
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


def wait_for(future: concurrent.futures.Future, timeout: float, phase: str):
    try:
        return future.result(timeout=timeout)
    except concurrent.futures.TimeoutError as error:
        raise AssertionError(f"{phase} did not complete within {timeout}s") from error


def receive_event(
    subscription: bridge.RuntimeEventSubscription,
    timeout: float,
    phase: str,
) -> bridge.EventEnvelope:
    try:
        return subscription.get(timeout=timeout)
    except queue.Empty as error:
        raise AssertionError(f"{phase} did not arrive within {timeout}s") from error


def make_secure_gateway(
    context: zmq.Context,
    endpoint: str,
    config: TransportConfig,
    *,
    supervisor: FakeSupervisor,
    registry: SecurityRegistry,
    server_config: CurveServerConfig,
) -> SecureZaraZmqGateway:
    return SecureZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        context=context,
        config=config,
        security_registry=registry,
        curve_server=server_config,
    )


def enrolled_client_config(
    state: PersistentSecurityState,
    registry: SecurityRegistry,
    server_public: str,
    *,
    device_id: str,
) -> CurveClientConfig:
    public, secret = keypair()
    state.enroll_client(
        public,
        device_id=device_id,
        principal=PrincipalContext("user:owner", kind="authenticated"),
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
        live_registry=registry,
    )
    return CurveClientConfig(
        public_key=public,
        secret_key=secret,
        server_public_key=server_public,
    )


def unenrolled_client_config(server_public: str) -> CurveClientConfig:
    public, secret = keypair()
    return CurveClientConfig(public_key=public, secret_key=secret, server_public_key=server_public)


def test_valid_client_completes_handshake_submit_and_terminal_event_over_tcp(
    tmp_path: Path,
    zmq_context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = tcp_endpoint()
    state = PersistentSecurityState(tmp_path / "security")
    server_config = state.initialize()
    registry = state.load_registry()
    credentials = enrolled_client_config(
        state,
        registry,
        server_config.public_key,
        device_id="guard-phone",
    )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_config=server_config,
    )
    gateway.start().result(timeout=5.0)
    client = ZmqZaraClient(
        endpoint,
        context=zmq_context,
        config=transport_config,
        curve_client=credentials,
    )
    subscription = client.subscribe(maxsize=8)
    try:
        started = time.monotonic()
        wait_for(client.start(), 5.0, "handshake")
        assert time.monotonic() - started <= 5.0
        assert client.state is ZaraClientState.READY
        assert client.session_id

        pong = wait_for(client.ping(), 2.0, "ping")
        assert pong.type == "pong"

        command = SubmitTurn(
            request_id="guard-turn",
            text="hello over tcp",
            conversation_id="conversation-guard",
        )
        receipt = wait_for(client.submit(command), 2.0, "turn submit")
        assert receipt == CommandReceipt(request_id="guard-turn", turn_id="turn-guard-turn")
        assert supervisor.commands[-1][0].principal_id == "user:owner"
        assert supervisor.commands[-1][1] == command

        supervisor.bus.publish(
            events.AssistantComplete(
                turn_id="turn-guard-turn",
                conversation_id="conversation-guard",
                text="hello back",
                success=True,
            )
        )
        completed = receive_event(subscription, 2.0, "assistant.completed terminal event")
        assert completed.event == events.AssistantComplete(
            turn_id="turn-guard-turn",
            conversation_id="conversation-guard",
            text="hello back",
            success=True,
        )

        supervisor.bus.publish(
            events.AgentCompleted(
                turn_id="turn-guard-turn",
                conversation_id="conversation-guard",
                success=True,
            )
        )
        terminal = receive_event(subscription, 2.0, "turn.completed terminal event")
        assert terminal.event == events.AgentCompleted(
            turn_id="turn-guard-turn",
            conversation_id="conversation-guard",
            success=True,
        )
    finally:
        client.close(timeout=2.0)
        gateway.close(timeout=2.0)


def test_unenrolled_curve_key_fails_bounded_with_typed_error(
    tmp_path: Path,
    zmq_context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = tcp_endpoint()
    state = PersistentSecurityState(tmp_path / "security")
    server_config = state.initialize()
    registry = state.load_registry()
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_config=server_config,
    )
    gateway.start().result(timeout=5.0)
    client = ZmqZaraClient(
        endpoint,
        context=zmq_context,
        config=transport_config,
        curve_client=unenrolled_client_config(server_config.public_key),
    )
    try:
        started = time.monotonic()
        with pytest.raises(ClientDisconnected) as error:
            client.start().result(timeout=10.0)
        elapsed = time.monotonic() - started
        assert elapsed <= HANDSHAKE_BUDGET_SECONDS
        assert "handshake" in str(error.value)
        assert client.state is ZaraClientState.FAILED
    finally:
        client.close(timeout=2.0)
        gateway.close(timeout=2.0)


def test_wrong_server_public_key_fails_bounded(
    tmp_path: Path,
    zmq_context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = tcp_endpoint()
    state = PersistentSecurityState(tmp_path / "security")
    server_config = state.initialize()
    registry = state.load_registry()
    credentials = enrolled_client_config(
        state,
        registry,
        server_config.public_key,
        device_id="guard-phone",
    )
    wrong_server_public, _wrong_server_secret = keypair()
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_config=server_config,
    )
    gateway.start().result(timeout=5.0)
    client = ZmqZaraClient(
        endpoint,
        context=zmq_context,
        config=transport_config,
        curve_client=CurveClientConfig(
            public_key=credentials.public_key,
            secret_key=credentials.secret_key,
            server_public_key=wrong_server_public,
        ),
    )
    try:
        started = time.monotonic()
        with pytest.raises(ClientDisconnected) as error:
            client.start().result(timeout=10.0)
        elapsed = time.monotonic() - started
        assert elapsed <= HANDSHAKE_BUDGET_SECONDS
        assert "handshake" in str(error.value)
        assert client.state is ZaraClientState.FAILED
    finally:
        client.close(timeout=2.0)
        gateway.close(timeout=2.0)


def test_endpoint_with_no_listener_fails_bounded(
    zmq_context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = tcp_endpoint()
    client = ZmqZaraClient(endpoint, context=zmq_context, config=transport_config)
    try:
        started = time.monotonic()
        with pytest.raises(ClientDisconnected) as error:
            client.start().result(timeout=10.0)
        elapsed = time.monotonic() - started
        assert elapsed <= HANDSHAKE_BUDGET_SECONDS
        assert "handshake" in str(error.value)
        assert client.state is ZaraClientState.FAILED
    finally:
        client.close(timeout=2.0)


def test_client_recovers_terminal_event_delivery_after_reconnect(
    tmp_path: Path,
    zmq_context: zmq.Context,
    transport_config: TransportConfig,
):
    endpoint = tcp_endpoint()
    state = PersistentSecurityState(tmp_path / "security")
    server_config = state.initialize()
    registry = state.load_registry()
    credentials = enrolled_client_config(
        state,
        registry,
        server_config.public_key,
        device_id="guard-phone",
    )
    supervisor = FakeSupervisor()
    gateway = make_secure_gateway(
        zmq_context,
        endpoint,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_config=server_config,
    )
    gateway.start().result(timeout=5.0)
    client = ZmqZaraClient(
        endpoint,
        context=zmq_context,
        config=transport_config,
        curve_client=credentials,
    )
    subscription = client.subscribe(maxsize=8)
    try:
        wait_for(client.start(), 5.0, "initial handshake")
        first_session = client.session_id
        conversation_id = "conversation-reconnect"
        opened = wait_for(client.open_conversation(conversation_id), 2.0, "conversation open")
        assert opened == conversation_id

        first = SubmitTurn(
            request_id="reconnect-first",
            text="before reconnect",
            conversation_id=conversation_id,
        )
        first_receipt = wait_for(client.submit(first), 2.0, "first turn submit")
        assert first_receipt.turn_id == "turn-reconnect-first"

        wait_for(client.reconnect(), 5.0, "reconnect")
        assert client.state is ZaraClientState.READY
        assert client.session_id
        assert client.session_id != first_session

        second = SubmitTurn(
            request_id="reconnect-second",
            text="after reconnect",
            conversation_id=conversation_id,
        )
        second_receipt = wait_for(client.submit(second), 2.0, "second turn submit")
        assert second_receipt.turn_id == "turn-reconnect-second"

        supervisor.bus.publish(
            events.AgentCompleted(
                turn_id="turn-reconnect-first",
                conversation_id=conversation_id,
                success=True,
            )
        )
        supervisor.bus.publish(
            events.AgentCompleted(
                turn_id="turn-reconnect-second",
                conversation_id=conversation_id,
                success=True,
            )
        )
        first_event = receive_event(subscription, 2.0, "first turn terminal event after reconnect")
        assert first_event.event == events.AgentCompleted(
            turn_id="turn-reconnect-first",
            conversation_id=conversation_id,
            success=True,
        )
        second_event = receive_event(subscription, 2.0, "second turn terminal event after reconnect")
        assert second_event.event == events.AgentCompleted(
            turn_id="turn-reconnect-second",
            conversation_id=conversation_id,
            success=True,
        )
    finally:
        client.close(timeout=2.0)
        gateway.close(timeout=2.0)
