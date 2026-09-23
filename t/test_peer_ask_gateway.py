from __future__ import annotations

import concurrent.futures
import socket as net_socket
import time

import pytest
import zmq

from zara.node import DeviceClass, ZaraNode
from zara.peer_protocol import (
    PeerCallBudget,
    PeerCallRemoteError,
    PeerCallRequest,
    PeerCancelRequest,
    PeerResult,
)
from zara.runtime import bridge, events
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import TransportConfig, ZmqZaraClient


class _Supervisor:
    state = ServerState.READY

    def __init__(self) -> None:
        self.bus = bridge.RuntimeEventBus()
        self.commands: list[tuple[PrincipalContext, object]] = []
        self._turn = 0

    def subscribe(self, _principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        if isinstance(command, SubmitTurn):
            self._turn += 1
            turn_id = f"peer-turn-{self._turn}"
            future.set_result(CommandReceipt(request_id=command.request_id, turn_id=turn_id))
            self.bus.publish(
                events.ResponseText(
                    turn_id=turn_id,
                    conversation_id=command.conversation_id,
                    label="Zara",
                    text=f"peer:{command.text}",
                    truncated=False,
                )
            )
            self.bus.publish(
                events.AgentCompleted(
                    turn_id=turn_id,
                    conversation_id=command.conversation_id,
                    label="agent",
                    success=True,
                )
            )
            return future
        if isinstance(command, CancelTurn):
            future.set_result(
                CommandReceipt(request_id=command.request_id, turn_id=command.turn_id)
            )
            self.bus.publish(
                events.TurnCancelled(
                    turn_id=command.turn_id,
                    label="runtime-host",
                    reason="cancel command",
                )
            )
            return future
        raise AssertionError(f"unexpected command: {command!r}")


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
        request_timeout=2.0,
        poll_interval_ms=5,
        event_queue_size=16,
        pending_request_limit=16,
    )


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    return f"tcp://127.0.0.1:{port}"


def _request(enrollment_generation: int, *, request_id: str = "peer-ask-1", **overrides):
    values = {
        "operation": "node.ask",
        "request_id": request_id,
        "content": "what changed?",
        "context_refs": (),
        "media_refs": (),
        "requested_capabilities": frozenset(),
        "budget": PeerCallBudget(
            wall_time_ms=1_000,
            max_output_bytes=4_096,
            max_tokens=0,
            max_cost_microunits=0,
            max_tool_calls=0,
            max_model_calls=0,
            max_recursion_depth=0,
        ),
        "deadline_ns": time.time_ns() + 5_000_000_000,
        "hop_limit": 2,
        "visited_nodes": (),
        "cycle_token": "cycle-peer-1",
        "trace_id": "trace-peer-1",
        "correlation_id": "corr-peer-1",
        "causation_id": "cause-peer-1",
        "expected_enrollment_generation": enrollment_generation,
    }
    values.update(overrides)
    return PeerCallRequest(**values)


def _start_pair(context, config):
    endpoint = _tcp_endpoint()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    principal = PrincipalContext("user:peer", kind="authenticated")
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        client_public,
        principal=principal,
        device_id="peer-phone",
        capabilities={
            Capability.SESSION_BASIC,
            Capability.TURN_SUBMIT,
            Capability.TURN_CANCEL,
        },
    )
    node = ZaraNode(
        node_id=enrolled.device_id,
        display_name="Peer phone",
        device_class=DeviceClass.ANDROID,
        curve_public_key=enrolled.public_key,
        endpoints=(endpoint,),
        capabilities=frozenset(),
        protocol_versions=frozenset({"ZARA/1"}),
        last_seen=1,
        enrollment_generation=enrolled.generation,
    )
    supervisor = _Supervisor()
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        context=context,
        config=config,
        local_node_id="desktop-node",
    )
    gateway.start().result(timeout=1.0)
    client = ZmqZaraClient(
        endpoint,
        context=context,
        config=config,
        curve_client=CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
        peer_node=node,
    )
    client.start().result(timeout=1.5)
    client.open_conversation("conversation-peer").result(timeout=1.0)
    return gateway, client, supervisor, registry, enrolled


def test_authenticated_peer_ask_streams_and_returns_terminal_result(
    zmq_context,
    transport_config,
):
    gateway, client, supervisor, _registry, enrolled = _start_pair(
        zmq_context, transport_config
    )
    subscription = client.subscribe(maxsize=16)
    try:
        result = client.peer_call(_request(enrolled.generation)).result(timeout=1.5)
        assert isinstance(result, PeerResult)
        assert result.request_id == "peer-ask-1"
        assert result.source_node_id == "desktop-node"
        assert result.runtime_id == "zara-runtime"
        assert result.text == "peer:what changed?"
        assert [type(command) for _, command in supervisor.commands] == [SubmitTurn]

        streamed = subscription.drain(limit=16)
        assert any(
            isinstance(envelope.event, events.ResponseText)
            and envelope.event.text == "peer:what changed?"
            for envelope in streamed
        )
    finally:
        subscription.close()
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_duplicate_peer_request_replays_terminal_without_double_submit(
    zmq_context,
    transport_config,
):
    gateway, client, supervisor, _registry, enrolled = _start_pair(
        zmq_context, transport_config
    )
    try:
        request = _request(enrolled.generation)
        first = client.peer_call(request).result(timeout=1.5)
        second = client.peer_call(request).result(timeout=1.5)
        assert second == first
        assert sum(isinstance(command, SubmitTurn) for _, command in supervisor.commands) == 1
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_peer_call_rejects_unenforceable_model_budget_before_runtime_dispatch(
    zmq_context,
    transport_config,
):
    gateway, client, supervisor, _registry, enrolled = _start_pair(
        zmq_context, transport_config
    )
    try:
        request = _request(
            enrolled.generation,
            budget=PeerCallBudget(
                wall_time_ms=1_000,
                max_output_bytes=4_096,
                max_model_calls=1,
            ),
        )
        with pytest.raises(PeerCallRemoteError, match="budget"):
            client.peer_call(request).result(timeout=1.5)
        assert supervisor.commands == []
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_peer_cancel_propagates_to_existing_runtime_cancel(
    zmq_context,
    transport_config,
):
    gateway, client, supervisor, _registry, enrolled = _start_pair(
        zmq_context, transport_config
    )
    try:
        original_submit = supervisor.submit

        def submit_without_terminal(principal, command):
            if isinstance(command, SubmitTurn):
                supervisor.commands.append((principal, command))
                future = concurrent.futures.Future()
                future.set_result(
                    CommandReceipt(request_id=command.request_id, turn_id="peer-open-turn")
                )
                return future
            return original_submit(principal, command)

        supervisor.submit = submit_without_terminal
        pending = client.peer_call(
            _request(enrolled.generation, request_id="peer-open-call")
        )
        deadline = time.monotonic() + 1.0
        while (
            not any(isinstance(command, SubmitTurn) for _, command in supervisor.commands)
            and time.monotonic() < deadline
        ):
            time.sleep(0.01)

        receipt = client.cancel_peer_call(
            PeerCancelRequest(
                request_id="peer-cancel-1",
                call_id="peer-open-call",
                turn_id="peer-open-turn",
                expected_enrollment_generation=enrolled.generation,
                reason="operator_cancelled",
            )
        ).result(timeout=1.5)
        assert receipt.turn_id == "peer-open-turn"
        assert any(isinstance(command, CancelTurn) for _, command in supervisor.commands)
        with pytest.raises(PeerCallRemoteError, match="cancelled"):
            pending.result(timeout=1.5)
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)
