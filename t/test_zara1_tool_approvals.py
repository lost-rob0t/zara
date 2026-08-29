from __future__ import annotations

import concurrent.futures
import queue
import socket as net_socket
import time
from dataclasses import replace
from pathlib import Path

import pytest
import zmq
from langchain_core.messages import AIMessage, HumanMessage, ToolMessage
from langchain_core.tools import tool

from zara.agent import AgentManager
from zara.runtime import bridge, events
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.runtime.commands import (
    ApproveTool,
    CancelTurn,
    CommandReceipt,
    RejectTool,
    SubmitTurn,
)
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.security import Capability, SecurityAuditLog, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import ProtocolRemoteError, TransportConfig, ZmqZaraClient


class PrincipalSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands: list[tuple[PrincipalContext, object]] = []
        self.buses: dict[str, bridge.RuntimeEventBus] = {}

    def bus(self, principal: PrincipalContext) -> bridge.RuntimeEventBus:
        return self.buses.setdefault(principal.principal_id, bridge.RuntimeEventBus())

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        return self.bus(principal).subscribe(maxsize=maxsize)

    def submit(self, principal: PrincipalContext, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        turn_id = None
        if isinstance(command, SubmitTurn):
            suffix = principal.principal_id.rsplit(":", 1)[-1]
            turn_id = f"turn-{suffix}-{len(self.commands)}"
        future.set_result(CommandReceipt(request_id=command.request_id, turn_id=turn_id))
        return future


class HostSupervisor:
    def __init__(
        self,
        principal: PrincipalContext,
        host: RuntimeHost,
        bus: bridge.RuntimeEventBus,
    ) -> None:
        self.state = ServerState.READY
        self.principal = principal
        self.host = host
        self.bus = bus

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        assert principal == self.principal
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal: PrincipalContext, command):
        assert principal == self.principal
        return self.host.submit(command)


class ApprovalConfig:
    config_dir = Path("/nonexistent-zara1-approval-config")

    def get_llm_config(self):
        return {}

    def get_section(self, name: str):
        sections = {
            "agent": {"conversation_timeout": 60, "max_steps": 4},
            "memory": {"max_chars": 1200, "top_k": 5},
            "tool_approval": {
                "required_tools": ["approval_effect"],
                "timeout_seconds": 5.0,
                "max_pending": 4,
            },
        }
        return sections.get(name, {})

    def get_module_search_paths(self):
        return []

    def get_tool_config(self):
        return {"file_tools": False}

    def get_agent_system_prompt(self):
        return "Test assistant."


class EmptyMemory:
    def retrieve(self, _query, k=5):
        return []


class ApprovalLLM:
    def __init__(self, secret: str) -> None:
        self.secret = secret

    def bind_tools(self, _tools):
        return self

    async def ainvoke(self, messages):
        if isinstance(messages[-1], ToolMessage):
            return AIMessage(content="finished")
        assert isinstance(messages[-1], HumanMessage)
        return AIMessage(
            content="",
            tool_calls=[
                {
                    "name": "approval_effect",
                    "args": {"value": self.secret},
                    "id": "approval-call-1",
                    "type": "tool_call",
                }
            ],
        )


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
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def enroll(
    registry: SecurityRegistry,
    principal: PrincipalContext,
    device_id: str,
    *,
    tool_approval: bool = True,
) -> tuple[str, str]:
    public, secret = keypair()
    capabilities = {
        Capability.SESSION_BASIC,
        Capability.TURN_SUBMIT,
        Capability.TURN_CANCEL,
    }
    if tool_approval:
        capabilities.add(Capability.TOOL_APPROVE)
    registry.enroll(
        public,
        principal=principal,
        device_id=device_id,
        capabilities=capabilities,
    )
    return public, secret


def make_client(
    endpoint: str,
    context: zmq.Context,
    config: TransportConfig,
    *,
    public: str,
    secret: str,
    server_public: str,
) -> ZmqZaraClient:
    return ZmqZaraClient(
        endpoint,
        context=context,
        config=config,
        curve_client=CurveClientConfig(
            public_key=public,
            secret_key=secret,
            server_public_key=server_public,
        ),
    )


def make_gateway(
    endpoint: str,
    context: zmq.Context,
    config: TransportConfig,
    *,
    supervisor: PrincipalSupervisor,
    registry: SecurityRegistry,
    server_public: str,
    server_secret: str,
    audit_log: SecurityAuditLog | None = None,
) -> SecureZaraZmqGateway:
    arguments = {}
    if audit_log is not None:
        arguments["audit_log"] = audit_log
    return SecureZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        context=context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            zap_domain="zara",
        ),
        **arguments,
    )


def publish_waiting(
    supervisor: PrincipalSupervisor,
    principal: PrincipalContext,
    *,
    turn_id: str,
    tool_run_id: str = "shared-tool-call",
) -> None:
    supervisor.bus(principal).publish(
        events.ToolWaitingForUser(
            turn_id=turn_id,
            conversation_id="conversation-1",
            tool_run_id=tool_run_id,
            tool_name="reviewed_effect",
            prompt="Approve reviewed_effect?",
        )
    )


def next_event(subscription, event_type, *, timeout: float = 3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        envelope = subscription.get(timeout=max(0.01, deadline - time.monotonic()))
        if isinstance(envelope.event, event_type):
            return envelope.event
    raise AssertionError(f"missing event {event_type.__name__}")


def build_production_host(monkeypatch, principal: PrincipalContext, secret: str):
    effects: list[str] = []

    @tool("approval_effect")
    def approval_effect(value: str) -> str:
        """Record one deterministic approval-protected side effect."""
        effects.append(value)
        return "effect complete"

    monkeypatch.setattr(
        "zara.agent.tools.builtin_tools.get_builtin_tools",
        lambda *_args, **_kwargs: [],
    )
    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda _self, _config: ApprovalLLM(secret),
    )
    manager = AgentManager(
        config=ApprovalConfig(),
        memory_manager=EmptyMemory(),
        principal=principal,
    )
    manager.tool_registry.register_tool(approval_effect)
    bus = bridge.RuntimeEventBus()
    host = RuntimeHost(
        lambda: LangGraphRuntimeBackend(lambda: manager),
        publisher=bus.publish,
    )
    return host, bus, effects


def stop_host(host: RuntimeHost) -> None:
    if host.state not in {
        RuntimeHostState.NEW,
        RuntimeHostState.STOPPED,
        RuntimeHostState.FAILED,
    }:
        host.shutdown("test cleanup").result(timeout=5.0)
    host.join(timeout=5.0)


def test_authenticated_clients_receive_and_answer_only_their_principal_events(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    alice = PrincipalContext("user:alice", kind="authenticated")
    bob = PrincipalContext("user:bob", kind="authenticated")
    alice_public, alice_secret = enroll(registry, alice, "alice-phone")
    bob_public, bob_secret = enroll(registry, bob, "bob-phone")
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    alice_client = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=alice_public,
        secret=alice_secret,
        server_public=server_public,
    )
    bob_client = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=bob_public,
        secret=bob_secret,
        server_public=server_public,
    )
    alice_events = alice_client.subscribe(maxsize=8)
    bob_events = bob_client.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    alice_client.start().result(timeout=1.0)
    bob_client.start().result(timeout=1.0)

    try:
        alice_turn = alice_client.submit(SubmitTurn(text="alice effect")).result(timeout=1.0)
        bob_turn = bob_client.submit(SubmitTurn(text="bob effect")).result(timeout=1.0)
        publish_waiting(supervisor, alice, turn_id=alice_turn.turn_id)
        publish_waiting(supervisor, bob, turn_id=bob_turn.turn_id)

        assert alice_events.get(timeout=1.0).event.turn_id == alice_turn.turn_id
        assert bob_events.get(timeout=1.0).event.turn_id == bob_turn.turn_id
        alice_client.submit(ApproveTool(tool_run_id="shared-tool-call")).result(timeout=1.0)
        bob_client.submit(RejectTool(tool_run_id="shared-tool-call")).result(timeout=1.0)

        decisions = [
            (principal, command)
            for principal, command in supervisor.commands
            if isinstance(command, (ApproveTool, RejectTool))
        ]
        assert [principal for principal, _ in decisions] == [alice, bob]
        assert isinstance(decisions[0][1], ApproveTool)
        assert isinstance(decisions[1][1], RejectTool)
    finally:
        alice_events.close()
        bob_events.close()
        alice_client.close(timeout=1.0)
        bob_client.close(timeout=1.0)
        service.close(timeout=1.0)


def test_same_principal_second_session_cannot_observe_or_answer_approval(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:alice", kind="authenticated")
    first_public, first_secret = enroll(registry, principal, "alice-phone")
    second_public, second_secret = enroll(registry, principal, "alice-laptop")
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    first = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=first_public,
        secret=first_secret,
        server_public=server_public,
    )
    second = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=second_public,
        secret=second_secret,
        server_public=server_public,
    )
    first_events = first.subscribe(maxsize=8)
    second_events = second.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    first.start().result(timeout=1.0)
    second.start().result(timeout=1.0)

    try:
        turn = first.submit(SubmitTurn(text="owned by phone")).result(timeout=1.0)
        publish_waiting(supervisor, principal, turn_id=turn.turn_id)
        assert first_events.get(timeout=1.0).event.tool_run_id == "shared-tool-call"
        with pytest.raises(queue.Empty):
            second_events.get(timeout=0.1)
        with pytest.raises(ProtocolRemoteError) as denied:
            second.submit(ApproveTool(tool_run_id="shared-tool-call")).result(timeout=1.0)
        assert denied.value.code == "approval_not_owned"
        first.submit(ApproveTool(tool_run_id="shared-tool-call")).result(timeout=1.0)
    finally:
        first_events.close()
        second_events.close()
        first.close(timeout=1.0)
        second.close(timeout=1.0)
        service.close(timeout=1.0)


def test_missing_tool_approval_capability_denies_before_runtime_dispatch(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:limited", kind="authenticated")
    public, secret = enroll(registry, principal, "limited-phone", tool_approval=False)
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    limited = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = limited.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    limited.start().result(timeout=1.0)

    try:
        turn = limited.submit(SubmitTurn(text="needs approval")).result(timeout=1.0)
        publish_waiting(supervisor, principal, turn_id=turn.turn_id)
        assert subscription.get(timeout=1.0).event.tool_run_id == "shared-tool-call"
        command_count = len(supervisor.commands)
        with pytest.raises(ProtocolRemoteError) as denied:
            limited.submit(ApproveTool(tool_run_id="shared-tool-call")).result(timeout=1.0)
        assert denied.value.code == "authorization_denied"
        assert len(supervisor.commands) == command_count
    finally:
        subscription.close()
        limited.close(timeout=1.0)
        service.close(timeout=1.0)


def test_tool_decision_request_replay_is_idempotent_but_new_request_is_stale(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:alice", kind="authenticated")
    public, secret = enroll(registry, principal, "alice-phone")
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        turn = authenticated.submit(SubmitTurn(text="replay proof")).result(timeout=1.0)
        publish_waiting(supervisor, principal, turn_id=turn.turn_id)
        subscription.get(timeout=1.0)
        decision = ApproveTool(request_id="stable-decision", tool_run_id="shared-tool-call")
        first = authenticated.submit(decision).result(timeout=1.0)
        replay = authenticated.submit(decision).result(timeout=1.0)
        assert first == replay == CommandReceipt(request_id="stable-decision")
        assert sum(isinstance(command, ApproveTool) for _, command in supervisor.commands) == 1

        with pytest.raises(ProtocolRemoteError) as stale:
            authenticated.submit(
                ApproveTool(request_id="new-decision", tool_run_id="shared-tool-call")
            ).result(timeout=1.0)
        assert stale.value.code == "approval_not_owned"
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)


def test_reconnect_does_not_inherit_pending_approval_ownership(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:alice", kind="authenticated")
    public, secret = enroll(registry, principal, "alice-phone")
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        old_session = authenticated.session_id
        turn = authenticated.submit(SubmitTurn(text="reconnect proof")).result(timeout=1.0)
        publish_waiting(supervisor, principal, turn_id=turn.turn_id)
        assert next_event(subscription, events.ToolWaitingForUser).tool_run_id == "shared-tool-call"

        authenticated.reconnect().result(timeout=1.0)
        assert authenticated.session_id != old_session
        with pytest.raises(ProtocolRemoteError) as stale:
            authenticated.submit(ApproveTool(tool_run_id="shared-tool-call")).result(timeout=1.0)
        assert stale.value.code == "approval_not_owned"
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)


def test_rejection_reason_is_absent_from_security_audit(
    zmq_context,
    transport_config,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:alice", kind="authenticated")
    public, secret = enroll(registry, principal, "alice-phone")
    audit = SecurityAuditLog(capacity=16)
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
        audit_log=audit,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        turn = authenticated.submit(SubmitTurn(text="private request")).result(timeout=1.0)
        publish_waiting(supervisor, principal, turn_id=turn.turn_id)
        next_event(subscription, events.ToolWaitingForUser)
        authenticated.submit(
            RejectTool(
                tool_run_id="shared-tool-call",
                reason="PRIVATE-REJECTION-REASON",
            )
        ).result(timeout=1.0)

        rendered = repr([record.as_dict() for record in audit.snapshot()])
        assert "PRIVATE-REJECTION-REASON" not in rendered
        assert any(record.action == "tool.reject" for record in audit.snapshot())
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)


def test_gateway_bounds_delivery_ownership_without_authorizing_excess_events(
    zmq_context,
    transport_config,
):
    bounded_config = replace(transport_config, pending_request_limit=2)
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    supervisor = PrincipalSupervisor()
    principal = PrincipalContext("user:alice", kind="authenticated")
    public, secret = enroll(registry, principal, "alice-phone")
    service = make_gateway(
        endpoint,
        zmq_context,
        bounded_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        bounded_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=8)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        turn = authenticated.submit(SubmitTurn(text="bounded approvals")).result(timeout=1.0)
        for index in range(3):
            publish_waiting(
                supervisor,
                principal,
                turn_id=turn.turn_id,
                tool_run_id=f"tool-call-{index}",
            )

        delivered = {
            next_event(subscription, events.ToolWaitingForUser).tool_run_id,
            next_event(subscription, events.ToolWaitingForUser).tool_run_id,
        }
        assert delivered == {"tool-call-0", "tool-call-1"}
        with pytest.raises(queue.Empty):
            subscription.get(timeout=0.1)
        assert len(service._approval_owners) == 2
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)


@pytest.mark.parametrize("approve", [True, False])
def test_production_runtime_approval_round_trip_over_authenticated_zara1(
    zmq_context,
    transport_config,
    monkeypatch,
    approve,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    principal = PrincipalContext("user:production", kind="authenticated")
    public, secret = enroll(registry, principal, "production-client")
    private_argument = "PRIVATE-TOOL-ARGUMENT"
    host, bus, effects = build_production_host(monkeypatch, principal, private_argument)
    supervisor = HostSupervisor(principal, host, bus)
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=32)
    host.start().result(timeout=5.0)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        receipt = authenticated.submit(
            SubmitTurn(text="run protected effect", conversation_id="conversation-1")
        ).result(timeout=1.0)
        waiting = next_event(subscription, events.ToolWaitingForUser)
        assert waiting.turn_id == receipt.turn_id
        assert waiting.tool_run_id == "approval-call-1"
        assert effects == []
        assert private_argument not in repr(waiting)

        if approve:
            authenticated.submit(
                ApproveTool(tool_run_id="approval-call-1")
            ).result(timeout=1.0)
            terminal = next_event(subscription, events.ToolCompleted, timeout=5.0)
            assert terminal.tool_run_id == "approval-call-1"
            assert effects == [private_argument]
        else:
            authenticated.submit(
                RejectTool(
                    tool_run_id="approval-call-1",
                    reason="PRIVATE-REJECTION-REASON",
                )
            ).result(timeout=1.0)
            terminal = next_event(subscription, events.ToolCancelled, timeout=5.0)
            assert terminal.tool_run_id == "approval-call-1"
            assert effects == []
            assert "PRIVATE-REJECTION-REASON" not in repr(terminal)

        completed = next_event(subscription, events.AgentCompleted, timeout=5.0)
        assert completed.turn_id == receipt.turn_id
        assert completed.success is True
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)
        stop_host(host)


def test_cancelled_production_turn_cannot_be_approved_over_zara1(
    zmq_context,
    transport_config,
    monkeypatch,
):
    endpoint = tcp_endpoint()
    server_public, server_secret = keypair()
    registry = SecurityRegistry()
    principal = PrincipalContext("user:production", kind="authenticated")
    public, secret = enroll(registry, principal, "production-client")
    host, bus, effects = build_production_host(monkeypatch, principal, "cancelled-secret")
    supervisor = HostSupervisor(principal, host, bus)
    service = make_gateway(
        endpoint,
        zmq_context,
        transport_config,
        supervisor=supervisor,
        registry=registry,
        server_public=server_public,
        server_secret=server_secret,
    )
    authenticated = make_client(
        endpoint,
        zmq_context,
        transport_config,
        public=public,
        secret=secret,
        server_public=server_public,
    )
    subscription = authenticated.subscribe(maxsize=32)
    host.start().result(timeout=5.0)
    service.start().result(timeout=1.0)
    authenticated.start().result(timeout=1.0)

    try:
        receipt = authenticated.submit(SubmitTurn(text="cancel protected effect")).result(timeout=1.0)
        next_event(subscription, events.ToolWaitingForUser)
        authenticated.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=1.0)
        next_event(subscription, events.ToolCancelled, timeout=5.0)

        with pytest.raises(ProtocolRemoteError) as stale:
            authenticated.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=1.0)
        assert stale.value.code == "approval_not_owned"
        assert effects == []
    finally:
        subscription.close()
        authenticated.close(timeout=1.0)
        service.close(timeout=1.0)
        stop_host(host)
