"""Two authenticated wake principals must be fully isolated (#244 AC3)."""

from __future__ import annotations

import concurrent.futures
import socket as net_socket
import threading
import time

import pytest
import zmq

from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.runtime.events import VoiceTranscriptFinal
from zara.security import Capability, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig
from zara.server import PrincipalContext, ServerState
from zara.wake_daemon import WakeDaemonClient
from zara.zmq_transport import TransportConfig, ZmqZaraClient


class MultiPrincipalSupervisor:
    """One bus and one submit ledger per principal, like the real daemon."""

    def __init__(self) -> None:
        self.state = ServerState.READY
        self.buses: dict[str, bridge.RuntimeEventBus] = {}
        self.submitted: dict[str, list[SubmitTurn]] = {}

    def bus(self, principal: PrincipalContext) -> bridge.RuntimeEventBus:
        return self.buses.setdefault(
            principal.principal_id, bridge.RuntimeEventBus()
        )

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        return self.bus(principal).subscribe(maxsize=maxsize)

    def submit(self, principal: PrincipalContext, command):
        assert isinstance(principal, PrincipalContext)
        ledger = self.submitted.setdefault(principal.principal_id, [])
        turn_id = None
        if isinstance(command, SubmitTurn):
            ledger.append(command)
            suffix = principal.principal_id.rsplit(":", 1)[-1]
            turn_id = f"turn-{suffix}-1"
        future = concurrent.futures.Future()
        future.set_result(
            CommandReceipt(request_id=command.request_id, turn_id=turn_id)
        )
        return future


def keypair():
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def tcp_endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


class GatewayHarness:
    def __init__(self, zmq_context: zmq.Context) -> None:
        self.context = zmq_context
        self.config = TransportConfig(
            sndhwm=16,
            rcvhwm=16,
            max_message_bytes=1024 * 1024,
            heartbeat_interval_ms=100,
            heartbeat_timeout_ms=500,
            linger_ms=0,
            request_timeout=1.0,
        )
        self.supervisor = MultiPrincipalSupervisor()
        self.registry = SecurityRegistry()
        self.server_public, self.server_secret = keypair()
        self.endpoint = tcp_endpoint()
        self.gateway = SecureZaraZmqGateway(
            self.endpoint,
            supervisor=self.supervisor,
            context=zmq_context,
            config=self.config,
            security_registry=self.registry,
            curve_server=CurveServerConfig(
                public_key=self.server_public,
                secret_key=self.server_secret,
                zap_domain="zara",
            ),
        )
        self.gateway.start().result(timeout=2.0)

    def enroll(self, principal: PrincipalContext, device_id: str):
        public, secret = keypair()
        self.registry.enroll(
            public,
            principal=principal,
            device_id=device_id,
            capabilities={
                Capability.SESSION_BASIC,
                Capability.TURN_SUBMIT,
                Capability.TURN_CANCEL,
            },
        )
        return public, secret

    def wake_client(
        self,
        principal: PrincipalContext,
        device_id: str,
        *,
        conversation_id: str,
    ) -> WakeDaemonClient:
        public, secret = self.enroll(principal, device_id)
        zara_client = ZmqZaraClient(
            self.endpoint,
            context=self.context,
            config=self.config,
            curve_client=CurveClientConfig(
                public_key=public,
                secret_key=secret,
                server_public_key=self.server_public,
            ),
        )
        client = WakeDaemonClient(client=zara_client)
        client.connect()
        client.client.open_conversation(conversation_id).result(timeout=1.0)
        client.start_pump()
        return client

    def publish_transcript(
        self,
        principal: PrincipalContext,
        *,
        text: str,
        stream_id: str,
        trace_id: str,
        conversation_id: str,
    ) -> None:
        self.supervisor.bus(principal).publish(
            VoiceTranscriptFinal(
                conversation_id=conversation_id,
                stream_id=stream_id,
                trace_id=trace_id,
                text=text,
            )
        )

    def close(self) -> None:
        self.gateway.close(timeout=1.0)


@pytest.fixture
def harness():
    context = zmq.Context()
    instance = GatewayHarness(context)
    try:
        yield instance
    finally:
        instance.close()
        context.term()


def wait_for(predicate, timeout: float = 2.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.01)
    return predicate()


def test_two_principals_receive_only_their_own_transcripts(harness):
    alice = PrincipalContext("user:alice", kind="authenticated")
    bob = PrincipalContext("user:bob", kind="authenticated")

    alice_client = harness.wake_client(
        alice, "device-alice", conversation_id="conv-alice"
    )
    bob_client = harness.wake_client(
        bob, "device-bob", conversation_id="conv-bob"
    )
    try:
        alice_final: list[str] = []
        bob_final: list[str] = []
        alice_client.on_transcript_final.append(
            lambda event: alice_final.append(event.text)
        )
        bob_client.on_transcript_final.append(
            lambda event: bob_final.append(event.text)
        )

        harness.publish_transcript(
            alice,
            text="alice secret transcript",
            stream_id="stream-alice",
            trace_id="trace-alice",
            conversation_id="conv-alice",
        )
        harness.publish_transcript(
            bob,
            text="bob secret transcript",
            stream_id="stream-bob",
            trace_id="trace-bob",
            conversation_id="conv-bob",
        )

        assert wait_for(lambda: alice_final == ["alice secret transcript"])
        assert wait_for(lambda: bob_final == ["bob secret transcript"])
        assert alice_final == ["alice secret transcript"]
        assert bob_final == ["bob secret transcript"]
    finally:
        alice_client.close()
        bob_client.close()


def test_principal_submit_uses_the_authenticated_principal(harness):
    alice = PrincipalContext("user:alice", kind="authenticated")
    bob = PrincipalContext("user:bob", kind="authenticated")

    alice_client = harness.wake_client(
        alice, "device-alice", conversation_id="conv-alice"
    )
    bob_client = harness.wake_client(
        bob, "device-bob", conversation_id="conv-bob"
    )
    try:
        alice_client.client.submit(
            SubmitTurn(text="alice turn", conversation_id="conv-alice")
        ).result(timeout=1.0)
        bob_client.client.submit(
            SubmitTurn(text="bob turn", conversation_id="conv-bob")
        ).result(timeout=1.0)

        alice_texts = [command.text for command in harness.supervisor.submitted["user:alice"]]
        bob_texts = [command.text for command in harness.supervisor.submitted["user:bob"]]
        assert alice_texts == ["alice turn"]
        assert bob_texts == ["bob turn"]
    finally:
        alice_client.close()
        bob_client.close()


def test_unknown_device_key_is_denied_before_any_session(harness):
    public, secret = keypair()

    client = ZmqZaraClient(
        harness.endpoint,
        context=harness.context,
        config=harness.config,
        curve_client=CurveClientConfig(
            public_key=public,
            secret_key=secret,
            server_public_key=harness.server_public,
        ),
    )
    with pytest.raises(Exception):
        client.start().result(timeout=2.0)
    assert client.state != "READY"


def test_wake_clients_share_no_subscription_state(harness):
    alice = PrincipalContext("user:alice", kind="authenticated")
    bob = PrincipalContext("user:bob", kind="authenticated")

    alice_client = harness.wake_client(
        alice, "device-alice", conversation_id="conv-alice"
    )
    bob_client = harness.wake_client(
        bob, "device-bob", conversation_id="conv-bob"
    )
    try:
        harness.publish_transcript(
            bob,
            text="bob private",
            stream_id="stream-bob",
            trace_id="trace-bob",
            conversation_id="conv-bob",
        )

        assert wait_for(lambda: harness.supervisor.submitted == {})
        assert alice_client._subscription is not bob_client._subscription
    finally:
        alice_client.close()
        bob_client.close()
