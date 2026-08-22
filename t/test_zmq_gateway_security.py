from __future__ import annotations

import concurrent.futures
import socket

import pytest
import zmq

from zara.protocol import ProtocolMessage
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.security import (
    Capability,
    CurveClientConfig,
    CurveServerConfig,
    KeyRecord,
    KeyRegistry,
)
from zara.server import PrincipalContext, ServerState
from zara.zmq_transport import ProtocolRemoteError, TransportConfig, ZaraZmqGateway, ZmqZaraClient


class RecordingSupervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.commands: list[tuple[PrincipalContext, object]] = []
        self.bus = bridge.RuntimeEventBus()

    def submit(self, principal, command):
        self.commands.append((principal, command))
        future = concurrent.futures.Future()
        future.set_result(CommandReceipt(request_id=command.request_id, turn_id="turn-security"))
        return future

    def subscribe(self, principal, *, maxsize=0):
        assert isinstance(principal, PrincipalContext)
        return self.bus.subscribe(maxsize=maxsize)


def _pair():
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _tcp_endpoint() -> str:
    probe = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    finally:
        probe.close()
    return f"tcp://127.0.0.1:{port}"


def _config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=0.75,
        poll_interval_ms=5,
    )


def _security_pair(*, capabilities):
    client_public, client_secret = _pair()
    server_public, server_secret = _pair()
    registry = KeyRegistry()
    principal = PrincipalContext("alice", kind="authenticated")
    registry.enroll(
        KeyRecord(
            public_key=client_public,
            principal=principal,
            device_id="alice-laptop",
            capabilities=frozenset(capabilities),
        )
    )
    return (
        registry,
        principal,
        CurveServerConfig(
            public_key=server_public,
            secret_key=server_secret,
            registry=registry,
            zap_domain="zara-test",
        ),
        CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
        client_public,
    )


def test_authenticated_key_mapping_is_the_runtime_principal_not_route_or_fallback():
    endpoint = _tcp_endpoint()
    supervisor = RecordingSupervisor()
    registry, authenticated, server_security, client_security, _ = _security_pair(
        capabilities=(Capability.CONVERSATION, Capability.STATUS)
    )
    fallback = PrincipalContext("local-fallback", kind="local-owner")
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=fallback,
        config=_config(),
        security=server_security,
    )
    client = ZmqZaraClient(endpoint, config=_config(), security=client_security)
    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)
        receipt = client.submit(SubmitTurn(text="authenticated turn")).result(timeout=1.0)

        assert receipt.turn_id == "turn-security"
        assert len(supervisor.commands) == 1
        principal, command = supervisor.commands[0]
        assert principal == authenticated
        assert principal != fallback
        assert isinstance(command, SubmitTurn)
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_capability_denial_happens_before_runtime_dispatch():
    endpoint = _tcp_endpoint()
    supervisor = RecordingSupervisor()
    _, _, server_security, client_security, _ = _security_pair(
        capabilities=(Capability.STATUS,)
    )
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-fallback", kind="local-owner"),
        config=_config(),
        security=server_security,
    )
    client = ZmqZaraClient(endpoint, config=_config(), security=client_security)
    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)

        with pytest.raises(ProtocolRemoteError) as denied:
            client.submit(SubmitTurn(text="must not dispatch")).result(timeout=1.0)

        assert denied.value.code == "forbidden"
        assert supervisor.commands == []
        assert client.ping().result(timeout=1.0).type == "pong"
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)


def test_revoked_established_key_is_denied_on_its_next_application_message():
    endpoint = _tcp_endpoint()
    supervisor = RecordingSupervisor()
    registry, _, server_security, client_security, client_public = _security_pair(
        capabilities=(Capability.CONVERSATION, Capability.STATUS)
    )
    gateway = ZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        principal=PrincipalContext("local-fallback", kind="local-owner"),
        config=_config(),
        security=server_security,
    )
    client = ZmqZaraClient(endpoint, config=_config(), security=client_security)
    try:
        gateway.start().result(timeout=1.0)
        client.start().result(timeout=1.0)
        assert client.ping().result(timeout=1.0).type == "pong"

        registry.revoke(client_public)

        with pytest.raises(ProtocolRemoteError) as revoked:
            client.ping().result(timeout=1.0)
        assert revoked.value.code == "authentication_revoked"
        assert supervisor.commands == []
    finally:
        client.close(timeout=1.0)
        gateway.close(timeout=1.0)
