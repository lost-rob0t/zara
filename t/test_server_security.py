from __future__ import annotations

import concurrent.futures

import pytest
import zmq

from zara.security import CurveServerConfig, KeyRegistry
from zara.server import PrincipalContext, RuntimeSupervisor, ServerLease, ZaraServer


class FakeHost:
    state = type("State", (), {"value": "running"})()
    is_alive = False

    def start(self):
        future = concurrent.futures.Future()
        future.set_result(None)
        return future

    def shutdown(self, reason=""):
        future = concurrent.futures.Future()
        future.set_result(reason)
        return future

    def join(self, timeout=None):
        return None


class FakeGateway:
    def __init__(self):
        self.started = False
        self.closed = False

    def start(self):
        self.started = True
        future = concurrent.futures.Future()
        future.set_result(True)
        return future

    def close(self, timeout=None):
        self.closed = True


def _server_security() -> CurveServerConfig:
    public, secret = zmq.curve_keypair()
    return CurveServerConfig(
        public_key=public.decode("ascii"),
        secret_key=secret.decode("ascii"),
        registry=KeyRegistry(),
    )


def _supervisor() -> RuntimeSupervisor:
    return RuntimeSupervisor(
        host_factory=lambda _principal, _bus: FakeHost(),
        shutdown_timeout=0.2,
    )


def test_tcp_listener_without_curve_security_is_rejected_before_start():
    with pytest.raises(ValueError, match="authentication|CURVE|security"):
        ZaraServer(endpoint="tcp://127.0.0.1:5555")


def test_tcp_listener_with_curve_security_is_allowed_and_forwarded_to_gateway(tmp_path):
    security = _server_security()
    captured = {}

    def gateway_factory(endpoint, *, supervisor, principal, security=None):
        captured.update(
            endpoint=endpoint,
            supervisor=supervisor,
            principal=principal,
            security=security,
        )
        return FakeGateway()

    supervisor = _supervisor()
    principal = PrincipalContext.local_owner()
    server = ZaraServer(
        supervisor=supervisor,
        lease=ServerLease(tmp_path / "runtime"),
        endpoint="tcp://127.0.0.1:5555",
        gateway_factory=gateway_factory,
        principal=principal,
        security=security,
        shutdown_timeout=0.2,
    )

    try:
        server.start()
        assert captured["endpoint"] == "tcp://127.0.0.1:5555"
        assert captured["supervisor"] is supervisor
        assert captured["principal"] == principal
        assert captured["security"] is security
    finally:
        server.stop()


def test_wildcard_tcp_listener_requires_and_accepts_complete_curve_security(tmp_path):
    security = _server_security()
    captured = {}

    def gateway_factory(endpoint, *, supervisor, principal, security=None):
        captured["endpoint"] = endpoint
        captured["security"] = security
        return FakeGateway()

    with pytest.raises(ValueError, match="authentication|CURVE|security"):
        ZaraServer(endpoint="tcp://*:5555")

    server = ZaraServer(
        supervisor=_supervisor(),
        lease=ServerLease(tmp_path / "runtime"),
        endpoint="tcp://*:5555",
        gateway_factory=gateway_factory,
        security=security,
        shutdown_timeout=0.2,
    )
    try:
        server.start()
        assert captured == {"endpoint": "tcp://*:5555", "security": security}
    finally:
        server.stop()


def test_owner_private_ipc_remains_available_without_curve_security(tmp_path):
    captured = {}

    def gateway_factory(endpoint, *, supervisor, principal, security=None):
        captured["endpoint"] = endpoint
        captured["security"] = security
        return FakeGateway()

    endpoint = f"ipc://{tmp_path / 'zara.sock'}"
    server = ZaraServer(
        supervisor=_supervisor(),
        lease=ServerLease(tmp_path / "runtime"),
        endpoint=endpoint,
        gateway_factory=gateway_factory,
        shutdown_timeout=0.2,
    )
    try:
        server.start()
        assert captured == {"endpoint": endpoint, "security": None}
    finally:
        server.stop()
