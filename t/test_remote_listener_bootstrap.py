from __future__ import annotations

import concurrent.futures
import threading
from pathlib import Path

import pytest

from zara.principals import PrincipalContext
from zara.security import Capability
from zara.security_admin import SecurityAdminClient, SecurityAdminError, SecurityAdminServer
from zara.security_state import PersistentSecurityState
from zara.server import ServerState, ZaraServer, default_security_state_directory


class _StartedGateway:
    def __init__(self, *, fail: bool = False) -> None:
        self.fail = fail
        self.start_calls = 0
        self.close_calls = 0

    def start(self):
        self.start_calls += 1
        future: concurrent.futures.Future[bool] = concurrent.futures.Future()
        if self.fail:
            future.set_exception(RuntimeError("bind failed"))
        else:
            future.set_result(True)
        return future

    def close(self, *, timeout: float) -> None:
        assert timeout > 0
        self.close_calls += 1


class _FakeSecurityState:
    def __init__(self, public_key: str) -> None:
        self._public_key = public_key

    def server_public_key(self) -> str:
        return self._public_key


def _ready_server(monkeypatch: pytest.MonkeyPatch, *, public_key: str):
    server = ZaraServer(supervisor=object())
    server._state = ServerState.READY
    server._voice_ingress = object()
    server._security_state = _FakeSecurityState(public_key)
    monkeypatch.setattr(server, "_ensure_security_admin", lambda: object())
    return server


def test_default_security_state_directory_is_persistent_and_xdg_scoped(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
):
    monkeypatch.delenv("ZARA_SECURITY_DIR", raising=False)
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path / "state"))
    assert default_security_state_directory() == tmp_path / "state" / "zarathushtra" / "security"

    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "explicit"))
    assert default_security_state_directory() == tmp_path / "explicit"


def test_owner_local_control_exposes_only_bounded_listener_actions(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    metadata = {
        "active": True,
        "endpoint": "tcp://192.0.2.10:17865",
        "server_public_key": state.server_public_key(),
    }
    ensure_calls = 0

    def ensure_listener():
        nonlocal ensure_calls
        ensure_calls += 1
        return dict(metadata)

    admin = SecurityAdminServer(
        state,
        capabilities={Capability.SESSION_BASIC},
        ensure_remote_listener=ensure_listener,
        remote_listener_status=lambda: dict(metadata),
    )
    admin.bind_registry(registry)
    admin.start()
    try:
        client = SecurityAdminClient(state.control_socket_path)
        assert client.request("remote_listener.status") == metadata
        assert client.request("remote_listener.ensure") == metadata
        assert ensure_calls == 1
        with pytest.raises(SecurityAdminError, match="invalid fields"):
            client.request("remote_listener.ensure", endpoint="tcp://0.0.0.0:1")
    finally:
        admin.close(timeout=1.0)


def test_remote_listener_ensure_is_idempotent_and_concurrency_fenced(
    monkeypatch: pytest.MonkeyPatch,
):
    public_key = "A" * 40
    server = _ready_server(monkeypatch, public_key=public_key)
    gateway = _StartedGateway()
    builds = 0

    def build(endpoint: str, *, supervisor, voice_ingress):
        nonlocal builds
        assert endpoint == "tcp://0.0.0.0:17865"
        assert supervisor is server._supervisor
        assert voice_ingress is server._voice_ingress
        builds += 1
        return gateway

    monkeypatch.setattr(server, "_build_secure_gateway", build)
    monkeypatch.setattr(
        "zara.server._advertised_remote_endpoint",
        lambda endpoint: "tcp://192.0.2.10:17865",
    )

    results: list[dict[str, object]] = []
    errors: list[BaseException] = []

    def ensure() -> None:
        try:
            results.append(server.ensure_remote_listener())
        except BaseException as error:  # pragma: no cover - asserted below
            errors.append(error)

    workers = [threading.Thread(target=ensure) for _ in range(8)]
    for worker in workers:
        worker.start()
    for worker in workers:
        worker.join(timeout=2.0)

    assert not errors
    assert len(results) == 8
    assert all(result == results[0] for result in results)
    assert results[0] == {
        "active": True,
        "endpoint": "tcp://192.0.2.10:17865",
        "server_public_key": public_key,
    }
    assert builds == 1
    assert gateway.start_calls == 1


def test_remote_listener_failed_start_is_not_published_and_can_retry(
    monkeypatch: pytest.MonkeyPatch,
):
    public_key = "B" * 40
    server = _ready_server(monkeypatch, public_key=public_key)
    first = _StartedGateway(fail=True)
    second = _StartedGateway()
    gateways = iter((first, second))
    monkeypatch.setattr(server, "_build_secure_gateway", lambda *args, **kwargs: next(gateways))
    monkeypatch.setattr(
        "zara.server._advertised_remote_endpoint",
        lambda endpoint: "tcp://192.0.2.10:17865",
    )

    with pytest.raises(RuntimeError, match="bind failed"):
        server.ensure_remote_listener()
    assert server.remote_listener_status() == {
        "active": False,
        "endpoint": None,
        "server_public_key": public_key,
    }
    assert first.close_calls == 1

    assert server.ensure_remote_listener()["active"] is True
    assert second.start_calls == 1
