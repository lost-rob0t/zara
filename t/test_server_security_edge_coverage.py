from __future__ import annotations

import argparse
import json
from pathlib import Path
from types import SimpleNamespace

import pytest

import zara.server as server_mod
from zara.server import ServerAlreadyRunning, ServerError, ServerState, ZaraServer


def _args(**overrides):
    values = {
        "security_init": False,
        "security_show_public_key": False,
        "security_enroll_key": None,
        "security_revoke_device": None,
        "security_list_clients": False,
        "security_dir": "/tmp/zara-security",
        "security_device_id": None,
        "runtime_dir": "/tmp/zara-runtime",
        "remote_endpoint": None,
        "endpoint": None,
        "shutdown_timeout": 0.1,
        "verbose": False,
    }
    values.update(overrides)
    return SimpleNamespace(**values)


class _FakeSecurityState:
    def __init__(self, control_socket_path: Path | None = None) -> None:
        self.control_socket_path = control_socket_path or Path("/tmp/fake-zara-control.sock")
        self.initialized = 0
        self.revoked: list[str] = []
        self.enrolled: list[tuple[str, str, object, set[object]]] = []

    def initialize(self) -> None:
        self.initialized += 1

    def server_public_key(self) -> str:
        return "SERVER-PUBLIC-KEY"

    def load_registry(self):
        return {"registry": "canonical"}

    def enroll_client(self, public_key, *, device_id, principal, capabilities):
        self.enrolled.append((public_key, device_id, principal, capabilities))
        return SimpleNamespace(
            device_id=device_id,
            principal=principal,
            public_key=public_key,
            capabilities=capabilities,
            active=True,
        )

    def revoke_device(self, device_id: str) -> None:
        self.revoked.append(device_id)

    def list_clients(self):
        return [{"device_id": "phone", "active": True}]


class _FakeAdmin:
    def __init__(self, responses=None, fail_start: bool = False, fail_close: bool = False):
        self.responses = dict(responses or {})
        self.fail_start = fail_start
        self.fail_close = fail_close
        self.requests: list[tuple[str, dict[str, object]]] = []
        self.binds: list[object] = []
        self.started = 0
        self.closed = 0

    def start(self) -> None:
        self.started += 1
        if self.fail_start:
            raise RuntimeError("admin start failed")

    def bind_registry(self, registry) -> None:
        self.binds.append(registry)

    def request(self, action: str, **payload):
        self.requests.append((action, payload))
        return self.responses.get(action, {"action": action, **payload})

    def close(self, *, timeout: float) -> None:
        assert timeout > 0
        self.closed += 1
        if self.fail_close:
            raise RuntimeError("admin close failed")


@pytest.mark.parametrize(
    ("endpoint", "expected"),
    [
        ("tcp://127.0.0.1:17865", ("127.0.0.1", 17865)),
        ("tcp://host.example:443", ("host.example", 443)),
        ("tcp://[2001:db8::1]:17865", ("2001:db8::1", 17865)),
    ],
)
def test_split_tcp_endpoint_accepts_only_canonical_tcp_forms(endpoint, expected):
    assert server_mod._split_tcp_endpoint(endpoint) == expected


@pytest.mark.parametrize(
    "endpoint",
    [
        "",
        "ipc:///tmp/zara.sock",
        "tcp://",
        "tcp://host",
        "tcp://:17865",
        "tcp://[::1]",
        "tcp://[::1]17865",
        "tcp://host:not-a-port",
        "tcp://host:0",
        "tcp://host:65536",
    ],
)
def test_split_tcp_endpoint_rejects_malformed_or_out_of_range_values(endpoint):
    with pytest.raises(ValueError):
        server_mod._split_tcp_endpoint(endpoint)


def test_local_route_address_closes_probe_and_rejects_unreachable_or_loopback(monkeypatch):
    class Probe:
        def __init__(self, address: str, *, fail: bool = False) -> None:
            self.address = address
            self.fail = fail
            self.closed = False

        def connect(self, target) -> None:
            assert target == ("192.0.2.1", 9)
            if self.fail:
                raise OSError("no route")

        def getsockname(self):
            return (self.address, 12345)

        def close(self) -> None:
            self.closed = True

    good = Probe("10.0.0.4")
    monkeypatch.setattr(server_mod.socket, "socket", lambda *_args: good)
    assert server_mod._local_route_address() == "10.0.0.4"
    assert good.closed

    loopback = Probe("127.0.0.1")
    monkeypatch.setattr(server_mod.socket, "socket", lambda *_args: loopback)
    with pytest.raises(ServerError, match="reachable local address"):
        server_mod._local_route_address()
    assert loopback.closed

    failed = Probe("0.0.0.0", fail=True)
    monkeypatch.setattr(server_mod.socket, "socket", lambda *_args: failed)
    with pytest.raises(ServerError, match="reachable local address"):
        server_mod._local_route_address()
    assert failed.closed


def test_advertised_remote_endpoint_preserves_specific_hosts_and_bounds_overrides(monkeypatch):
    monkeypatch.delenv("ZARA_ADVERTISE_HOST", raising=False)
    assert server_mod._advertised_remote_endpoint("tcp://10.1.2.3:17865") == "tcp://10.1.2.3:17865"
    assert server_mod._advertised_remote_endpoint("tcp://[2001:db8::2]:17865") == "tcp://[2001:db8::2]:17865"

    monkeypatch.setenv("ZARA_ADVERTISE_HOST", "[2001:db8::9]")
    assert server_mod._advertised_remote_endpoint("tcp://0.0.0.0:17865") == "tcp://[2001:db8::9]:17865"

    monkeypatch.setenv("ZARA_ADVERTISE_HOST", "bad host")
    with pytest.raises(ServerError, match="invalid"):
        server_mod._advertised_remote_endpoint("tcp://0.0.0.0:17865")

    monkeypatch.setenv("ZARA_ADVERTISE_HOST", "x" * 256)
    with pytest.raises(ServerError, match="invalid"):
        server_mod._advertised_remote_endpoint("tcp://0.0.0.0:17865")


def test_secure_server_constructor_rejects_parallel_or_unauthenticated_authorities(monkeypatch):
    calls: list[tuple[str, bool, bool]] = []
    monkeypatch.setattr(
        "zara.security.validate_listener_security",
        lambda endpoint, *, curve_enabled, zap_enabled: calls.append(
            (endpoint, curve_enabled, zap_enabled)
        ),
    )
    state = _FakeSecurityState()

    with pytest.raises(ValueError, match="explicit security state"):
        ZaraServer(supervisor=object(), endpoint="tcp://127.0.0.1:17865")
    with pytest.raises(ValueError, match="explicit security state"):
        ZaraServer(supervisor=object(), remote_endpoint="tcp://0.0.0.0:17865")
    with pytest.raises(ValueError, match="remote endpoint must use TCP"):
        ZaraServer(supervisor=object(), remote_endpoint="ipc:///tmp/remote.sock", security_state=state)
    with pytest.raises(ValueError, match="local IPC"):
        ZaraServer(
            supervisor=object(),
            endpoint="tcp://127.0.0.1:17865",
            remote_endpoint="tcp://0.0.0.0:17865",
            security_state=state,
        )
    with pytest.raises(ValueError, match="custom gateway"):
        ZaraServer(
            supervisor=object(),
            remote_endpoint="tcp://0.0.0.0:17865",
            security_state=state,
            gateway_factory=lambda *args, **kwargs: object(),
        )
    with pytest.raises(ValueError, match="custom gateway"):
        ZaraServer(
            supervisor=object(),
            endpoint="tcp://127.0.0.1:17865",
            security_state=state,
            gateway_factory=lambda *args, **kwargs: object(),
        )

    secure = ZaraServer(
        supervisor=object(),
        endpoint="tcp://127.0.0.1:17865",
        security_state=state,
    )
    assert secure._secure_tcp is True
    assert secure._endpoint_override == "tcp://127.0.0.1:17865"
    assert calls[-1] == ("tcp://127.0.0.1:17865", True, True)


def test_security_state_and_control_socket_reuse_single_canonical_owner(monkeypatch, tmp_path):
    created: list[Path] = []

    class Persistent:
        def __init__(self, path):
            created.append(Path(path))
            self.control_socket_path = tmp_path / "security-control.sock"

    monkeypatch.setattr("zara.security_state.PersistentSecurityState", Persistent)
    monkeypatch.setenv("ZARA_SECURITY_DIR", str(tmp_path / "security"))
    server = ZaraServer(supervisor=object())
    first = server._security_state_object()
    second = server._security_state_object()
    assert first is second
    assert created == [tmp_path / "security"]

    secure = ZaraServer(
        supervisor=object(),
        endpoint="tcp://127.0.0.1:17865",
        security_state=_FakeSecurityState(tmp_path / "secure-control.sock"),
    )
    assert secure._control_socket_path() == tmp_path / "secure-control.sock"

    local = ZaraServer(supervisor=object(), runtime_dir=tmp_path / "runtime")
    local._lease._path = tmp_path / "runtime" / "zara-server.lock"
    assert local._control_socket_path() == tmp_path / "runtime" / "zara-control.sock"


def test_security_admin_start_and_registry_binding_are_fail_closed(monkeypatch, tmp_path):
    state = _FakeSecurityState(tmp_path / "control.sock")
    server = ZaraServer(supervisor=object(), security_state=state)
    server._security_state_object = lambda: state
    server._control_socket_path = lambda: tmp_path / "control.sock"

    created: list[_FakeAdmin] = []

    class Admin(_FakeAdmin):
        def __init__(self, *_args, **_kwargs):
            super().__init__()
            created.append(self)

    monkeypatch.setattr("zara.security_admin.SecurityAdminServer", Admin)
    admin = server._ensure_security_admin()
    assert admin is server._ensure_security_admin()
    assert admin.started == 1

    registry = server._ensure_security_registry()
    assert registry == {"registry": "canonical"}
    assert state.initialized == 1
    assert admin.binds == [registry]
    assert server._ensure_security_registry() is registry

    server._security_admin_closing = True
    with pytest.raises(server_mod.ServerStateError, match="shutting down"):
        server._ensure_security_admin()

    broken = ZaraServer(supervisor=object(), security_state=state)
    broken._security_state_object = lambda: state
    broken._control_socket_path = lambda: tmp_path / "broken.sock"

    class BrokenAdmin(_FakeAdmin):
        def __init__(self, *_args, **_kwargs):
            super().__init__(fail_start=True)

    monkeypatch.setattr("zara.security_admin.SecurityAdminServer", BrokenAdmin)
    with pytest.raises(RuntimeError, match="admin start failed"):
        broken._ensure_security_admin()
    assert broken._security_admin is None


def test_security_admin_close_failure_clears_cached_authority():
    server = ZaraServer(supervisor=object())
    admin = _FakeAdmin(fail_close=True)
    server._security_admin = admin
    server._security_registry = object()

    assert server._close_security_admin() is False
    assert admin.closed == 1
    assert server._security_admin is None
    assert server._security_registry is None
    assert server._close_security_admin() is True


def test_live_security_admin_prefers_runtime_control_socket(monkeypatch, tmp_path):
    runtime_dir = tmp_path / "runtime"
    runtime_dir.mkdir()
    runtime_socket = runtime_dir / "zara-control.sock"
    runtime_socket.write_text("placeholder")
    state = _FakeSecurityState(tmp_path / "state-control.sock")
    clients: list[Path] = []

    class Client:
        def __init__(self, path):
            clients.append(Path(path))

    monkeypatch.setattr("zara.security_admin.SecurityAdminClient", Client)
    assert isinstance(server_mod._live_security_admin(state, runtime_dir=runtime_dir), Client)
    assert clients[-1] == runtime_socket

    runtime_socket.unlink()
    state.control_socket_path.write_text("placeholder")
    assert isinstance(server_mod._live_security_admin(state, runtime_dir=runtime_dir), Client)
    assert clients[-1] == state.control_socket_path

    state.control_socket_path.unlink()
    assert server_mod._live_security_admin(state, runtime_dir=runtime_dir) is None


def test_require_daemon_offline_releases_probe_and_refuses_live_lease(monkeypatch):
    instances = []

    class Lease:
        def __init__(self, runtime_dir):
            self.runtime_dir = runtime_dir
            self.held = False
            self.released = 0
            instances.append(self)

        def acquire(self):
            self.held = True

        def release(self):
            self.held = False
            self.released += 1

    monkeypatch.setattr(server_mod, "ServerLease", Lease)
    server_mod._require_daemon_offline(_args())
    assert instances[-1].released == 1

    class BusyLease(Lease):
        def acquire(self):
            raise ServerAlreadyRunning("busy")

    monkeypatch.setattr(server_mod, "ServerLease", BusyLease)
    with pytest.raises(RuntimeError, match="refusing disk-only security mutation"):
        server_mod._require_daemon_offline(_args())


def test_security_management_exercises_offline_and_live_owner_paths(monkeypatch, capsys):
    state = _FakeSecurityState()
    offline_checks: list[str] = []
    monkeypatch.setattr(server_mod, "_security_state", lambda _args: state)
    monkeypatch.setattr(
        server_mod,
        "_require_daemon_offline",
        lambda args: offline_checks.append(args.runtime_dir),
    )
    monkeypatch.setattr(server_mod, "_live_security_admin", lambda *_args, **_kwargs: None)

    assert server_mod._run_security_management(_args()) is None

    assert server_mod._run_security_management(_args(security_init=True)) == 0
    assert capsys.readouterr().out.strip() == "SERVER-PUBLIC-KEY"
    assert state.initialized == 1

    assert server_mod._run_security_management(_args(security_show_public_key=True)) == 0
    assert capsys.readouterr().out.strip() == "SERVER-PUBLIC-KEY"

    with pytest.raises(ValueError, match="device-id"):
        server_mod._run_security_management(
            _args(security_enroll_key="A" * 40)
        )

    assert server_mod._run_security_management(
        _args(security_enroll_key="A" * 40, security_device_id="phone")
    ) == 0
    enrolled = json.loads(capsys.readouterr().out)
    assert enrolled["device_id"] == "phone"
    assert enrolled["active"] is True
    assert state.enrolled[-1][0] == "A" * 40

    assert server_mod._run_security_management(
        _args(security_revoke_device="phone")
    ) == 0
    assert json.loads(capsys.readouterr().out) == {"active": False, "device_id": "phone"}
    assert state.revoked == ["phone"]

    assert server_mod._run_security_management(_args(security_list_clients=True)) == 0
    assert json.loads(capsys.readouterr().out) == [{"active": True, "device_id": "phone"}]
    assert len(offline_checks) == 4

    monkeypatch.setattr(server_mod, "_security_state", lambda _args: None)
    with pytest.raises(ValueError, match="requires --security-dir"):
        server_mod._run_security_management(_args(security_init=True))


def test_security_management_uses_live_admin_without_disk_mutation(monkeypatch, capsys):
    state = _FakeSecurityState()
    admin = _FakeAdmin(
        {
            "enroll": {"device_id": "phone", "active": True},
            "revoke": {"device_id": "phone", "active": False},
            "list": [{"device_id": "phone", "active": True}],
        }
    )
    monkeypatch.setattr(server_mod, "_security_state", lambda _args: state)
    monkeypatch.setattr(server_mod, "_live_security_admin", lambda *_args, **_kwargs: admin)
    monkeypatch.setattr(
        server_mod,
        "_require_daemon_offline",
        lambda _args: pytest.fail("live admin must own security mutation"),
    )

    assert server_mod._run_security_management(
        _args(security_enroll_key="A" * 40, security_device_id="phone")
    ) == 0
    assert json.loads(capsys.readouterr().out)["active"] is True

    assert server_mod._run_security_management(
        _args(security_revoke_device="phone")
    ) == 0
    assert json.loads(capsys.readouterr().out)["active"] is False

    assert server_mod._run_security_management(_args(security_list_clients=True)) == 0
    assert json.loads(capsys.readouterr().out)[0]["device_id"] == "phone"
    assert [action for action, _payload in admin.requests] == ["enroll", "revoke", "list"]


def test_server_main_fail_closed_management_and_transport_paths(monkeypatch, capsys):
    class Parser:
        def __init__(self, args):
            self.args = args

        def parse_args(self, _argv):
            return self.args

    monkeypatch.setattr(server_mod.signal, "signal", lambda *_args: None)

    monkeypatch.setattr(server_mod, "_parser", lambda: Parser(_args()))
    monkeypatch.setattr(server_mod, "_run_security_management", lambda _args: 0)
    assert server_mod.main([]) == 0

    monkeypatch.setattr(
        server_mod,
        "_run_security_management",
        lambda _args: (_ for _ in ()).throw(ValueError("bad management")),
    )
    assert server_mod.main([]) == 2
    assert "bad management" in capsys.readouterr().err

    monkeypatch.setattr(server_mod, "_run_security_management", lambda _args: None)
    monkeypatch.setattr(server_mod, "_security_state", lambda _args: None)

    monkeypatch.setattr(
        server_mod,
        "_parser",
        lambda: Parser(_args(remote_endpoint="tcp://0.0.0.0:17865")),
    )
    assert server_mod.main([]) == 2
    assert "requires --security-dir" in capsys.readouterr().err

    monkeypatch.setattr(
        server_mod,
        "_parser",
        lambda: Parser(_args(endpoint="tcp://127.0.0.1:17865")),
    )
    assert server_mod.main([]) == 2
    assert "requires --security-dir" in capsys.readouterr().err


def test_server_main_maps_constructor_run_interrupt_and_fatal_failures(monkeypatch, capsys):
    class Parser:
        def parse_args(self, _argv):
            return _args()

    monkeypatch.setattr(server_mod, "_parser", lambda: Parser())
    monkeypatch.setattr(server_mod, "_run_security_management", lambda _args: None)
    monkeypatch.setattr(server_mod, "_security_state", lambda _args: None)
    monkeypatch.setattr(server_mod.signal, "signal", lambda *_args: None)

    monkeypatch.setattr(
        server_mod,
        "ZaraServer",
        lambda **_kwargs: (_ for _ in ()).throw(ValueError("constructor rejected")),
    )
    assert server_mod.main([]) == 2
    assert "constructor rejected" in capsys.readouterr().err

    class FakeServer:
        def __init__(self, failure, *, clean=True):
            self.failure = failure
            self.clean = clean
            self.stop_calls = 0

        def run(self, _event):
            raise self.failure

        def stop(self):
            self.stop_calls += 1
            return self.clean

    running = FakeServer(ServerAlreadyRunning("already running"))
    monkeypatch.setattr(server_mod, "ZaraServer", lambda **_kwargs: running)
    assert server_mod.main([]) == 2
    assert "already running" in capsys.readouterr().err

    interrupted = FakeServer(KeyboardInterrupt(), clean=True)
    monkeypatch.setattr(server_mod, "ZaraServer", lambda **_kwargs: interrupted)
    assert server_mod.main([]) == 0
    assert interrupted.stop_calls == 1

    unclean_interrupt = FakeServer(KeyboardInterrupt(), clean=False)
    monkeypatch.setattr(server_mod, "ZaraServer", lambda **_kwargs: unclean_interrupt)
    assert server_mod.main([]) == 1

    fatal = FakeServer(RuntimeError("boom"))
    monkeypatch.setattr(server_mod, "ZaraServer", lambda **_kwargs: fatal)
    assert server_mod.main([]) == 1
    assert fatal.stop_calls == 1
