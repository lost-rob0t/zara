from __future__ import annotations

import concurrent.futures
from pathlib import Path

import pytest

import zara.server_core as core
from zara.principals import PrincipalContext
from zara.runtime import bridge
from zara.runtime.host import RuntimeHostState


def _completed(value=None):
    future = concurrent.futures.Future()
    future.set_result(value)
    return future


def _failed(error: BaseException):
    future = concurrent.futures.Future()
    future.set_exception(error)
    return future


class _Host:
    def __init__(self, start_error: BaseException | None = None) -> None:
        self.state = RuntimeHostState.NEW
        self.is_alive = False
        self.start_error = start_error

    def start(self):
        self.is_alive = True
        if self.start_error is not None:
            self.state = RuntimeHostState.DEGRADED
            return _failed(self.start_error)
        self.state = RuntimeHostState.RUNNING
        return _completed()

    def shutdown(self, reason=""):
        self.state = RuntimeHostState.STOPPED
        self.is_alive = False
        return _completed(reason)

    def join(self, timeout=None):
        assert timeout is None or timeout >= 0


def test_global_config_composition_and_tts_fallback_use_canonical_config(monkeypatch):
    class Config:
        def get_module_search_paths(self):
            return []

        def get_section(self, name):
            assert name == "tts"
            return {"provider": "local-test"}

    config = Config()
    monkeypatch.setattr("zara.config.get_config", lambda: config)

    captured_host: dict[str, object] = {}

    class Host:
        def __init__(self, **kwargs):
            captured_host.update(kwargs)

    monkeypatch.setattr(core, "RuntimeHost", Host)
    supervisor = core.RuntimeSupervisor(
        config=None,
        prolog_factory=lambda _principal: None,
    )
    bus = bridge.RuntimeEventBus()
    host = supervisor._build_default_host(PrincipalContext("owner"), bus)

    assert isinstance(host, Host)
    assert captured_host["config"] is config
    assert captured_host["plugin_paths"] == ()
    assert callable(captured_host["backend_factory"])

    built_tts: list[tuple[str, dict[str, object]]] = []

    class TtsEngine:
        def __init__(self, *, provider, config):
            built_tts.append((provider, config))

    monkeypatch.setattr("zara.tts.engine.TTSEngine", TtsEngine)
    server = core.ZaraServer(supervisor=object(), config=None)
    assert isinstance(server._build_tts_engine(), TtsEngine)
    assert built_tts == [
        ("local-test", {"tts": {"provider": "local-test"}}),
    ]

    with pytest.raises(ValueError, match="non-empty"):
        core.ZaraServer(supervisor=object(), endpoint="   ")

    server._state = core.ServerState.STARTING
    with pytest.raises(core.ServerStateError, match="cannot start"):
        server.start()


def test_supervisor_degraded_admission_and_empty_shutdown_are_explicit():
    healthy = _Host()
    degraded = _Host(start_error=RuntimeError("runtime degraded"))

    def factory(principal, _bus):
        return healthy if principal.principal_id == "owner" else degraded

    supervisor = core.RuntimeSupervisor(
        host_factory=factory,
        max_active_principals=2,
        shutdown_timeout=0.1,
    )
    supervisor.start(PrincipalContext("owner"))
    slot = supervisor.open_principal(PrincipalContext("degraded"))

    assert isinstance(slot.startup_error, RuntimeError)
    assert supervisor.state is core.ServerState.DEGRADED
    assert supervisor.shutdown() is True

    empty = core.RuntimeSupervisor(host_factory=lambda _principal, _bus: _Host())
    assert empty.shutdown() is True
    assert empty.state is core.ServerState.STOPPED
    assert empty.shutdown() is True


def test_start_cleanup_failures_still_release_lease_and_fail_closed():
    class Supervisor:
        state = core.ServerState.READY

        def start(self, _principal):
            return None

        def shutdown(self):
            raise RuntimeError("rollback failed")

    class Lease:
        def __init__(self):
            self.release_calls = 0

        def acquire(self):
            return Path("/tmp/zara-server.lock")

        def release(self):
            self.release_calls += 1

    class Gateway:
        def start(self):
            return _failed(RuntimeError("gateway failed"))

        def close(self, *, timeout):
            assert timeout > 0
            raise RuntimeError("gateway close failed")

    lease = Lease()
    server = core.ZaraServer(
        supervisor=Supervisor(),
        lease=lease,
        endpoint="ipc:///tmp/z.sock",
        gateway_factory=lambda *_args, **_kwargs: Gateway(),
        shutdown_timeout=0.1,
    )

    with pytest.raises(RuntimeError, match="gateway failed"):
        server.start()

    assert server.state is core.ServerState.FAILED
    assert lease.release_calls == 1
    assert server._gateway is None


def test_run_maps_cleanup_truth_to_process_exit_status(monkeypatch):
    stop_event = core.threading.Event()
    stop_event.set()

    failed = core.ZaraServer(supervisor=object())
    monkeypatch.setattr(
        failed,
        "start",
        lambda: setattr(failed, "_state", core.ServerState.DEGRADED)
        or core.ServerState.DEGRADED,
    )
    monkeypatch.setattr(failed, "stop", lambda: False)
    assert failed.run(stop_event) == 1

    clean = core.ZaraServer(supervisor=object())
    monkeypatch.setattr(
        clean,
        "start",
        lambda: setattr(clean, "_state", core.ServerState.READY)
        or core.ServerState.READY,
    )
    monkeypatch.setattr(clean, "stop", lambda: True)
    assert clean.run(stop_event) == 0
