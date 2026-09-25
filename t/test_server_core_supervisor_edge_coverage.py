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
    def __init__(
        self,
        *,
        start_error: BaseException | None = None,
        shutdown_error: BaseException | None = None,
        shutdown_raises: BaseException | None = None,
        alive_after_join: bool = False,
    ) -> None:
        self.state = RuntimeHostState.NEW
        self.is_alive = False
        self.start_error = start_error
        self.shutdown_error = shutdown_error
        self.shutdown_raises = shutdown_raises
        self.alive_after_join = alive_after_join
        self.shutdown_calls = 0
        self.join_calls = 0

    def start(self):
        self.is_alive = True
        if self.start_error is not None:
            self.state = RuntimeHostState.DEGRADED
            return _failed(self.start_error)
        self.state = RuntimeHostState.RUNNING
        return _completed()

    def shutdown(self, reason=""):
        self.shutdown_calls += 1
        if self.shutdown_raises is not None:
            raise self.shutdown_raises
        if self.shutdown_error is not None:
            return _failed(self.shutdown_error)
        self.state = RuntimeHostState.STOPPED
        self.is_alive = False
        return _completed(reason)

    def join(self, timeout=None):
        assert timeout is None or timeout >= 0
        self.join_calls += 1
        self.is_alive = self.alive_after_join


def test_ipc_endpoint_validation_and_long_path_fallback_are_fail_closed(monkeypatch, tmp_path):
    monkeypatch.setattr(core, "_ipc_path_limit", lambda: 16)

    with pytest.raises(ValueError, match="require authentication"):
        core._validate_ipc_endpoint("tcp://127.0.0.1:1")
    with pytest.raises(ValueError, match="must not be empty"):
        core._validate_ipc_endpoint("ipc://")
    with pytest.raises(ValueError, match="too long"):
        core._validate_ipc_endpoint("ipc:///this/path/is/definitely/too/long")

    fallback = Path("/tmp/z.sock")
    seen: list[Path] = []
    monkeypatch.setattr(
        core,
        "_private_ipc_fallback",
        lambda runtime_dir: seen.append(runtime_dir) or fallback,
    )
    long_runtime_path = tmp_path / ("x" * 40)
    endpoint = core.default_zmq_endpoint(long_runtime_path)
    assert endpoint == f"ipc://{fallback}"
    assert seen == [long_runtime_path]


def test_principal_runtime_health_tracks_startup_and_host_state():
    principal = PrincipalContext("owner")
    host = _Host()
    slot = core.PrincipalRuntime(principal=principal, host=host, bus=bridge.RuntimeEventBus())
    assert slot.healthy is False

    host.state = RuntimeHostState.RUNNING
    assert slot.healthy is True

    slot.startup_error = RuntimeError("degraded")
    assert slot.healthy is False


def test_supervisor_rejects_invalid_states_missing_principals_and_duplicate_start():
    with pytest.raises(ValueError, match="at least 1"):
        core.RuntimeSupervisor(max_active_principals=0)

    supervisor = core.RuntimeSupervisor(host_factory=lambda _principal, _bus: _Host())
    owner = PrincipalContext("owner")

    with pytest.raises(core.ServerStateError, match="not accepting"):
        supervisor.open_principal(owner)
    with pytest.raises(KeyError):
        supervisor.runtime(owner)

    slot = supervisor.start(owner)
    with pytest.raises(core.ServerStateError, match="cannot start"):
        supervisor.start(owner)

    assert supervisor.open_principal(owner) is slot
    assert supervisor.shutdown()


def test_supervisor_shutdown_collects_request_future_and_liveness_failures():
    hosts = {
        "raise": _Host(shutdown_raises=RuntimeError("request failed")),
        "future": _Host(shutdown_error=RuntimeError("future failed")),
        "alive": _Host(alive_after_join=True),
    }

    def factory(principal, _bus):
        return hosts[principal.principal_id]

    supervisor = core.RuntimeSupervisor(
        host_factory=factory,
        max_active_principals=3,
        shutdown_timeout=0.1,
    )
    supervisor.start(PrincipalContext("raise"))
    supervisor.open_principal(PrincipalContext("future"))
    supervisor.open_principal(PrincipalContext("alive"))

    assert supervisor.shutdown() is False
    assert supervisor.state is core.ServerState.FAILED
    assert supervisor.principals == ()
    assert hosts["raise"].shutdown_calls == 1
    assert hosts["future"].join_calls == 1
    assert hosts["alive"].is_alive is True


def test_default_prolog_factory_returns_engine_or_none_without_cross_principal_state(monkeypatch):
    created: list[Path] = []

    class Engine:
        def __init__(self, path):
            created.append(Path(path))

    monkeypatch.setattr("zara.prolog_engine.locate_main_pl", lambda: Path("/tmp/main.pl"))
    monkeypatch.setattr("zara.prolog_engine.PrologEngine", Engine)
    principal = PrincipalContext("owner")
    engine = core.RuntimeSupervisor._default_prolog_factory(principal)
    assert isinstance(engine, Engine)
    assert created == [Path("/tmp/main.pl")]

    monkeypatch.setattr(
        "zara.prolog_engine.locate_main_pl",
        lambda: (_ for _ in ()).throw(RuntimeError("prolog missing")),
    )
    assert core.RuntimeSupervisor._default_prolog_factory(principal) is None


def test_default_host_composition_keeps_one_runtime_and_optional_router(monkeypatch):
    class Config:
        def get_module_search_paths(self):
            return ["/plugins/a", "/plugins/b"]

    captured: list[dict[str, object]] = []
    captured_backends: list[tuple[object, object]] = []
    routers: list[object] = []

    class CapturedHost:
        def __init__(self, **kwargs):
            captured.append(kwargs)

    class CapturedBackend:
        def __init__(self, manager_factory, *, router):
            captured_backends.append((manager_factory, router))

    class Router:
        def __init__(self, engine, *, wake_words, principal_id):
            self.engine = engine
            self.wake_words = wake_words
            self.principal_id = principal_id
            routers.append(self)

    monkeypatch.setattr(core, "RuntimeHost", CapturedHost)
    monkeypatch.setattr(core, "AgentRuntimeBackend", CapturedBackend)
    monkeypatch.setattr("zara.runtime.intent_router.PrologFirstRouter", Router)
    monkeypatch.setattr("zara.wake_words.resolve_wake_words", lambda _config, _engine: ("zara",))

    principal = PrincipalContext("owner")
    bus = bridge.RuntimeEventBus()
    engine = object()
    supervisor = core.RuntimeSupervisor(
        config=Config(),
        prolog_factory=lambda _principal: engine,
    )

    host = supervisor._build_default_host(principal, bus)
    assert isinstance(host, CapturedHost)
    kwargs = captured[-1]
    assert kwargs["publisher"] == bus.publish
    assert kwargs["subscriber"] == bus.subscribe
    assert kwargs["plugin_paths"] == ("/plugins/a", "/plugins/b")

    backend = kwargs["backend_factory"]()
    assert isinstance(backend, CapturedBackend)
    assert len(captured_backends) == 1
    manager_factory, captured_router = captured_backends[0]
    assert callable(manager_factory)
    assert captured_router is routers[0]
    assert captured_router.engine is engine
    assert captured_router.principal_id == "owner"


def test_server_lease_runtime_directory_selection_is_owner_scoped(monkeypatch, tmp_path):
    explicit = core.ServerLease(tmp_path / "explicit")
    assert explicit._runtime_dir() == tmp_path / "explicit"

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path / "xdg"))
    assert core.ServerLease()._runtime_dir() == tmp_path / "xdg" / "zarathushtra"

    monkeypatch.setenv("XDG_RUNTIME_DIR", "relative")
    monkeypatch.setattr(core.tempfile, "gettempdir", lambda: str(tmp_path / "tmp"))
    assert core.ServerLease()._runtime_dir() == tmp_path / "tmp" / f"zarathushtra-{core.os.getuid()}"


def test_core_server_audio_configuration_and_tts_factory_are_bounded(monkeypatch):
    class Config:
        def __init__(self, daemon, tts):
            self.daemon = daemon
            self.tts = tts

        def get_section(self, name):
            if name == "daemon":
                return self.daemon
            if name == "tts":
                return self.tts
            raise AssertionError(name)

    configured = core.ZaraServer(
        supervisor=object(),
        config=Config(
            {"audio_output_sample_rate": "48000"},
            {"provider": "test-provider", "voice": "local"},
        ),
    )
    assert configured._audio_output_sample_rate() == 48000

    invalid = core.ZaraServer(
        supervisor=object(),
        config=Config({"audio_output_sample_rate": 0}, {}),
    )
    assert invalid._audio_output_sample_rate() == 24000

    built = []

    class Engine:
        def __init__(self, *, provider, config):
            built.append((provider, config))

    monkeypatch.setattr("zara.tts.engine.TTSEngine", Engine)
    assert isinstance(configured._build_tts_engine(), Engine)
    assert built == [
        ("test-provider", {"tts": {"provider": "test-provider", "voice": "local"}})
    ]


def test_core_server_gateway_start_failure_rolls_back_runtime_and_lease(tmp_path):
    host = _Host()
    supervisor = core.RuntimeSupervisor(
        host_factory=lambda _principal, _bus: host,
        shutdown_timeout=0.2,
    )
    lease = core.ServerLease(tmp_path / "runtime")

    class Gateway:
        def __init__(self):
            self.close_calls = 0

        def start(self):
            return _failed(RuntimeError("gateway bind failed"))

        def close(self, *, timeout):
            assert timeout > 0
            self.close_calls += 1

    gateway = Gateway()
    server = core.ZaraServer(
        supervisor=supervisor,
        lease=lease,
        gateway_factory=lambda *_args, **_kwargs: gateway,
        shutdown_timeout=0.2,
    )

    with pytest.raises(RuntimeError, match="gateway bind failed"):
        server.start()

    assert gateway.close_calls == 1
    assert host.shutdown_calls == 1
    assert host.join_calls == 1
    assert server.state is core.ServerState.FAILED
    assert lease.held is False


def test_core_server_close_helpers_fail_closed_and_clear_component_handles():
    server = core.ZaraServer(supervisor=object())

    class BrokenVoice:
        def close(self, *, timeout):
            assert timeout > 0
            raise RuntimeError("voice close failed")

    class BrokenTts:
        def stop(self, *, timeout):
            assert timeout > 0
            raise RuntimeError("tts stop failed")

    server._voice_ingress = BrokenVoice()
    server._tts_bridge = BrokenTts()

    assert server._close_voice_ingress() is False
    assert server._voice_ingress is None
    assert server._close_tts_bridge() is False
    assert server._tts_bridge is None
    assert server._close_voice_ingress() is True
    assert server._close_tts_bridge() is True


def test_core_server_stop_aggregates_gateway_and_supervisor_cleanup_failures():
    class Supervisor:
        def shutdown(self):
            raise RuntimeError("supervisor shutdown failed")

    class Lease:
        def __init__(self):
            self.release_calls = 0

        def release(self):
            self.release_calls += 1

    class Gateway:
        def close(self, *, timeout):
            assert timeout > 0
            raise RuntimeError("gateway close failed")

    lease = Lease()
    server = core.ZaraServer(supervisor=Supervisor(), lease=lease)
    server._state = core.ServerState.READY
    server._gateway = Gateway()

    assert server.stop() is False
    assert server.state is core.ServerState.FAILED
    assert server._gateway is None
    assert lease.release_calls == 1
