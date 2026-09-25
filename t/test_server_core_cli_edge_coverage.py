from __future__ import annotations

from pathlib import Path

import pytest

import zara.server_core as core
from zara.principals import PrincipalContext
from zara.runtime.host import RuntimeHostState


class _Host:
    def __init__(self) -> None:
        self.state = RuntimeHostState.NEW
        self.is_alive = False

    def start(self):
        raise AssertionError("host start must not run while stopping")


def test_supervisor_stopping_fence_and_server_principal_property():
    supervisor = core.RuntimeSupervisor(host_factory=lambda _principal, _bus: _Host())
    supervisor._state = core.ServerState.STOPPING

    with pytest.raises(core.ServerStateError, match="stopping"):
        supervisor._open_runtime(PrincipalContext("owner"))

    principal = PrincipalContext("owner")
    server = core.ZaraServer(supervisor=object(), principal=principal)
    assert server.principal is principal


def test_server_lease_reuses_held_lock(tmp_path):
    lease = core.ServerLease(tmp_path / "lease")
    try:
        first = lease.acquire()
        assert lease.acquire() == first
    finally:
        lease.release()


def test_server_main_maps_success_and_terminal_failures(monkeypatch, tmp_path, capsys):
    monkeypatch.setattr(core.signal, "signal", lambda *_args: None)

    class Server:
        mode = "success"
        last = None

        def __init__(self, **_kwargs):
            type(self).last = self
            self.stop_calls = 0

        def run(self, _stop_event):
            if self.mode == "running":
                raise core.ServerAlreadyRunning("already running")
            if self.mode == "interrupt":
                raise KeyboardInterrupt
            if self.mode == "error":
                raise RuntimeError("boom")
            return 0

        def stop(self):
            self.stop_calls += 1
            return True

    monkeypatch.setattr(core, "ZaraServer", Server)

    Server.mode = "success"
    assert core.main(["--runtime-dir", str(Path(tmp_path))]) == 0

    Server.mode = "running"
    assert core.main([]) == 2
    assert "already running" in capsys.readouterr().err

    Server.mode = "interrupt"
    assert core.main([]) == 0
    assert Server.last.stop_calls == 1

    Server.mode = "error"
    assert core.main(["--verbose"]) == 1
    assert Server.last.stop_calls == 1
