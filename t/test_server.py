import concurrent.futures
import os
import stat
import threading

import pytest

from zara.runtime import bridge
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.server import (
    PrincipalContext,
    PrincipalLimitExceeded,
    RuntimeSupervisor,
    ServerAlreadyRunning,
    ServerLease,
    ServerState,
    ZaraServer,
)


def completed(value=None):
    future = concurrent.futures.Future()
    future.set_result(value)
    return future


def failed(error):
    future = concurrent.futures.Future()
    future.set_exception(error)
    return future


class FakeHost:
    def __init__(self, *, start_error=None):
        self.state = RuntimeHostState.NEW
        self.is_alive = False
        self.start_error = start_error
        self.shutdown_calls = 0
        self.join_calls = 0

    def start(self):
        self.is_alive = True
        if self.start_error is not None:
            self.state = RuntimeHostState.DEGRADED
            return failed(self.start_error)
        self.state = RuntimeHostState.RUNNING
        return completed(None)

    def submit(self, command):
        return completed(command)

    def shutdown(self, reason=""):
        self.shutdown_calls += 1
        self.state = RuntimeHostState.STOPPED
        self.is_alive = False
        return completed(reason)

    def join(self, timeout=None):
        self.join_calls += 1


class BlockingBackend(RuntimeBackend):
    def __init__(self):
        self.entered = threading.Event()
        self.cancelled = threading.Event()
        self.stopped = threading.Event()

    async def start(self):
        return None

    async def submit_turn(self, text, *, turn_id, conversation_id=None, context_ids=()):
        import asyncio

        self.entered.set()
        await asyncio.Event().wait()
        return RuntimeTurnResult(response="late")

    async def cancel_turn(self, turn_id):
        self.cancelled.set()

    async def stop(self):
        self.stopped.set()


def test_server_lease_uses_kernel_lock_and_owner_private_directory(tmp_path):
    runtime_dir = tmp_path / "runtime"
    first = ServerLease(runtime_dir)
    second = ServerLease(runtime_dir)

    path = first.acquire()
    assert path.name == "zara-server.lock"
    assert stat.S_IMODE(os.stat(runtime_dir).st_mode) == 0o700
    assert stat.S_IMODE(os.stat(path).st_mode) == 0o600

    with pytest.raises(ServerAlreadyRunning):
        second.acquire()

    first.release()
    second.acquire()
    second.release()


def test_supervisor_requires_explicit_principal_and_defaults_to_single_principal():
    hosts = {}

    def factory(principal, _bus):
        host = FakeHost()
        hosts[principal.principal_id] = host
        return host

    supervisor = RuntimeSupervisor(host_factory=factory, shutdown_timeout=0.2)
    owner = PrincipalContext("owner")
    other = PrincipalContext("other")

    with pytest.raises(TypeError):
        supervisor.start("owner")

    supervisor.start(owner)
    assert supervisor.state is ServerState.READY
    assert supervisor.runtime(owner).principal is owner

    with pytest.raises(PrincipalLimitExceeded):
        supervisor.open_principal(other)

    assert supervisor.shutdown()
    assert hosts["owner"].shutdown_calls == 1
    assert hosts["owner"].join_calls == 1
    assert supervisor.state is ServerState.STOPPED


def test_synthetic_multi_principal_slots_have_separate_hosts_and_buses():
    def factory(_principal, _bus):
        return FakeHost()

    supervisor = RuntimeSupervisor(
        host_factory=factory,
        max_active_principals=2,
        shutdown_timeout=0.2,
    )
    first = PrincipalContext("a")
    second = PrincipalContext("b")

    first_slot = supervisor.start(first)
    second_slot = supervisor.open_principal(second)

    assert first_slot.host is not second_slot.host
    assert first_slot.bus is not second_slot.bus
    assert supervisor.principals == (first, second)
    assert supervisor.shutdown()


def test_failed_runtime_start_degrades_supervisor_without_losing_shutdown_control():
    host = FakeHost(start_error=RuntimeError("backend unavailable"))
    supervisor = RuntimeSupervisor(
        host_factory=lambda _principal, _bus: host,
        shutdown_timeout=0.2,
    )

    slot = supervisor.start(PrincipalContext("owner"))
    assert slot.startup_error is not None
    assert supervisor.state is ServerState.DEGRADED
    assert supervisor.shutdown()
    assert host.shutdown_calls == 1
    assert host.join_calls == 1


def test_zara_server_acquires_lease_before_runtime_and_releases_after_stop(tmp_path):
    host = FakeHost()
    supervisor = RuntimeSupervisor(
        host_factory=lambda _principal, _bus: host,
        shutdown_timeout=0.2,
    )
    lease = ServerLease(tmp_path / "runtime")
    server = ZaraServer(supervisor=supervisor, lease=lease)

    assert server.start() is ServerState.READY
    assert lease.held
    assert server.stop()
    assert server.state is ServerState.STOPPED
    assert not lease.held
    assert host.join_calls == 1


def test_supervisor_shutdown_cancels_active_runtime_turn():
    backend = BlockingBackend()

    def factory(_principal, bus: bridge.RuntimeEventBus):
        return RuntimeHost(
            backend_factory=lambda: backend,
            publisher=bus.publish,
            subscriber=bus.subscribe,
            shutdown_timeout=0.5,
        )

    supervisor = RuntimeSupervisor(
        host_factory=factory,
        shutdown_timeout=1.0,
    )
    principal = PrincipalContext("owner")
    supervisor.start(principal)
    receipt = supervisor.submit(principal, SubmitTurn(text="block")).result(timeout=0.5)
    assert receipt.turn_id
    assert backend.entered.wait(timeout=0.5)

    assert supervisor.shutdown()
    assert backend.cancelled.is_set()
    assert backend.stopped.is_set()
    assert supervisor.state is ServerState.STOPPED
