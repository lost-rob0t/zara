from __future__ import annotations

import asyncio
import threading

import pytest

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import RestartRuntime
from zara.runtime.host import RuntimeHost, RuntimeHostState, RuntimeNotReady


class SlowStartBackend(RuntimeBackend):
    def __init__(self) -> None:
        self.entered = threading.Event()
        self.release = threading.Event()
        self.stopped = threading.Event()

    async def start(self) -> None:
        self.entered.set()
        await asyncio.to_thread(self.release.wait)

    async def stop(self) -> None:
        self.stopped.set()


class ReadyBackend(RuntimeBackend):
    def __init__(self) -> None:
        self.stopped = threading.Event()

    async def stop(self) -> None:
        self.stopped.set()


def test_explicit_shutdown_queued_during_initial_startup():
    backend = SlowStartBackend()
    bus = runtime_bridge.RuntimeEventBus()
    emitted: list[events.RuntimeEvent] = []

    def publish(event: events.RuntimeEvent):
        emitted.append(event)
        return bus.publish(event)

    host = RuntimeHost(lambda: backend, publisher=publish)
    start_future = host.start()
    assert backend.entered.wait(timeout=5)
    assert host.state is RuntimeHostState.STARTING

    shutdown_future = host.shutdown("quit during startup")
    assert shutdown_future.done() is False

    backend.release.set()

    with pytest.raises(RuntimeNotReady, match="shutdown requested during startup"):
        start_future.result(timeout=5)

    receipt = shutdown_future.result(timeout=5)
    host.join(timeout=5)

    assert receipt.detail == "runtime stopped"
    assert host.state is RuntimeHostState.STOPPED
    assert host.is_alive is False
    assert backend.stopped.is_set()
    assert not any(isinstance(event, events.RuntimeStarted) for event in emitted)
    assert any(
        isinstance(event, events.RuntimeStopped)
        and event.reason == "quit during startup"
        for event in emitted
    )


def test_explicit_shutdown_queued_during_runtime_restart():
    ready = ReadyBackend()
    restarting = SlowStartBackend()
    backends = iter((ready, restarting))
    emitted: list[events.RuntimeEvent] = []
    bus = runtime_bridge.RuntimeEventBus()

    def publish(event: events.RuntimeEvent):
        emitted.append(event)
        return bus.publish(event)

    host = RuntimeHost(lambda: next(backends), publisher=publish)
    host.start().result(timeout=5)
    assert host.state is RuntimeHostState.RUNNING

    restart_future = host.submit(RestartRuntime())
    assert restarting.entered.wait(timeout=5)
    assert host.state is RuntimeHostState.STARTING

    shutdown_future = host.shutdown("quit during restart")
    assert shutdown_future.done() is False
    restarting.release.set()

    restart_receipt = restart_future.result(timeout=5)
    shutdown_receipt = shutdown_future.result(timeout=5)
    host.join(timeout=5)

    assert restart_receipt.detail == "runtime restart interrupted by shutdown"
    assert shutdown_receipt.detail == "runtime stopped"
    assert host.state is RuntimeHostState.STOPPED
    assert host.is_alive is False
    assert ready.stopped.is_set()
    assert restarting.stopped.is_set()
    assert sum(isinstance(event, events.RuntimeStarted) for event in emitted) == 1
    assert any(
        isinstance(event, events.RuntimeStopped)
        and event.reason == "quit during restart"
        for event in emitted
    )
