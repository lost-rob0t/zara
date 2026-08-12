from __future__ import annotations

import asyncio
import threading

import pytest

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend
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
