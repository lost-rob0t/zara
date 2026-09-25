from __future__ import annotations

import asyncio
import concurrent.futures

import pytest

from zara.runtime import events
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import MuteSpeech, ShutdownRuntime, StopVoice, SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostError, RuntimeHostState, RuntimeNotReady
from zara.runtime.turn_context import TurnCapabilityLease


class AliveThread:
    def is_alive(self) -> bool:
        return True


class CapabilityBackend(RuntimeBackend):
    def __init__(self) -> None:
        self.stop_voice_calls = 0
        self.mute_values: list[bool] = []

    @property
    def principal_id(self) -> str:
        return "principal"

    async def stop_voice(self) -> None:
        self.stop_voice_calls += 1

    async def mute_speech(self, enabled: bool) -> None:
        self.mute_values.append(enabled)


def future_raises(future: concurrent.futures.Future, error_type) -> None:
    with pytest.raises(error_type):
        future.result(timeout=1)


def test_host_public_boundaries_fail_closed_without_live_loop() -> None:
    host = RuntimeHost(lambda: CapabilityBackend(), publisher=lambda _event: None)
    future_raises(host.submit(object()), TypeError)

    coroutine = asyncio.sleep(0)
    try:
        future_raises(host.run_coroutine(coroutine), RuntimeNotReady)
    finally:
        coroutine.close()

    host._state = RuntimeHostState.STOPPING
    future_raises(host.start(), RuntimeNotReady)

    host._state = RuntimeHostState.STARTING
    host._thread = AliveThread()
    sentinel = concurrent.futures.Future()
    host._startup_future = sentinel
    assert host.start() is sentinel

    host._state = RuntimeHostState.RUNNING
    host._loop = None
    future_raises(host.submit(SubmitTurn(text="hello")), RuntimeNotReady)


def test_host_turn_capability_lease_lifecycle_fences_duplicates() -> None:
    host = RuntimeHost(lambda: CapabilityBackend(), publisher=lambda _event: None)
    lease = host._new_turn_capability_lease("turn-1")
    assert isinstance(lease, TurnCapabilityLease)

    with pytest.raises(RuntimeHostError, match="duplicate"):
        host._new_turn_capability_lease("turn-1")

    host._invalidate_turn_capability_lease("missing")
    host._invalidate_turn_capability_lease("turn-1")
    assert lease.active is False

    replacement = TurnCapabilityLease("turn-1")
    host._turn_capability_leases["turn-1"] = replacement
    host._release_turn_capability_lease("turn-1", lease)
    assert host._turn_capability_leases["turn-1"] is replacement

    second = host._new_turn_capability_lease("turn-2")
    host._invalidate_all_turn_capability_leases()
    assert replacement.active is False
    assert second.active is False
    assert host._turn_capability_leases == {}


def test_host_dispatches_local_controls_and_rejects_unknown_commands() -> None:
    published: list[events.RuntimeEvent] = []
    backend = CapabilityBackend()
    host = RuntimeHost(lambda: backend, publisher=published.append)
    host._backend = backend

    assert asyncio.run(host._dispatch(StopVoice())).detail == "voice stopped"
    assert asyncio.run(host._dispatch(MuteSpeech(enabled=True))).detail == "speech mute updated"
    assert backend.stop_voice_calls == 1
    assert backend.mute_values == [True]

    with pytest.raises(TypeError, match="unsupported runtime command"):
        asyncio.run(host._dispatch(object()))
    assert isinstance(published[-1], events.RuntimeError)


def test_host_task_and_shutdown_request_failure_edges_are_bounded() -> None:
    host = RuntimeHost(lambda: CapabilityBackend(), publisher=lambda _event: None)

    with pytest.raises(RuntimeHostError, match="capability lease"):
        asyncio.run(
            host._task_submit_turn(
                "task",
                turn_id="missing",
                conversation_id="c",
                system_context=None,
                latency_trace=None,
            )
        )

    async def unexpected(_message):
        return object()

    host._coordinator_ask = unexpected
    with pytest.raises(RuntimeHostError, match="unexpected turn coordinator reply"):
        asyncio.run(host._allocate_task_turn_id())

    request_a = ShutdownRuntime(reason="a")
    request_b = ShutdownRuntime(reason="b")
    future_a = concurrent.futures.Future()
    future_b = concurrent.futures.Future()

    async def fail_shutdown(_command):
        raise RuntimeError("shutdown failed")

    host._shutdown = fail_shutdown
    with pytest.raises(RuntimeError, match="shutdown failed"):
        asyncio.run(
            host._complete_startup_shutdown_requests(
                [(request_a, future_a), (request_b, future_b)]
            )
        )
    future_raises(future_a, RuntimeError)
    future_raises(future_b, RuntimeError)


def test_host_private_absence_and_cleanup_failures_are_bounded() -> None:
    host = RuntimeHost(lambda: CapabilityBackend(), publisher=lambda _event: None)

    with pytest.raises(RuntimeNotReady, match="backend"):
        host._require_backend()
    with pytest.raises(RuntimeNotReady, match="coordinator"):
        asyncio.run(host._coordinator_ask(object()))
    assert asyncio.run(host._turn_is_active("turn")) is False
    asyncio.run(host._stop_backend())
    asyncio.run(host._stop_coordinator())

    class BrokenBackend(RuntimeBackend):
        async def stop(self) -> None:
            raise RuntimeError("stop failed")

    host._backend = BrokenBackend()
    asyncio.run(host._stop_backend())
    assert host._backend is None

    class SlowBackend(RuntimeBackend):
        async def stop(self) -> None:
            await asyncio.sleep(1)

    host._shutdown_timeout = 0.1
    host._backend = SlowBackend()
    asyncio.run(host._stop_backend())
    assert host._backend is None

    class BrokenCoordinator:
        def ask(self, *_args, **_kwargs):
            raise RuntimeError("drain failed")

        def stop(self, *_args, **_kwargs):
            raise RuntimeError("stop failed")

    host._coordinator = BrokenCoordinator()
    asyncio.run(host._stop_coordinator())
    assert host._coordinator is None
