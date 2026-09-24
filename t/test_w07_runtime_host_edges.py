from __future__ import annotations

import asyncio
import concurrent.futures
from unittest.mock import AsyncMock

import pytest

from zara.runtime import events
from zara.runtime import host as host_module
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import RuntimeCommand, ShutdownRuntime, StartVoice
from zara.runtime.host import RuntimeHost, RuntimeHostError, RuntimeHostState, RuntimeNotReady


@pytest.mark.asyncio
async def test_host_prestart_capability_and_fail_closed_boundaries():
    published = []
    host = RuntimeHost(lambda: RuntimeBackend(), publisher=published.append)
    coroutine = asyncio.sleep(0)
    future = host.run_coroutine(coroutine)
    with pytest.raises(RuntimeNotReady):
        future.result(timeout=1)
    coroutine.close()

    host._state = RuntimeHostState.STOPPING
    with pytest.raises(RuntimeNotReady, match="stopping"):
        host.start().result(timeout=1)
    with pytest.raises(TypeError, match="RuntimeCommand"):
        host.submit("bad").result(timeout=1)
    host._state = RuntimeHostState.NEW
    assert host.shutdown("already").result(timeout=1).detail == "already stopped"
    host.join(timeout=0)

    with pytest.raises(RuntimeNotReady, match="backend"):
        host._require_backend()
    assert await host._turn_is_active("missing") is False
    with pytest.raises(RuntimeNotReady, match="coordinator"):
        await host._coordinator_ask(object())

    lease = host._new_turn_capability_lease("turn")
    with pytest.raises(RuntimeHostError, match="duplicate"):
        host._new_turn_capability_lease("turn")
    host._invalidate_turn_capability_lease("turn")
    assert lease.active is False
    host._release_turn_capability_lease("turn", lease)
    assert "turn" not in host._turn_capability_leases


@pytest.mark.asyncio
async def test_host_task_cancel_dispatch_backend_and_coordinator_failures(monkeypatch):
    published = []
    host = RuntimeHost(lambda: RuntimeBackend(), publisher=published.append)
    with pytest.raises(RuntimeHostError, match="lease is unavailable"):
        await host._task_submit_turn("task", turn_id="missing", conversation_id="c", system_context="system", latency_trace=None)

    backend = RuntimeBackend()
    backend.cancel_turn = AsyncMock(side_effect=RuntimeError("cancel failed"))
    backend.start_voice = AsyncMock(side_effect=RuntimeError("voice failed"))
    backend.stop = AsyncMock(side_effect=RuntimeError("stop failed"))
    host._backend = backend
    host._coordinator_ask = AsyncMock(side_effect=RuntimeError("coordinator failed"))
    host._new_turn_capability_lease("turn")
    await host._cancel_task_turn("turn")
    assert host._turn_capability_leases["turn"].active is False

    with pytest.raises(RuntimeError, match="voice failed"):
        await host._dispatch(StartVoice())
    assert any(isinstance(event, events.RuntimeError) and event.label == "StartVoice" for event in published)
    with pytest.raises(TypeError, match="unsupported runtime command"):
        await host._dispatch(RuntimeCommand())
    assert any(isinstance(event, events.RuntimeError) and event.label == "RuntimeCommand" for event in published)

    await host._stop_backend()
    assert host._backend is None
    host._coordinator = object()
    await host._stop_coordinator()
    assert host._coordinator is None


@pytest.mark.asyncio
async def test_host_coordinator_startup_and_pending_shutdown_failures(monkeypatch):
    published = []
    host = RuntimeHost(lambda: RuntimeBackend(), publisher=published.append)
    monkeypatch.setattr(
        host_module.TurnCoordinator,
        "start",
        staticmethod(lambda: (_ for _ in ()).throw(RuntimeError("actor failed"))),
    )
    await host._run()
    assert host.state is RuntimeHostState.FAILED
    with pytest.raises(RuntimeError, match="actor failed"):
        host._startup_future.result()
    assert any(isinstance(event, events.RuntimeError) and event.fatal for event in published)

    pending = RuntimeHost(lambda: RuntimeBackend(), publisher=lambda _event: None)
    requests = [
        (ShutdownRuntime(reason="a"), concurrent.futures.Future()),
        (ShutdownRuntime(reason="b"), concurrent.futures.Future()),
    ]
    pending._shutdown = AsyncMock(side_effect=RuntimeError("shutdown failed"))
    with pytest.raises(RuntimeError, match="shutdown failed"):
        await pending._complete_startup_shutdown_requests(requests)
    for _, future in requests:
        with pytest.raises(RuntimeError, match="shutdown failed"):
            future.result()

    queued = concurrent.futures.Future()
    pending._startup_shutdown_requests = [(ShutdownRuntime(), queued)]
    pending._fail_startup_shutdown_requests(ValueError("startup failed"))
    with pytest.raises(ValueError, match="startup failed"):
        queued.result()
    assert pending._startup_shutdown_requests == []
