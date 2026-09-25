from __future__ import annotations

import asyncio
import concurrent.futures

import pytest

from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import CancelTurn, ShutdownRuntime, SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostError, RuntimeHostState, RuntimeNotReady
from zara.runtime.turn_context import TurnCapabilityLease


class _Backend(RuntimeBackend):
    @property
    def principal_id(self) -> str:
        return "principal"


def _future_raises(future: concurrent.futures.Future, error_type) -> None:
    with pytest.raises(error_type):
        future.result(timeout=1)


def test_submission_loop_close_races_fail_closed(monkeypatch) -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)

    class _OpenLoop:
        def is_closed(self) -> bool:
            return False

    def _closed_before_submit(coroutine, _loop):
        coroutine.close()
        raise RuntimeError("event loop closed during submission")

    monkeypatch.setattr(asyncio, "run_coroutine_threadsafe", _closed_before_submit)
    host._state = RuntimeHostState.RUNNING
    host._loop = _OpenLoop()

    coroutine = asyncio.sleep(0)
    _future_raises(host.run_coroutine(coroutine), RuntimeNotReady)
    _future_raises(host.submit(SubmitTurn(text="hello")), RuntimeNotReady)


def test_task_runner_cleanup_contains_backend_and_runner_failures() -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)

    class _BrokenToolBackend(_Backend):
        def unregister_tools(self, _names) -> None:
            raise RuntimeError("unregister failed")

    class _BrokenRunner:
        async def stop(self) -> None:
            raise RuntimeError("runner stop failed")

    host._backend = _BrokenToolBackend()
    host._task_runner = _BrokenRunner()

    asyncio.run(host._stop_task_runner())

    assert host._task_runner is None


def test_task_cancel_contains_coordinator_plugin_and_backend_failures() -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)
    lease = TurnCapabilityLease("turn-1")
    host._turn_capability_leases["turn-1"] = lease

    async def _broken_ask(_message):
        raise RuntimeError("coordinator unavailable")

    class _BrokenPluginManager:
        def cancel_capability_turn(self, _turn_id: str) -> None:
            raise RuntimeError("plugin cancellation failed")

    class _BrokenBackend(_Backend):
        async def cancel_turn(self, _turn_id: str) -> None:
            raise RuntimeError("backend cancellation failed")

    host._coordinator_ask = _broken_ask
    host._plugin_manager = _BrokenPluginManager()
    host._backend = _BrokenBackend()

    asyncio.run(host._cancel_task_turn("turn-1"))

    assert lease.active is False


def test_startup_shutdown_failure_propagates_to_pending_waiter() -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)
    waiter = concurrent.futures.Future()
    host._startup_shutdown_requests.append(
        (ShutdownRuntime(reason="startup failed"), waiter)
    )

    host._fail_startup_shutdown_requests(RuntimeError("startup failed"))

    _future_raises(waiter, RuntimeError)
    assert host._startup_shutdown_requests == []


def test_cancel_reply_and_active_probe_fail_closed() -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)

    async def _unexpected_cancel(_message):
        return object()

    host._coordinator_ask = _unexpected_cancel
    with pytest.raises(RuntimeHostError, match="unexpected turn cancellation reply"):
        asyncio.run(host._cancel_turn(CancelTurn(turn_id="turn-1")))

    host._coordinator = object()

    async def _broken_probe(_message):
        raise RuntimeError("coordinator unavailable")

    host._coordinator_ask = _broken_probe
    assert asyncio.run(host._turn_is_active("turn-1")) is False


def test_latency_trace_config_load_failure_is_nonfatal(monkeypatch) -> None:
    host = RuntimeHost(lambda: _Backend(), publisher=lambda _event: None)

    import zara.config as config_module

    def _broken_config():
        raise RuntimeError("config unavailable")

    monkeypatch.setattr(config_module, "get_config", _broken_config)

    assert host._build_turn_latency_trace(SubmitTurn(text="hello")) is None
