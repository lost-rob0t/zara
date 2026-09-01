from __future__ import annotations

import asyncio
import pathlib
import threading
import time

import pytest

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand
from zara.runtime.commands import CancelTurn, RestartRuntime, StartVoice, SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostState, RuntimeNotReady


class ImmediateBackend(RuntimeBackend):
    def __init__(self, response: str = "done") -> None:
        self.response = response
        self.start_thread_id = None
        self.turn_thread_id = None
        self.stop_thread_id = None
        self.cancelled: list[str] = []

    async def start(self) -> None:
        self.start_thread_id = threading.get_ident()

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id=None,
        context_ids=(),
    latency_trace=None,
    ) -> RuntimeTurnResult:
        self.turn_thread_id = threading.get_ident()
        return RuntimeTurnResult(response=f"{self.response}:{text}")

    async def cancel_turn(self, turn_id: str) -> None:
        self.cancelled.append(turn_id)

    async def stop(self) -> None:
        self.stop_thread_id = threading.get_ident()


class SwallowCancellationBackend(RuntimeBackend):
    """Deliberately returns a stale answer after asyncio cancellation."""

    def __init__(self) -> None:
        self.entered = threading.Event()
        self.cancel_hook = threading.Event()

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id=None,
        context_ids=(),
    latency_trace=None,
    ) -> RuntimeTurnResult:
        self.entered.set()
        try:
            await asyncio.Event().wait()
        except asyncio.CancelledError:
            return RuntimeTurnResult(response="STALE RESULT")

    async def cancel_turn(self, turn_id: str) -> None:
        self.cancel_hook.set()


class FailingStartBackend(RuntimeBackend):
    async def start(self) -> None:
        raise RuntimeError("startup boom")


class RecordingPublisher:
    def __init__(self) -> None:
        self.bus = runtime_bridge.RuntimeEventBus()
        self.events: list[events.RuntimeEvent] = []
        self.output_ready = threading.Event()

    def __call__(self, event: events.RuntimeEvent):
        self.events.append(event)
        if isinstance(event, events.OutputReady):
            self.output_ready.set()
        return self.bus.publish(event)


def stop_host(host: RuntimeHost) -> None:
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED, RuntimeHostState.FAILED}:
        try:
            host.shutdown("test cleanup").result(timeout=5)
        except Exception:
            pass
    host.join(timeout=5)


def test_runtime_host_executes_backend_off_caller_thread():
    main_thread_id = threading.get_ident()
    backend = ImmediateBackend()
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(
            SubmitTurn(text="hello", conversation_id="conversation-1")
        ).result(timeout=5)

        assert receipt.turn_id is not None
        assert publisher.output_ready.wait(timeout=5)
        assert host.thread_id is not None
        assert host.thread_id != main_thread_id
        assert backend.start_thread_id == host.thread_id
        assert backend.turn_thread_id == host.thread_id

        correlated = [
            event
            for event in publisher.events
            if event.turn_id == receipt.turn_id
        ]
        assert [type(event) for event in correlated] == [
            events.TurnStarted,
            events.AgentStarted,
            events.ResponseText,
            events.AgentCompleted,
            events.OutputReady,
        ]
        assert all(event.conversation_id == "conversation-1" for event in correlated)
        assert isinstance(correlated[2], events.ResponseText)
        assert correlated[2].text == "done:hello"
    finally:
        stop_host(host)

    assert backend.stop_thread_id is not None
    assert backend.stop_thread_id != main_thread_id


def test_cancel_turn_is_idempotent_for_canonical_turn():
    backend = SwallowCancellationBackend()
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="slow")).result(timeout=5)
        assert receipt.turn_id is not None
        assert backend.entered.wait(timeout=5)

        first = host.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=5)
        second = host.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=5)

        assert first.detail == "turn cancelled"
        assert second.detail == "turn already cancelled"
        assert backend.cancel_hook.wait(timeout=5)
        cancelled = [
            event
            for event in publisher.events
            if isinstance(event, events.TurnCancelled)
            and event.turn_id == receipt.turn_id
        ]
        assert len(cancelled) == 1
    finally:
        stop_host(host)


def test_cancelled_turn_suppresses_backend_that_returns_stale_result():
    backend = SwallowCancellationBackend()
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="slow")).result(timeout=5)
        assert receipt.turn_id is not None
        assert backend.entered.wait(timeout=5)
        host.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=5)

        deadline = time.monotonic() + 2.0
        while time.monotonic() < deadline:
            stale = [
                event
                for event in publisher.events
                if event.turn_id == receipt.turn_id
                and isinstance(event, (events.ResponseText, events.OutputReady))
            ]
            if stale:
                break
            time.sleep(0.01)

        stale = [
            event
            for event in publisher.events
            if event.turn_id == receipt.turn_id
            and isinstance(event, (events.ResponseText, events.OutputReady))
        ]
        assert stale == []
    finally:
        stop_host(host)


def test_startup_failure_stays_alive_for_explicit_restart():
    created: list[RuntimeBackend] = []

    def factory() -> RuntimeBackend:
        backend: RuntimeBackend
        if not created:
            backend = FailingStartBackend()
        else:
            backend = ImmediateBackend("recovered")
        created.append(backend)
        return backend

    publisher = RecordingPublisher()
    host = RuntimeHost(factory, publisher=publisher)

    try:
        with pytest.raises(RuntimeError, match="startup boom"):
            host.start().result(timeout=5)

        assert host.state is RuntimeHostState.DEGRADED
        assert host.is_alive is True
        assert any(
            isinstance(event, events.RuntimeError)
            and event.fatal is False
            and "startup boom" in event.reason
            for event in publisher.events
        )

        restarted = host.submit(RestartRuntime()).result(timeout=5)
        assert restarted.detail == "runtime restarted"
        assert host.state is RuntimeHostState.RUNNING

        publisher.output_ready.clear()
        receipt = host.submit(SubmitTurn(text="again")).result(timeout=5)
        assert receipt.turn_id is not None
        assert publisher.output_ready.wait(timeout=5)
    finally:
        stop_host(host)


def test_unsupported_optional_command_does_not_kill_host():
    backend = ImmediateBackend()
    host = RuntimeHost(lambda: backend, publisher=RecordingPublisher())

    try:
        host.start().result(timeout=5)
        with pytest.raises(UnsupportedRuntimeCommand):
            host.submit(StartVoice()).result(timeout=5)
        assert host.state is RuntimeHostState.RUNNING
        assert host.is_alive is True
    finally:
        stop_host(host)


def test_submit_before_start_fails_without_blocking():
    host = RuntimeHost(lambda: ImmediateBackend(), publisher=RecordingPublisher())
    future = host.submit(SubmitTurn(text="too early"))

    with pytest.raises(RuntimeNotReady):
        future.result(timeout=1)

    assert host.state is RuntimeHostState.NEW


def test_shutdown_stops_backend_coordinator_and_worker():
    backend = ImmediateBackend()
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)

    host.start().result(timeout=5)
    receipt = host.shutdown("done").result(timeout=5)
    host.join(timeout=5)

    assert receipt.detail == "runtime stopped"
    assert host.state is RuntimeHostState.STOPPED
    assert host.is_alive is False
    assert backend.stop_thread_id is not None
    assert any(
        isinstance(event, events.RuntimeStopped) and event.reason == "done"
        for event in publisher.events
    )


def test_runtime_package_remains_qt_free():
    runtime_root = pathlib.Path(__file__).parents[1] / "zara" / "runtime"
    source = "\n".join(
        path.read_text(encoding="utf-8")
        for path in runtime_root.glob("*.py")
    )

    assert "PySide6" not in source
    assert "PyQt" not in source
