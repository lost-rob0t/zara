from __future__ import annotations

import threading
import time

from PySide6.QtCore import QCoreApplication, QObject, QThread, Slot

from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import StartVoice, SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostState


class FakeBackend(RuntimeBackend):
    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id=None,
        context_ids=(),
    ) -> RuntimeTurnResult:
        return RuntimeTurnResult(response=f"reply:{text}")


class Receiver(QObject):
    def __init__(self) -> None:
        super().__init__()
        self.events = []
        self.receipts = []
        self.failures = []
        self.event_thread = None
        self.command_thread = None
        self.failure_thread = None

    @Slot(object)
    def on_event(self, envelope) -> None:
        self.events.append(envelope)
        self.event_thread = QThread.currentThread()

    @Slot(object)
    def on_completed(self, receipt) -> None:
        self.receipts.append(receipt)
        self.command_thread = QThread.currentThread()

    @Slot(str, str)
    def on_failed(self, request_id: str, message: str) -> None:
        self.failures.append((request_id, message))
        self.failure_thread = QThread.currentThread()


def app() -> QCoreApplication:
    return QCoreApplication.instance() or QCoreApplication([])


def process_until(predicate, timeout: float = 5.0) -> bool:
    qt_app = app()
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        qt_app.processEvents()
        if predicate():
            return True
        time.sleep(0.005)
    qt_app.processEvents()
    return bool(predicate())


def stop_host(host: RuntimeHost) -> None:
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED, RuntimeHostState.FAILED}:
        try:
            host.shutdown("qt test cleanup").result(timeout=5)
        except Exception:
            pass
    host.join(timeout=5)


def test_qt_bridge_drains_runtime_events_on_qt_thread(monkeypatch):
    qt_app = app()
    bus = runtime_bridge.RuntimeEventBus()
    monkeypatch.setattr(runtime_bridge, "subscribe", bus.subscribe)
    host = RuntimeHost(lambda: FakeBackend(), publisher=bus.publish)
    qt_bridge = QtRuntimeBridge(host, auto_start_timer=False)
    receiver = Receiver()
    qt_bridge.runtime_event.connect(receiver.on_event)

    try:
        bus.publish(events.RuntimeIdle(label="test"))
        qt_bridge.drain_events()

        assert len(receiver.events) == 1
        assert isinstance(receiver.events[0].event, events.RuntimeIdle)
        assert receiver.events[0].event.label == "test"
        assert receiver.event_thread == qt_app.thread()
    finally:
        qt_bridge.close()
        stop_host(host)


def test_qt_bridge_command_completion_is_queued_to_qt_thread(monkeypatch):
    qt_app = app()
    main_thread_id = threading.get_ident()
    bus = runtime_bridge.RuntimeEventBus()
    monkeypatch.setattr(runtime_bridge, "subscribe", bus.subscribe)
    host = RuntimeHost(lambda: FakeBackend(), publisher=bus.publish)
    qt_bridge = QtRuntimeBridge(host, auto_start_timer=False)
    receiver = Receiver()
    qt_bridge.command_completed.connect(receiver.on_completed)

    try:
        host.start().result(timeout=5)
        future = qt_bridge.submit(
            SubmitTurn(text="hello", conversation_id="conversation-qt")
        )
        receipt = future.result(timeout=5)

        assert receipt.turn_id is not None
        assert host.thread_id is not None
        assert host.thread_id != main_thread_id
        assert process_until(lambda: len(receiver.receipts) == 1)
        assert receiver.receipts[0] == receipt
        assert receiver.command_thread == qt_app.thread()
    finally:
        qt_bridge.close()
        stop_host(host)


def test_qt_bridge_command_failure_is_queued_and_does_not_kill_runtime(monkeypatch):
    qt_app = app()
    bus = runtime_bridge.RuntimeEventBus()
    monkeypatch.setattr(runtime_bridge, "subscribe", bus.subscribe)
    host = RuntimeHost(lambda: FakeBackend(), publisher=bus.publish)
    qt_bridge = QtRuntimeBridge(host, auto_start_timer=False)
    receiver = Receiver()
    qt_bridge.command_failed.connect(receiver.on_failed)

    try:
        host.start().result(timeout=5)
        command = StartVoice()
        future = qt_bridge.submit(command)

        try:
            future.result(timeout=5)
        except Exception:
            pass

        assert process_until(lambda: len(receiver.failures) == 1)
        request_id, message = receiver.failures[0]
        assert request_id == command.request_id
        assert "voice start is not available" in message
        assert receiver.failure_thread == qt_app.thread()
        assert host.state is RuntimeHostState.RUNNING
    finally:
        qt_bridge.close()
        stop_host(host)


def test_qt_bridge_close_stops_delivery(monkeypatch):
    bus = runtime_bridge.RuntimeEventBus()
    monkeypatch.setattr(runtime_bridge, "subscribe", bus.subscribe)
    host = RuntimeHost(lambda: FakeBackend(), publisher=bus.publish)
    qt_bridge = QtRuntimeBridge(host, auto_start_timer=False)
    receiver = Receiver()
    qt_bridge.runtime_event.connect(receiver.on_event)

    qt_bridge.close()
    bus.publish(events.RuntimeIdle())
    qt_bridge.drain_events()

    assert qt_bridge.closed is True
    assert receiver.events == []
