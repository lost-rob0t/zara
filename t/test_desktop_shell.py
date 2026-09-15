from __future__ import annotations

import concurrent.futures
import os
import threading
from types import SimpleNamespace

import pytest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication, QSystemTrayIcon

from zara.desktop.app import create_application
from zara.desktop.controller import DesktopController
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.tray import ZaraTray
from zara.desktop.windows import DesktopStatusWindow
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend
from zara.runtime.commands import CommandReceipt, RestartRuntime
from zara.runtime.host import RuntimeHost


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


def completed(value=None):
    future: concurrent.futures.Future = concurrent.futures.Future()
    future.set_result(value)
    return future


class FakeAction:
    def __init__(self) -> None:
        self.enabled = True

    def setEnabled(self, enabled: bool) -> None:  # noqa: N802 - mirrors Qt
        self.enabled = enabled


class FakeTray(QObject):
    toggle_requested = Signal()
    settings_requested = Signal()
    restart_requested = Signal()
    diagnostics_requested = Signal()
    quit_requested = Signal()

    def __init__(self, *, available: bool = True) -> None:
        super().__init__()
        self._available = available
        self.statuses = []
        self.hidden = False
        self.quit_action = FakeAction()

    def show_if_available(self) -> bool:
        return self._available

    def set_status(self, status) -> None:
        self.statuses.append(status)

    def hide(self) -> None:
        self.hidden = True


class FakeWindow(QObject):
    restart_requested = Signal()
    diagnostics_requested = Signal()
    settings_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.statuses = []
        self.show_count = 0
        self.toggle_count = 0
        self.allow_close = False
        self.closed = False

    def set_status(self, status) -> None:
        self.statuses.append(status)

    def show_raised(self) -> None:
        self.show_count += 1

    def toggle_visibility(self) -> None:
        self.toggle_count += 1

    def prepare_for_quit(self) -> None:
        self.allow_close = True

    def close(self) -> None:
        self.closed = True


class FakeSettings(QObject):
    theme_preview_requested = Signal(str)
    restart_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.show_count = 0
        self.allow_close = False
        self.closed = False

    def show_raised(self) -> None:
        self.show_count += 1

    def prepare_for_quit(self) -> None:
        self.allow_close = True

    def close(self) -> None:
        self.closed = True


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(self) -> None:
        super().__init__()
        self.commands = []
        self.closed = False

    def submit(self, command):
        self.commands.append(command)
        return completed(None)

    def close(self) -> None:
        self.closed = True


class FakeHost:
    def __init__(self) -> None:
        self.start_calls = 0
        self.shutdown_calls = []
        self.close_calls = 0
        self.bus = runtime_bridge.RuntimeEventBus()

    def start(self):
        self.start_calls += 1
        return completed(None)

    def submit(self, command):
        return completed(CommandReceipt(request_id=command.request_id))

    def subscribe(self, *, maxsize: int = 0):
        return self.bus.subscribe(maxsize=maxsize)

    def shutdown(self, reason=""):
        self.shutdown_calls.append(reason)
        return completed(None)

    def close(self, timeout=None):
        self.close_calls += 1


class NullBackend(RuntimeBackend):
    pass


def make_controller(*, tray_available=True, settings_factory=None):
    qt_app = app()
    host = FakeHost()
    bridge = FakeBridge()
    tray = FakeTray(available=tray_available)
    window = FakeWindow()
    controller = DesktopController(
        qt_app,
        host,  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        window_factory=lambda: window,  # type: ignore[arg-type]
        settings_factory=settings_factory,
    )
    return qt_app, controller, host, bridge, tray, window


def test_settings_is_one_reused_surface_with_live_theme_and_restart_hooks():
    settings = FakeSettings()
    qt_app, controller, _, bridge, tray, window = make_controller(
        settings_factory=lambda: settings
    )
    try:
        tray.settings_requested.emit()
        window.settings_requested.emit()
        assert controller.settings_window is settings
        assert settings.show_count == 2

        settings.theme_preview_requested.emit("nord")
        assert qt_app.property("zaraTheme") == "nord"

        settings.restart_requested.emit()
        assert isinstance(bridge.commands[-1], RestartRuntime)
    finally:
        dispose_controller(controller)


def dispose_controller(controller: DesktopController) -> None:
    controller.setParent(None)
    controller.deleteLater()
    app().processEvents()


def test_status_window_close_hides_but_explicit_quit_closes():
    qt_app = app()
    window = DesktopStatusWindow()

    window.show()
    qt_app.processEvents()
    assert window.isVisible()

    assert window.close() is False
    qt_app.processEvents()
    assert window.isVisible() is False

    window.show()
    window.prepare_for_quit()
    assert window.close() is True
    qt_app.processEvents()
    assert window.isVisible() is False
    window.deleteLater()


def test_tray_actions_and_left_click_emit_shell_requests():
    tray = ZaraTray()
    seen = {"toggle": 0, "settings": 0, "restart": 0, "diagnostics": 0, "quit": 0}
    tray.toggle_requested.connect(lambda: seen.__setitem__("toggle", seen["toggle"] + 1))
    tray.settings_requested.connect(lambda: seen.__setitem__("settings", seen["settings"] + 1))
    tray.restart_requested.connect(lambda: seen.__setitem__("restart", seen["restart"] + 1))
    tray.diagnostics_requested.connect(
        lambda: seen.__setitem__("diagnostics", seen["diagnostics"] + 1)
    )
    tray.quit_requested.connect(lambda: seen.__setitem__("quit", seen["quit"] + 1))

    tray.open_action.trigger()
    tray.settings_action.trigger()
    tray.restart_action.trigger()
    tray.diagnostics_action.trigger()
    tray.quit_action.trigger()
    tray._on_activated(QSystemTrayIcon.ActivationReason.Trigger)

    assert seen == {"toggle": 2, "settings": 1, "restart": 1, "diagnostics": 1, "quit": 1}

    status = DesktopStatus(DesktopRuntimeState.ERROR, "provider failed")
    tray.set_status(status)
    assert tray.status_action.text() == "Status: error"
    assert "provider failed" in tray.toolTip()
    assert isinstance(tray.show_if_available(), bool)
    tray.hide()
    tray.deleteLater()


def test_controller_never_becomes_unreachable_without_a_tray():
    _, controller, host, _, _, window = make_controller(tray_available=False)
    try:
        controller.start()
        assert host.start_calls == 1
        assert window.show_count == 1
    finally:
        dispose_controller(controller)


@pytest.mark.parametrize("outcome", ["success", "failure", "cancelled"])
def test_client_start_completion_is_projected_on_qt_thread(outcome):
    qt_app, controller, host, _, tray, window = make_controller()
    future = concurrent.futures.Future()
    host.start = lambda: future
    updates = []
    original = window.set_status

    def record(status):
        updates.append(threading.get_ident())
        original(status)

    window.set_status = record
    try:
        assert controller.start() is future
        updates.clear()

        def finish():
            if outcome == "failure":
                future.set_exception(RuntimeError("handshake required"))
            elif outcome == "cancelled":
                future.cancel()
            else:
                future.set_result(True)

        worker = threading.Thread(target=finish)
        worker.start()
        worker.join()
        assert updates == []
        assert controller.status.state is DesktopRuntimeState.STARTING
        qt_app.processEvents()
        expected = DesktopRuntimeState.IDLE if outcome == "success" else DesktopRuntimeState.DISCONNECTED
        assert controller.status.state is expected
        assert updates == [threading.get_ident()]
        assert tray.statuses[-1] == window.statuses[-1] == controller.status
        if outcome != "success":
            assert "zara-server" in controller.status.detail
        if outcome == "failure":
            assert "handshake required" in controller.status.detail
    finally:
        dispose_controller(controller)


def test_start_is_idempotent_even_after_completion():
    qt_app, controller, host, _, _, _ = make_controller()
    try:
        future = controller.start()
        assert controller.start() is future
        qt_app.processEvents()
        assert controller.status.state is DesktopRuntimeState.IDLE
        assert controller.start() is future
        assert host.start_calls == 1
    finally:
        dispose_controller(controller)


def test_synchronous_start_failure_is_projected():
    qt_app, controller, host, _, _, _ = make_controller()

    def fail():
        raise RuntimeError("client cannot start from failed")

    host.start = fail
    try:
        future = controller.start()
        assert isinstance(future.exception(), RuntimeError)
        qt_app.processEvents()
        assert controller.status.state is DesktopRuntimeState.DISCONNECTED
        assert "client cannot start" in controller.status.detail
    finally:
        dispose_controller(controller)


@pytest.mark.parametrize("quit_method", ["request_quit", "_about_to_quit"])
@pytest.mark.parametrize("complete_before_quit", [False, True])
def test_late_start_completion_after_quit_is_ignored(quit_method, complete_before_quit):
    qt_app, controller, host, _, tray, window = make_controller()
    future = concurrent.futures.Future()
    host.start = lambda: future
    try:
        controller.start()
        if complete_before_quit:
            worker = threading.Thread(target=lambda: future.set_result(True))
            worker.start()
            worker.join()
        getattr(controller, quit_method)()
        count = len(window.statuses)
        if not complete_before_quit:
            future.set_exception(RuntimeError("closed"))
        qt_app.processEvents()
        assert len(window.statuses) == len(tray.statuses) == count
        assert controller.start() is future
        assert host.close_calls == 1
    finally:
        dispose_controller(controller)


@pytest.mark.parametrize("event", [events.AgentStarted(), events.RuntimeError(reason="provider failed")])
def test_start_completion_preserves_newer_runtime_status(event):
    qt_app, controller, host, bridge, _, _ = make_controller()
    future = concurrent.futures.Future()
    host.start = lambda: future
    try:
        controller.start()
        bridge.runtime_event.emit(SimpleNamespace(event=event))
        status = controller.status
        future.set_result(True)
        qt_app.processEvents()
        assert controller.status == status
    finally:
        dispose_controller(controller)


def test_runtime_events_update_tray_and_status_window_from_one_reducer():
    _, controller, _, bridge, tray, window = make_controller()
    try:
        bridge.runtime_event.emit(SimpleNamespace(event=events.AgentStarted()))
        assert controller.status.state is DesktopRuntimeState.THINKING
        assert tray.statuses[-1] == controller.status
        assert window.statuses[-1] == controller.status

        bridge.runtime_event.emit(SimpleNamespace(event=events.RuntimeError(reason="boom")))
        assert controller.status == DesktopStatus(DesktopRuntimeState.ERROR, "boom")
    finally:
        dispose_controller(controller)


def test_restart_is_command_driven_and_duplicate_requests_are_suppressed():
    _, controller, _, bridge, tray, _ = make_controller()
    try:
        tray.restart_requested.emit()
        tray.restart_requested.emit()

        assert len(bridge.commands) == 1
        command = bridge.commands[0]
        assert isinstance(command, RestartRuntime)
        assert controller.status.state is DesktopRuntimeState.STARTING

        bridge.command_completed.emit(CommandReceipt(request_id=command.request_id))
        assert controller.status.state is DesktopRuntimeState.IDLE
        assert tray.statuses[-1] == controller.status
        tray.restart_requested.emit()
        assert len(bridge.commands) == 2
    finally:
        dispose_controller(controller)


def test_synchronous_restart_failure_is_projected_and_allows_retry():
    _, controller, _, bridge, _, _ = make_controller()
    submit = bridge.submit

    def fail(command):
        raise RuntimeError("handshake required")

    bridge.submit = fail
    try:
        controller.restart_runtime()
        assert controller.status.state is DesktopRuntimeState.ERROR
        assert "handshake required" in controller.status.detail
        bridge.submit = submit
        controller.restart_runtime()
        assert len(bridge.commands) == 1
    finally:
        dispose_controller(controller)


@pytest.mark.parametrize("event", [events.AgentStarted(), events.RuntimeError(reason="failed")])
def test_restart_receipt_preserves_newer_status(event):
    _, controller, _, bridge, _, _ = make_controller()
    try:
        controller.restart_runtime()
        bridge.runtime_event.emit(SimpleNamespace(event=event))
        status = controller.status
        bridge.command_completed.emit(CommandReceipt(request_id=bridge.commands[-1].request_id))
        assert controller.status == status
    finally:
        dispose_controller(controller)


def test_pending_start_does_not_complete_restart_or_unrelated_receipt():
    qt_app, controller, host, bridge, _, _ = make_controller()
    future = concurrent.futures.Future()
    host.start = lambda: future
    try:
        controller.start()
        controller.restart_runtime()
        status = controller.status
        future.set_result(True)
        qt_app.processEvents()
        bridge.command_completed.emit(CommandReceipt(request_id="unrelated"))
        assert controller.status == status
        bridge.command_failed.emit(bridge.commands[-1].request_id, "restart unavailable")
        assert controller.status == DesktopStatus(DesktopRuntimeState.ERROR, "restart unavailable")
        controller.restart_runtime()
        assert len(bridge.commands) == 2
    finally:
        dispose_controller(controller)


@pytest.mark.parametrize("quit_method", ["request_quit", "_about_to_quit"])
def test_quit_rejects_new_start_and_ignores_queued_runtime_updates(quit_method):
    _, controller, host, bridge, tray, window = make_controller()
    try:
        controller.restart_runtime()
        request_id = bridge.commands[-1].request_id
        getattr(controller, quit_method)()
        count = len(window.statuses)
        assert controller.start().cancelled()
        assert host.start_calls == 0
        controller.restart_runtime()
        bridge.command_completed.emit(CommandReceipt(request_id=request_id))
        bridge.command_failed.emit(request_id, "closed")
        bridge.runtime_event.emit(SimpleNamespace(event=events.RuntimeStarted()))
        assert len(window.statuses) == len(tray.statuses) == count
        assert len(bridge.commands) == 1
    finally:
        dispose_controller(controller)


def test_diagnostics_action_reopens_status_surface_and_emits_hook():
    _, controller, _, _, tray, window = make_controller()
    seen = []
    controller.diagnostics_requested.connect(lambda: seen.append(True))
    try:
        tray.diagnostics_requested.emit()
        assert window.show_count == 1
        assert seen == [True]
    finally:
        dispose_controller(controller)


def test_explicit_quit_closes_client_without_daemon_shutdown_command():
    _, controller, client, bridge, tray, window = make_controller()
    try:
        controller.request_quit()

        assert controller.quitting is True
        assert bridge.commands == []
        assert client.shutdown_calls == []
        assert client.close_calls == 1
        assert bridge.closed is True
        assert tray.hidden is True
        assert window.allow_close is True
        assert window.closed is True
        assert tray.quit_action.enabled is False
    finally:
        dispose_controller(controller)


def test_about_to_quit_closes_client_without_daemon_shutdown():
    _, controller, client, bridge, tray, window = make_controller()
    try:
        controller._about_to_quit()

        assert bridge.commands == []
        assert client.shutdown_calls == []
        assert client.close_calls == 1
        assert bridge.closed is True
        assert tray.hidden is True
        assert window.closed is True
    finally:
        dispose_controller(controller)


def test_create_application_accepts_supplied_zara_client():
    qt_app = app()
    if hasattr(qt_app, "_zara_desktop_controller"):
        delattr(qt_app, "_zara_desktop_controller")

    client = FakeHost()
    app_one, controller = create_application([], client=client)

    try:
        assert app_one is qt_app
        assert controller.client is client
        assert controller.host is client
    finally:
        controller.bridge.close()
        controller.tray.hide()
        controller.window.prepare_for_quit()
        controller.window.close()
        delattr(qt_app, "_zara_desktop_controller")
        controller.setParent(None)
        controller.deleteLater()
        controller.tray.deleteLater()
        controller.window.deleteLater()
        qt_app.processEvents()


def test_create_application_reuses_one_controller_and_canonical_tray():
    qt_app = app()
    if hasattr(qt_app, "_zara_desktop_controller"):
        delattr(qt_app, "_zara_desktop_controller")

    host_one = RuntimeHost(lambda: NullBackend())
    host_two = RuntimeHost(lambda: NullBackend())
    app_one, controller_one = create_application([], host=host_one)
    app_two, controller_two = create_application([], host=host_two)

    try:
        assert app_one is app_two
        assert controller_one is controller_two
        assert controller_one.host is host_one
        assert app_one.quitOnLastWindowClosed() is False
    finally:
        controller_one.bridge.close()
        controller_one.tray.hide()
        controller_one.window.prepare_for_quit()
        controller_one.window.close()
        delattr(qt_app, "_zara_desktop_controller")
        controller_one.setParent(None)
        controller_one.deleteLater()
        controller_one.tray.deleteLater()
        controller_one.window.deleteLater()
        qt_app.processEvents()
