from __future__ import annotations

import concurrent.futures
import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import ConversationService, ConversationStore, MessageRole
from zara.runtime import events
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn


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

    def setEnabled(self, enabled: bool) -> None:  # noqa: N802 - Qt API
        self.enabled = enabled


class FakeTray(QObject):
    toggle_requested = Signal()
    quick_requested = Signal()
    full_chat_requested = Signal()
    restart_requested = Signal()
    diagnostics_requested = Signal()
    quit_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self.quit_action = FakeAction()

    def show_if_available(self) -> bool:
        return True

    def set_status(self, _status) -> None:
        return None

    def hide(self) -> None:
        return None


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(self) -> None:
        super().__init__()
        self.commands = []

    def submit(self, command):
        self.commands.append(command)
        return completed(None)

    def close(self) -> None:
        return None


class FakeHost:
    def start(self):
        return completed(None)

    def shutdown(self, _reason=""):
        return completed(None)


def make_controller(tmp_path):
    qt_app = app()
    bridge = FakeBridge()
    tray = FakeTray()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "handoff-edges.db"))
    )
    controller = DesktopController(
        qt_app,
        FakeHost(),  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        conversation_service=service,
    )
    qt_app.processEvents()
    assert controller.quick_window is not None
    return qt_app, controller, bridge, tray, service


def dispose(controller: DesktopController) -> None:
    controller.window.prepare_for_quit()
    controller.window.close()
    controller.window.deleteLater()
    if controller.quick_window is not None:
        controller.quick_window.prepare_for_quit()
        controller.quick_window.close()
        controller.quick_window.deleteLater()
    controller.setParent(None)
    controller.deleteLater()
    app().processEvents()


def test_tray_reuses_existing_quick_and_full_chat_windows(tmp_path):
    qt_app, controller, _, tray, service = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    full = controller.window
    try:
        assert quick.conversations is service
        assert full.conversations is service

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is quick
        assert quick.isVisible()

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is quick

        tray.full_chat_requested.emit()
        qt_app.processEvents()
        assert controller.window is full
        assert full.isVisible()
    finally:
        dispose(controller)


def test_local_quick_message_projects_to_full_chat_before_runtime_receipt(tmp_path):
    qt_app, controller, bridge, _, service = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        conversation_id = quick.current_conversation_id
        assert controller.window.current_conversation_id == conversation_id

        quick.composer.setPlainText("visible before receipt")
        quick.submit_current_text()
        qt_app.processEvents()

        state = service.get_state(conversation_id)
        user_message = state.latest_message(role=MessageRole.USER)
        assert user_message is not None
        assert user_message.id in quick.message_widgets
        assert user_message.id in controller.window.message_widgets
        assert len(bridge.commands) == 1
        assert isinstance(bridge.commands[0], SubmitTurn)
    finally:
        dispose(controller)


def test_cancel_in_progress_survives_handoff_and_blocks_duplicate_stop(tmp_path):
    qt_app, controller, bridge, _, service = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        conversation_id = quick.current_conversation_id
        quick.composer.setPlainText("cancel during handoff")
        quick.submit_current_text()
        submit = bridge.commands[-1]
        assert isinstance(submit, SubmitTurn)

        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-handoff-cancel")
        )
        qt_app.processEvents()
        quick.stop_button.click()
        qt_app.processEvents()

        cancel = bridge.commands[-1]
        assert isinstance(cancel, CancelTurn)
        assert cancel.turn_id == "turn-handoff-cancel"
        state = service.get_state(conversation_id)
        assert state.cancel_request_id == cancel.request_id

        command_count = len(bridge.commands)
        controller.expand_quick_to_full_chat()
        qt_app.processEvents()
        assert controller.window.current_conversation_id == conversation_id
        assert controller.window.stop_button.isEnabled() is False

        controller.window.stop_button.click()
        qt_app.processEvents()
        assert len(bridge.commands) == command_count

        bridge.runtime_event.emit(
            SimpleNamespace(
                event=events.TurnCancelled(
                    turn_id="turn-handoff-cancel",
                    reason="cancel command",
                )
            )
        )
        qt_app.processEvents()
        assert state.active_turn_id is None
        assert state.cancel_request_id is None
        assert controller.window.stop_button.isEnabled() is False
    finally:
        dispose(controller)
