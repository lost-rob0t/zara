from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.windows import CopilotPresentation, CopilotWindow


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
    def setEnabled(self, _enabled: bool) -> None:  # noqa: N802 - Qt API
        return None


class FakeTray(QObject):
    toggle_requested = Signal()
    quick_requested = Signal()
    full_chat_requested = Signal()
    settings_requested = Signal()
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
        self.commands: list[object] = []

    def submit(self, command):
        self.commands.append(command)
        return completed(None)

    def close(self) -> None:
        return None


class FakeClient:
    def start(self):
        return completed(None)

    def shutdown(self, _reason=""):
        return completed(None)

    def close(self, timeout=None) -> None:
        return None


def test_controller_owns_one_adaptive_copilot_for_compact_and_full_entrypoints(tmp_path):
    qt_app = app()
    bridge = FakeBridge()
    tray = FakeTray()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "controller-collapse.db"))
    )
    controller = DesktopController(
        qt_app,
        FakeClient(),  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        conversation_service=service,
    )
    qt_app.processEvents()

    try:
        assert isinstance(controller.window, CopilotWindow)
        assert controller.quick_window is controller.window
        surface = controller.window
        conversation_id = surface.current_conversation_id
        surface.composer.setPlainText("draft survives entrypoint changes")

        tray.full_chat_requested.emit()
        qt_app.processEvents()

        assert controller.window is surface
        assert controller.quick_window is surface
        assert surface.presentation is CopilotPresentation.EXPANDED
        assert surface.current_conversation_id == conversation_id
        assert surface.composer.toPlainText() == "draft survives entrypoint changes"

        tray.quick_requested.emit()
        qt_app.processEvents()

        assert controller.window is surface
        assert controller.quick_window is surface
        assert len(bridge.commands) == 0
    finally:
        surface = controller.window
        surface.prepare_for_quit()
        surface.close()
        surface.deleteLater()
        controller.setParent(None)
        controller.deleteLater()
        qt_app.processEvents()
