from __future__ import annotations

import concurrent.futures
import json
import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.visual_fixtures import REQUIRED_UI_FIXTURES, render_ui_fixtures
from zara.desktop.windows import CopilotMode, CopilotWindow


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
        self.commands = []

    def submit(self, command):
        self.commands.append(command)
        return completed(None)

    def close(self) -> None:
        return None


class FakeClient:
    def start(self):
        return completed(None)

    def close(self) -> None:
        return None


def make_controller(tmp_path):
    qt_app = app()
    bridge = FakeBridge()
    tray = FakeTray()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "unified-copilot.db"))
    )
    controller = DesktopController(
        qt_app,
        FakeClient(),  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        conversation_service=service,
    )
    qt_app.processEvents()
    return qt_app, controller, service


def dispose(controller: DesktopController) -> None:
    controller.bridge.close()
    controller.tray.hide()
    controller.window.prepare_for_quit()
    controller.window.close()
    controller.window.deleteLater()
    controller.setParent(None)
    controller.deleteLater()
    app().processEvents()


def test_controller_owns_one_canonical_copilot_instance(tmp_path):
    _, controller, _ = make_controller(tmp_path)
    try:
        assert isinstance(controller.window, CopilotWindow)
        assert controller.copilot_window is controller.window
        assert controller.quick_window is controller.window
        assert controller.window.mode is CopilotMode.COMPACT
    finally:
        dispose(controller)


def test_compact_expanded_transition_preserves_conversation_and_draft(tmp_path):
    qt_app, controller, service = make_controller(tmp_path)
    window = controller.window
    try:
        conversation_id = window.current_conversation_id
        service.add_user_message(
            conversation_id,
            "state must survive the presentation transition",
            request_id="mode-state",
        )
        window.apply_conversation_update(None)
        window.composer.setPlainText("draft survives too")

        window.set_mode(CopilotMode.EXPANDED)
        qt_app.processEvents()
        assert window.mode is CopilotMode.EXPANDED
        assert window.sidebar.isVisible()
        assert window.current_conversation_id == conversation_id
        assert window.composer.toPlainText() == "draft survives too"
        assert any(
            widget.message.content == "state must survive the presentation transition"
            for widget in window.message_widgets.values()
        )

        window.set_mode(CopilotMode.COMPACT)
        qt_app.processEvents()
        assert window.mode is CopilotMode.COMPACT
        assert not window.sidebar.isVisible()
        assert window.current_conversation_id == conversation_id
        assert window.composer.toPlainText() == "draft survives too"
    finally:
        dispose(controller)


def test_visual_fixture_harness_writes_required_pngs_and_manifest(tmp_path):
    output = tmp_path / "artifacts" / "ui"
    manifest_path = render_ui_fixtures(output)

    assert manifest_path == output / "manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    fixture_names = {entry["fixture"] for entry in manifest["fixtures"]}
    assert fixture_names == set(REQUIRED_UI_FIXTURES)

    for fixture_name in REQUIRED_UI_FIXTURES:
        png = output / f"{fixture_name}.png"
        assert png.is_file()
        assert png.stat().st_size > 0

    assert all(Path(entry["path"]).suffix == ".png" for entry in manifest["fixtures"])
