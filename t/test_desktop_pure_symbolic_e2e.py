from __future__ import annotations

import os
import queue
import time

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop import app as desktop_app
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import ConversationService, ConversationStore, MessageRole
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.runtime import events


class PureSymbolicConfig:
    def get(self, section: str, key: str, default=None):
        if section == "conversation" and key == "execution_policy":
            return "pure_symbolic"
        if section == "agent" and key == "backend":
            return "langgraph"
        return default

    def get_api_service_config(self):
        return {"enabled": False}

    def get_tasks_config(self):
        return {"enabled": False}

    def get_latency_config(self):
        return {"enabled": False}

    def get_module_search_paths(self):
        return []

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 0.2,
            "event_queue_size": 4,
            "max_managed_workers": 1,
        }

    def get_plugin_config(self, _name: str):
        return {}


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


def _app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    app = instance or QApplication([])
    app.setQuitOnLastWindowClosed(False)
    return app


def _wait_until(app: QApplication, predicate, timeout: float = 8.0) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        app.processEvents()
        if predicate():
            return
        time.sleep(0.01)
    app.processEvents()
    assert predicate(), "timed out waiting for Desktop pure-symbolic state"


def test_real_desktop_surface_runs_symbolic_greeting_without_runtime_errors(tmp_path):
    qt_app = _app()
    config = PureSymbolicConfig()
    client = desktop_app._default_desktop_client(config)  # type: ignore[arg-type]
    diagnostics = client.subscribe(maxsize=256)
    bridge = QtRuntimeBridge(client, parent=qt_app, auto_start_timer=True)
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "desktop-symbolic.db"))
    )
    controller = DesktopController(
        qt_app,
        client,
        bridge,
        tray_factory=FakeTray,
        conversation_service=service,
    )
    surface = controller.window

    try:
        controller.start().result(timeout=8.0)
        _wait_until(qt_app, lambda: controller.status.detail == "Zara is ready")

        conversation_id = surface.current_conversation_id
        surface.composer.setPlainText("hello")
        surface.submit_current_text()

        def has_symbolic_reply() -> bool:
            state = service.get_state(conversation_id)
            return any(
                message.role is MessageRole.ASSISTANT
                and message.content == "Hey — what can I help with?"
                for message in state.messages
            )

        _wait_until(qt_app, has_symbolic_reply)
        state = service.get_state(conversation_id)
        assert [message.content for message in state.messages] == [
            "hello",
            "Hey — what can I help with?",
        ]
        assert state.active_turn_id is None
        assert config.get("agent", "backend", "missing") == "langgraph"

        observed_errors = []
        while True:
            try:
                envelope = diagnostics.get(timeout=0.01)
            except queue.Empty:
                break
            if isinstance(envelope.event, events.RuntimeError):
                observed_errors.append(envelope.event)
        assert observed_errors == []
    finally:
        client.close(timeout=5.0)
        bridge.close()
        surface.prepare_for_quit()
        surface.close()
        surface.deleteLater()
        controller.setParent(None)
        controller.deleteLater()
        qt_app.processEvents()
