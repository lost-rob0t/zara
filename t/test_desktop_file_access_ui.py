"""Desktop per-chat file access selector and the in-process control port."""

from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.agent.tools.file_access import FileAccessPolicy
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.file_access import (
    InProcessFileAccessControl,
    file_access_control_for_host,
)
from zara.desktop.windows import CopilotWindow
from zara.database import DatabaseManager


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def submit(self, command):
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


class _FakeHost:
    pass


class _FakeBackend:
    def __init__(self, policy):
        self.file_access_policy = policy


def qt_app():
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_window(tmp_path, file_access=None):
    application = qt_app()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "chat.db"))
    )
    window = CopilotWindow(FakeBridge(), service, file_access=file_access)
    window.show()
    application.processEvents()
    return application, service, window


def close(window) -> None:
    window.close()
    window.deleteLater()
    qt_app().processEvents()


def test_in_process_control_resolves_and_mutates_modes():
    policy = FileAccessPolicy(default_mode="sandbox")
    control = InProcessFileAccessControl(policy)

    assert control.mode_for("chat-desktop-1") == "sandbox"
    assert control.set_mode("chat-desktop-1", "ask") == ""
    assert control.mode_for("chat-desktop-1") == "ask"
    assert control.available() is True


def test_in_process_control_reports_conflicts_without_raising(monkeypatch):
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("chat-1", "readonly", expected_revision=0, actor="other")
    control = InProcessFileAccessControl(policy)
    # Simulate another surface winning a mutation between read and write.
    monkeypatch.setattr(policy, "revision", lambda _conversation_id: 0)

    error = control.set_mode("chat-1", "full")
    assert "File access change failed" in error
    assert control.mode_for("chat-1") == "readonly"


def test_host_resolver_reads_runtime_backend_policy():
    policy = FileAccessPolicy(default_mode="sandbox")
    host = _FakeHost()
    host.backend = _FakeBackend(policy)
    control = file_access_control_for_host(host)
    assert control is not None
    assert control.mode_for("any-chat") == "sandbox"

    assert file_access_control_for_host(_FakeHost()) is None


def test_window_hides_selector_without_control(tmp_path):
    application, service, window = make_window(tmp_path)
    try:
        assert window.file_access_combo.isHidden()
    finally:
        close(window)


def test_window_selector_reflects_and_mutates_chat_mode(tmp_path):
    policy = FileAccessPolicy(default_mode="sandbox")
    control = InProcessFileAccessControl(policy)
    application, service, window = make_window(tmp_path, file_access=control)
    try:
        assert not window.file_access_combo.isHidden()
        assert window.file_access_combo.currentData() == "sandbox"

        policy.set_mode(
            window.current_conversation_id, "ask", expected_revision=0, actor="test"
        )
        window._sync_file_access_mode()
        assert window.file_access_combo.currentData() == "ask"

        full_index = [
            index
            for index in range(window.file_access_combo.count())
            if window.file_access_combo.itemData(index) == "full"
        ][0]
        window.file_access_combo.setCurrentIndex(full_index)
        assert control.mode_for(window.current_conversation_id) == "full"

        other = service.create_conversation()
        window.bind_conversation(other.conversation.id)
        assert window.file_access_combo.currentData() == "sandbox"
    finally:
        close(window)


def test_selector_reports_failure_and_restores(tmp_path):
    class _RejectingControl(InProcessFileAccessControl):
        def set_mode(self, conversation_id, mode):
            return "File access change failed: rejected by runtime"

    control = _RejectingControl(FileAccessPolicy(default_mode="sandbox"))
    application, service, window = make_window(tmp_path, file_access=control)
    try:
        full_index = [
            index
            for index in range(window.file_access_combo.count())
            if window.file_access_combo.itemData(index) == "full"
        ][0]
        window.file_access_combo.setCurrentIndex(full_index)
        assert not window.command_error_label.isHidden()
        assert window.file_access_combo.currentData() == "sandbox"
    finally:
        close(window)
