from __future__ import annotations

import concurrent.futures
import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, QRect, QSettings, QSize, Qt, Signal
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.controller import DesktopController
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRole,
    MessageStatus,
)
from zara.desktop.state import DesktopRuntimeState
from zara.desktop.windows import QuickCopilotWindow
from zara.desktop.windows.quick import recover_quick_geometry
from zara.runtime import events
from zara.runtime.commands import CancelTurn, CommandReceipt, RestartRuntime, SubmitTurn


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
        self.statuses = []
        self.hidden = False
        self.quit_action = FakeAction()

    def show_if_available(self) -> bool:
        return True

    def set_status(self, status) -> None:
        self.statuses.append(status)

    def hide(self) -> None:
        self.hidden = True


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

    def start(self):
        self.start_calls += 1
        return completed(None)

    def shutdown(self, reason=""):
        self.shutdown_calls.append(reason)
        return completed(None)


def make_controller(tmp_path):
    qt_app = app()
    bridge = FakeBridge()
    host = FakeHost()
    tray = FakeTray()
    database = DatabaseManager(tmp_path / "quick.db")
    service = ConversationService(ConversationStore(database))
    controller = DesktopController(
        qt_app,
        host,  # type: ignore[arg-type]
        bridge,  # type: ignore[arg-type]
        tray_factory=lambda: tray,  # type: ignore[arg-type]
        conversation_service=service,
    )
    qt_app.processEvents()
    assert controller.quick_window is not None
    return qt_app, controller, bridge, tray, service, database


def dispose_controller(controller: DesktopController) -> None:
    controller.bridge.close()
    controller.tray.hide()
    controller.window.prepare_for_quit()
    controller.window.close()
    if controller.quick_window is not None:
        controller.quick_window.prepare_for_quit()
        controller.quick_window.close()
        controller.quick_window.deleteLater()
    controller.window.deleteLater()
    controller.setParent(None)
    controller.deleteLater()
    app().processEvents()


def emit(bridge: FakeBridge, event) -> None:
    bridge.runtime_event.emit(SimpleNamespace(event=event))
    app().processEvents()


def test_quick_is_one_reused_keyboard_first_window(tmp_path):
    qt_app, controller, bridge, tray, service, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        original = quick
        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is original
        assert quick.isVisible()
        assert quick.composer.hasFocus()

        quick.composer.setPlainText("hello quick")
        QTest.keyClick(quick.composer, Qt.Key.Key_Return)
        qt_app.processEvents()
        assert len(bridge.commands) == 1
        submit = bridge.commands[0]
        assert isinstance(submit, SubmitTurn)
        assert submit.text == "hello quick"
        assert submit.conversation_id == quick.current_conversation_id
        assert len(
            [
                message
                for message in service.get_state(quick.current_conversation_id).messages
                if message.role is MessageRole.USER
            ]
        ) == 1

        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-quick")
        )
        qt_app.processEvents()
        assert quick.action_button.isEnabled()
        assert quick.action_button.action_mode == "stop"

        quick.composer.setPlainText("line one")
        cursor = quick.composer.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        quick.composer.setTextCursor(cursor)
        QTest.keyClick(
            quick.composer,
            Qt.Key.Key_Return,
            Qt.KeyboardModifier.ShiftModifier,
        )
        qt_app.processEvents()
        assert quick.composer.toPlainText() == "line one\n"
        assert len(bridge.commands) == 1

        QTest.keyClick(quick.composer, Qt.Key.Key_Escape)
        qt_app.processEvents()
        assert not quick.isVisible()

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is original
        assert quick.isVisible()
        assert quick.composer.hasFocus()

        assert quick.close() is False
        qt_app.processEvents()
        assert not quick.isVisible()
    finally:
        dispose_controller(controller)


def test_quick_copilot_exposes_signal_cabin_visual_hierarchy(tmp_path):
    _, controller, _, _, _, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        assert quick.objectName() == "zaraQuickCopilot"
        assert quick.header_frame.objectName() == "zaraQuickHeader"
        assert quick.status_frame.objectName() == "zaraRuntimeRail"
        assert quick.status_lamp.objectName() == "zaraStatusLamp"
        assert quick.status_lamp.property("runtimeState") == "starting"
        assert quick.message_scroll.objectName() == "zaraConversationViewport"
        assert quick.composer_shell.objectName() == "zaraComposerShell"
        assert quick.action_button.objectName() == "zaraComposerAction"
        assert quick.action_button.action_mode == "send"
        assert quick.action_button.accessibleName() == "Send message"
    finally:
        dispose_controller(controller)


def test_quick_send_action_tracks_meaningful_composer_text(tmp_path):
    qt_app, controller, _, _, _, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        assert quick.action_button.isEnabled() is False
        quick.composer.setPlainText("route this")
        qt_app.processEvents()
        assert quick.action_button.isEnabled() is True
        assert quick.action_button.action_mode == "send"
        quick.composer.setPlainText("   ")
        qt_app.processEvents()
        assert quick.action_button.isEnabled() is False
    finally:
        dispose_controller(controller)


def test_handoff_preserves_exact_shared_state_without_resubmit_or_duplication(tmp_path):
    qt_app, controller, bridge, _, service, database = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        quick_id = quick.current_conversation_id
        other = service.create_conversation("Other history")
        controller.window.load_conversation(other.conversation.id)
        assert controller.window.current_conversation_id == other.conversation.id
        assert quick.current_conversation_id == quick_id

        quick.composer.setPlainText("stream this")
        quick.submit_current_text()
        submit = bridge.commands[-1]
        assert isinstance(submit, SubmitTurn)
        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-shared")
        )
        qt_app.processEvents()

        emit(
            bridge,
            events.AssistantStarted(
                conversation_id=quick_id,
                turn_id="turn-shared",
            ),
        )
        emit(
            bridge,
            events.AssistantDelta(
                conversation_id=quick_id,
                turn_id="turn-shared",
                text="partial ",
            ),
        )
        emit(
            bridge,
            events.AssistantDelta(
                conversation_id=quick_id,
                turn_id="turn-shared",
                text="response",
            ),
        )

        state = service.get_state(quick_id)
        assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-shared")
        assert assistant is not None
        assert assistant.content == "partial response"
        assert assistant.status is MessageStatus.STREAMING
        assert state.active_turn_id == "turn-shared"
        assert quick.message_widgets[assistant.id].message.content == "partial response"

        conversation_count = len(service.list_conversations())
        command_count = len(bridge.commands)
        controller.expand_quick_to_full_chat()
        qt_app.processEvents()

        assert controller.window.current_conversation_id == quick_id
        assert quick.current_conversation_id == quick_id
        assert len(service.list_conversations()) == conversation_count
        assert len(bridge.commands) == command_count
        assert service.get_state(quick_id).active_turn_id == "turn-shared"
        assert controller.window.message_widgets[assistant.id].message.content == "partial response"

        emit(
            bridge,
            events.ToolStarted(
                conversation_id=quick_id,
                turn_id="turn-shared",
                tool_run_id="tool-1",
                tool_name="search",
            ),
        )
        tool_messages = [
            message for message in state.messages if message.role is MessageRole.TOOL
        ]
        assert len(tool_messages) == 1
        assert tool_messages[0].content == "search: running"
        assert tool_messages[0].id in quick.message_widgets
        assert tool_messages[0].id in controller.window.message_widgets

        emit(
            bridge,
            events.ProviderChanged(
                conversation_id=quick_id,
                provider="openrouter",
                model="test-model",
            ),
        )
        assert "openrouter" in quick.provider_label.text()
        assert "openrouter" in controller.window.provider_label.text()

        emit(
            bridge,
            events.AssistantComplete(
                conversation_id=quick_id,
                turn_id="turn-shared",
                text="partial response",
            ),
        )
        emit(
            bridge,
            events.AgentCompleted(
                conversation_id=quick_id,
                turn_id="turn-shared",
            ),
        )
        assert state.active_turn_id is None
        assistant_rows = [
            message
            for message in state.messages
            if message.role is MessageRole.ASSISTANT and message.turn_id == "turn-shared"
        ]
        assert len(assistant_rows) == 1
        assert assistant_rows[0].content == "partial response"

        reloaded = ConversationService(ConversationStore(database)).get_state(quick_id)
        assert [message.content for message in reloaded.messages] == [
            message.content for message in state.messages
        ]

        controller.window.load_conversation(other.conversation.id)
        assert controller.window.current_conversation_id == other.conversation.id
        assert quick.current_conversation_id == quick_id
    finally:
        dispose_controller(controller)


def test_buffered_response_and_failures_project_consistently(tmp_path):
    qt_app, controller, bridge, _, service, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        conversation_id = quick.current_conversation_id
        emit(
            bridge,
            events.ResponseText(
                conversation_id=conversation_id,
                turn_id="buffered-turn",
                text="one buffered response",
            ),
        )
        assistant = service.get_state(conversation_id).latest_message(
            role=MessageRole.ASSISTANT,
            turn_id="buffered-turn",
        )
        assert assistant is not None
        assert assistant.content == "one buffered response"
        assert assistant.status is MessageStatus.COMPLETE
        assert quick.message_widgets[assistant.id].message.content == "one buffered response"

        quick.composer.setPlainText("rejected")
        quick.submit_current_text()
        rejected = bridge.commands[-1]
        bridge.command_failed.emit(rejected.request_id, "runtime unavailable")
        qt_app.processEvents()
        assert not quick.command_error_label.isHidden()
        assert "runtime unavailable" in quick.command_error_label.text()
        assert not service.has_pending_request(conversation_id)

        emit(
            bridge,
            events.ProviderUnavailable(
                conversation_id=conversation_id,
                reason="provider offline",
            ),
        )
        assert quick.status.state is DesktopRuntimeState.DISCONNECTED
        assert controller.window.status.state is DesktopRuntimeState.DISCONNECTED
    finally:
        dispose_controller(controller)


def test_stop_uses_canonical_active_turn_and_cancel_propagates(tmp_path):
    qt_app, controller, bridge, _, service, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        conversation_id = quick.current_conversation_id
        quick.composer.setPlainText("cancel me")
        quick.submit_current_text()
        submit = bridge.commands[-1]
        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-cancel")
        )
        qt_app.processEvents()

        quick.action_button.click()
        qt_app.processEvents()
        cancel = bridge.commands[-1]
        assert isinstance(cancel, CancelTurn)
        assert cancel.turn_id == "turn-cancel"

        emit(
            bridge,
            events.TurnCancelled(
                turn_id="turn-cancel",
                reason="cancel command",
            ),
        )
        assert service.get_state(conversation_id).active_turn_id is None
        assert quick.action_button.action_mode == "send"
        assert not quick.action_button.isEnabled()

        quick.composer.setPlainText("second turn")
        quick.submit_current_text()
        second = bridge.commands[-1]
        bridge.command_completed.emit(
            CommandReceipt(request_id=second.request_id, turn_id="turn-second")
        )
        qt_app.processEvents()
        quick.action_button.click()
        assert isinstance(bridge.commands[-1], CancelTurn)
        assert bridge.commands[-1].turn_id == "turn-second"
        assert bridge.commands[-1].turn_id != "turn-cancel"

        controller.restart_runtime()
        assert isinstance(bridge.commands[-1], RestartRuntime)
        emit(
            bridge,
            events.TurnCancelled(
                turn_id="turn-second",
                reason="runtime restart",
            ),
        )
        emit(bridge, events.RuntimeStopped(reason="runtime restart"))
        assert service.get_state(conversation_id).active_turn_id is None
        assert quick.action_button.action_mode == "send"
        assert not quick.action_button.isEnabled()
    finally:
        dispose_controller(controller)


def test_quick_new_chat_is_durable_and_reopen_keeps_binding(tmp_path):
    qt_app, controller, _, tray, service, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        before = len(service.list_conversations())
        quick.new_chat()
        created_id = quick.current_conversation_id
        assert len(service.list_conversations()) == before + 1

        quick.hide()
        tray.quick_requested.emit()
        qt_app.processEvents()
        assert quick.current_conversation_id == created_id

        full_only = service.create_conversation("Full only")
        controller.window.load_conversation(full_only.conversation.id)
        assert quick.current_conversation_id == created_id
    finally:
        dispose_controller(controller)


def test_interrupted_persisted_turn_recovers_without_stale_active_id(tmp_path):
    database = DatabaseManager(tmp_path / "interrupted.db")
    first_service = ConversationService(ConversationStore(database))
    state = first_service.create_conversation()
    first_service.apply_event(
        events.AssistantStarted(
            conversation_id=state.conversation.id,
            turn_id="interrupted-turn",
        )
    )
    assert state.active_turn_id == "interrupted-turn"

    recovered_service = ConversationService(ConversationStore(database))
    recovered = recovered_service.get_state(state.conversation.id)
    assert recovered.active_turn_id is None
    assistant = recovered.latest_message(
        role=MessageRole.ASSISTANT,
        turn_id="interrupted-turn",
    )
    assert assistant is not None
    assert assistant.status is MessageStatus.CANCELLED
    assert "Interrupted" in assistant.error


def test_recover_quick_geometry_keeps_valid_and_clamps_partial_windows():
    screen = QRect(0, 0, 1920, 1080)
    valid = QRect(100, 120, 680, 460)
    assert recover_quick_geometry(valid, [screen]) == valid

    partial = QRect(-200, 100, 680, 460)
    recovered = recover_quick_geometry(partial, [screen])
    assert recovered.left() == 12
    assert recovered.top() == 100
    assert screen.contains(recovered.adjusted(-12, -12, 12, 12))


def test_recover_quick_geometry_handles_offscreen_disconnected_and_preferred_screen():
    primary = QRect(0, 0, 1920, 1080)
    secondary = QRect(1920, 0, 1280, 1024)
    offscreen = QRect(5000, 5000, 680, 460)

    recovered = recover_quick_geometry(
        offscreen,
        [primary, secondary],
        preferred_screen=secondary,
    )
    assert secondary.contains(recovered.center())

    disconnected = recover_quick_geometry(offscreen, [primary])
    assert primary.contains(disconnected.center())

    no_saved = recover_quick_geometry(
        None,
        [primary, secondary],
        preferred_screen=secondary,
        default_size=QSize(700, 500),
    )
    assert secondary.contains(no_saved.center())
    assert no_saved.size() == QSize(700, 500)


def test_recover_quick_geometry_clamps_after_resolution_change():
    small = QRect(0, 0, 800, 600)
    huge = QRect(20, 20, 1400, 1000)
    recovered = recover_quick_geometry(huge, [small])
    assert recovered.width() <= 800 - 24
    assert recovered.height() <= 600 - 24
    assert recovered.left() >= 12
    assert recovered.top() >= 12
