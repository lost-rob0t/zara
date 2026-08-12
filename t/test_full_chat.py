from __future__ import annotations

import concurrent.futures
import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Qt, Signal
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore, MessageRole, MessageStatus
from zara.desktop.windows import FullChatWindow
from zara.runtime import events
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(self) -> None:
        super().__init__()
        self.commands = []

    def submit(self, command):
        self.commands.append(command)
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


def make_window(tmp_path):
    qt_app = app()
    bridge = FakeBridge()
    service = ConversationService(ConversationStore(DatabaseManager(tmp_path / "chat.db")))
    window = FullChatWindow(bridge, service)  # type: ignore[arg-type]
    window.show()
    qt_app.processEvents()
    return qt_app, bridge, service, window


def dispose(window: FullChatWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def emit(bridge: FakeBridge, event) -> None:
    bridge.runtime_event.emit(SimpleNamespace(event=event))
    app().processEvents()


def test_enter_submits_shift_enter_adds_newline_and_stop_cancels(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        window.composer.setFocus()
        window.composer.setPlainText("hello")
        QTest.keyClick(window.composer, Qt.Key.Key_Return)
        qt_app.processEvents()

        assert len(bridge.commands) == 1
        submit = bridge.commands[0]
        assert isinstance(submit, SubmitTurn)
        assert submit.text == "hello"
        assert submit.conversation_id == window.current_conversation_id
        assert window.composer.toPlainText() == ""
        assert window.send_button.isEnabled() is False

        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-qt")
        )
        qt_app.processEvents()
        assert window.stop_button.isEnabled() is True

        window.stop_button.click()
        qt_app.processEvents()
        assert len(bridge.commands) == 2
        assert isinstance(bridge.commands[1], CancelTurn)
        assert bridge.commands[1].turn_id == "turn-qt"

        emit(
            bridge,
            events.TurnCancelled(
                conversation_id=window.current_conversation_id,
                turn_id="turn-qt",
                reason="cancelled in test",
            ),
        )
        assert service.get_state(window.current_conversation_id).active_turn_id is None

        window.composer.setPlainText("line one")
        cursor = window.composer.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        window.composer.setTextCursor(cursor)
        QTest.keyClick(
            window.composer,
            Qt.Key.Key_Return,
            Qt.KeyboardModifier.ShiftModifier,
        )
        qt_app.processEvents()
        assert window.composer.toPlainText() == "line one\n"
        assert len(bridge.commands) == 2
    finally:
        dispose(window)


def test_fake_streaming_updates_one_message_widget_and_code_copy(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
        window.composer.setPlainText("show code")
        window.submit_current_text()
        submit = bridge.commands[0]
        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-stream")
        )
        qt_app.processEvents()

        emit(
            bridge,
            events.AssistantStarted(
                conversation_id=conversation_id,
                turn_id="turn-stream",
            ),
        )
        state = service.get_state(conversation_id)
        assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-stream")
        widget_before = window.message_widgets[assistant.id]

        emit(
            bridge,
            events.AssistantDelta(
                conversation_id=conversation_id,
                turn_id="turn-stream",
                text="Here is code:\n```python\n",
            ),
        )
        emit(
            bridge,
            events.AssistantDelta(
                conversation_id=conversation_id,
                turn_id="turn-stream",
                text='print("hi")\n```',
            ),
        )
        emit(
            bridge,
            events.AssistantComplete(
                conversation_id=conversation_id,
                turn_id="turn-stream",
                text='Here is code:\n```python\nprint("hi")\n```',
            ),
        )

        assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-stream")
        widget_after = window.message_widgets[assistant.id]
        assert widget_after is widget_before
        assert assistant.status is MessageStatus.COMPLETE
        assert assistant.content.endswith('print("hi")\n```')
        assert widget_after.code_blocks == ['print("hi")\n']

        widget_after.code_copy_buttons[0].click()
        qt_app.processEvents()
        assert QApplication.clipboard().text() == 'print("hi")\n'
    finally:
        dispose(window)


def test_failure_is_compact_and_history_rename_search_reload_work(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        original_id = window.current_conversation_id
        window.composer.setPlainText("remember this nebula")
        window.submit_current_text()
        submit = bridge.commands[0]
        bridge.command_completed.emit(
            CommandReceipt(request_id=submit.request_id, turn_id="turn-fail")
        )
        qt_app.processEvents()

        emit(
            bridge,
            events.AssistantFailed(
                conversation_id=original_id,
                turn_id="turn-fail",
                reason="fixture provider failed",
            ),
        )
        state = service.get_state(original_id)
        failed = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-fail")
        widget = window.message_widgets[failed.id]
        assert widget.status_label.text() == "Error"
        assert widget.error_label.text() == "fixture provider failed"

        window.rename_current("Renamed Nebula")
        assert service.get_state(original_id).conversation.title == "Renamed Nebula"

        window.new_chat()
        assert window.current_conversation_id != original_id
        window.refresh_history("nebula")
        assert window.history_list.count() == 1
        item = window.history_list.item(0)
        assert item.text() == "Renamed Nebula"

        window.load_conversation(original_id)
        reloaded = service.get_state(original_id)
        assert any(message.content == "remember this nebula" for message in reloaded.messages)
        assert failed.id in window.message_widgets
    finally:
        dispose(window)


def test_buffered_runtime_response_renders_directly_as_complete(tmp_path):
    _, bridge, service, window = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
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
        assert assistant.content == "one buffered response"
        assert assistant.status is MessageStatus.COMPLETE
        assert window.message_widgets[assistant.id].status_label.text() == ""
    finally:
        dispose(window)
