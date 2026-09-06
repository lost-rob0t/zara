from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, QPoint, QRect, QSettings, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.windows import CopilotPresentation, CopilotWindow


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


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


def make_window(tmp_path, *, settings: QSettings | None = None):
    qt_app = app()
    bridge = FakeBridge()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "adaptive-copilot.db"))
    )
    window = CopilotWindow(bridge, service, settings=settings)  # type: ignore[arg-type]
    qt_app.processEvents()
    return qt_app, bridge, service, window


def dispose(window: CopilotWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def test_presentation_transition_preserves_one_renderer_state(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
        service.add_user_message(
            conversation_id,
            "existing message",
            request_id="adaptive-existing-message",
        )
        window.sync_from_shared_state()
        qt_app.processEvents()
        message_widgets = window.message_widgets
        assert message_widgets

        window.composer.setPlainText("draft survives mode changes")
        assert window.presentation is CopilotPresentation.COMPACT

        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()
        assert window.presentation is CopilotPresentation.EXPANDED
        assert window.current_conversation_id == conversation_id
        assert window.composer.toPlainText() == "draft survives mode changes"
        assert window.message_widgets == message_widgets

        window.set_presentation(CopilotPresentation.COMPACT)
        qt_app.processEvents()
        assert window.presentation is CopilotPresentation.COMPACT
        assert window.current_conversation_id == conversation_id
        assert window.composer.toPlainText() == "draft survives mode changes"
        assert window.message_widgets == message_widgets
        assert bridge.commands == []
    finally:
        dispose(window)


def test_repeated_presentation_transition_keeps_same_widget_instance(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        identity = id(window)
        for _ in range(4):
            window.set_presentation(CopilotPresentation.EXPANDED)
            window.set_presentation(CopilotPresentation.COMPACT)
            qt_app.processEvents()
        assert id(window) == identity
    finally:
        dispose(window)


def test_canonical_copilot_projects_complete_conversation_in_both_modes(tmp_path):
    qt_app, _, service, window = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
        expected_ids = []
        for index in range(9):
            message, _ = service.add_user_message(
                conversation_id,
                f"message {index}",
                request_id=f"adaptive-message-{index}",
            )
            expected_ids.append(message.id)

        window.sync_from_shared_state()
        qt_app.processEvents()
        assert list(window.message_widgets) == expected_ids

        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()
        assert list(window.message_widgets) == expected_ids

        window.set_presentation(CopilotPresentation.COMPACT)
        qt_app.processEvents()
        assert list(window.message_widgets) == expected_ids
    finally:
        dispose(window)


def test_expanded_history_search_selection_and_rename_use_same_renderer(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        original_id = window.current_conversation_id
        service.rename_conversation(original_id, "Original chat")
        target = service.create_conversation(title="Target history chat")
        service.add_user_message(
            target.conversation.id,
            "history target message",
            request_id="history-target-message",
        )

        assert window.presentation is CopilotPresentation.COMPACT
        assert window.history_panel.isHidden()

        identity = id(window)
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.refresh_history("Target history")
        qt_app.processEvents()

        assert not window.history_panel.isHidden()
        assert window.history_list.count() == 1
        item = window.history_list.item(0)
        assert item.text() == "Target history chat"
        window.history_list.itemActivated.emit(item)
        qt_app.processEvents()

        assert id(window) == identity
        assert window.current_conversation_id == target.conversation.id
        assert bridge.commands == []

        window.rename_current("Renamed in Copilot")
        qt_app.processEvents()
        assert service.get_state(target.conversation.id).conversation.title == "Renamed in Copilot"
        assert window.title_label.text() == "Renamed in Copilot"

        window.set_presentation(CopilotPresentation.COMPACT)
        qt_app.processEvents()
        assert window.history_panel.isHidden()
        assert window.current_conversation_id == target.conversation.id
    finally:
        dispose(window)


def test_compact_and_expanded_geometry_are_saved_and_restored_independently(tmp_path):
    settings = QSettings(str(tmp_path / "copilot.ini"), QSettings.Format.IniFormat)
    qt_app, _, _, window = make_window(tmp_path, settings=settings)
    try:
        compact = QRect(24, 28, 520, 360)
        expanded = QRect(42, 46, 720, 520)

        window.setGeometry(compact)
        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()

        window.setGeometry(expanded)
        window.set_presentation(CopilotPresentation.COMPACT)
        qt_app.processEvents()
        assert window.geometry() == compact

        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()
        assert window.geometry() == expanded

        compact_saved = settings.value("desktop/copilot/compact-geometry")
        expanded_saved = settings.value("desktop/copilot/expanded-geometry")
        assert compact_saved == compact
        assert expanded_saved == expanded
    finally:
        dispose(window)


def test_native_chrome_demotes_healthy_runtime_noise_without_hiding_failures(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.set_status(DesktopStatus(DesktopRuntimeState.IDLE, "Zara is ready"))
        qt_app.processEvents()

        assert window.brand_label.isHidden()
        assert window.provider_label.isHidden()
        assert window.status_frame.isHidden()
        assert not window.status_lamp.isHidden()
        assert not window.runtime_status_label.isHidden()
        assert window.status_lamp.accessibleName() == "Runtime status indicator"
        assert window.runtime_status_label.accessibleName() == "Runtime status"

        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()
        assert not window.provider_label.isHidden()
        assert window.status_frame.isHidden()

        window.set_status(
            DesktopStatus(DesktopRuntimeState.DISCONNECTED, "Daemon unavailable")
        )
        qt_app.processEvents()
        assert not window.status_frame.isHidden()
        assert window.runtime_detail_label.text() == "Daemon unavailable"
    finally:
        dispose(window)


def test_expanded_history_is_sidebar_beside_chat_without_stealing_viewport_height(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(960, 700)
        window.show()
        qt_app.processEvents()

        def window_rect(widget) -> QRect:
            origin = widget.mapTo(window, QPoint(0, 0))
            return QRect(origin, widget.size())

        history_rect = window_rect(window.history_panel)
        message_rect = window_rect(window.message_scroll)
        composer_rect = window_rect(window.composer_shell)

        assert history_rect.right() < message_rect.left()
        assert history_rect.top() <= message_rect.bottom()
        assert message_rect.top() <= history_rect.bottom()
        assert history_rect.width() <= 320
        assert message_rect.width() > history_rect.width()
        assert composer_rect.left() == message_rect.left()
        assert composer_rect.top() > message_rect.top()
    finally:
        dispose(window)
