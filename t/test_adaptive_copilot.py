from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, QPoint, QRect, QSettings, Qt, Signal
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
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
    settings = settings or QSettings(str(tmp_path / "copilot.ini"), QSettings.Format.IniFormat)
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


def test_expanded_history_toggle_preserves_draft_and_conversation(tmp_path):
    qt_app, bridge, _, window = make_window(tmp_path)
    try:
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(1000, 700)
        window.show()
        qt_app.processEvents()
        conversation_id = window.current_conversation_id
        window.composer.setPlainText("unfinished draft")

        assert window.history_panel.isVisible()
        assert window.history_toggle_button.accessibleName() == "Hide recent conversations"
        window.history_toggle_button.click()
        qt_app.processEvents()
        assert not window.history_panel.isVisible()
        assert window.chat_column.isVisible()
        assert window.history_toggle_button.accessibleName() == "Show recent conversations"

        window.history_toggle_button.click()
        qt_app.processEvents()
        assert window.history_panel.isVisible()
        assert window.current_conversation_id == conversation_id
        assert window.composer.toPlainText() == "unfinished draft"
        assert bridge.commands == []
    finally:
        dispose(window)


def test_narrow_expanded_history_uses_separate_view(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(520, 520)
        window.show()
        qt_app.processEvents()
        assert not window.history_panel.isVisible()
        assert window.chat_column.isVisible()

        window.history_toggle_button.click()
        qt_app.processEvents()
        assert window.history_panel.isVisible()
        assert not window.chat_column.isVisible()
        assert window.expand_button.isHidden()
        assert window.history_panel.width() >= 400

        window.history_toggle_button.click()
        qt_app.processEvents()
        assert not window.history_panel.isVisible()
        assert window.chat_column.isVisible()
        assert not window.expand_button.isHidden()
    finally:
        dispose(window)


def test_narrow_history_keyboard_toggle_and_selection_returns_to_chat(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        target = service.create_conversation(title="Saved chat")
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(520, 520)
        window.show()
        qt_app.processEvents()

        window.history_toggle_button.setFocus()
        QTest.keyClick(window.history_toggle_button, Qt.Key.Key_Space)
        qt_app.processEvents()
        assert window.history_panel.isVisible()
        assert not window.chat_column.isVisible()

        window.refresh_history()
        item = next(
            window.history_list.item(index)
            for index in range(window.history_list.count())
            if window.history_list.item(index).data(Qt.ItemDataRole.UserRole) == target.conversation.id
        )
        window.history_list.itemActivated.emit(item)
        qt_app.processEvents()
        assert window.current_conversation_id == target.conversation.id
        assert not window.history_panel.isVisible()
        assert window.chat_column.isVisible()
        assert bridge.commands == []
    finally:
        dispose(window)


def test_expanded_chat_column_is_centered_and_bounded(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(1400, 800)
        window.show()
        qt_app.processEvents()
        column = window.chat_column
        assert column.width() <= 760
        assert column.width() >= 730
        column_center = column.mapTo(window, column.rect().center())
        area_center = window.chat_area.mapTo(window, window.chat_area.rect().center())
        assert abs(column_center.x() - area_center.x()) <= 2
    finally:
        dispose(window)


def test_compact_chat_keeps_usable_message_width(tmp_path):
    qt_app, _, service, window = make_window(tmp_path)
    try:
        service.add_user_message(window.current_conversation_id, "A short message", request_id="width")
        window.sync_from_shared_state()
        window.resize(680, 460)
        window.show()
        qt_app.processEvents()
        assert window.chat_column.width() >= 580
        assert window.composer_shell.width() >= 580
    finally:
        dispose(window)


def test_brand_uses_packaged_icon_without_wordmark(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.show()
        qt_app.processEvents()
        assert window.brand_label.isVisible()
        assert window.brand_label.text() == ""
        assert not window.brand_label.pixmap().isNull()
        assert window.brand_label.accessibleName() == "Zara"
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
        window.resize(980, 680)
        window.show()
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


def test_new_chat_immediately_clears_stale_messages_and_selects_new_history(tmp_path):
    qt_app, bridge, service, window = make_window(tmp_path)
    try:
        previous_id = window.current_conversation_id
        previous_message, _ = service.add_user_message(
            previous_id,
            "This must disappear immediately.",
            request_id="stale-before-new-chat",
        )
        window.sync_from_shared_state()
        assert previous_message.id in window.message_widgets

        window.set_presentation(CopilotPresentation.EXPANDED)
        assert window.new_chat_button.isHidden()
        assert not window.sidebar_new_chat_button.isHidden()
        window.sidebar_new_chat_button.click()
        qt_app.processEvents()

        assert window.current_conversation_id != previous_id
        assert window.message_widgets == {}
        assert not window.empty_state.isHidden()
        assert window.title_label.text() == "New chat"
        assert window.history_list.currentItem() is not None
        assert window.history_list.currentItem().data(Qt.ItemDataRole.UserRole) == window.current_conversation_id
        assert bridge.commands == []
    finally:
        dispose(window)


def test_composer_starts_compact_and_grows_only_for_multiline_text(tmp_path):
    qt_app, _, _, window = make_window(tmp_path)
    try:
        window.resize(680, 460)
        window.show()
        qt_app.processEvents()
        compact_height = window.composer.height()

        assert compact_height <= 56
        window.composer.setPlainText("one\ntwo\nthree\nfour")
        qt_app.processEvents()
        assert compact_height < window.composer.height() <= window.composer.maximumHeight()
    finally:
        dispose(window)


def test_short_messages_remain_compact_bubbles_without_large_blank_rows(tmp_path):
    qt_app, _, service, window = make_window(tmp_path)
    try:
        conversation_id = window.current_conversation_id
        for index, text in enumerate(("First short message", "Second short message")):
            service.add_user_message(
                conversation_id,
                text,
                request_id=f"compact-row-{index}",
            )
        window.sync_from_shared_state()
        window.resize(680, 460)
        window.show()
        qt_app.processEvents()

        widgets = list(window.message_widgets.values())
        assert len(widgets) == 2
        assert all(widget.bubble.objectName() == "zaraMessageBubble" for widget in widgets)
        assert all(widget.height() < 96 for widget in widgets)
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
