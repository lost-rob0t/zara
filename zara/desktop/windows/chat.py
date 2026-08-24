"""Persistent full-chat PySide6 surface driven only by RuntimeHost events/commands."""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QCloseEvent
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QFrame,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QScrollArea,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.chat_widgets import ChatComposer, ComposerActionButton, MessageWidget
from zara.desktop.conversation import ConversationService, ConversationUpdate
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import DesktopStatus, INITIAL_STATUS
from zara.desktop.theme import refresh_dynamic_style
from zara.runtime.commands import CancelTurn, CommandReceipt, SubmitTurn


class FullChatWindow(QWidget):
    """Native durable conversation surface shared with Quick Copilot."""

    restart_requested = Signal()
    diagnostics_requested = Signal()
    settings_requested = Signal()
    conversation_changed = Signal(object)

    def __init__(
        self,
        bridge: QtRuntimeBridge,
        conversations: ConversationService,
        parent: Optional[QWidget] = None,
        *,
        manage_runtime_events: bool = True,
    ) -> None:
        super().__init__(parent)
        self.bridge = bridge
        self.conversations = conversations
        self._manage_runtime_events = manage_runtime_events
        self._allow_close = False
        self._status = INITIAL_STATUS
        self._current_conversation_id: Optional[str] = None
        self._message_widgets: dict[str, MessageWidget] = {}
        self._cancel_request_id: Optional[str] = None
        self._cancel_conversation_id: Optional[str] = None

        self.setObjectName("zaraFullChat")
        self.setWindowTitle("Zara")
        self.setMinimumSize(760, 520)
        self.resize(980, 700)

        self.search_edit = QLineEdit()
        self.search_edit.setObjectName("zaraConversationSearch")
        self.search_edit.setPlaceholderText("Search chats")
        self.history_list = QListWidget()
        self.history_list.setObjectName("zaraConversationHistory")
        self.new_chat_button = QPushButton("New chat")
        self.new_chat_button.setObjectName("zaraPrimaryAction")
        self.rename_button = QPushButton("Rename")
        self.rename_button.setObjectName("zaraSecondaryAction")
        self.settings_button = QPushButton("Settings")
        self.settings_button.setObjectName("zaraSecondaryAction")

        sidebar_buttons = QHBoxLayout()
        sidebar_buttons.addWidget(self.new_chat_button)
        sidebar_buttons.addWidget(self.rename_button)

        self.sidebar = QWidget()
        self.sidebar.setObjectName("zaraConversationSidebar")
        sidebar_layout = QVBoxLayout(self.sidebar)
        sidebar_layout.setContentsMargins(16, 18, 16, 16)
        sidebar_layout.setSpacing(10)
        self.brand_label = QLabel("ZARA")
        self.brand_label.setObjectName("zaraBrandName")
        self.history_label = QLabel("Conversations")
        self.history_label.setObjectName("zaraSurfaceName")
        sidebar_layout.addWidget(self.brand_label)
        sidebar_layout.addWidget(self.history_label)
        sidebar_layout.addWidget(self.search_edit)
        sidebar_layout.addWidget(self.history_list, 1)
        sidebar_layout.addLayout(sidebar_buttons)
        sidebar_layout.addWidget(self.settings_button)

        self.title_label = QLabel("Zara")
        self.title_label.setObjectName("zaraConversationTitle")
        self.provider_label = QLabel("Provider: runtime default")
        self.provider_label.setObjectName("zaraProviderStatus")
        self.runtime_status_label = QLabel()
        self.runtime_status_label.setObjectName("zaraRuntimeStatus")
        self.runtime_detail_label = QLabel()
        self.runtime_detail_label.setObjectName("zaraRuntimeDetail")
        self.runtime_detail_label.setWordWrap(True)
        self.command_error_label = QLabel()
        self.command_error_label.setObjectName("zaraCommandError")
        self.command_error_label.setWordWrap(True)
        self.command_error_label.hide()

        self.restart_button = QPushButton("Restart Runtime")
        self.diagnostics_button = QPushButton("Diagnostics")
        self.restart_button.setObjectName("zaraSecondaryAction")
        self.diagnostics_button.setObjectName("zaraSecondaryAction")

        self.header_frame = QFrame()
        self.header_frame.setObjectName("zaraConversationHeader")
        header_top = QHBoxLayout(self.header_frame)
        header_top.setContentsMargins(0, 0, 0, 12)
        header_top.setSpacing(10)
        header_top.addWidget(self.title_label, 1)
        header_top.addWidget(self.provider_label)

        self.status_frame = QFrame()
        self.status_frame.setObjectName("zaraRuntimeRail")
        runtime_row = QHBoxLayout(self.status_frame)
        runtime_row.setContentsMargins(12, 8, 12, 8)
        runtime_row.setSpacing(10)
        self.status_lamp = QFrame()
        self.status_lamp.setObjectName("zaraStatusLamp")
        self.status_lamp.setFixedSize(8, 8)
        runtime_row.addWidget(self.status_lamp)
        runtime_row.addWidget(self.runtime_status_label)
        runtime_row.addWidget(self.runtime_detail_label, 1)
        runtime_row.addWidget(self.restart_button)
        runtime_row.addWidget(self.diagnostics_button)

        self.message_container = QWidget()
        self.message_container.setObjectName("zaraMessageContainer")
        self.message_layout = QVBoxLayout(self.message_container)
        self.message_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self.message_layout.setContentsMargins(4, 6, 10, 6)
        self.message_layout.setSpacing(11)

        self.message_scroll = QScrollArea()
        self.message_scroll.setObjectName("zaraConversationViewport")
        self.message_scroll.setFrameShape(QFrame.Shape.NoFrame)
        self.message_scroll.setWidgetResizable(True)
        self.message_scroll.setWidget(self.message_container)

        self.composer = ChatComposer()
        self.composer.setPlaceholderText("Message Zara…")
        self.composer.setMinimumHeight(70)
        self.composer.setMaximumHeight(130)
        self.action_button = ComposerActionButton()
        self.action_button.setEnabled(False)

        composer_buttons = QVBoxLayout()
        composer_buttons.addWidget(self.action_button)
        composer_buttons.addStretch(1)

        self.composer_shell = QFrame()
        self.composer_shell.setObjectName("zaraComposerShell")
        self.composer_shell.setMaximumHeight(138)
        composer_row = QHBoxLayout(self.composer_shell)
        composer_row.setContentsMargins(8, 8, 8, 8)
        composer_row.setSpacing(8)
        composer_row.addWidget(self.composer, 1)
        composer_row.addLayout(composer_buttons)

        conversation = QWidget()
        conversation.setObjectName("zaraConversationSurface")
        conversation_layout = QVBoxLayout(conversation)
        conversation_layout.setContentsMargins(22, 18, 22, 20)
        conversation_layout.setSpacing(12)
        conversation_layout.addWidget(self.header_frame)
        conversation_layout.addWidget(self.status_frame)
        conversation_layout.addWidget(self.command_error_label)
        conversation_layout.addWidget(self.message_scroll, 1)
        conversation_layout.addWidget(self.composer_shell)

        self.splitter = QSplitter()
        self.splitter.setObjectName("zaraConversationSplitter")
        self.splitter.addWidget(self.sidebar)
        self.splitter.addWidget(conversation)
        self.splitter.setStretchFactor(0, 0)
        self.splitter.setStretchFactor(1, 1)
        self.splitter.setSizes([240, 740])

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.splitter)

        self.search_edit.textChanged.connect(self.refresh_history)
        self.history_list.itemActivated.connect(self._activate_history_item)
        self.history_list.currentItemChanged.connect(self._history_selection_changed)
        self.new_chat_button.clicked.connect(self.new_chat)
        self.rename_button.clicked.connect(lambda _checked=False: self.rename_current())
        self.composer.submit_requested.connect(self.submit_current_text)
        self.composer.textChanged.connect(self._sync_controls)
        self.action_button.clicked.connect(self._activate_composer_action)
        self.restart_button.clicked.connect(self.restart_requested.emit)
        self.diagnostics_button.clicked.connect(self.diagnostics_requested.emit)
        self.settings_button.clicked.connect(self.settings_requested.emit)

        if self._manage_runtime_events:
            self.bridge.runtime_event.connect(self._on_runtime_envelope)
            self.bridge.command_completed.connect(self._on_command_completed)
            self.bridge.command_failed.connect(self._on_command_failed)

        self.set_status(INITIAL_STATUS)
        self._open_initial_conversation()

    @property
    def status(self) -> DesktopStatus:
        return self._status

    @property
    def current_conversation_id(self) -> str:
        assert self._current_conversation_id is not None
        return self._current_conversation_id

    @property
    def message_widgets(self) -> dict[str, MessageWidget]:
        return dict(self._message_widgets)

    def set_status(self, status: DesktopStatus) -> None:
        self._status = status
        self.status_lamp.setProperty("runtimeState", status.state.value)
        self.runtime_status_label.setProperty("runtimeState", status.state.value)
        self.runtime_status_label.setText(status.state.value.replace("-", " ").title())
        self.runtime_detail_label.setText(status.detail or "Zara is ready.")
        self.restart_button.setEnabled(status.state.value != "starting")
        refresh_dynamic_style(self.status_lamp)
        refresh_dynamic_style(self.runtime_status_label)

    def show_raised(self) -> None:
        self.show()
        if self.isMinimized():
            self.showNormal()
        self.raise_()
        self.activateWindow()
        self.composer.setFocus()

    def toggle_visibility(self) -> None:
        if self.isVisible():
            self.hide()
        else:
            self.show_raised()

    def prepare_for_quit(self) -> None:
        self._allow_close = True

    def closeEvent(self, event: QCloseEvent) -> None:  # noqa: N802 - Qt API
        if self._allow_close:
            event.accept()
            return
        self.hide()
        event.ignore()

    def new_chat(self) -> None:
        state = self.conversations.create_conversation()
        self.load_conversation(state.conversation.id)
        self.refresh_history()
        self.conversation_changed.emit(
            ConversationUpdate(
                conversation_id=state.conversation.id,
                metadata_changed=True,
                full_reload=True,
            )
        )
        self.composer.setFocus()

    def open_conversation(self, conversation_id: str) -> None:
        self.load_conversation(conversation_id)
        self.refresh_history()
        self.show_raised()

    def load_conversation(self, conversation_id: str) -> None:
        state = self.conversations.get_state(conversation_id)
        self._current_conversation_id = conversation_id
        self.title_label.setText(state.conversation.title)
        self._update_provider_label()
        self.command_error_label.hide()
        self._render_current()
        self._sync_controls()
        self._select_history_id(conversation_id)

    def rename_current(self, title: Optional[str] = None) -> None:
        if title is None:
            state = self.conversations.get_state(self.current_conversation_id)
            title, accepted = QInputDialog.getText(
                self,
                "Rename chat",
                "Title",
                text=state.conversation.title,
            )
            if not accepted:
                return
        try:
            update = self.conversations.rename_conversation(self.current_conversation_id, title)
        except ValueError:
            return
        self._render_update(update)
        self.refresh_history()
        self.conversation_changed.emit(update)

    def refresh_history(self, query: Optional[str] = None) -> None:
        if query is None:
            query = self.search_edit.text()
        records = self.conversations.list_conversations(query)
        current_id = self._current_conversation_id
        self.history_list.blockSignals(True)
        self.history_list.clear()
        for record in records:
            item = QListWidgetItem(record.title)
            item.setData(Qt.ItemDataRole.UserRole, record.id)
            self.history_list.addItem(item)
            if record.id == current_id:
                self.history_list.setCurrentItem(item)
        self.history_list.blockSignals(False)

    def submit_current_text(self) -> None:
        text = self.composer.toPlainText().strip()
        if not text:
            return
        state = self.conversations.get_state(self.current_conversation_id)
        if state.active_turn_id or self.conversations.has_pending_request(self.current_conversation_id):
            return

        command = SubmitTurn(text=text, conversation_id=self.current_conversation_id)
        _, update = self.conversations.add_user_message(
            self.current_conversation_id,
            text,
            request_id=command.request_id,
        )
        self.composer.clear()
        self.command_error_label.hide()
        self._render_update(update)
        self._sync_controls()
        self.conversation_changed.emit(update)
        self.bridge.submit(command)

    def cancel_active_turn(self) -> None:
        state = self.conversations.get_state(self.current_conversation_id)
        if not state.active_turn_id or state.cancel_request_id is not None:
            return
        command = CancelTurn(turn_id=state.active_turn_id)
        state.cancel_request_id = command.request_id
        self._cancel_request_id = command.request_id
        self._cancel_conversation_id = self.current_conversation_id
        self._sync_controls()
        self.conversation_changed.emit(
            ConversationUpdate(
                conversation_id=self.current_conversation_id,
                active_turn_changed=True,
            )
        )
        self.bridge.submit(command)

    def apply_conversation_update(self, update: Optional[ConversationUpdate]) -> None:
        if update is not None and update.conversation_id == self._current_conversation_id:
            self._render_update(update)
        self._sync_controls()

    def handle_command_completed(
        self,
        receipt: object,
        update: Optional[ConversationUpdate] = None,
    ) -> None:
        request_id = getattr(receipt, "request_id", None)
        if request_id == self._cancel_request_id:
            self._clear_owned_cancellation()
        self.apply_conversation_update(update)

    def handle_command_failed(
        self,
        request_id: str,
        message: str,
        update: Optional[ConversationUpdate] = None,
    ) -> None:
        if request_id == self._cancel_request_id:
            self._clear_owned_cancellation()
            self.command_error_label.setText(message or "Cancellation failed")
            self.command_error_label.show()
            self._sync_controls()
            return
        if update is not None and update.conversation_id == self._current_conversation_id:
            self.command_error_label.setText(message or "Message could not be sent")
            self.command_error_label.show()
        self.apply_conversation_update(update)

    def _open_initial_conversation(self) -> None:
        history = self.conversations.list_conversations(limit=1)
        if history:
            self.load_conversation(history[0].id)
        else:
            state = self.conversations.create_conversation()
            self.load_conversation(state.conversation.id)
        self.refresh_history()

    def _activate_history_item(self, item: QListWidgetItem) -> None:
        conversation_id = item.data(Qt.ItemDataRole.UserRole)
        if conversation_id:
            self.load_conversation(str(conversation_id))

    def _history_selection_changed(
        self,
        current: Optional[QListWidgetItem],
        _previous: Optional[QListWidgetItem],
    ) -> None:
        if current is None:
            return
        conversation_id = current.data(Qt.ItemDataRole.UserRole)
        if conversation_id and str(conversation_id) != self._current_conversation_id:
            self.load_conversation(str(conversation_id))

    def _on_runtime_envelope(self, envelope) -> None:
        event = getattr(envelope, "event", None)
        if event is None:
            return
        self.apply_conversation_update(self.conversations.apply_event(event))

    def _on_command_completed(self, receipt) -> None:
        if not isinstance(receipt, CommandReceipt):
            return
        update = self.conversations.bind_receipt(receipt)
        self.handle_command_completed(receipt, update)

    def _on_command_failed(self, request_id: str, message: str) -> None:
        if request_id == self._cancel_request_id:
            self.handle_command_failed(request_id, message)
            return
        update = self.conversations.mark_command_failed(request_id, message)
        self.handle_command_failed(request_id, message, update)

    def _render_update(self, update: ConversationUpdate) -> None:
        if update.conversation_id != self._current_conversation_id:
            return
        state = self.conversations.get_state(update.conversation_id)
        if update.full_reload:
            self._render_current()
        else:
            for message_id in update.message_ids:
                message = state.message_by_id(message_id)
                if message is None:
                    continue
                widget = self._message_widgets.get(message_id)
                if widget is None:
                    widget = MessageWidget(message)
                    self._message_widgets[message_id] = widget
                    self.message_layout.addWidget(widget)
                else:
                    widget.set_message(message)
        if update.metadata_changed:
            self.title_label.setText(state.conversation.title)
            self._update_provider_label()
            self.refresh_history()
        self._scroll_to_bottom()

    def _render_current(self) -> None:
        while self.message_layout.count():
            item = self.message_layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.setParent(None)
                widget.deleteLater()
        self._message_widgets.clear()
        state = self.conversations.get_state(self.current_conversation_id)
        for message in state.messages:
            widget = MessageWidget(message)
            self._message_widgets[message.id] = widget
            self.message_layout.addWidget(widget)
        self.title_label.setText(state.conversation.title)
        self._update_provider_label()
        self._scroll_to_bottom()

    def _update_provider_label(self) -> None:
        if self._current_conversation_id is None:
            self.provider_label.setText("Provider: runtime default")
            return
        state = self.conversations.get_state(self.current_conversation_id)
        provider = state.provider or "runtime default"
        suffix = f" / {state.model}" if state.model else ""
        self.provider_label.setText(f"Provider: {provider}{suffix}")

    def _sync_controls(self) -> None:
        if self._current_conversation_id is None:
            self.action_button.set_action_mode("send")
            self.action_button.setEnabled(False)
            return
        state = self.conversations.get_state(self.current_conversation_id)
        pending = self.conversations.has_pending_request(self.current_conversation_id)
        active = bool(state.active_turn_id)
        has_text = bool(self.composer.toPlainText().strip())
        if not active:
            state.cancel_request_id = None
        self.action_button.set_action_mode("stop" if active else "send")
        self.action_button.setEnabled(
            active and state.cancel_request_id is None
            or has_text and not pending and not active
        )

    def _activate_composer_action(self) -> None:
        if self.action_button.action_mode == "stop":
            self.cancel_active_turn()
            return
        self.submit_current_text()

    def _clear_owned_cancellation(self) -> None:
        if self._cancel_conversation_id is not None:
            try:
                state = self.conversations.get_state(self._cancel_conversation_id)
            except KeyError:
                state = None
            if state is not None and state.cancel_request_id == self._cancel_request_id:
                state.cancel_request_id = None
        self._cancel_request_id = None
        self._cancel_conversation_id = None

    def _select_history_id(self, conversation_id: str) -> None:
        for index in range(self.history_list.count()):
            item = self.history_list.item(index)
            if str(item.data(Qt.ItemDataRole.UserRole)) == conversation_id:
                self.history_list.blockSignals(True)
                self.history_list.setCurrentItem(item)
                self.history_list.blockSignals(False)
                return

    def _scroll_to_bottom(self) -> None:
        QTimer.singleShot(
            0,
            lambda: self.message_scroll.verticalScrollBar().setValue(
                self.message_scroll.verticalScrollBar().maximum()
            ),
        )


__all__ = ["FullChatWindow"]
