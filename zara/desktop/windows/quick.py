"""Compact Quick Copilot surface over Zara's shared desktop conversation state."""

from __future__ import annotations

from typing import Optional, Sequence

from PySide6.QtCore import QRect, QSettings, QSize, Qt, QTimer, Signal
from PySide6.QtGui import QCloseEvent, QCursor, QHideEvent, QKeyEvent, QShowEvent
from PySide6.QtWidgets import (
    QApplication,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QScrollArea,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.chat_widgets import ChatComposer, MessageWidget
from zara.desktop.conversation import ConversationService, ConversationUpdate
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import DesktopStatus, INITIAL_STATUS
from zara.runtime.commands import CancelTurn, SubmitTurn

_DEFAULT_SIZE = QSize(680, 460)
_GEOMETRY_KEY = "desktop/quick-copilot/geometry"
_MAX_VISIBLE_MESSAGES = 6


def recover_quick_geometry(
    saved: Optional[QRect],
    screens: Sequence[QRect],
    *,
    preferred_screen: Optional[QRect] = None,
    default_size: QSize = _DEFAULT_SIZE,
    margin: int = 12,
) -> QRect:
    """Recover a Quick Copilot window inside the current virtual desktop."""
    available = [QRect(screen) for screen in screens if screen.isValid()]
    if not available:
        if saved is not None and saved.isValid():
            return QRect(saved)
        return QRect(0, 0, default_size.width(), default_size.height())

    target: Optional[QRect] = None
    candidate = QRect(saved) if saved is not None and saved.isValid() else None
    if candidate is not None:
        intersections = [candidate.intersected(screen) for screen in available]
        areas = [rect.width() * rect.height() if rect.isValid() else 0 for rect in intersections]
        best_index = max(range(len(areas)), key=areas.__getitem__)
        if areas[best_index] > 0:
            target = available[best_index]

    if target is None:
        if preferred_screen is not None:
            for screen in available:
                if screen == preferred_screen or screen.intersects(preferred_screen):
                    target = screen
                    break
        target = target or available[0]
        width = min(default_size.width(), max(1, target.width() - 2 * margin))
        height = min(default_size.height(), max(1, target.height() - 2 * margin))
        x = target.x() + (target.width() - width) // 2
        y = target.y() + (target.height() - height) // 2
        candidate = QRect(x, y, width, height)

    assert candidate is not None
    width = min(candidate.width(), max(1, target.width() - 2 * margin))
    height = min(candidate.height(), max(1, target.height() - 2 * margin))
    left = target.left() + margin
    top = target.top() + margin
    right = target.right() - margin - width + 1
    bottom = target.bottom() - margin - height + 1
    x = max(left, min(candidate.x(), right))
    y = max(top, min(candidate.y(), bottom))
    return QRect(x, y, width, height)


class QuickComposer(ChatComposer):
    """Quick composer with Escape-to-hide in addition to chat key behavior."""

    escape_requested = Signal()

    def keyPressEvent(self, event: QKeyEvent) -> None:  # noqa: N802 - Qt API
        if event.key() == Qt.Key.Key_Escape:
            self.escape_requested.emit()
            event.accept()
            return
        super().keyPressEvent(event)


class QuickCopilotWindow(QWidget):
    """Keyboard-first projection over the shared ConversationService."""

    expand_requested = Signal(str)
    conversation_changed = Signal(object)

    def __init__(
        self,
        bridge: QtRuntimeBridge,
        conversations: ConversationService,
        *,
        initial_conversation_id: Optional[str] = None,
        settings: Optional[QSettings] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.bridge = bridge
        self.conversations = conversations
        self._settings = settings or QSettings()
        self._status = INITIAL_STATUS
        self._allow_close = False
        self._conversation_id = self._resolve_initial_conversation(initial_conversation_id)
        self._message_widgets: dict[str, MessageWidget] = {}
        self._rendered_message_ids: tuple[str, ...] = ()
        self._cancel_request_id: Optional[str] = None
        self._cancel_conversation_id: Optional[str] = None
        self._submit_request_id: Optional[str] = None

        self.setWindowTitle("Ask Zara")
        self.setWindowFlag(Qt.WindowType.Tool, True)
        self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, True)
        self.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose, False)
        self.setMinimumSize(480, 320)
        self.resize(_DEFAULT_SIZE)

        self.title_label = QLabel("Ask Zara")
        self.title_label.setObjectName("zaraQuickTitle")
        self.provider_label = QLabel()
        self.provider_label.setObjectName("zaraQuickProvider")
        self.new_chat_button = QPushButton("New Chat")
        self.expand_button = QPushButton("Open in Chat")

        header = QHBoxLayout()
        header.addWidget(self.title_label)
        header.addStretch(1)
        header.addWidget(self.provider_label)
        header.addWidget(self.new_chat_button)
        header.addWidget(self.expand_button)

        self.runtime_status_label = QLabel()
        self.runtime_status_label.setObjectName("zaraQuickRuntimeStatus")
        self.runtime_detail_label = QLabel()
        self.runtime_detail_label.setObjectName("zaraQuickRuntimeDetail")
        self.runtime_detail_label.setWordWrap(True)
        self.command_error_label = QLabel()
        self.command_error_label.setObjectName("zaraQuickCommandError")
        self.command_error_label.setWordWrap(True)
        self.command_error_label.hide()

        status_row = QHBoxLayout()
        status_row.addWidget(self.runtime_status_label)
        status_row.addWidget(self.runtime_detail_label, 1)

        self.message_container = QWidget()
        self.message_layout = QVBoxLayout(self.message_container)
        self.message_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self.message_layout.setContentsMargins(6, 6, 6, 6)
        self.message_layout.setSpacing(6)

        self.message_scroll = QScrollArea()
        self.message_scroll.setWidgetResizable(True)
        self.message_scroll.setWidget(self.message_container)

        self.composer = QuickComposer()
        self.composer.setPlaceholderText("Ask Zara…")
        self.composer.setMaximumHeight(120)
        self.setFocusProxy(self.composer)
        self.send_button = QPushButton("Send")
        self.stop_button = QPushButton("Stop")

        buttons = QVBoxLayout()
        buttons.addWidget(self.send_button)
        buttons.addWidget(self.stop_button)
        buttons.addStretch(1)

        composer_row = QHBoxLayout()
        composer_row.addWidget(self.composer, 1)
        composer_row.addLayout(buttons)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(10, 10, 10, 10)
        layout.addLayout(header)
        layout.addLayout(status_row)
        layout.addWidget(self.command_error_label)
        layout.addWidget(self.message_scroll, 1)
        layout.addLayout(composer_row)

        self.composer.submit_requested.connect(self.submit_current_text)
        self.composer.escape_requested.connect(self.hide)
        self.send_button.clicked.connect(self.submit_current_text)
        self.stop_button.clicked.connect(self.cancel_active_turn)
        self.new_chat_button.clicked.connect(self.new_chat)
        self.expand_button.clicked.connect(self._request_expand)

        self.set_status(INITIAL_STATUS)
        self.sync_from_shared_state()

    @property
    def current_conversation_id(self) -> str:
        return self._conversation_id

    @property
    def message_widgets(self) -> dict[str, MessageWidget]:
        return dict(self._message_widgets)

    @property
    def status(self) -> DesktopStatus:
        return self._status

    def bind_conversation(self, conversation_id: str) -> None:
        self.conversations.get_state(conversation_id)
        self._conversation_id = conversation_id
        self.command_error_label.hide()
        self._rendered_message_ids = ()
        self.sync_from_shared_state()

    def new_chat(self) -> None:
        state = self.conversations.create_conversation()
        self.bind_conversation(state.conversation.id)
        self.conversation_changed.emit(
            ConversationUpdate(
                conversation_id=state.conversation.id,
                metadata_changed=True,
                full_reload=True,
            )
        )
        self.composer.setFocus()

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
        self._submit_request_id = command.request_id
        self.composer.clear()
        self.command_error_label.hide()
        self.sync_from_shared_state()
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

    def set_status(self, status: DesktopStatus) -> None:
        self._status = status
        self.runtime_status_label.setText(status.state.value.replace("-", " ").title())
        self.runtime_detail_label.setText(status.detail or "Zara is ready.")

    def sync_from_shared_state(self, _event: object = None) -> None:
        state = self.conversations.get_state(self.current_conversation_id)
        visible = state.messages[-_MAX_VISIBLE_MESSAGES:]
        visible_ids = tuple(message.id for message in visible)
        if visible_ids != self._rendered_message_ids:
            self._rebuild_messages(visible)
        else:
            for message in visible:
                widget = self._message_widgets.get(message.id)
                if widget is not None:
                    widget.set_message(message)
        self._update_provider_label()
        self._sync_controls()
        self._scroll_to_bottom()

    def handle_command_completed(self, receipt: object) -> None:
        request_id = getattr(receipt, "request_id", None)
        if request_id == self._cancel_request_id:
            self._clear_owned_cancellation()
        if request_id == self._submit_request_id:
            self._submit_request_id = None
        self.sync_from_shared_state()

    def handle_command_failed(self, request_id: str, message: str) -> None:
        relevant = False
        if request_id == self._cancel_request_id:
            self._clear_owned_cancellation()
            relevant = True
        if request_id == self._submit_request_id:
            self._submit_request_id = None
            relevant = True
        if relevant:
            self.command_error_label.setText(message or "Runtime command failed")
            self.command_error_label.show()
        self.sync_from_shared_state()

    def show_raised(self) -> None:
        self._recover_geometry()
        self.show()
        if self.isMinimized():
            self.showNormal()
        self._focus_composer()
        QTimer.singleShot(0, self._focus_composer)

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
        self._save_geometry()
        self.hide()
        event.ignore()

    def hideEvent(self, event: QHideEvent) -> None:  # noqa: N802 - Qt API
        self._save_geometry()
        super().hideEvent(event)

    def showEvent(self, event: QShowEvent) -> None:  # noqa: N802 - Qt API
        super().showEvent(event)
        QTimer.singleShot(0, self._focus_composer)

    def keyPressEvent(self, event: QKeyEvent) -> None:  # noqa: N802 - Qt API
        if event.key() == Qt.Key.Key_Escape:
            self.hide()
            event.accept()
            return
        super().keyPressEvent(event)

    def _focus_composer(self) -> None:
        if not self.isVisible():
            return
        app = QApplication.instance()
        if isinstance(app, QApplication):
            app.setActiveWindow(self)
        self.raise_()
        self.activateWindow()
        self.composer.setFocus(Qt.FocusReason.ActiveWindowFocusReason)

    def _request_expand(self) -> None:
        self.expand_requested.emit(self.current_conversation_id)

    def _resolve_initial_conversation(self, requested: Optional[str]) -> str:
        if requested:
            self.conversations.get_state(requested)
            return requested
        history = self.conversations.list_conversations(limit=1)
        if history:
            return history[0].id
        return self.conversations.create_conversation().conversation.id

    def _rebuild_messages(self, messages) -> None:
        while self.message_layout.count():
            item = self.message_layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()
        self._message_widgets.clear()
        for message in messages:
            widget = MessageWidget(message)
            self._message_widgets[message.id] = widget
            self.message_layout.addWidget(widget)
        self._rendered_message_ids = tuple(message.id for message in messages)

    def _update_provider_label(self) -> None:
        state = self.conversations.get_state(self.current_conversation_id)
        provider = state.provider or "runtime default"
        suffix = f" / {state.model}" if state.model else ""
        self.provider_label.setText(f"{provider}{suffix}")

    def _sync_controls(self) -> None:
        state = self.conversations.get_state(self.current_conversation_id)
        pending = self.conversations.has_pending_request(self.current_conversation_id)
        active = bool(state.active_turn_id)
        if not active:
            state.cancel_request_id = None
        self.send_button.setEnabled(not pending and not active)
        self.stop_button.setEnabled(active and state.cancel_request_id is None)

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

    def _scroll_to_bottom(self) -> None:
        QTimer.singleShot(
            0,
            lambda: self.message_scroll.verticalScrollBar().setValue(
                self.message_scroll.verticalScrollBar().maximum()
            ),
        )

    def _saved_geometry(self) -> Optional[QRect]:
        value = self._settings.value(_GEOMETRY_KEY)
        if isinstance(value, QRect) and value.isValid():
            return QRect(value)
        return None

    def _save_geometry(self) -> None:
        geometry = self.geometry()
        if geometry.isValid():
            self._settings.setValue(_GEOMETRY_KEY, geometry)

    def _recover_geometry(self) -> None:
        app = QApplication.instance()
        if not isinstance(app, QApplication):
            return
        screens = [screen.availableGeometry() for screen in app.screens()]
        preferred = app.screenAt(QCursor.pos()) or app.primaryScreen()
        preferred_geometry = preferred.availableGeometry() if preferred is not None else None
        recovered = recover_quick_geometry(
            self._saved_geometry(),
            screens,
            preferred_screen=preferred_geometry,
            default_size=self.size() if self.size().isValid() else _DEFAULT_SIZE,
        )
        self.setGeometry(recovered)


__all__ = ["QuickCopilotWindow", "QuickComposer", "recover_quick_geometry"]
