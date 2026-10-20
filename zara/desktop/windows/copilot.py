"""Adaptive single-renderer Copilot presentation seam."""

from __future__ import annotations

from enum import Enum
from typing import Optional

from PySide6.QtCore import QRect, QSettings, Qt, Signal
from PySide6.QtGui import QResizeEvent
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.conversation import ConversationService
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.windows.quick import QuickCopilotWindow


class CopilotPresentation(str, Enum):
    """Visual presentation of the one canonical Copilot renderer."""

    COMPACT = "compact"
    EXPANDED = "expanded"


_GEOMETRY_KEYS = {
    CopilotPresentation.COMPACT: "desktop/copilot/compact-geometry",
    CopilotPresentation.EXPANDED: "desktop/copilot/expanded-geometry",
}


class CopilotWindow(QuickCopilotWindow):
    """One chat renderer that changes presentation without copying state."""

    restart_requested = Signal()
    diagnostics_requested = Signal()

    def __init__(
        self,
        bridge: QtRuntimeBridge,
        conversations: ConversationService,
        *,
        initial_conversation_id: Optional[str] = None,
        settings: Optional[QSettings] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(
            bridge,
            conversations,
            initial_conversation_id=initial_conversation_id,
            settings=settings,
            parent=parent,
        )
        self._presentation = CopilotPresentation.COMPACT
        self._history_open = True
        self._history_user_toggled = False
        self.setObjectName("zaraCopilot")

        self.history_toggle_button = QPushButton("Hide history")
        self.history_toggle_button.setObjectName("zaraSecondaryAction")
        header = self.header_frame.layout()
        header.insertWidget(header.indexOf(self.expand_button), self.history_toggle_button)
        self.history_toggle_button.clicked.connect(self.toggle_history)

        self.history_panel = QWidget(self)
        self.history_panel.setObjectName("zaraConversationHistoryPanel")
        self.history_panel.setMinimumWidth(220)
        self.history_panel.setMaximumWidth(280)
        history_layout = QVBoxLayout(self.history_panel)
        history_layout.setContentsMargins(12, 12, 12, 12)
        history_layout.setSpacing(10)

        self.sidebar_new_chat_button = QPushButton("New chat")
        self.sidebar_new_chat_button.setObjectName("zaraPrimaryAction")
        self.sidebar_new_chat_button.setAccessibleName("Start a new chat")

        history_header = QHBoxLayout()
        history_label = QLabel("Conversations")
        history_label.setObjectName("zaraSurfaceName")
        self.rename_button = QPushButton("Rename")
        self.rename_button.setObjectName("zaraSecondaryAction")
        history_header.addWidget(history_label)
        history_header.addStretch(1)
        history_header.addWidget(self.rename_button)

        self.search_edit = QLineEdit()
        self.search_edit.setObjectName("zaraConversationSearch")
        self.search_edit.setPlaceholderText("Search chats")
        self.history_list = QListWidget()
        self.history_list.setObjectName("zaraConversationHistory")
        self.history_list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.history_list.setTextElideMode(Qt.TextElideMode.ElideRight)

        history_layout.addWidget(self.sidebar_new_chat_button)
        history_layout.addLayout(history_header)
        history_layout.addWidget(self.search_edit)
        history_layout.addWidget(self.history_list)

        root_layout = self.layout()
        self.chat_column = QWidget(self)
        self.chat_column.setObjectName("zaraCopilotChatColumn")
        self.chat_column.setMaximumWidth(760)
        self.chat_column.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        chat_layout = QVBoxLayout(self.chat_column)
        chat_layout.setContentsMargins(0, 0, 0, 0)
        chat_layout.setSpacing(root_layout.spacing())
        for widget in (
            self.command_error_label,
            self.message_scroll,
            self.composer_shell,
        ):
            root_layout.removeWidget(widget)
            chat_layout.addWidget(widget)
        chat_layout.setStretchFactor(self.message_scroll, 1)

        self.copilot_body = QWidget(self)
        self.copilot_body.setObjectName("zaraCopilotBody")
        body_layout = QHBoxLayout(self.copilot_body)
        body_layout.setContentsMargins(0, 0, 0, 0)
        body_layout.setSpacing(root_layout.spacing())
        body_layout.addWidget(self.history_panel)
        self.chat_area = QWidget(self.copilot_body)
        self.chat_area.setObjectName("zaraCopilotChatArea")
        chat_area_layout = QHBoxLayout(self.chat_area)
        chat_area_layout.setContentsMargins(0, 0, 0, 0)
        chat_area_layout.setSpacing(0)
        chat_area_layout.addStretch(1)
        chat_area_layout.addWidget(self.chat_column, 100)
        chat_area_layout.addStretch(1)
        body_layout.addWidget(self.chat_area, 1)
        root_layout.addWidget(self.copilot_body, 1)

        self.search_edit.textChanged.connect(self.refresh_history)
        self.history_list.itemActivated.connect(self._activate_history_item)
        self.rename_button.clicked.connect(lambda _checked=False: self.rename_current())
        self.sidebar_new_chat_button.clicked.connect(self.new_chat)

        self.expand_button.clicked.disconnect()
        self.expand_button.clicked.connect(self.toggle_presentation)
        self.refresh_history()
        self._sync_conversation_title()
        self._apply_presentation()

    @property
    def presentation(self) -> CopilotPresentation:
        return self._presentation

    def set_presentation(self, presentation: CopilotPresentation) -> None:
        """Change layout mode without touching conversation/runtime state."""
        if not isinstance(presentation, CopilotPresentation):
            raise TypeError("presentation must be a CopilotPresentation")
        if presentation is self._presentation:
            return
        self._save_geometry()
        self._presentation = presentation
        self._recover_geometry()
        self._apply_presentation()

    def toggle_presentation(self) -> None:
        target = (
            CopilotPresentation.EXPANDED
            if self._presentation is CopilotPresentation.COMPACT
            else CopilotPresentation.COMPACT
        )
        self.set_presentation(target)

    def toggle_history(self) -> None:
        self._history_open = not self.history_panel.isVisible()
        self._history_user_toggled = True
        self._update_history_layout()

    def resizeEvent(self, event: QResizeEvent) -> None:  # noqa: N802 - Qt API
        super().resizeEvent(event)
        if hasattr(self, "history_panel"):
            self._update_history_layout()

    def bind_conversation(self, conversation_id: str) -> None:
        """Rebind the one renderer to durable state without runtime traffic."""
        super().bind_conversation(conversation_id)
        if self._presentation is CopilotPresentation.EXPANDED and self.width() < 760:
            self._history_open = False
            self._history_user_toggled = True
            self._update_history_layout()
        self._sync_conversation_title()
        self.refresh_history()

    def new_chat(self) -> None:
        super().new_chat()
        self._sync_conversation_title()
        self.refresh_history()

    def refresh_history(self, query: Optional[str] = None) -> None:
        """Project durable conversation metadata into the expanded history list."""
        if query is None:
            query = self.search_edit.text()
        records = self.conversations.list_conversations(query)
        current_id = self.current_conversation_id

        self.history_list.blockSignals(True)
        self.history_list.clear()
        for record in records:
            item = QListWidgetItem(record.title)
            item.setData(Qt.ItemDataRole.UserRole, record.id)
            self.history_list.addItem(item)
            if record.id == current_id:
                self.history_list.setCurrentItem(item)
        self.history_list.blockSignals(False)

    def rename_current(self, title: Optional[str] = None) -> None:
        """Rename the selected durable conversation through ConversationService."""
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
        self._sync_conversation_title()
        self.refresh_history()
        self.conversation_changed.emit(update)

    def _activate_history_item(self, item: QListWidgetItem) -> None:
        conversation_id = item.data(Qt.ItemDataRole.UserRole)
        if conversation_id:
            self.bind_conversation(str(conversation_id))

    def _sync_conversation_title(self) -> None:
        state = self.conversations.get_state(self.current_conversation_id)
        self.title_label.setText(state.conversation.title)

    def _project_messages(self, state):
        return state.messages

    def _saved_geometry(self) -> Optional[QRect]:
        value = self._settings.value(_GEOMETRY_KEYS[self._presentation])
        if isinstance(value, QRect) and value.isValid():
            return QRect(value)
        return None

    def _save_geometry(self) -> None:
        geometry = self.geometry()
        if geometry.isValid():
            self._settings.setValue(_GEOMETRY_KEYS[self._presentation], geometry)

    def _apply_presentation(self) -> None:
        expanded = self._presentation is CopilotPresentation.EXPANDED
        self.setProperty("presentation", self._presentation.value)
        self.expand_button.setText("Chat" if expanded else "History")
        self.expand_button.setToolTip("Hide history" if expanded else "Show history")
        self.new_chat_button.setVisible(not expanded)
        self._update_history_layout()
        self._apply_header_density()
        if expanded:
            self.refresh_history()
        self.setWindowTitle("Zara — Copilot" if expanded else "Ask Zara")

    def _update_history_layout(self) -> None:
        expanded = self._presentation is CopilotPresentation.EXPANDED
        narrow = self.width() < 760
        if not self._history_user_toggled:
            self._history_open = not narrow
        show_history = expanded and self._history_open
        self.history_panel.setMaximumWidth(16777215 if narrow else 280)
        self.history_panel.setVisible(show_history)
        self.chat_area.setVisible(not (expanded and narrow and show_history))
        self.expand_button.setVisible(not (expanded and narrow and show_history))
        self.history_toggle_button.setVisible(expanded)
        label = "Hide recent conversations" if show_history else "Show recent conversations"
        self.history_toggle_button.setText("Chat" if narrow and show_history else "Recent")
        self.history_toggle_button.setAccessibleName(label)
        self.history_toggle_button.setToolTip(label)
