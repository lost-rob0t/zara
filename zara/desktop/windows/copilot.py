"""Adaptive single-renderer Copilot presentation seam."""

from __future__ import annotations

from enum import Enum
from typing import Optional

from PySide6.QtCore import QRect, QSettings, Qt, Signal
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.conversation import ConversationService
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.windows.quick import QuickCopilotWindow


class CopilotPresentation(str, Enum):
    """Visual presentation of the one canonical Copilot renderer."""

    COMPACT = "compact"
    EXPANDED = "expanded"


_GEOMETRY_KEYS = {
    CopilotPresentation.COMPACT: "desktop/copilot/compact-geometry",
    CopilotPresentation.EXPANDED: "desktop/copilot/expanded-geometry",
}

_DETAIL_RUNTIME_STATES = frozenset(
    {
        DesktopRuntimeState.STARTING,
        DesktopRuntimeState.DISCONNECTED,
        DesktopRuntimeState.ERROR,
    }
)


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
        self.setObjectName("zaraCopilot")

        header_layout = self.header_frame.layout()
        status_layout = self.status_frame.layout()
        assert isinstance(header_layout, QHBoxLayout)
        assert isinstance(status_layout, QHBoxLayout)
        status_layout.removeWidget(self.status_lamp)
        status_layout.removeWidget(self.runtime_status_label)
        header_layout.insertWidget(2, self.status_lamp)
        header_layout.insertWidget(3, self.runtime_status_label)
        self.status_lamp.setAccessibleName("Runtime status indicator")
        self.runtime_status_label.setAccessibleName("Runtime status")

        self.history_panel = QWidget(self)
        self.history_panel.setObjectName("zaraConversationHistoryPanel")
        history_layout = QVBoxLayout(self.history_panel)
        history_layout.setContentsMargins(0, 0, 0, 8)
        history_layout.setSpacing(8)

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

        history_layout.addLayout(history_header)
        history_layout.addWidget(self.search_edit)
        history_layout.addWidget(self.history_list)
        self.layout().insertWidget(1, self.history_panel)

        self.search_edit.textChanged.connect(self.refresh_history)
        self.history_list.itemActivated.connect(self._activate_history_item)
        self.rename_button.clicked.connect(lambda _checked=False: self.rename_current())

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

    def bind_conversation(self, conversation_id: str) -> None:
        """Rebind the one renderer to durable state without runtime traffic."""
        super().bind_conversation(conversation_id)
        self._sync_conversation_title()
        self.refresh_history()

    def new_chat(self) -> None:
        super().new_chat()
        self._sync_conversation_title()
        self.refresh_history()

    def set_status(self, status: DesktopStatus) -> None:
        super().set_status(status)
        if hasattr(self, "_presentation"):
            self._sync_native_chrome()

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

    def _sync_native_chrome(self) -> None:
        expanded = self._presentation is CopilotPresentation.EXPANDED
        self.brand_label.hide()
        self.provider_label.setVisible(expanded)
        self.status_lamp.show()
        self.runtime_status_label.show()
        self.status_frame.setVisible(self._status.state in _DETAIL_RUNTIME_STATES)

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
        self.expand_button.setText("Compact" if expanded else "Expand")
        self.history_panel.setVisible(expanded)
        if expanded:
            self.refresh_history()
        self.setWindowTitle("Zara — Copilot" if expanded else "Ask Zara")
        self._sync_native_chrome()
