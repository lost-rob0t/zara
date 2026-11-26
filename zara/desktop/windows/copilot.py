"""Adaptive single-renderer Copilot presentation seam."""

from __future__ import annotations

from enum import Enum
from typing import Optional

from PySide6.QtCore import QEvent, QRect, QSettings, Qt, Signal
from PySide6.QtGui import QKeySequence, QShortcut
from PySide6.QtWidgets import (
    QApplication,
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
from zara.desktop.inspector import InspectorDrawer
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import DesktopRuntimeState
from zara.desktop.windows.quick import QuickCopilotWindow


class CopilotPresentation(str, Enum):
    """Visual presentation of the one canonical Copilot renderer."""

    COMPACT = "compact"
    EXPANDED = "expanded"


_GEOMETRY_KEYS = {
    CopilotPresentation.COMPACT: "desktop/copilot/compact-geometry",
    CopilotPresentation.EXPANDED: "desktop/copilot/expanded-geometry",
}

_INSPECTOR_VISIBLE_KEYS = {
    CopilotPresentation.COMPACT: "desktop/copilot/inspector-visible-compact",
    CopilotPresentation.EXPANDED: "desktop/copilot/inspector-visible-expanded",
}

_INSPECTOR_OVERLAY_WIDTH = 400


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
        prolog_query: Optional[object] = None,
        fact_store=None,
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

        self.inspector_button = QPushButton("Inspector")
        self.inspector_button.setObjectName("zaraSecondaryAction")
        self.inspector_button.setAccessibleName("Toggle Prolog inspector")
        self.inspector_button.setToolTip("Toggle the Prolog inspector (Ctrl+I)")
        header_layout = self.header_frame.layout()
        header_layout.insertWidget(
            header_layout.indexOf(self.settings_button),
            self.inspector_button,
        )
        self.inspector_shortcut = QShortcut(QKeySequence("Ctrl+I"), self)
        self.inspector_shortcut.setContext(Qt.ShortcutContext.ApplicationShortcut)
        self.inspector_shortcut.activated.connect(self.toggle_inspector)

        self.inspector = InspectorDrawer(
            prolog_query,
            fact_store=fact_store,
            live_state_provider=self._render_live_state,
            parent=self,
        )
        self.inspector.hide()

        self.history_panel = QWidget(self)
        self.history_panel.setObjectName("zaraConversationHistoryPanel")
        self.history_panel.setFixedWidth(360)
        self.history_panel.hide()
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
        self._body_layout = QHBoxLayout(self.copilot_body)
        body_layout = self._body_layout
        body_layout.setContentsMargins(0, 0, 0, 0)
        body_layout.setSpacing(root_layout.spacing())
        body_layout.addWidget(self.chat_column, 1)
        root_layout.addWidget(self.copilot_body, 1)

        self.inspector_button.clicked.connect(self.toggle_inspector)

        self._history_palette_open = False
        self.expand_button.clicked.disconnect()
        self.expand_button.clicked.connect(self._header_history_action)
        self.palette_shortcut = QShortcut(QKeySequence("Ctrl+K"), self)
        self.palette_shortcut.setContext(Qt.ShortcutContext.ApplicationShortcut)
        self.palette_shortcut.activated.connect(self.toggle_history_palette)
        self.history_escape_shortcut = QShortcut(
            QKeySequence(Qt.Key.Key_Escape), self.history_panel
        )
        self.history_escape_shortcut.setContext(
            Qt.ShortcutContext.WidgetWithChildrenShortcut
        )
        self.history_escape_shortcut.activated.connect(self._dismiss_history_palette)
        application = QApplication.instance()
        if application is not None:
            application.installEventFilter(self)

        self.search_edit.textChanged.connect(self.refresh_history)
        self.history_list.itemActivated.connect(self._activate_history_item)
        self.rename_button.clicked.connect(lambda _checked=False: self.rename_current())
        self.sidebar_new_chat_button.clicked.connect(self.new_chat)

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

    def toggle_inspector(self) -> None:
        """Summon or dismiss the Prolog inspector without touching chat state."""
        visible = not self.inspector.isVisible()
        self.inspector.setVisible(visible)
        self._settings.setValue(
            _INSPECTOR_VISIBLE_KEYS[self._presentation],
            visible,
        )
        if visible:
            self.inspector.refresh_live_state()
            self._position_inspector_overlay()
        self.inspector_button.setAccessibleName(
            "Hide Prolog inspector" if visible else "Toggle Prolog inspector"
        )

    def _render_live_state(self) -> str:
        """Project the canonical symbolic projection; zero runtime commands."""
        projection = self.conversations.store.load_symbolic_projection(
            self.current_conversation_id
        )
        if projection is None:
            return "No symbolic projection recorded"
        lines = [
            f"generation: {projection.projection_generation}",
            f"context: {projection.dialogue_state.get('prolog_context_term', '[]')}",
        ]
        previous_act = projection.dialogue_state.get("response_act_term")
        if previous_act:
            lines.append(f"previous act: {previous_act}")
        for question in projection.unresolved_questions:
            text = question.get("question") if isinstance(question, dict) else question
            if text:
                lines.append(f"clarify: {text}")
        for entity in projection.discourse_entities:
            name = entity.get("name") if isinstance(entity, dict) else entity
            if name:
                lines.append(f"entity: {name}")
        return "\n".join(lines)

    def _position_inspector_overlay(self) -> None:
        if self._presentation is not CopilotPresentation.COMPACT:
            return
        header_bottom = self.header_frame.geometry().bottom()
        self.inspector.setGeometry(
            max(0, self.width() - _INSPECTOR_OVERLAY_WIDTH - 6),
            header_bottom + 6,
            _INSPECTOR_OVERLAY_WIDTH,
            max(120, self.height() - header_bottom - 12),
        )
        self.inspector.raise_()

    def resizeEvent(self, event) -> None:  # noqa: N802 - Qt API
        super().resizeEvent(event)
        if self.inspector.isVisible():
            self._position_inspector_overlay()
        if self._history_palette_open:
            self._position_history_palette()

    # -- history palette ---------------------------------------------------

    def toggle_history_palette(self) -> None:
        """Summon or dismiss the floating history palette (Ctrl+K)."""
        if self._history_palette_open:
            self._dismiss_history_palette()
        else:
            self._open_history_palette()
            self.refresh_history()

    def _open_history_palette(self) -> None:
        self._history_palette_open = True
        self.history_panel.show()
        self._position_history_palette()
        self.search_edit.setFocus()

    def _dismiss_history_palette(self) -> None:
        self._history_palette_open = False
        self.history_panel.hide()

    def _position_history_palette(self) -> None:
        header_bottom = self.header_frame.geometry().bottom()
        width = self.history_panel.width()
        x = max(0, (self.width() - width) // 2)
        height = max(160, min(360, self.height() - header_bottom - 24))
        self.history_panel.setGeometry(x, header_bottom + 6, width, height)
        self.history_panel.raise_()

    def _header_history_action(self) -> None:
        if self._presentation is CopilotPresentation.COMPACT:
            self.toggle_presentation()
        else:
            self.toggle_history_palette()

    def eventFilter(self, watched, event) -> bool:  # noqa: N802 - Qt API
        if (
            self._history_palette_open
            and self._presentation is CopilotPresentation.EXPANDED
            and event.type() == QEvent.Type.MouseButtonPress
        ):
            target = watched if isinstance(watched, QWidget) else None
            inside_palette = target is not None and self.history_panel.isAncestorOf(
                target
            )
            if not inside_palette and target is not self.expand_button:
                self._dismiss_history_palette()
        return super().eventFilter(watched, event)

    def prepare_for_quit(self) -> None:
        application = QApplication.instance()
        if application is not None:
            application.removeEventFilter(self)
        super().prepare_for_quit()

    def _inspector_persisted_visible(self) -> bool:
        return bool(self._settings.value(_INSPECTOR_VISIBLE_KEYS[self._presentation], False, type=bool))

    def bind_conversation(self, conversation_id: str) -> None:
        """Rebind the one renderer to durable state without runtime traffic."""
        super().bind_conversation(conversation_id)
        self._sync_conversation_title()
        self.refresh_history()
        self.inspector.refresh_live_state()

    def new_chat(self) -> None:
        super().new_chat()
        self._sync_conversation_title()
        self.refresh_history()

    def refresh_history(self, query: Optional[str] = None) -> None:
        """Project durable conversation metadata into the expanded history list."""
        if query is None:
            query = self.search_edit.text()
        if (
            query
            and self._presentation is CopilotPresentation.EXPANDED
            and not self._history_palette_open
        ):
            # An explicit search summons the palette with the filtered results.
            self._open_history_palette()
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
        self.expand_button.setText("History")
        self.expand_button.setToolTip(
            "Show history palette" if expanded else "Show history"
        )
        self.new_chat_button.setVisible(not expanded)
        self._apply_header_density()
        if expanded and self._history_palette_open:
            self._open_history_palette()
        else:
            self._history_palette_open = False
            self.history_panel.hide()
        if expanded:
            self.refresh_history()
        self.setWindowTitle("Zara — Copilot" if expanded else "Ask Zara")
        self._apply_inspector_presentation(expanded)

    def _apply_inspector_presentation(self, expanded: bool) -> None:
        """Dock the drawer in expanded mode; float it above the stream in compact."""
        was_visible = self.inspector.isVisible()
        if expanded:
            self.inspector.setParent(self.copilot_body)
            self._body_layout.addWidget(self.inspector)
        else:
            self.inspector.setParent(self)
        visible = was_visible or self._inspector_persisted_visible()
        self.inspector.setVisible(visible)
        if visible:
            self.inspector.refresh_live_state()
            self._position_inspector_overlay()

    def set_status(self, status) -> None:
        super().set_status(status)
        inspector = getattr(self, "inspector", None)
        if inspector is not None and status.state in (
            DesktopRuntimeState.DISCONNECTED,
            DesktopRuntimeState.ERROR,
        ):
            # Fence inflight drawer results across restarts and daemon loss.
            inspector.controller.fence_restart()
