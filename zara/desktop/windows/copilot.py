"""One adaptive desktop Copilot over Zara's durable conversation state."""

from __future__ import annotations

from enum import Enum
from typing import Optional

from PySide6.QtCore import QEvent, QRect, QSettings, QSize, Qt, QTimer
from PySide6.QtGui import QCloseEvent, QCursor, QHideEvent, QKeyEvent, QShowEvent
from PySide6.QtWidgets import QApplication, QHBoxLayout, QPushButton, QWidget

from zara.desktop.conversation import ConversationService
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import DesktopRuntimeState, DesktopStatus

from .chat import FullChatWindow
from .quick import recover_quick_geometry

_COMPACT_SIZE = QSize(680, 460)
_EXPANDED_SIZE = QSize(980, 700)
_COMPACT_MINIMUM = QSize(480, 320)
_EXPANDED_MINIMUM = QSize(760, 520)
_COMPACT_GEOMETRY_KEY = "desktop/copilot/compact-geometry"
_EXPANDED_GEOMETRY_KEY = "desktop/copilot/expanded-geometry"
_HEALTHY_COMPACT_STATES = {
    DesktopRuntimeState.IDLE,
    DesktopRuntimeState.READY,
}


class CopilotMode(str, Enum):
    """Presentation modes for the one process-owned Copilot window."""

    COMPACT = "compact"
    EXPANDED = "expanded"


class CopilotWindow(FullChatWindow):
    """The canonical desktop chat surface in compact or expanded presentation."""

    def __init__(
        self,
        bridge: QtRuntimeBridge,
        conversations: ConversationService,
        parent: Optional[QWidget] = None,
        *,
        manage_runtime_events: bool = True,
        settings: Optional[QSettings] = None,
        initial_mode: CopilotMode = CopilotMode.COMPACT,
    ) -> None:
        self._settings = settings or QSettings()
        self._mode = initial_mode
        super().__init__(
            bridge,
            conversations,
            parent,
            manage_runtime_events=manage_runtime_events,
        )

        self.setObjectName("zaraCopilot")
        self.setWindowTitle("Zara")
        self.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose, False)
        self.setFocusProxy(self.composer)

        self._conversation_surface = self.findChild(QWidget, "zaraConversationSurface")
        if self._conversation_surface is None:
            raise RuntimeError("canonical conversation surface is missing")
        self._conversation_layout = self._conversation_surface.layout()
        if self._conversation_layout is None:
            raise RuntimeError("canonical conversation layout is missing")

        header_layout = self.header_frame.layout()
        if not isinstance(header_layout, QHBoxLayout):
            raise RuntimeError("canonical Copilot header layout is missing")

        self.compact_new_chat_button = QPushButton("New")
        self.compact_new_chat_button.setObjectName("zaraCopilotNewChat")
        self.compact_new_chat_button.setAccessibleName("New chat")
        self.mode_button = QPushButton("Expand")
        self.mode_button.setObjectName("zaraCopilotMode")
        self.mode_button.setAccessibleName("Expand Copilot")
        self.compact_settings_button = QPushButton("Settings")
        self.compact_settings_button.setObjectName("zaraCopilotSettings")
        self.compact_settings_button.setAccessibleName("Open Zara settings")

        header_layout.addWidget(self.compact_new_chat_button)
        header_layout.addWidget(self.mode_button)
        header_layout.addWidget(self.compact_settings_button)

        self.compact_new_chat_button.clicked.connect(self.new_chat)
        self.mode_button.clicked.connect(self.toggle_mode)
        self.compact_settings_button.clicked.connect(self.settings_requested.emit)
        self.composer.installEventFilter(self)

        self._apply_mode()
        self._recover_geometry()

    @property
    def mode(self) -> CopilotMode:
        return self._mode

    def set_mode(self, mode: CopilotMode) -> None:
        mode = CopilotMode(mode)
        if mode is self._mode:
            self._apply_mode()
            return

        self._save_geometry()
        self._mode = mode
        self._apply_mode()
        self._recover_geometry()
        self._focus_composer()

    def toggle_mode(self) -> None:
        target = (
            CopilotMode.EXPANDED
            if self._mode is CopilotMode.COMPACT
            else CopilotMode.COMPACT
        )
        self.set_mode(target)

    def set_status(self, status: DesktopStatus) -> None:
        super().set_status(status)
        if hasattr(self, "_mode"):
            self._sync_runtime_visibility()

    def show_raised(self) -> None:
        self._recover_geometry()
        self.show()
        if self.isMinimized():
            self.showNormal()
        self._focus_composer()
        QTimer.singleShot(0, self._focus_composer)

    def bind_conversation(self, conversation_id: str) -> None:
        """Compatibility name for the old Quick Copilot binding operation."""
        self.load_conversation(conversation_id)

    def sync_from_shared_state(self, _event: object = None) -> None:
        """Compatibility hook that refreshes the one canonical renderer."""
        self._render_current()
        self._sync_controls()

    def eventFilter(self, watched, event) -> bool:  # noqa: N802 - Qt API
        if (
            watched is self.composer
            and self._mode is CopilotMode.COMPACT
            and event.type() == QEvent.Type.KeyPress
            and isinstance(event, QKeyEvent)
            and event.key() == Qt.Key.Key_Escape
        ):
            self.hide()
            event.accept()
            return True
        return super().eventFilter(watched, event)

    def keyPressEvent(self, event: QKeyEvent) -> None:  # noqa: N802 - Qt API
        if self._mode is CopilotMode.COMPACT and event.key() == Qt.Key.Key_Escape:
            self.hide()
            event.accept()
            return
        super().keyPressEvent(event)

    def closeEvent(self, event: QCloseEvent) -> None:  # noqa: N802 - Qt API
        self._save_geometry()
        super().closeEvent(event)

    def hideEvent(self, event: QHideEvent) -> None:  # noqa: N802 - Qt API
        self._save_geometry()
        super().hideEvent(event)

    def showEvent(self, event: QShowEvent) -> None:  # noqa: N802 - Qt API
        super().showEvent(event)
        QTimer.singleShot(0, self._focus_composer)

    def _apply_mode(self) -> None:
        expanded = self._mode is CopilotMode.EXPANDED

        self.sidebar.setVisible(expanded)
        self.provider_label.setVisible(expanded)
        self.compact_new_chat_button.setVisible(not expanded)
        self.compact_settings_button.setVisible(not expanded)
        self.mode_button.setText("Compact" if expanded else "Expand")
        self.mode_button.setAccessibleName(
            "Use compact Copilot" if expanded else "Expand Copilot"
        )

        header_layout = self.header_frame.layout()
        if header_layout is not None:
            header_layout.setContentsMargins(0, 0, 0, 10 if expanded else 4)
            header_layout.setSpacing(10 if expanded else 6)

        if expanded:
            self.setMinimumSize(_EXPANDED_MINIMUM)
            self._conversation_layout.setContentsMargins(22, 18, 22, 20)
            self._conversation_layout.setSpacing(12)
            self.message_layout.setContentsMargins(4, 6, 10, 6)
            self.message_layout.setSpacing(11)
            self.composer.setMinimumHeight(70)
            self.composer.setMaximumHeight(130)
            self.composer_shell.setMaximumHeight(138)
        else:
            self.setMinimumSize(_COMPACT_MINIMUM)
            self._conversation_layout.setContentsMargins(14, 10, 14, 12)
            self._conversation_layout.setSpacing(7)
            self.message_layout.setContentsMargins(4, 2, 6, 2)
            self.message_layout.setSpacing(8)
            self.composer.setMinimumHeight(48)
            self.composer.setMaximumHeight(84)
            self.composer_shell.setMaximumHeight(102)

        self._sync_runtime_visibility()

    def _sync_runtime_visibility(self) -> None:
        if self._mode is CopilotMode.EXPANDED:
            self.status_frame.show()
            self.restart_button.show()
            self.diagnostics_button.show()
            return

        healthy = self.status.state in _HEALTHY_COMPACT_STATES
        self.status_frame.setVisible(not healthy)
        self.restart_button.hide()
        self.diagnostics_button.hide()

    def _focus_composer(self) -> None:
        if not self.isVisible():
            return
        qt_app = QApplication.instance()
        if isinstance(qt_app, QApplication):
            qt_app.setActiveWindow(self)
        self.raise_()
        self.activateWindow()
        self.composer.setFocus(Qt.FocusReason.ActiveWindowFocusReason)

    def _geometry_key(self) -> str:
        if self._mode is CopilotMode.COMPACT:
            return _COMPACT_GEOMETRY_KEY
        return _EXPANDED_GEOMETRY_KEY

    def _default_size(self) -> QSize:
        if self._mode is CopilotMode.COMPACT:
            return QSize(_COMPACT_SIZE)
        return QSize(_EXPANDED_SIZE)

    def _saved_geometry(self) -> Optional[QRect]:
        value = self._settings.value(self._geometry_key())
        if isinstance(value, QRect) and value.isValid():
            return QRect(value)
        return None

    def _save_geometry(self) -> None:
        geometry = self.geometry()
        if geometry.isValid():
            self._settings.setValue(self._geometry_key(), geometry)

    def _recover_geometry(self) -> None:
        qt_app = QApplication.instance()
        if not isinstance(qt_app, QApplication):
            return

        screens = [screen.availableGeometry() for screen in qt_app.screens()]
        preferred = qt_app.screenAt(QCursor.pos()) or qt_app.primaryScreen()
        preferred_geometry = preferred.availableGeometry() if preferred is not None else None
        recovered = recover_quick_geometry(
            self._saved_geometry(),
            screens,
            preferred_screen=preferred_geometry,
            default_size=self._default_size(),
        )
        self.setGeometry(recovered)


__all__ = ["CopilotMode", "CopilotWindow"]
