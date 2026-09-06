"""Adaptive single-renderer Copilot presentation seam."""

from __future__ import annotations

from enum import Enum
from typing import Optional

from PySide6.QtCore import QSettings, Signal
from PySide6.QtWidgets import QWidget

from zara.desktop.conversation import ConversationService
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.windows.quick import QuickCopilotWindow


class CopilotPresentation(str, Enum):
    """Visual presentation of the one canonical Copilot renderer."""

    COMPACT = "compact"
    EXPANDED = "expanded"


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
        self.title_label.setText("Copilot")

        self.expand_button.clicked.disconnect()
        self.expand_button.clicked.connect(self.toggle_presentation)
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
        self._presentation = presentation
        self._apply_presentation()

    def toggle_presentation(self) -> None:
        target = (
            CopilotPresentation.EXPANDED
            if self._presentation is CopilotPresentation.COMPACT
            else CopilotPresentation.COMPACT
        )
        self.set_presentation(target)

    def _apply_presentation(self) -> None:
        expanded = self._presentation is CopilotPresentation.EXPANDED
        self.setProperty("presentation", self._presentation.value)
        self.expand_button.setText("Compact" if expanded else "Expand")
        self.setWindowTitle("Zara — Copilot" if expanded else "Ask Zara")
