from __future__ import annotations

from typing import Optional

from PySide6.QtCore import QSettings
from PySide6.QtWidgets import QWidget

from zara.config import get_config
from zara.desktop.conversation import ConversationService
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.ui_extensions import DesktopUiExtensionHost, build_desktop_ui_registry
from zara.ui.extensions import UiSlot

from .copilot import CopilotPresentation, CopilotWindow as BaseCopilotWindow


class CopilotWindow(BaseCopilotWindow):
    """Canonical Copilot plus init/plugin-provided semantic UI slots."""

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
        self.ui_registry = build_desktop_ui_registry(get_config())
        self.drawer_extensions = DesktopUiExtensionHost(self.ui_registry, UiSlot.DRAWER, self)
        self.plugin_extensions = DesktopUiExtensionHost(self.ui_registry, UiSlot.PLUGINS, self)
        self.chat_top_extensions = DesktopUiExtensionHost(self.ui_registry, UiSlot.CHAT_TOP, self)
        self.chat_bottom_extensions = DesktopUiExtensionHost(self.ui_registry, UiSlot.CHAT_BOTTOM, self)

        history_layout = self.history_panel.layout()
        history_layout.insertWidget(1, self.drawer_extensions)
        history_layout.addWidget(self.plugin_extensions)

        chat_layout = self.chat_column.layout()
        chat_layout.insertWidget(0, self.chat_top_extensions)
        chat_layout.insertWidget(max(0, chat_layout.count() - 1), self.chat_bottom_extensions)

        for host in (
            self.drawer_extensions,
            self.plugin_extensions,
            self.chat_top_extensions,
            self.chat_bottom_extensions,
        ):
            host.action_requested.connect(self._handle_ui_extension_action)

    def _handle_ui_extension_action(self, action: str) -> None:
        if action.startswith("route:"):
            route = action.removeprefix("route:")
            if route == "settings":
                self.settings_requested.emit()
                return
            if route == "chat":
                self.set_presentation(CopilotPresentation.EXPANDED)
                self.show_raised()
                return
            if route == "logic":
                self.set_presentation(CopilotPresentation.EXPANDED)
                self.composer.setPlainText("/prolog ")
                self.composer.setFocus()
                return
            self.composer.setPlainText(f"/{route} ")
            self.composer.setFocus()
            return

        if action.startswith("prompt:"):
            self.composer.setPlainText(action.removeprefix("prompt:"))
            self.composer.setFocus()
            return

        if action.startswith("submit:"):
            self.composer.setPlainText(action.removeprefix("submit:"))
            self.submit_current_text()
            return

        if action.startswith("plugin:"):
            self.composer.setPlainText(action.removeprefix("plugin:"))
            self.composer.setFocus()


__all__ = ["CopilotWindow"]
