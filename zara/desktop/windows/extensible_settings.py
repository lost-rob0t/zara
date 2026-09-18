from __future__ import annotations

from pathlib import Path
from typing import Callable, Optional

from PySide6.QtCore import Signal
from PySide6.QtWidgets import QScrollArea, QVBoxLayout, QWidget

from zara.config import ZaraConfig
from zara.desktop.ui_extensions import (
    DesktopUiExtensionHost,
    build_desktop_ui_registry,
    desktop_ui_action_bus,
)
from zara.ui.extensions import UiSlot

from .settings import SettingsWindow as BaseSettingsWindow


class SettingsWindow(BaseSettingsWindow):
    """Canonical settings workspace plus shared init/plugin UI contributions."""

    ui_action_requested = Signal(str)

    def __init__(
        self,
        config: ZaraConfig,
        *,
        repo_root: Path | None = None,
        prolog_reload: Callable[[], bool] | None = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(
            config,
            repo_root=repo_root,
            prolog_reload=prolog_reload,
            parent=parent,
        )
        self.ui_registry = build_desktop_ui_registry(config)
        self.settings_extensions = DesktopUiExtensionHost(
            self.ui_registry,
            UiSlot.SETTINGS,
            self,
        )
        self.settings_extensions.action_requested.connect(self._dispatch_ui_action)

        advanced = self.stack.widget(5)
        if isinstance(advanced, QScrollArea):
            body = advanced.widget()
            if body is not None:
                body_layout = body.layout()
                if isinstance(body_layout, QVBoxLayout):
                    body_layout.insertWidget(max(0, body_layout.count() - 1), self.settings_extensions)

    def _dispatch_ui_action(self, action: str) -> None:
        self.ui_action_requested.emit(action)
        desktop_ui_action_bus().action_requested.emit(action)


__all__ = ["SettingsWindow"]
