from __future__ import annotations

import logging
from typing import Optional

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QCheckBox, QLabel, QPushButton, QVBoxLayout, QWidget

from zara.ui.extensions import (
    UiContributionKind,
    UiExtensionRegistry,
    UiInitLoader,
    UiPlatform,
    UiSlot,
)
from zara.ui.plugin_manifests import UiManifestLoader
from zara.ui.prolog_init import PrologUiInitLoader

logger = logging.getLogger(__name__)


class DesktopUiActionBus(QObject):
    """Process-local bridge from extension controls to the canonical Copilot path."""

    action_requested = Signal(str)


_ACTION_BUS: DesktopUiActionBus | None = None


def desktop_ui_action_bus() -> DesktopUiActionBus:
    global _ACTION_BUS
    if _ACTION_BUS is None:
        _ACTION_BUS = DesktopUiActionBus()
    return _ACTION_BUS


class DesktopUiExtensionHost(QWidget):
    """Render one semantic UI slot with native Qt widgets."""

    action_requested = Signal(str)

    def __init__(
        self,
        registry: UiExtensionRegistry,
        slot: UiSlot,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.registry = registry
        self.slot = slot
        self.layout_ = QVBoxLayout(self)
        self.layout_.setContentsMargins(0, 0, 0, 0)
        self.layout_.setSpacing(6)
        self.refresh()

    def refresh(self) -> None:
        while self.layout_.count():
            item = self.layout_.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()

        contributions = self.registry.for_platform(UiPlatform.DESKTOP, self.slot)
        for contribution in contributions:
            widget = self._widget_for(contribution)
            if widget is not None:
                self.layout_.addWidget(widget)
        self.setVisible(bool(contributions))

    def _widget_for(self, contribution):
        if contribution.kind in {
            UiContributionKind.SECTION,
            UiContributionKind.TEXT,
            UiContributionKind.STATUS,
        }:
            label = QLabel(contribution.label, self)
            label.setWordWrap(True)
            label.setObjectName(
                "zaraSurfaceName"
                if contribution.kind is UiContributionKind.SECTION
                else "zaraSettingsHint"
            )
            return label

        if contribution.kind in {
            UiContributionKind.BUTTON,
            UiContributionKind.SURFACE,
        }:
            button = QPushButton(contribution.label, self)
            button.setObjectName("zaraSecondaryAction")
            button.setAccessibleName(contribution.label)
            if contribution.action.startswith("plugin:"):
                button.setEnabled(False)
                button.setToolTip(
                    "Unavailable until the typed plugin host dispatcher is connected."
                )
            else:
                button.clicked.connect(
                    lambda _checked=False, action=contribution.action: self.action_requested.emit(action)
                )
            return button

        if contribution.kind is UiContributionKind.TOGGLE:
            toggle = QCheckBox(contribution.label, self)
            toggle.setAccessibleName(contribution.label)
            toggle.setEnabled(False)
            toggle.setToolTip(
                "State unavailable until the canonical revisioned plugin-settings projection is connected."
            )
            return toggle
        return None


def build_desktop_ui_registry(config) -> UiExtensionRegistry:
    """Load Python, Prolog and plugin-manifest UI layers with failure isolation."""
    registry = UiExtensionRegistry()

    for loader in (
        UiInitLoader(config_dir=config.config_dir, registry=registry),
        PrologUiInitLoader(config_dir=config.config_dir, registry=registry),
    ):
        try:
            loader.load()
        except Exception:
            logger.warning("Could not load %s", loader.path, exc_info=True)

    plugin_paths = tuple(config.get_module_search_paths())

    def enabled(plugin_name: str) -> bool:
        value = config.get_plugin_config(plugin_name).get("enabled", True)
        return value if isinstance(value, bool) else False

    try:
        UiManifestLoader(
            plugin_paths,
            registry=registry,
            enabled_provider=enabled,
        ).load()
    except Exception:
        logger.warning("Could not load plugin UI manifests", exc_info=True)
    return registry


__all__ = [
    "DesktopUiActionBus",
    "DesktopUiExtensionHost",
    "build_desktop_ui_registry",
    "desktop_ui_action_bus",
]
