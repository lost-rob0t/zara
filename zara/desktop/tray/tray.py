"""Canonical Zara desktop system tray."""

from __future__ import annotations

from typing import Optional

from PySide6.QtGui import QAction, QIcon
from PySide6.QtWidgets import QApplication, QMenu, QStyle, QSystemTrayIcon
from PySide6.QtCore import QObject, Signal

from zara.desktop.state import DesktopRuntimeState, DesktopStatus


class ZaraTray(QSystemTrayIcon):
    """Capability-derived canonical tray for the native desktop shell."""

    toggle_requested = Signal()
    quick_requested = Signal()
    full_chat_requested = Signal()
    restart_requested = Signal()
    diagnostics_requested = Signal()
    quit_requested = Signal()

    def __init__(
        self,
        parent: Optional[QObject] = None,
        *,
        icon: Optional[QIcon] = None,
    ) -> None:
        super().__init__(parent)
        self._available = False

        if icon is None:
            style = QApplication.instance().style() if QApplication.instance() else None
            icon = style.standardIcon(QStyle.StandardPixmap.SP_ComputerIcon) if style else QIcon()
        self.setIcon(icon)
        self.setToolTip("Zara — starting")

        menu = QMenu()
        self.open_action = QAction("Ask Zara", menu)
        self.open_action.triggered.connect(self._request_quick)
        menu.addAction(self.open_action)

        self.full_chat_action = QAction("Open Full Chat", menu)
        self.full_chat_action.triggered.connect(self.full_chat_requested.emit)
        menu.addAction(self.full_chat_action)
        menu.addSeparator()

        self.status_action = QAction("Status: starting", menu)
        self.status_action.setEnabled(False)
        menu.addAction(self.status_action)
        menu.addSeparator()

        self.restart_action = QAction("Restart Runtime", menu)
        self.restart_action.triggered.connect(self.restart_requested.emit)
        menu.addAction(self.restart_action)

        self.diagnostics_action = QAction("Diagnostics", menu)
        self.diagnostics_action.triggered.connect(self.diagnostics_requested.emit)
        menu.addAction(self.diagnostics_action)
        menu.addSeparator()

        self.quit_action = QAction("Quit Zara", menu)
        self.quit_action.triggered.connect(self.quit_requested.emit)
        menu.addAction(self.quit_action)

        self.setContextMenu(menu)
        self.activated.connect(self._on_activated)

    @property
    def available(self) -> bool:
        return self._available

    def show_if_available(self) -> bool:
        """Show the tray only when the Qt platform reports support."""
        self._available = bool(QSystemTrayIcon.isSystemTrayAvailable())
        if self._available:
            self.show()
        return self._available

    def set_status(self, status: DesktopStatus) -> None:
        state_text = status.state.value.replace("-", " ")
        self.status_action.setText(f"Status: {state_text}")
        detail = status.detail.strip()
        self.setToolTip(f"Zara — {detail or state_text}")
        self.restart_action.setEnabled(status.state is not DesktopRuntimeState.STARTING)

    def set_restarting(self) -> None:
        self.set_status(
            DesktopStatus(
                DesktopRuntimeState.STARTING,
                "Restarting Zara runtime…",
            )
        )

    def _request_quick(self) -> None:
        self.quick_requested.emit()
        self.toggle_requested.emit()

    def _on_activated(self, reason: QSystemTrayIcon.ActivationReason) -> None:
        if reason is QSystemTrayIcon.ActivationReason.Trigger:
            self._request_quick()
