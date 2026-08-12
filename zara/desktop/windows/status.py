"""Minimal desktop status window used until the full chat lands in #85."""

from __future__ import annotations

from PySide6.QtCore import Signal
from PySide6.QtGui import QCloseEvent
from PySide6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.state import DesktopStatus, INITIAL_STATUS


class DesktopStatusWindow(QWidget):
    restart_requested = Signal()
    diagnostics_requested = Signal()

    def __init__(self) -> None:
        super().__init__()
        self._allow_close = False
        self._status = INITIAL_STATUS

        self.setWindowTitle("Zara")
        self.setMinimumWidth(360)

        title = QLabel("Zara")
        title.setObjectName("zaraTitle")
        self.status_label = QLabel()
        self.status_label.setObjectName("zaraRuntimeStatus")
        self.detail_label = QLabel()
        self.detail_label.setObjectName("zaraRuntimeDetail")
        self.detail_label.setWordWrap(True)

        self.restart_button = QPushButton("Restart Runtime")
        self.restart_button.clicked.connect(self.restart_requested.emit)
        self.diagnostics_button = QPushButton("Diagnostics")
        self.diagnostics_button.clicked.connect(self.diagnostics_requested.emit)

        buttons = QHBoxLayout()
        buttons.addWidget(self.restart_button)
        buttons.addWidget(self.diagnostics_button)
        buttons.addStretch(1)

        layout = QVBoxLayout(self)
        layout.addWidget(title)
        layout.addWidget(self.status_label)
        layout.addWidget(self.detail_label)
        layout.addLayout(buttons)

        self.set_status(INITIAL_STATUS)

    @property
    def status(self) -> DesktopStatus:
        return self._status

    def set_status(self, status: DesktopStatus) -> None:
        self._status = status
        state_text = status.state.value.replace("-", " ").title()
        self.status_label.setText(state_text)
        self.detail_label.setText(status.detail or "Zara is running in the background.")
        self.restart_button.setEnabled(status.state.value != "starting")

    def show_raised(self) -> None:
        self.show()
        if self.isMinimized():
            self.showNormal()
        self.raise_()
        self.activateWindow()

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
        self.hide()
        event.ignore()
