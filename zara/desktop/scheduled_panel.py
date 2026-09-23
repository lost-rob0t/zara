"""Compact Copilot controls for the canonical scheduled-task tools."""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import Signal
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.qt_bridge import QtRuntimeBridge


class ScheduledPanel(QWidget):
    """Project scheduler readiness and emit tool requests through Copilot."""

    prompt_requested = Signal(str)

    def __init__(
        self,
        bridge: QtRuntimeBridge,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self.setObjectName("zaraScheduledPanel")

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(6)

        header = QHBoxLayout()
        title = QLabel("Scheduled")
        title.setObjectName("zaraSurfaceName")
        new_button = QPushButton("New")
        list_button = QPushButton("List")
        for button in (new_button, list_button):
            button.setObjectName("zaraSecondaryAction")
        header.addWidget(title)
        header.addStretch(1)
        header.addWidget(new_button)
        header.addWidget(list_button)

        hint = QLabel("cron / interval → Prolog → LLM")
        hint.setObjectName("zaraMutedLabel")
        self.status_label = QLabel()
        self.status_label.setObjectName("zaraMutedLabel")

        controls = QHBoxLayout()
        pause_button = QPushButton("Pause")
        resume_button = QPushButton("Resume")
        cancel_button = QPushButton("Cancel")
        for button in (pause_button, resume_button, cancel_button):
            button.setObjectName("zaraSecondaryAction")
            controls.addWidget(button)

        self._action_buttons = (
            new_button,
            list_button,
            pause_button,
            resume_button,
            cancel_button,
        )

        layout.addLayout(header)
        layout.addWidget(hint)
        layout.addWidget(self.status_label)
        layout.addLayout(controls)

        new_button.clicked.connect(self.create_schedule)
        list_button.clicked.connect(
            lambda: self.prompt_requested.emit(
                "Use schedule_list. List schedules concisely with id, label, recurrence, state, next run, and last outcome."
            )
        )
        pause_button.clicked.connect(lambda: self._control("pause"))
        resume_button.clicked.connect(lambda: self._control("resume"))
        cancel_button.clicked.connect(lambda: self._control("cancel"))
        self.refresh()

    def refresh(self) -> None:
        """Reflect whether RuntimeHost actually owns a live scheduler service."""
        host = getattr(self._bridge, "host", None)
        available = getattr(host, "scheduled_tasks", None) is not None
        self.status_label.setText(
            "Scheduler ready"
            if available
            else "Scheduler disabled — enable [tasks] on the Zara runtime"
        )
        for button in self._action_buttons:
            button.setEnabled(available)

    def create_schedule(self) -> None:
        cron, accepted = QInputDialog.getText(
            self,
            "New scheduled task",
            "Cron / interval",
            text="@every 6h",
        )
        if not accepted or not cron.strip():
            return
        goal, accepted = QInputDialog.getMultiLineText(
            self,
            "New scheduled task",
            "Task",
        )
        if not accepted or not goal.strip():
            return
        self.prompt_requested.emit(
            "Use schedule_create with "
            f"cron {cron.strip()!r}, mode 'auto', and goal {goal.strip()!r}."
        )

    def _control(self, action: str) -> None:
        schedule_id, accepted = QInputDialog.getText(
            self,
            f"{action.title()} scheduled task",
            "Schedule ID",
        )
        if not accepted or not schedule_id.strip():
            return
        self.prompt_requested.emit(
            f"Use schedule_{action} for schedule id {schedule_id.strip()!r}."
        )
