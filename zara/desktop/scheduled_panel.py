"""Compact canonical scheduled-task projection for Copilot."""

from __future__ import annotations

from typing import Optional

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.qt_bridge import QtRuntimeBridge


class ScheduledPanel(QWidget):
    rows_ready = Signal(object)
    operation_failed = Signal(str)

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
        self.new_button = QPushButton("New")
        self.new_button.setObjectName("zaraSecondaryAction")
        header.addWidget(title)
        header.addStretch(1)
        header.addWidget(self.new_button)

        self.status = QLabel("")
        self.status.setObjectName("zaraMutedLabel")
        self.list = QListWidget()
        self.list.setObjectName("zaraScheduledList")
        self.list.setMaximumHeight(150)
        self.list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.list.setTextElideMode(Qt.TextElideMode.ElideRight)

        controls = QHBoxLayout()
        self.pause_button = QPushButton("Pause")
        self.resume_button = QPushButton("Resume")
        self.cancel_button = QPushButton("Cancel")
        for button in (self.pause_button, self.resume_button, self.cancel_button):
            button.setObjectName("zaraSecondaryAction")
            controls.addWidget(button)

        layout.addLayout(header)
        layout.addWidget(self.status)
        layout.addWidget(self.list)
        layout.addLayout(controls)

        self.rows_ready.connect(self._apply_rows)
        self.operation_failed.connect(self._show_error)
        self.new_button.clicked.connect(self.create_schedule)
        self.pause_button.clicked.connect(lambda: self._control("pause"))
        self.resume_button.clicked.connect(lambda: self._control("resume"))
        self.cancel_button.clicked.connect(lambda: self._control("cancel"))
        self.list.itemSelectionChanged.connect(self._sync_controls)
        self.refresh()

    def refresh(self) -> None:
        service = self._bridge.host.scheduled_tasks
        if service is None:
            self._set_unavailable("Scheduled tasks are disabled")
            return

        async def load():
            return service.list_schedules()

        self.status.setText("Loading…")
        self._watch(self._bridge.host.run_coroutine(load()), emit_rows=True)

    def create_schedule(self) -> None:
        service = self._bridge.host.scheduled_tasks
        if service is None:
            self._set_unavailable("Scheduled tasks are disabled")
            return
        cron, accepted = QInputDialog.getText(
            self,
            "New scheduled task",
            "Cron",
            text="0 9 * * 1-5",
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
        future = self._bridge.host.run_coroutine(
            service.create_schedule(cron=cron.strip(), goal=goal.strip(), mode="auto")
        )
        self._watch(future)

    def _control(self, action: str) -> None:
        item = self.list.currentItem()
        service = self._bridge.host.scheduled_tasks
        if item is None or service is None:
            return
        schedule_id = str(item.data(Qt.ItemDataRole.UserRole))
        operation = {
            "pause": service.pause_schedule,
            "resume": service.resume_schedule,
            "cancel": service.cancel_schedule,
        }[action]
        self._watch(self._bridge.host.run_coroutine(operation(schedule_id)))

    def _watch(self, future, *, emit_rows: bool = False) -> None:
        def done(completed) -> None:
            try:
                value = completed.result()
            except Exception as error:
                self.operation_failed.emit(str(error))
                return
            if emit_rows:
                self.rows_ready.emit(value)
            else:
                self.refresh()

        future.add_done_callback(done)

    def _apply_rows(self, rows) -> None:
        self.list.clear()
        for row in rows:
            next_run = row.next_run_at or "—"
            item = QListWidgetItem(
                f"{row.label}  ·  {row.cron}  ·  {row.state.value}\n{next_run}"
            )
            item.setData(Qt.ItemDataRole.UserRole, row.schedule_id)
            item.setData(Qt.ItemDataRole.UserRole + 1, row.state.value)
            self.list.addItem(item)
        self.status.setText("" if rows else "No scheduled tasks")
        self._sync_controls()

    def _sync_controls(self) -> None:
        item = self.list.currentItem()
        state = item.data(Qt.ItemDataRole.UserRole + 1) if item is not None else None
        self.pause_button.setEnabled(state == "active")
        self.resume_button.setEnabled(state == "paused")
        self.cancel_button.setEnabled(state in {"active", "paused"})

    def _set_unavailable(self, message: str) -> None:
        self.list.clear()
        self.status.setText(message)
        self.new_button.setEnabled(False)
        self._sync_controls()

    def _show_error(self, message: str) -> None:
        self.status.setText(message)
        self._sync_controls()
