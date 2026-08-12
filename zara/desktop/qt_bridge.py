"""Qt-safe adapter for :mod:`zara.runtime` commands and events.

This module is intentionally UI-free. It translates the queue/future based
RuntimeHost boundary into Qt signals without ever running assistant work on the
Qt main thread.
"""

from __future__ import annotations

import concurrent.futures
from typing import Optional

from PySide6.QtCore import QObject, QTimer, Signal, Slot

from zara.runtime import bridge as runtime_bridge
from zara.runtime.commands import RuntimeCommand
from zara.runtime.host import RuntimeHost


class QtRuntimeBridge(QObject):
    """Deliver RuntimeHost events and command completions through Qt signals."""

    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def __init__(
        self,
        host: RuntimeHost,
        parent: Optional[QObject] = None,
        *,
        poll_interval_ms: int = 16,
        max_events_per_tick: int = 128,
        auto_start_timer: bool = True,
    ) -> None:
        super().__init__(parent)
        if poll_interval_ms < 1:
            raise ValueError("poll_interval_ms must be >= 1")
        if max_events_per_tick < 1:
            raise ValueError("max_events_per_tick must be >= 1")

        self._host = host
        self._max_events_per_tick = max_events_per_tick
        self._subscription = runtime_bridge.subscribe()
        self._closed = False

        self._timer = QTimer(self)
        self._timer.setInterval(poll_interval_ms)
        self._timer.timeout.connect(self.drain_events)
        if auto_start_timer:
            self._timer.start()

    @property
    def host(self) -> RuntimeHost:
        return self._host

    @property
    def closed(self) -> bool:
        return self._closed

    def submit(self, command: RuntimeCommand) -> concurrent.futures.Future:
        """Submit without blocking Qt and signal the eventual receipt/error."""
        future = self._host.submit(command)
        request_id = command.request_id

        def completed(done: concurrent.futures.Future) -> None:
            try:
                receipt = done.result()
            except Exception as error:
                # Qt signals are thread-safe. Receivers living on the Qt main
                # thread are invoked through Qt's queued cross-thread delivery.
                self.command_failed.emit(request_id, str(error))
                return
            self.command_completed.emit(receipt)

        future.add_done_callback(completed)
        return future

    @Slot()
    def drain_events(self) -> None:
        """Drain a bounded batch on the Qt thread and emit ordered envelopes."""
        if self._closed:
            return
        for envelope in self._subscription.drain(limit=self._max_events_per_tick):
            self.runtime_event.emit(envelope)

    @Slot()
    def close(self) -> None:
        if self._closed:
            return
        self._closed = True
        self._timer.stop()
        self._subscription.close()
