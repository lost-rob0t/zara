"""Process-level controller for the native Zara desktop shell."""

from __future__ import annotations

import concurrent.futures
from typing import Callable, Optional

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import (
    DesktopRuntimeState,
    DesktopStatus,
    INITIAL_STATUS,
    reduce_runtime_event,
)
from zara.desktop.tray import ZaraTray
from zara.desktop.windows import FullChatWindow
from zara.runtime.commands import RestartRuntime, ShutdownRuntime
from zara.runtime.host import RuntimeHost


class DesktopController(QObject):
    """Own the desktop shell while delegating assistant work to RuntimeHost."""

    diagnostics_requested = Signal()

    def __init__(
        self,
        app: QApplication,
        host: RuntimeHost,
        bridge: QtRuntimeBridge,
        *,
        tray_factory: Callable[[], ZaraTray] = ZaraTray,
        window_factory: Optional[Callable[[], object]] = None,
        conversation_service: Optional[ConversationService] = None,
    ) -> None:
        super().__init__(app)
        self.app = app
        self.host = host
        self.bridge = bridge
        self.tray = tray_factory()

        if window_factory is None:
            self.conversation_service = conversation_service or ConversationService(ConversationStore())
            self.window = FullChatWindow(self.bridge, self.conversation_service)
        else:
            # Tests/embedders can still provide a shell-compatible window
            # without causing desktop conversation storage side effects.
            self.conversation_service = conversation_service
            self.window = window_factory()

        self.status = INITIAL_STATUS

        self._started = False
        self._quitting = False
        self._finalized = False
        self._quit_request_id: Optional[str] = None
        self._restart_request_id: Optional[str] = None

        self.tray.toggle_requested.connect(self.window.toggle_visibility)
        self.tray.restart_requested.connect(self.restart_runtime)
        self.tray.diagnostics_requested.connect(self.show_diagnostics)
        self.tray.quit_requested.connect(self.request_quit)

        self.window.restart_requested.connect(self.restart_runtime)
        self.window.diagnostics_requested.connect(self.show_diagnostics)

        self.bridge.runtime_event.connect(self._on_runtime_envelope)
        self.bridge.command_completed.connect(self._on_command_completed)
        self.bridge.command_failed.connect(self._on_command_failed)
        self.app.aboutToQuit.connect(self._about_to_quit)

        self._set_status(INITIAL_STATUS)

    @property
    def quitting(self) -> bool:
        return self._quitting

    def start(self) -> concurrent.futures.Future:
        """Show a reachable desktop surface and start Zara asynchronously."""
        if self._started:
            return self.host.start()
        self._started = True
        self._set_status(INITIAL_STATUS)

        tray_available = self.tray.show_if_available()
        if not tray_available:
            # Never create a background-only application the user cannot reach.
            self.window.show_raised()

        return self.host.start()

    def restart_runtime(self) -> None:
        if self._quitting or self._restart_request_id is not None:
            return
        command = RestartRuntime()
        self._restart_request_id = command.request_id
        self._set_status(
            DesktopStatus(
                DesktopRuntimeState.STARTING,
                "Restarting Zara runtime…",
            )
        )
        self.bridge.submit(command)

    def show_diagnostics(self) -> None:
        """Expose a stable hook until the full diagnostics surface lands in #92."""
        self.window.show_raised()
        self.diagnostics_requested.emit()

    def request_quit(self) -> None:
        """Explicit Quit: stop the runtime first, then terminate QApplication."""
        if self._quitting:
            return
        self._quitting = True
        command = ShutdownRuntime(reason="desktop quit")
        self._quit_request_id = command.request_id
        self.tray.quit_action.setEnabled(False)
        self.bridge.submit(command)

    def _on_runtime_envelope(self, envelope) -> None:
        event = getattr(envelope, "event", None)
        if event is None:
            return
        self._set_status(reduce_runtime_event(self.status, event))

    def _on_command_completed(self, receipt) -> None:
        request_id = getattr(receipt, "request_id", None)
        if request_id == self._quit_request_id:
            self._quit_request_id = None
            self._finalize_quit()
            return
        if request_id == self._restart_request_id:
            self._restart_request_id = None

    def _on_command_failed(self, request_id: str, message: str) -> None:
        if request_id == self._quit_request_id:
            # If the runtime is already failed/unavailable there is nothing
            # useful to drain. Explicit Quit must still close the UI.
            self._quit_request_id = None
            self._finalize_quit()
            return
        if request_id == self._restart_request_id:
            self._restart_request_id = None
            self._set_status(
                DesktopStatus(
                    DesktopRuntimeState.ERROR,
                    message or "Runtime restart failed",
                )
            )

    def _set_status(self, status: DesktopStatus) -> None:
        self.status = status
        self.tray.set_status(status)
        self.window.set_status(status)

    def _finalize_quit(self) -> None:
        if self._finalized:
            return
        self._finalized = True
        self.bridge.close()
        self.tray.hide()
        self.window.prepare_for_quit()
        self.window.close()
        self.app.quit()

    def _about_to_quit(self) -> None:
        """Best-effort cleanup for exits that bypass the tray Quit action."""
        if not self._finalized:
            self.bridge.close()
            self.tray.hide()
            self.window.prepare_for_quit()
            self.window.close()
            # RuntimeHost uses a daemon worker, but ask it to stop even when a
            # platform/session shutdown bypassed the normal explicit-quit flow.
            self.host.shutdown("desktop application exiting")
            self._finalized = True
