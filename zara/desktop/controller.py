"""Process-level controller for the native Zara desktop shell."""

from __future__ import annotations

import concurrent.futures
from typing import Callable, Optional

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.client import ZaraClient
from zara.config import get_config
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.state import (
    DesktopRuntimeState,
    DesktopStatus,
    INITIAL_STATUS,
    reduce_runtime_event,
)
from zara.desktop.tray import ZaraTray
from zara.desktop.theme import apply_desktop_theme
from zara.desktop.windows import FullChatWindow, QuickCopilotWindow, SettingsWindow
from zara.runtime.commands import CommandReceipt, RestartRuntime


class DesktopController(QObject):
    """Own the desktop shell while delegating assistant work to ZaraClient."""

    diagnostics_requested = Signal()

    def __init__(
        self,
        app: QApplication,
        client: ZaraClient,
        bridge: QtRuntimeBridge,
        *,
        tray_factory: Callable[[], ZaraTray] = ZaraTray,
        window_factory: Optional[Callable[[], object]] = None,
        settings_factory: Optional[Callable[[], object]] = None,
        conversation_service: Optional[ConversationService] = None,
    ) -> None:
        super().__init__(app)
        self.app = app
        self.client = client
        # Compatibility alias for existing embedders/tests while #133 migrates
        # callers. New desktop code must use the ZaraClient-facing name.
        self.host = client
        self.bridge = bridge
        self.tray = tray_factory()
        self.quick_window: Optional[QuickCopilotWindow] = None
        self.settings_window: Optional[object] = None
        self._settings_factory = settings_factory or (lambda: SettingsWindow(get_config()))

        if window_factory is None:
            self.conversation_service = conversation_service or ConversationService(ConversationStore())
            self.window = FullChatWindow(
                self.bridge,
                self.conversation_service,
                manage_runtime_events=False,
            )
            self.quick_window = QuickCopilotWindow(
                self.bridge,
                self.conversation_service,
                initial_conversation_id=self.window.current_conversation_id,
            )
            self.window.conversation_changed.connect(self._on_surface_conversation_changed)
            self.quick_window.conversation_changed.connect(self._on_surface_conversation_changed)
            self.quick_window.expand_requested.connect(self.expand_quick_to_full_chat)
            self.quick_window.settings_requested.connect(self.open_settings)
        else:
            self.conversation_service = conversation_service
            self.window = window_factory()

        self.status = INITIAL_STATUS

        self._started = False
        self._quitting = False
        self._finalized = False
        self._restart_request_id: Optional[str] = None

        quick_requested = getattr(self.tray, "quick_requested", None)
        full_chat_requested = getattr(self.tray, "full_chat_requested", None)
        settings_requested = getattr(self.tray, "settings_requested", None)
        if quick_requested is not None and self.quick_window is not None:
            quick_requested.connect(self.show_quick_copilot)
            if full_chat_requested is not None:
                full_chat_requested.connect(self.open_full_chat)
        else:
            self.tray.toggle_requested.connect(self.window.toggle_visibility)
        if settings_requested is not None:
            settings_requested.connect(self.open_settings)
        self.tray.restart_requested.connect(self.restart_runtime)
        self.tray.diagnostics_requested.connect(self.show_diagnostics)
        self.tray.quit_requested.connect(self.request_quit)

        self.window.restart_requested.connect(self.restart_runtime)
        self.window.diagnostics_requested.connect(self.show_diagnostics)
        window_settings_requested = getattr(self.window, "settings_requested", None)
        if window_settings_requested is not None:
            window_settings_requested.connect(self.open_settings)

        self.bridge.runtime_event.connect(self._on_runtime_envelope)
        self.bridge.command_completed.connect(self._on_command_completed)
        self.bridge.command_failed.connect(self._on_command_failed)
        self.app.aboutToQuit.connect(self._about_to_quit)

        self._set_status(INITIAL_STATUS)

    @property
    def quitting(self) -> bool:
        return self._quitting

    def start(self) -> concurrent.futures.Future:
        """Show a reachable desktop surface and start its Zara client."""
        if self._started:
            return self.client.start()
        self._started = True
        self._set_status(INITIAL_STATUS)

        tray_available = self.tray.show_if_available()
        if not tray_available:
            self.window.show_raised()

        return self.client.start()

    def show_quick_copilot(self) -> None:
        """Summon the one process-owned Quick Copilot instance."""
        if self.quick_window is None:
            self.window.show_raised()
            return
        self.quick_window.sync_from_shared_state()
        self.quick_window.show_raised()

    def open_full_chat(self, conversation_id: Optional[str] = None) -> None:
        """Show Full Chat, optionally selecting one durable conversation."""
        if conversation_id:
            self.window.open_conversation(conversation_id)
            return
        self.window.show_raised()

    def expand_quick_to_full_chat(self, conversation_id: Optional[str] = None) -> None:
        """Handoff Quick Copilot to Full Chat without copying or resubmitting."""
        if self.quick_window is None:
            self.open_full_chat(conversation_id)
            return
        target = conversation_id or self.quick_window.current_conversation_id
        self.open_full_chat(target)
        self.quick_window.hide()

    def open_settings(self) -> None:
        """Create one reusable settings workspace and apply live theme previews."""
        if self.settings_window is None:
            self.settings_window = self._settings_factory()
            theme_signal = getattr(self.settings_window, "theme_preview_requested", None)
            if theme_signal is not None:
                theme_signal.connect(self.apply_theme)
            restart_signal = getattr(self.settings_window, "restart_requested", None)
            if restart_signal is not None:
                restart_signal.connect(self.restart_runtime)
        self.settings_window.show_raised()

    def apply_theme(self, theme_key: str) -> None:
        """Preview one registered theme across every open desktop surface."""
        apply_desktop_theme(self.app, theme_key)

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
        """Disconnect this desktop client without stopping a shared daemon."""
        if self._quitting:
            return
        self._quitting = True
        self.tray.quit_action.setEnabled(False)
        self._finalize_quit()

    def _on_surface_conversation_changed(self, update) -> None:
        if self.conversation_service is None:
            return
        self.window.apply_conversation_update(update)
        if getattr(update, "metadata_changed", False) or getattr(update, "full_reload", False):
            self.window.refresh_history()
        if self.quick_window is not None:
            self.quick_window.sync_from_shared_state()

    def _on_runtime_envelope(self, envelope) -> None:
        event = getattr(envelope, "event", None)
        if event is None:
            return
        self._set_status(reduce_runtime_event(self.status, event))
        update = None
        if self.conversation_service is not None:
            update = self.conversation_service.apply_event(event)
            self.window.apply_conversation_update(update)
        if self.quick_window is not None:
            self.quick_window.sync_from_shared_state(event)

    def _on_command_completed(self, receipt) -> None:
        update = None
        if self.conversation_service is not None and isinstance(receipt, CommandReceipt):
            update = self.conversation_service.bind_receipt(receipt)
            self.window.handle_command_completed(receipt, update)
        if self.quick_window is not None:
            self.quick_window.handle_command_completed(receipt)
        self._resync_conversation_surfaces()

        request_id = getattr(receipt, "request_id", None)
        if request_id == self._restart_request_id:
            self._restart_request_id = None

    def _on_command_failed(self, request_id: str, message: str) -> None:
        update = None
        if self.conversation_service is not None:
            update = self.conversation_service.mark_command_failed(request_id, message)
            self.window.handle_command_failed(request_id, message, update)
        if self.quick_window is not None:
            self.quick_window.handle_command_failed(request_id, message)
        self._resync_conversation_surfaces()

        if request_id == self._restart_request_id:
            self._restart_request_id = None
            self._set_status(
                DesktopStatus(
                    DesktopRuntimeState.ERROR,
                    message or "Runtime restart failed",
                )
            )

    def _resync_conversation_surfaces(self) -> None:
        if self.conversation_service is None:
            return
        self.window.apply_conversation_update(None)
        if self.quick_window is not None:
            self.quick_window.sync_from_shared_state()

    def _set_status(self, status: DesktopStatus) -> None:
        self.status = status
        self.tray.set_status(status)
        self.window.set_status(status)
        if self.quick_window is not None:
            self.quick_window.set_status(status)

    def _close_client(self) -> None:
        close = getattr(self.client, "close", None)
        if callable(close):
            close()
            return
        # Temporary compatibility for legacy RuntimeHost injection. Normal
        # application construction always supplies a ZaraClient.
        shutdown = getattr(self.client, "shutdown", None)
        if callable(shutdown):
            shutdown("desktop standalone compatibility exit")

    def _close_surfaces(self) -> None:
        self.bridge.close()
        self.tray.hide()
        self.window.prepare_for_quit()
        self.window.close()
        if self.quick_window is not None:
            self.quick_window.prepare_for_quit()
            self.quick_window.close()
        if self.settings_window is not None:
            self.settings_window.prepare_for_quit()
            self.settings_window.close()

    def _finalize_quit(self) -> None:
        if self._finalized:
            return
        # Mark finalized before QApplication.quit() emits aboutToQuit, keeping
        # cleanup idempotent and preventing a second client close.
        self._finalized = True
        try:
            self._close_client()
        finally:
            self._close_surfaces()
        self.app.quit()

    def _about_to_quit(self) -> None:
        """Best-effort client/surface cleanup for non-tray application exits."""
        if self._finalized:
            return
        self._finalized = True
        try:
            self._close_client()
        finally:
            self._close_surfaces()
