"""Deterministic offscreen screenshot fixtures for the adaptive Copilot.

This module is deliberately UI-only: it uses temporary conversation/config state,
never connects to a daemon/provider, and never reads the user's XDG state.
"""

from __future__ import annotations

import concurrent.futures
import json
import tempfile
from pathlib import Path
from typing import Callable

from PySide6.QtCore import QSettings
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.windows import CopilotPresentation, CopilotWindow

_COMPACT_SIZE = (680, 460)
_EXPANDED_SIZE = (960, 680)
_MINIMUM_SIZE = (480, 320)
_THEME = "default"

_FIXTURES: tuple[tuple[str, str], ...] = (
    ("empty-compact", "copilot-empty-compact.png"),
    ("short-chat-compact", "copilot-short-chat-compact.png"),
    ("long-wrap-compact", "copilot-long-wrap-compact.png"),
    ("streaming", "copilot-streaming.png"),
    ("error", "copilot-error.png"),
    ("disconnected", "copilot-disconnected.png"),
    ("tool-running", "copilot-tool-running.png"),
    ("tool-approval", "copilot-tool-approval.png"),
    ("voice-listening", "copilot-voice-listening.png"),
    ("voice-partial", "copilot-voice-partial.png"),
    ("expanded", "copilot-expanded.png"),
    ("history", "copilot-history.png"),
    ("smallest-supported", "copilot-smallest-supported.png"),
)


class _FixtureBridge:
    """No-I/O runtime bridge sufficient for presentation fixtures."""

    def __init__(self) -> None:
        self.commands: list[object] = []

    def submit(self, command: object) -> concurrent.futures.Future[None]:
        self.commands.append(command)
        future: concurrent.futures.Future[None] = concurrent.futures.Future()
        future.set_result(None)
        return future


def _application() -> QApplication:
    instance = QApplication.instance()
    if instance is not None:
        if not isinstance(instance, QApplication):
            raise RuntimeError("Qt application is not a QApplication")
        return instance
    app = QApplication([])
    app.setQuitOnLastWindowClosed(False)
    return app


def _add_user_messages(service: ConversationService, conversation_id: str, *messages: str) -> None:
    for index, text in enumerate(messages):
        service.add_user_message(
            conversation_id,
            text,
            request_id=f"fixture-{index}-{conversation_id}",
        )


def _configure_fixture(state: str, window: CopilotWindow, service: ConversationService) -> None:
    conversation_id = window.current_conversation_id

    if state == "empty-compact":
        return
    if state == "short-chat-compact":
        _add_user_messages(service, conversation_id, "Can you summarize today's plan?", "Keep it concise.")
    elif state == "long-wrap-compact":
        _add_user_messages(
            service,
            conversation_id,
            "Explain how the daemon keeps one conversation continuous across reconnects while "
            "rejecting stale events and preserving exact request, turn, session, and conversation correlation.",
        )
    elif state == "streaming":
        _add_user_messages(service, conversation_id, "What changed in Zara today?")
        window.runtime_status_label.setText("Thinking")
        window.runtime_detail_label.setText("Streaming assistant response…")
    elif state == "error":
        _add_user_messages(service, conversation_id, "Run the last request again.")
        window.command_error_label.setText("The runtime rejected this turn. Nothing was executed.")
        window.command_error_label.show()
    elif state == "disconnected":
        window.runtime_status_label.setText("Disconnected")
        window.runtime_detail_label.setText("Daemon unavailable — reconnecting.")
    elif state == "tool-running":
        _add_user_messages(service, conversation_id, "Check the repository status.")
        window.runtime_status_label.setText("Tool running")
        window.runtime_detail_label.setText("git status · running")
    elif state == "tool-approval":
        _add_user_messages(service, conversation_id, "Apply the verified update.")
        window.runtime_status_label.setText("Approval required")
        window.runtime_detail_label.setText("A side effect is waiting for your approval.")
    elif state == "voice-listening":
        window.runtime_status_label.setText("Listening")
        window.runtime_detail_label.setText("Microphone active · waiting for speech")
    elif state == "voice-partial":
        window.runtime_status_label.setText("Listening")
        window.runtime_detail_label.setText("Partial transcript: open roam daily…")
    elif state == "expanded":
        _add_user_messages(service, conversation_id, "Show this conversation in expanded mode.")
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(*_EXPANDED_SIZE)
    elif state == "history":
        _add_user_messages(
            service,
            conversation_id,
            "First turn",
            "Second turn",
            "Third turn",
            "Fourth turn",
            "Fifth turn",
            "Most recent turn",
        )
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(*_EXPANDED_SIZE)
    elif state == "smallest-supported":
        _add_user_messages(service, conversation_id, "Composer stays visible at the minimum supported size.")
        window.resize(*_MINIMUM_SIZE)
    else:  # pragma: no cover - fixture table is closed and tested by the caller
        raise ValueError(f"unknown Copilot fixture state: {state}")

    window.sync_from_shared_state()


def _render_one(
    output_dir: Path,
    state: str,
    filename: str,
    *,
    source_commit: str,
    root: Path,
) -> dict[str, object]:
    app = _application()
    db_path = root / f"{state}.db"
    settings_path = root / f"{state}.ini"
    service = ConversationService(ConversationStore(DatabaseManager(db_path)))
    settings = QSettings(str(settings_path), QSettings.Format.IniFormat)
    bridge = _FixtureBridge()
    window = CopilotWindow(bridge, service, settings=settings)  # type: ignore[arg-type]
    window.resize(*_COMPACT_SIZE)

    try:
        _configure_fixture(state, window, service)
        window.show()
        app.processEvents()
        pixmap = window.grab()
        if pixmap.isNull():
            raise RuntimeError(f"failed to render Copilot fixture: {state}")
        target = output_dir / filename
        if not pixmap.save(str(target), "PNG"):
            raise RuntimeError(f"failed to save Copilot fixture: {target}")
        return {
            "state": state,
            "path": filename,
            "width": pixmap.width(),
            "height": pixmap.height(),
            "theme": _THEME,
            "source_commit": source_commit,
        }
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        app.processEvents()
        settings.sync()


def render_copilot_fixtures(output_dir: Path | str, *, source_commit: str) -> dict[str, object]:
    """Render the closed #324 fixture matrix without touching user state or I/O.

    ``source_commit`` is evidence supplied by the caller; rendering does not invoke
    Git, the daemon, providers, microphones, or the network.
    """
    target = Path(output_dir)
    target.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="zara-copilot-fixtures-") as temp_dir:
        root = Path(temp_dir)
        fixtures = [
            _render_one(
                target,
                state,
                filename,
                source_commit=source_commit,
                root=root,
            )
            for state, filename in _FIXTURES
        ]

    manifest: dict[str, object] = {
        "schema": 1,
        "fixtures": fixtures,
    }
    manifest_path = target / "manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return manifest


__all__ = ["render_copilot_fixtures"]
