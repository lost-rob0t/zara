"""Deterministic offline screenshot fixtures for Zara's native Copilot."""

from __future__ import annotations

import concurrent.futures
import json
import os
import tempfile
from pathlib import Path

from PySide6.QtCore import QObject, QSettings, QSize, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.theme import apply_desktop_theme
from zara.desktop.windows.copilot import CopilotMode, CopilotWindow
from zara.runtime import events

REQUIRED_UI_FIXTURES = (
    "copilot-empty-compact",
    "copilot-short-chat-compact",
    "copilot-long-wrap-compact",
    "copilot-streaming",
    "copilot-error",
    "copilot-disconnected",
    "copilot-tool-running",
    "copilot-tool-approval",
    "copilot-voice-listening",
    "copilot-voice-partial",
    "copilot-expanded",
    "copilot-history",
    "copilot-smallest-supported",
)


class _FixtureBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def submit(self, _command):
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


_FIXTURE_SIZES = {
    "copilot-empty-compact": QSize(680, 460),
    "copilot-short-chat-compact": QSize(680, 460),
    "copilot-long-wrap-compact": QSize(680, 460),
    "copilot-streaming": QSize(680, 460),
    "copilot-error": QSize(680, 460),
    "copilot-disconnected": QSize(680, 460),
    "copilot-tool-running": QSize(680, 460),
    "copilot-tool-approval": QSize(680, 460),
    "copilot-voice-listening": QSize(680, 460),
    "copilot-voice-partial": QSize(680, 460),
    "copilot-expanded": QSize(980, 700),
    "copilot-history": QSize(980, 700),
    "copilot-smallest-supported": QSize(480, 320),
}


def _app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    qt_app = instance or QApplication([])
    qt_app.setQuitOnLastWindowClosed(False)
    qt_app.setFont(QFont("DejaVu Sans", 10))
    apply_desktop_theme(qt_app, "chatgpt-neutral")
    return qt_app


def _add_exchange(
    service: ConversationService,
    conversation_id: str,
    *,
    user: str,
    assistant: str,
    turn_id: str,
) -> None:
    service.add_user_message(
        conversation_id,
        user,
        request_id=f"fixture-{turn_id}",
    )
    service.apply_event(
        events.AssistantComplete(
            conversation_id=conversation_id,
            turn_id=turn_id,
            text=assistant,
        )
    )


def _populate_fixture(
    fixture: str,
    window: CopilotWindow,
    service: ConversationService,
) -> DesktopStatus:
    conversation_id = window.current_conversation_id
    ready = DesktopStatus(DesktopRuntimeState.READY, "Zara is ready")

    if fixture == "copilot-empty-compact":
        return ready

    if fixture == "copilot-short-chat-compact":
        _add_exchange(
            service,
            conversation_id,
            user="What is the next deployment step?",
            assistant="Run the staging gate, inspect the result, then promote the exact green revision.",
            turn_id="short",
        )
        return ready

    if fixture == "copilot-long-wrap-compact":
        _add_exchange(
            service,
            conversation_id,
            user="Summarize why the desktop should keep one conversation renderer instead of two synchronized windows.",
            assistant=(
                "One renderer removes duplicated focus, geometry, status, and message presentation state while preserving "
                "the same durable conversation identity. Compact and expanded layouts can expose different controls "
                "without copying messages or handing the turn to another top-level window."
            ),
            turn_id="long-wrap",
        )
        return ready

    if fixture == "copilot-streaming":
        service.add_user_message(
            conversation_id,
            "Stream a concise deployment plan.",
            request_id="fixture-stream",
        )
        service.apply_event(
            events.AssistantStarted(
                conversation_id=conversation_id,
                turn_id="stream",
            )
        )
        service.apply_event(
            events.AssistantDelta(
                conversation_id=conversation_id,
                turn_id="stream",
                text="First validate the exact candidate, then inspect visual artifacts…",
            )
        )
        return DesktopStatus(DesktopRuntimeState.THINKING, "Thinking…")

    if fixture == "copilot-error":
        service.add_user_message(
            conversation_id,
            "Run the provider-backed operation.",
            request_id="fixture-error",
        )
        service.apply_event(
            events.AssistantFailed(
                conversation_id=conversation_id,
                turn_id="error",
                reason="Provider unavailable in deterministic fixture",
            )
        )
        return DesktopStatus(
            DesktopRuntimeState.ERROR,
            "Provider unavailable in deterministic fixture",
        )

    if fixture == "copilot-disconnected":
        _add_exchange(
            service,
            conversation_id,
            user="Are you connected?",
            assistant="The conversation is preserved while the daemon reconnects.",
            turn_id="disconnect",
        )
        return DesktopStatus(DesktopRuntimeState.DISCONNECTED, "Reconnecting to Zara daemon…")

    if fixture == "copilot-tool-running":
        service.add_user_message(
            conversation_id,
            "Inspect the current repository state.",
            request_id="fixture-tool-running",
        )
        service.apply_event(
            events.ToolStarted(
                conversation_id=conversation_id,
                turn_id="tool-running",
                tool_run_id="fixture-tool-1",
                tool_name="repository search",
            )
        )
        return DesktopStatus(DesktopRuntimeState.TOOL_RUNNING, "Running repository search…")

    if fixture == "copilot-tool-approval":
        service.add_user_message(
            conversation_id,
            "Apply the reviewed change.",
            request_id="fixture-tool-approval",
        )
        service.apply_event(
            events.ToolWaitingForUser(
                conversation_id=conversation_id,
                turn_id="tool-approval",
                tool_run_id="fixture-tool-2",
                tool_name="deployment",
                prompt="Approve deployment to staging?",
            )
        )
        return DesktopStatus(DesktopRuntimeState.NEEDS_INPUT, "Approve deployment to staging?")

    if fixture == "copilot-voice-listening":
        return DesktopStatus(DesktopRuntimeState.LISTENING, "Listening…")

    if fixture == "copilot-voice-partial":
        return DesktopStatus(
            DesktopRuntimeState.LISTENING,
            'Hearing: "show me the latest deployment…"',
        )

    if fixture == "copilot-expanded":
        _add_exchange(
            service,
            conversation_id,
            user="Show the full project context.",
            assistant="Expanded mode exposes history and controls around the same conversation renderer.",
            turn_id="expanded",
        )
        window.set_mode(CopilotMode.EXPANDED)
        return ready

    if fixture == "copilot-history":
        service.rename_conversation(conversation_id, "Unified Copilot")
        service.create_conversation("Voice session: deployment review")
        service.create_conversation("Hydra debugging")
        service.create_conversation("Android Zara planning")
        window.refresh_history()
        window.set_mode(CopilotMode.EXPANDED)
        return ready

    if fixture == "copilot-smallest-supported":
        _add_exchange(
            service,
            conversation_id,
            user="Small window?",
            assistant="Composer and conversation remain usable.",
            turn_id="small",
        )
        return ready

    raise ValueError(f"unknown UI fixture: {fixture}")


def render_ui_fixtures(output_dir: Path) -> Path:
    """Render the required offline Copilot states and return the manifest path."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    qt_app = _app()
    manifest_entries: list[dict[str, object]] = []

    with tempfile.TemporaryDirectory(prefix="zara-ui-fixtures-") as temp_root:
        root = Path(temp_root)
        for fixture in REQUIRED_UI_FIXTURES:
            fixture_root = root / fixture
            fixture_root.mkdir(parents=True, exist_ok=True)
            service = ConversationService(
                ConversationStore(DatabaseManager(fixture_root / "conversation.db"))
            )
            settings = QSettings(
                str(fixture_root / "desktop.ini"),
                QSettings.Format.IniFormat,
            )
            window = CopilotWindow(
                _FixtureBridge(),  # type: ignore[arg-type]
                service,
                manage_runtime_events=False,
                settings=settings,
            )
            status = _populate_fixture(fixture, window, service)
            window.load_conversation(window.current_conversation_id)
            window.set_status(status)
            size = _FIXTURE_SIZES[fixture]
            window.resize(size)
            window.show()
            qt_app.processEvents()

            png_path = output_dir / f"{fixture}.png"
            pixmap = window.grab()
            if pixmap.isNull() or not pixmap.save(str(png_path), "PNG"):
                raise RuntimeError(f"failed to render UI fixture {fixture}")

            manifest_entries.append(
                {
                    "fixture": fixture,
                    "mode": window.mode.value,
                    "width": window.width(),
                    "height": window.height(),
                    "theme": "chatgpt-neutral",
                    "state": status.state.value,
                    "path": str(png_path),
                }
            )
            window.prepare_for_quit()
            window.close()
            window.deleteLater()
            qt_app.processEvents()

    manifest = {
        "source_commit": os.environ.get("GITHUB_SHA", "unknown"),
        "theme": "chatgpt-neutral",
        "fixtures": manifest_entries,
    }
    manifest_path = output_dir / "manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return manifest_path


def main() -> int:
    """Render CI review artifacts into the configured artifact directory."""
    artifact_root = Path(os.environ.get("ARTIFACT_DIR", "artifacts"))
    manifest = render_ui_fixtures(artifact_root / "ui")
    print(f"Rendered Zara UI fixtures: {manifest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = ["REQUIRED_UI_FIXTURES", "render_ui_fixtures"]
