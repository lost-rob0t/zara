from __future__ import annotations

import os
import re
import subprocess
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Qt
from PySide6.QtGui import QColor
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication, QPushButton

from zara.database import DatabaseManager
from zara.desktop.chat_widgets import MessageWidget
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from zara.desktop.theme import (
    ANDROID_THEME_TOKENS,
    MIN_TEXT_CONTRAST,
    THEME_REGISTRY,
    contrast_ratio,
    resolve_theme,
)
from zara.desktop.windows import CopilotPresentation, CopilotWindow, QuickCopilotWindow
from zara.runtime import events
from zara.runtime.commands import ApproveTool, RejectTool

REPO_ROOT = Path(__file__).resolve().parents[1]
KOTLIN_THEME = (
    REPO_ROOT
    / "android/shared-ui/src/main/java/ai/zara/ui/theme/ZaraTheme.kt"
)


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


class RecordingBridge(QObject):
    def __init__(self) -> None:
        super().__init__()
        self.commands = []

    def submit(self, command):
        self.commands.append(command)
        return None


def _token_values(block: str) -> dict[str, str]:
    return {
        name: f"#{rgb.upper()}"
        for name, rgb in re.findall(
            r"(\w+)\s*=\s*Color\(0xFF([0-9A-Fa-f]{6})\)",
            block,
        )
    }


def _android_kotlin_tokens() -> dict[str, dict[str, str]]:
    source = KOTLIN_THEME.read_text(encoding="utf-8")
    base_match = re.search(
        r"private val OutrunTokens = ZaraSemanticTokens\((.*?)\n\)",
        source,
        re.DOTALL,
    )
    assert base_match is not None
    outrun = _token_values(base_match.group(1))
    result = {"outrun": outrun}
    for kotlin_name, key in (
        ("StarIntel", "starintel"),
        ("Midnight", "midnight"),
        ("Terminal", "terminal"),
        ("Light", "light"),
    ):
        match = re.search(
            rf"ZaraTheme\.{kotlin_name} -> OutrunTokens\.copy\((.*?)\n\s{{8}}\)",
            source,
            re.DOTALL,
        )
        assert match is not None
        result[key] = {**outrun, **_token_values(match.group(1))}
    return result


def test_desktop_android_theme_tokens_match_canonical_kotlin_source() -> None:
    assert ANDROID_THEME_TOKENS == _android_kotlin_tokens()


def test_android_parity_themes_are_first_class_and_outrun_is_default() -> None:
    assert tuple(THEME_REGISTRY)[:5] == (
        "outrun",
        "starintel",
        "midnight",
        "terminal",
        "light",
    )
    assert resolve_theme(None).key == "outrun"
    assert THEME_REGISTRY["outrun"].colors["ground"] == "#02040B"
    assert THEME_REGISTRY["outrun"].colors["primary"] == "#E21CF2"


def test_android_parity_themes_pass_raw_wcag_conformance_without_repair() -> None:
    for key in ("outrun", "starintel", "midnight", "terminal", "light"):
        colors = THEME_REGISTRY[key].colors
        for foreground, background in (
            ("text", "ground"),
            ("text", "panel_deep"),
            ("text", "panel"),
            ("text", "panel_lift"),
            ("text_muted", "panel_deep"),
            ("on_primary", "primary"),
        ):
            assert (
                contrast_ratio(
                    QColor(colors[foreground]),
                    QColor(colors[background]),
                )
                >= MIN_TEXT_CONTRAST
            ), f"{key}: {foreground} on {background}"


def test_desktop_design_expert_explains_android_token_mapping() -> None:
    goal = (
        f"use_module('{REPO_ROOT / 'modules/desktop_design_expert.pl'}'), "
        "desktop_design_expert:desktop_role_source(outrun, ground, background), "
        "desktop_design_expert:desktop_role_source(outrun, panel_lift, surfaceElevated), "
        "desktop_design_expert:desktop_role_derivation("
        "outrun, on_primary, contrast_text(primary)), "
        "desktop_design_expert:desktop_role_derivation("
        "outrun, danger_deep, mix(surfaceInput, error, 18)), "
        "findall(Role, desktop_design_expert:desktop_role_derivation("
        "outrun, Role, _), Roles), sort(Roles, UniqueRoles), length(UniqueRoles, 15), "
        "desktop_design_expert:theme_provenance("
        "outrun, 'android/shared-ui/src/main/java/ai/zara/ui/theme/ZaraTheme.kt'), "
        "halt."
    )
    completed = subprocess.run(
        ["swipl", "-q", "-g", goal],
        cwd=REPO_ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, completed.stderr


def test_waiting_tool_card_exposes_bounded_approve_reject_actions() -> None:
    qt_app = app()
    message = MessageWidget(
        MessageRecord(
            id="tool-message",
            conversation_id="conversation-1",
            sequence=1,
            role=MessageRole.TOOL,
            content="calendar.sync: waiting for approval",
            status=MessageStatus.PENDING,
            created_at="2026-09-20T00:00:00Z",
            updated_at="2026-09-20T00:00:00Z",
            turn_id="turn-1",
            tool_run_id="tool-run-1",
        )
    )
    actions: list[tuple[str, str]] = []
    message.tool_action_requested.connect(
        lambda tool_run_id, action: actions.append((tool_run_id, action))
    )
    try:
        message.show()
        qt_app.processEvents()
        approve = message.findChild(QPushButton, "zaraToolApprove")
        reject = message.findChild(QPushButton, "zaraToolReject")
        assert approve is not None
        assert reject is not None
        QTest.mouseClick(approve, Qt.MouseButton.LeftButton)
        QTest.mouseClick(reject, Qt.MouseButton.LeftButton)
        assert actions == [
            ("tool-run-1", "approve"),
            ("tool-run-1", "reject"),
        ]
    finally:
        message.deleteLater()
        qt_app.processEvents()


def test_copilot_routes_tool_card_actions_through_runtime_commands(tmp_path) -> None:
    qt_app = app()
    database = DatabaseManager(tmp_path / "desktop-mobile-parity.db")
    conversations = ConversationService(ConversationStore(database))
    bridge = RecordingBridge()
    window = QuickCopilotWindow(bridge, conversations)
    try:
        conversation_id = window.current_conversation_id
        conversations.apply_event(
            events.ToolWaitingForUser(
                conversation_id=conversation_id,
                turn_id="turn-1",
                tool_run_id="tool-run-1",
                tool_name="calendar.sync",
            )
        )
        window.sync_from_shared_state()
        tool_widget = next(
            widget
            for widget in window.message_widgets.values()
            if widget.message.tool_run_id == "tool-run-1"
        )
        approve = tool_widget.findChild(QPushButton, "zaraToolApprove")
        reject = tool_widget.findChild(QPushButton, "zaraToolReject")
        assert approve is not None
        assert reject is not None

        QTest.mouseClick(approve, Qt.MouseButton.LeftButton)
        QTest.mouseClick(reject, Qt.MouseButton.LeftButton)

        assert isinstance(bridge.commands[-2], ApproveTool)
        assert bridge.commands[-2].tool_run_id == "tool-run-1"
        assert isinstance(bridge.commands[-1], RejectTool)
        assert bridge.commands[-1].tool_run_id == "tool-run-1"
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        qt_app.processEvents()


def test_adaptive_copilot_uses_mobile_first_header_hierarchy(tmp_path) -> None:
    qt_app = app()
    database = DatabaseManager(tmp_path / "desktop-mobile-header.db")
    conversations = ConversationService(ConversationStore(database))
    bridge = RecordingBridge()
    window = CopilotWindow(bridge, conversations)
    try:
        window.resize(900, 640)
        window.show()
        qt_app.processEvents()
        assert window.brand_label.isHidden()
        assert window.expand_button.text() == "History"

        window.set_presentation(CopilotPresentation.EXPANDED)
        qt_app.processEvents()
        assert window.brand_label.isHidden()
        assert window.expand_button.text() == "Chat"
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        qt_app.processEvents()
