from __future__ import annotations

import os
import re
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtGui import QColor
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.theme import (
    ANDROID_ROLE_DERIVATIONS,
    ANDROID_THEME_SOURCE,
    ANDROID_THEME_TOKENS,
    MIN_TEXT_CONTRAST,
    THEME_REGISTRY,
    contrast_ratio,
    desktop_stylesheet,
    resolve_theme,
)
from zara.desktop.windows import CopilotPresentation, CopilotWindow


REPO_ROOT = Path(__file__).resolve().parents[1]
KOTLIN_THEME = REPO_ROOT / ANDROID_THEME_SOURCE
ANDROID_THEME_KEYS = ("outrun", "starintel", "midnight", "terminal", "light")
ANDROID_TOKEN_KEYS = {
    "background",
    "surface",
    "surfaceElevated",
    "surfaceInput",
    "border",
    "borderActive",
    "primary",
    "secondary",
    "accentMagenta",
    "accentCyan",
    "text",
    "textMuted",
    "success",
    "warning",
    "error",
    "focus",
    "ambientGlow",
}
DESKTOP_ROLE_KEYS = {
    "ground",
    "panel_deep",
    "panel",
    "panel_lift",
    "line",
    "line_strong",
    "text",
    "text_muted",
    "primary",
    "primary_hover",
    "primary_deep",
    "on_primary",
    "active",
    "danger",
    "danger_deep",
}


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


class RecordingBridge:
    def submit(self, _command):
        raise AssertionError("visual parity checks must not submit runtime commands")


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
    assert tuple(ANDROID_THEME_TOKENS) == ANDROID_THEME_KEYS
    assert ANDROID_THEME_TOKENS == _android_kotlin_tokens()
    assert all(set(tokens) == ANDROID_TOKEN_KEYS for tokens in ANDROID_THEME_TOKENS.values())


def test_all_desktop_roles_have_queryable_android_derivation_provenance() -> None:
    assert set(ANDROID_ROLE_DERIVATIONS) == DESKTOP_ROLE_KEYS
    assert ANDROID_ROLE_DERIVATIONS["ground"].source_tokens == ("background",)
    assert ANDROID_ROLE_DERIVATIONS["panel_lift"].source_tokens == ("surfaceElevated",)
    assert ANDROID_ROLE_DERIVATIONS["on_primary"].operation == "contrast_text"
    assert ANDROID_ROLE_DERIVATIONS["on_primary"].source_tokens == ("primary",)
    assert ANDROID_ROLE_DERIVATIONS["danger_deep"].operation == "mix"
    assert ANDROID_ROLE_DERIVATIONS["danger_deep"].source_tokens == ("surfaceInput", "error")
    assert ANDROID_ROLE_DERIVATIONS["danger_deep"].weight == 0.18
    assert all(set(THEME_REGISTRY[key].colors) == DESKTOP_ROLE_KEYS for key in ANDROID_THEME_KEYS)


def test_android_parity_themes_are_first_class_and_outrun_is_default() -> None:
    assert tuple(THEME_REGISTRY)[:5] == ANDROID_THEME_KEYS
    assert resolve_theme(None).key == "outrun"
    assert resolve_theme("missing").key == "outrun"
    assert THEME_REGISTRY["outrun"].colors["ground"] == "#02040B"
    assert THEME_REGISTRY["outrun"].colors["primary"] == "#E21CF2"
    assert {"signal-cabin", "dotfiles-outrun", "nord", "dracula", "chatgpt-neutral"} <= set(
        THEME_REGISTRY
    )


def test_android_parity_themes_pass_raw_wcag_conformance_without_repair() -> None:
    for key in ANDROID_THEME_KEYS:
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
                contrast_ratio(QColor(colors[foreground]), QColor(colors[background]))
                >= MIN_TEXT_CONTRAST
            ), f"{key}: {foreground} on {background}"


def test_outrun_stylesheet_uses_android_surface_and_density_vocabulary() -> None:
    stylesheet = desktop_stylesheet("outrun")
    runtime_rule = stylesheet.split("QFrame#zaraRuntimeRail {", 1)[1].split("}", 1)[0]
    composer_rule = stylesheet.split("QFrame#zaraComposerShell {", 1)[1].split("}", 1)[0]
    history_rule = stylesheet.split("QWidget#zaraConversationHistoryPanel {", 1)[1].split("}", 1)[0]

    assert "background: #07101B" in runtime_rule
    assert "border: 1px solid #1A2A49" in runtime_rule
    assert "background: #080F1E" in composer_rule
    assert "border-radius: 20px" in composer_rule
    assert "background: #07101B" in history_rule
    assert "border: 1px solid #1A2A49" in history_rule
    assert "QScrollArea#zaraThemeStrip" in stylesheet


def test_adaptive_copilot_uses_mobile_first_header_hierarchy(tmp_path) -> None:
    qt_app = app()
    database = DatabaseManager(tmp_path / "desktop-mobile-header.db")
    conversations = ConversationService(ConversationStore(database))
    window = CopilotWindow(RecordingBridge(), conversations)
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
