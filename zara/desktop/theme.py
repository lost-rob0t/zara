"""Signal Cabin visual system for Zara Desktop.

THESIS: One conversation follows one visible route; the interface refuses generic AI-card chrome.
OWN-WORLD: Charcoal enamel, warm ivory type, mint route lamps, amber activity, and signal red.
STORY: Summon Zara, read the live route, work in place, then carry the same trace into Full Chat.
FIRST VIEWPORT: A precise status rail anchors the top, conversation owns the field, and the composer forms the control bed.
FORM: Railway signal cabin, second on the grounded list; seed 35e80c4d.
FINISH: unreviewed and undocumented is unfinished; this build ends with the finish review, the verdict, DESIGN.md, and every shipping raster carrying its provenance
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from types import MappingProxyType
from typing import Mapping

from PySide6.QtGui import QColor, QPalette
from PySide6.QtWidgets import QApplication

MIN_TEXT_CONTRAST = 4.5


@dataclass(frozen=True)
class ThemeDefinition:
    key: str
    label: str
    description: str
    colors: Mapping[str, str]


def _theme(
    key: str,
    label: str,
    description: str,
    *,
    ground: str,
    panel_deep: str,
    panel: str,
    panel_lift: str,
    line: str,
    line_strong: str,
    text: str,
    text_muted: str,
    primary: str,
    primary_hover: str,
    primary_deep: str,
    on_primary: str,
    active: str,
    danger: str,
    danger_deep: str,
) -> ThemeDefinition:
    colors = {
        "ground": ground,
        "panel_deep": panel_deep,
        "panel": panel,
        "panel_lift": panel_lift,
        "line": line,
        "line_strong": line_strong,
        "text": text,
        "text_muted": text_muted,
        "primary": primary,
        "primary_hover": primary_hover,
        "primary_deep": primary_deep,
        "on_primary": on_primary,
        "active": active,
        "danger": danger,
        "danger_deep": danger_deep,
    }
    return ThemeDefinition(key, label, description, MappingProxyType(colors))


THEME_REGISTRY: Mapping[str, ThemeDefinition] = MappingProxyType(
    {
        "signal-cabin": _theme(
            "signal-cabin",
            "Signal Cabin",
            "Charcoal enamel with warm ivory and precise route signals.",
            ground="#0A1012",
            panel_deep="#0D1518",
            panel="#111A1E",
            panel_lift="#172226",
            line="#2A393E",
            line_strong="#3C5358",
            text="#F2E9D8",
            text_muted="#A8B7B3",
            primary="#61D095",
            primary_hover="#7ADDA8",
            primary_deep="#17382B",
            on_primary="#0A1012",
            active="#E7B84B",
            danger="#E6544D",
            danger_deep="#562727",
        ),
        "dotfiles-outrun": _theme(
            "dotfiles-outrun",
            "Dotfiles Outrun",
            "The Doom Electric Outrun palette from the user's Qtile desktop.",
            ground="#170C32",
            panel_deep="#1B153A",
            panel="#202146",
            panel_lift="#2A2056",
            line="#56325F",
            line_strong="#92406E",
            text="#F3F4F5",
            text_muted="#D7B9D0",
            primary="#2DE2E6",
            primary_hover="#72F5F7",
            primary_deep="#173F51",
            on_primary="#170C32",
            active="#FBA922",
            danger="#DD546E",
            danger_deep="#4D1F3C",
        ),
        "nord": _theme(
            "nord",
            "Nord",
            "Polar-night surfaces with frost-blue controls.",
            ground="#2E3440",
            panel_deep="#282E39",
            panel="#3B4252",
            panel_lift="#434C5E",
            line="#4C566A",
            line_strong="#5E81AC",
            text="#ECEFF4",
            text_muted="#D8DEE9",
            primary="#88C0D0",
            primary_hover="#8FBCBB",
            primary_deep="#3B5368",
            on_primary="#20242C",
            active="#EBCB8B",
            danger="#BF616A",
            danger_deep="#4C3038",
        ),
        "dracula": _theme(
            "dracula",
            "Dracula",
            "Ink-dark violet surfaces with bright terminal accents.",
            ground="#282A36",
            panel_deep="#21222C",
            panel="#343746",
            panel_lift="#44475A",
            line="#525568",
            line_strong="#6272A4",
            text="#F8F8F2",
            text_muted="#C8C8D0",
            primary="#50FA7B",
            primary_hover="#69FF94",
            primary_deep="#24452E",
            on_primary="#20222B",
            active="#F1FA8C",
            danger="#FF5555",
            danger_deep="#5A2A34",
        ),
        "chatgpt-neutral": _theme(
            "chatgpt-neutral",
            "ChatGPT Neutral",
            "A calm neutral workspace inspired by modern conversational tools.",
            ground="#FFFFFF",
            panel_deep="#F7F7F8",
            panel="#ECECF1",
            panel_lift="#FFFFFF",
            line="#D9D9E3",
            line_strong="#B4B4C0",
            text="#202123",
            text_muted="#5F6368",
            primary="#10A37F",
            primary_hover="#0E8F70",
            primary_deep="#D1F4EA",
            on_primary="#0D0D0D",
            active="#9A6700",
            danger="#C92A2A",
            danger_deep="#FDE8E8",
        ),
    }
)

SIGNAL_CABIN_COLORS = {
    **THEME_REGISTRY["signal-cabin"].colors,
    "ready": THEME_REGISTRY["signal-cabin"].colors["primary"],
    "ready_hover": THEME_REGISTRY["signal-cabin"].colors["primary_hover"],
    "ready_deep": THEME_REGISTRY["signal-cabin"].colors["primary_deep"],
}

_FOREGROUND_BACKGROUND_ROLES: tuple[tuple[QPalette.ColorRole, QPalette.ColorRole], ...] = (
    (QPalette.ColorRole.WindowText, QPalette.ColorRole.Window),
    (QPalette.ColorRole.Text, QPalette.ColorRole.Base),
    (QPalette.ColorRole.ButtonText, QPalette.ColorRole.Button),
    (QPalette.ColorRole.PlaceholderText, QPalette.ColorRole.Base),
    (QPalette.ColorRole.HighlightedText, QPalette.ColorRole.Highlight),
    (QPalette.ColorRole.ToolTipText, QPalette.ColorRole.ToolTipBase),
)

_COLOR_GROUPS: tuple[QPalette.ColorGroup, ...] = (
    QPalette.ColorGroup.Active,
    QPalette.ColorGroup.Inactive,
    QPalette.ColorGroup.Disabled,
)


def _linear_channel(value: float) -> float:
    if value <= 0.04045:
        return value / 12.92
    return ((value + 0.055) / 1.055) ** 2.4


def _opaque_rgb(color: QColor, background: QColor | None = None) -> tuple[float, float, float]:
    red = color.redF()
    green = color.greenF()
    blue = color.blueF()
    alpha = color.alphaF()
    if alpha >= 1.0:
        return red, green, blue

    if background is None:
        background = QColor("white")
    bg_red, bg_green, bg_blue = _opaque_rgb(background)
    return (
        red * alpha + bg_red * (1.0 - alpha),
        green * alpha + bg_green * (1.0 - alpha),
        blue * alpha + bg_blue * (1.0 - alpha),
    )


def relative_luminance(color: QColor, *, background: QColor | None = None) -> float:
    """Return WCAG relative luminance for a Qt color."""
    red, green, blue = _opaque_rgb(color, background)
    return (
        0.2126 * _linear_channel(red)
        + 0.7152 * _linear_channel(green)
        + 0.0722 * _linear_channel(blue)
    )


def contrast_ratio(foreground: QColor, background: QColor) -> float:
    """Return the WCAG contrast ratio for a foreground/background pair."""
    background_luminance = relative_luminance(background)
    foreground_luminance = relative_luminance(foreground, background=background)
    lighter = max(foreground_luminance, background_luminance)
    darker = min(foreground_luminance, background_luminance)
    return (lighter + 0.05) / (darker + 0.05)


def _best_text_color(background: QColor) -> QColor:
    black = QColor("black")
    white = QColor("white")
    if contrast_ratio(black, background) >= contrast_ratio(white, background):
        return black
    return white


def _repair_group(
    palette: QPalette,
    group: QPalette.ColorGroup,
    pairs: Iterable[tuple[QPalette.ColorRole, QPalette.ColorRole]],
    minimum_contrast: float,
) -> None:
    for foreground_role, background_role in pairs:
        foreground = palette.color(group, foreground_role)
        background = palette.color(group, background_role)
        if contrast_ratio(foreground, background) >= minimum_contrast:
            continue
        palette.setColor(group, foreground_role, _best_text_color(background))


def repair_palette(
    source: QPalette,
    *,
    minimum_contrast: float = MIN_TEXT_CONTRAST,
) -> QPalette:
    """Copy a system palette and repair only unreadable foreground roles."""
    palette = QPalette(source)
    for group in _COLOR_GROUPS:
        _repair_group(
            palette,
            group,
            _FOREGROUND_BACKGROUND_ROLES,
            minimum_contrast,
        )
    return palette


def apply_readable_palette(
    app: QApplication,
    *,
    minimum_contrast: float = MIN_TEXT_CONTRAST,
) -> QPalette:
    """Repair the host palette and install it as Zara Desktop's app palette."""
    palette = repair_palette(app.palette(), minimum_contrast=minimum_contrast)
    app.setPalette(palette)
    return palette


def resolve_theme(theme_key: str | None) -> ThemeDefinition:
    """Return a known theme, falling back to the durable default."""
    return THEME_REGISTRY.get(theme_key or "", THEME_REGISTRY["signal-cabin"])


def build_theme_palette(theme_key: str = "signal-cabin") -> QPalette:
    """Build one accessible palette from the semantic desktop registry."""
    colors = resolve_theme(theme_key).colors
    palette = QPalette()
    for group in _COLOR_GROUPS:
        palette.setColor(group, QPalette.ColorRole.Window, QColor(colors["ground"]))
        palette.setColor(group, QPalette.ColorRole.WindowText, QColor(colors["text"]))
        palette.setColor(group, QPalette.ColorRole.Base, QColor(colors["panel_deep"]))
        palette.setColor(group, QPalette.ColorRole.AlternateBase, QColor(colors["panel"]))
        palette.setColor(group, QPalette.ColorRole.Text, QColor(colors["text"]))
        palette.setColor(group, QPalette.ColorRole.Button, QColor(colors["panel_lift"]))
        palette.setColor(group, QPalette.ColorRole.ButtonText, QColor(colors["text"]))
        palette.setColor(group, QPalette.ColorRole.PlaceholderText, QColor(colors["text_muted"]))
        palette.setColor(group, QPalette.ColorRole.Highlight, QColor(colors["primary"]))
        palette.setColor(group, QPalette.ColorRole.HighlightedText, QColor(colors["on_primary"]))
        palette.setColor(group, QPalette.ColorRole.ToolTipBase, QColor(colors["panel_lift"]))
        palette.setColor(group, QPalette.ColorRole.ToolTipText, QColor(colors["text"]))
        palette.setColor(group, QPalette.ColorRole.Link, QColor(colors["primary"]))
        palette.setColor(group, QPalette.ColorRole.LinkVisited, QColor(colors["active"]))
    return repair_palette(palette)


def build_signal_cabin_palette() -> QPalette:
    """Retain the original public helper for compatibility."""
    return build_theme_palette("signal-cabin")


def desktop_stylesheet(theme_key: str = "signal-cabin") -> str:
    """Return Zara Desktop's complete stylesheet for one theme."""
    colors = resolve_theme(theme_key).colors
    return f"""
QWidget {{
    background: {colors["ground"]};
    color: {colors["text"]};
    font-family: "Adwaita Sans";
    font-size: 14px;
    selection-background-color: {colors["primary"]};
    selection-color: {colors["on_primary"]};
}}

QWidget#zaraQuickCopilot,
QWidget#zaraFullChat,
QWidget#zaraSettings,
QWidget#zaraStatusWindow {{
    background: {colors["ground"]};
}}

QLabel {{ background: transparent; }}

QFrame#zaraQuickHeader,
QFrame#zaraConversationHeader {{
    background: transparent;
    border: none;
    border-bottom: 1px solid {colors["line"]};
}}

QLabel#zaraBrandName {{
    color: {colors["text"]};
    font-family: "Nimbus Sans Narrow";
    font-size: 19px;
    font-weight: 700;
    letter-spacing: 3px;
}}

QLabel#zaraSurfaceName,
QLabel#zaraProviderStatus,
QLabel#zaraQuickProvider,
QLabel#zaraRuntimeDetail,
QLabel#zaraQuickRuntimeDetail {{
    color: {colors["text_muted"]};
}}

QLabel#zaraConversationTitle,
QLabel#zaraQuickTitle,
QLabel#zaraTitle {{
    color: {colors["text"]};
    font-family: "Nimbus Sans Narrow";
    font-size: 23px;
    font-weight: 700;
}}

QFrame#zaraRuntimeRail {{
    background: {colors["panel"]};
    border: 1px solid {colors["line"]};
    border-radius: 10px;
}}

QFrame#zaraStatusLamp {{
    background: {colors["primary"]};
    border: none;
    border-radius: 4px;
}}

QFrame#zaraStatusLamp[runtimeState="starting"],
QFrame#zaraStatusLamp[runtimeState="thinking"],
QFrame#zaraStatusLamp[runtimeState="tool-running"] {{ background: {colors["active"]}; }}
QFrame#zaraStatusLamp[runtimeState="error"],
QFrame#zaraStatusLamp[runtimeState="disconnected"] {{ background: {colors["danger"]}; }}

QLabel#zaraRuntimeStatus,
QLabel#zaraQuickRuntimeStatus {{
    color: {colors["primary"]};
    font-family: "Hack Nerd Font Mono";
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 1px;
}}

QLabel#zaraRuntimeStatus[runtimeState="starting"],
QLabel#zaraQuickRuntimeStatus[runtimeState="starting"] {{ color: {colors["active"]}; }}
QLabel#zaraRuntimeStatus[runtimeState="error"],
QLabel#zaraQuickRuntimeStatus[runtimeState="error"] {{ color: {colors["danger"]}; }}

QLabel#zaraCommandError,
QLabel#zaraQuickCommandError,
QLabel#zaraMessageError {{
    background: {colors["danger_deep"]};
    color: {colors["text"]};
    border: 1px solid {colors["danger"]};
    border-radius: 9px;
    padding: 9px 11px;
}}

QWidget#zaraConversationSidebar {{
    background: {colors["panel_deep"]};
    border: none;
    border-right: 1px solid {colors["line"]};
}}

QLineEdit,
QPlainTextEdit,
QTextBrowser,
QListWidget {{
    color: {colors["text"]};
    background: {colors["panel_deep"]};
    border: 1px solid {colors["line"]};
    border-radius: 10px;
    padding: 9px 11px;
}}

QLineEdit:focus,
QPlainTextEdit:focus,
QTextBrowser:focus,
QListWidget:focus {{
    border-color: {colors["primary"]};
}}

QListWidget#zaraConversationHistory {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 4px 0;
    outline: none;
}}

QListWidget#zaraConversationHistory::item {{
    color: {colors["text_muted"]};
    border-radius: 9px;
    margin: 2px 0;
    padding: 10px 12px;
}}

QListWidget#zaraConversationHistory::item:hover {{
    color: {colors["text"]};
    background: {colors["panel"]};
}}

QListWidget#zaraConversationHistory::item:selected {{
    color: {colors["text"]};
    background: {colors["primary_deep"]};
}}

QListWidget#zaraSettingsCategories,
QListWidget#zaraFactList {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 4px 0;
    outline: none;
}}

QListWidget#zaraSettingsCategories::item,
QListWidget#zaraFactList::item {{
    color: {colors["text_muted"]};
    border-radius: 9px;
    margin: 2px 0;
    padding: 10px 12px;
}}

QListWidget#zaraSettingsCategories::item:hover,
QListWidget#zaraFactList::item:hover {{
    color: {colors["text"]};
    background: {colors["panel"]};
}}

QListWidget#zaraSettingsCategories::item:selected,
QListWidget#zaraFactList::item:selected {{
    color: {colors["text"]};
    background: {colors["primary_deep"]};
}}

QScrollArea#zaraConversationViewport,
QScrollArea#zaraConversationViewport > QWidget > QWidget {{
    background: transparent;
    border: none;
}}

QWidget#zaraMessageContainer {{
    background: transparent;
    border: none;
    border-left: 1px solid {colors["line_strong"]};
}}

QFrame#zaraMessage {{
    background: transparent;
    border: none;
    border-top: 1px solid {colors["line"]};
    border-radius: 0;
}}

QFrame#zaraMessage[messageRole="user"] {{
    background: transparent;
    border-top-color: {colors["primary"]};
}}

QFrame#zaraMessage[messageRole="system"],
QFrame#zaraMessage[messageRole="tool"] {{
    background: transparent;
}}

QLabel#zaraMessageRole,
QLabel#zaraMessageStatus {{
    color: {colors["text_muted"]};
    font-family: "Hack Nerd Font Mono";
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 1px;
}}

QLabel#zaraMessageStatus[messageStatus="streaming"],
QLabel#zaraMessageStatus[messageStatus="pending"] {{ color: {colors["active"]}; }}
QLabel#zaraMessageStatus[messageStatus="error"] {{ color: {colors["danger"]}; }}
QLabel#zaraMessageStatus[messageStatus="complete"] {{ color: {colors["primary"]}; }}

QFrame#zaraMessage[messageRole="user"] QLabel#zaraMessageRole {{ color: {colors["primary"]}; }}

QTextBrowser#zaraMessageBody,
QTextBrowser#zaraMessageBody QWidget {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 0;
}}

QFrame#zaraCodeBlock {{
    background: {colors["panel_deep"]};
    border: 1px solid {colors["line_strong"]};
    border-radius: 10px;
}}

QPlainTextEdit#zaraCodeEditor {{
    background: {colors["ground"]};
    border: none;
    border-radius: 7px;
    color: {colors["text"]};
    font-family: "Hack Nerd Font Mono";
}}

QFrame#zaraComposerShell {{
    background: {colors["panel_lift"]};
    border: 1px solid {colors["line_strong"]};
    border-radius: 15px;
}}

QFrame#zaraComposerShell QPlainTextEdit {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 7px 8px;
}}

QPushButton {{
    min-height: 34px;
    padding: 0 13px;
    color: {colors["text"]};
    background: {colors["panel_lift"]};
    border: 1px solid {colors["line_strong"]};
    border-radius: 9px;
    font-weight: 600;
}}

QPushButton:hover {{ background: {colors["line"]}; }}
QPushButton:focus {{ border-color: {colors["primary"]}; }}
QPushButton:disabled {{
    color: {colors["text_muted"]};
    background: {colors["panel"]};
    border-color: {colors["line"]};
}}

QPushButton#zaraThemePreview {{
    min-height: 72px;
    max-height: 72px;
    padding: 0;
}}

QPushButton#zaraPrimaryAction {{
    color: {colors["on_primary"]};
    background: {colors["primary"]};
    border-color: {colors["primary"]};
}}

QPushButton#zaraPrimaryAction:hover {{
    background: {colors["primary_hover"]};
    border-color: {colors["primary_hover"]};
}}

QPushButton#zaraPrimaryAction:disabled {{
    color: {colors["text_muted"]};
    background: {colors["panel"]};
    border-color: {colors["line"]};
}}

QPushButton#zaraDangerAction {{
    color: {colors["danger"]};
    background: transparent;
    border-color: {colors["danger"]};
}}

QPushButton#zaraDangerAction:hover {{
    color: {colors["text"]};
    background: {colors["danger_deep"]};
}}

QPushButton#zaraDangerAction:disabled {{
    color: {colors["text_muted"]};
    background: transparent;
    border-color: {colors["line"]};
}}

QPushButton#zaraComposerAction {{
    min-width: 38px;
    max-width: 38px;
    min-height: 38px;
    max-height: 38px;
    padding: 0;
    color: {colors["on_primary"]};
    background: {colors["primary"]};
    border: 1px solid {colors["primary"]};
    border-radius: 12px;
}}

QPushButton#zaraComposerAction:hover {{
    background: {colors["primary_hover"]};
    border-color: {colors["primary_hover"]};
}}

QPushButton#zaraComposerAction:disabled {{
    background: {colors["panel"]};
    border-color: {colors["line"]};
}}

QPushButton#zaraComposerAction[actionMode="stop"] {{
    color: {colors["text"]};
    background: {colors["danger"]};
    border-color: {colors["danger"]};
}}

QPushButton#zaraComposerAction[actionMode="stop"]:hover {{
    background: {colors["danger_deep"]};
}}

QWidget#zaraSettingsRail,
QWidget#zaraKnowledgeStudioRail {{
    background: {colors["panel_deep"]};
    border-right: 1px solid {colors["line"]};
}}

QFrame#zaraSettingsHeader,
QFrame#zaraSettingsFooter {{
    background: transparent;
    border: none;
    border-bottom: 1px solid {colors["line"]};
}}

QPlainTextEdit#zaraPrologEditor,
QPlainTextEdit#zaraConfigEditor {{
    font-family: "Hack Nerd Font Mono";
    font-size: 13px;
}}

QFrame#zaraSettingsFooter {{
    border-top: 1px solid {colors["line"]};
    border-bottom: none;
}}

QLabel#zaraSectionTitle {{
    color: {colors["text"]};
    font-size: 21px;
    font-weight: 700;
}}

QLabel#zaraSectionDescription,
QLabel#zaraSettingsHint {{ color: {colors["text_muted"]}; }}

QComboBox,
QSpinBox,
QDoubleSpinBox {{
    color: {colors["text"]};
    background: {colors["panel_deep"]};
    border: 1px solid {colors["line"]};
    border-radius: 9px;
    min-height: 34px;
    padding: 0 10px;
}}

QComboBox:focus,
QSpinBox:focus,
QDoubleSpinBox:focus {{ border-color: {colors["primary"]}; }}

QComboBox QAbstractItemView {{
    color: {colors["text"]};
    background: {colors["panel_lift"]};
    selection-color: {colors["on_primary"]};
    selection-background-color: {colors["primary"]};
    border: 1px solid {colors["line_strong"]};
}}

QCheckBox {{ spacing: 9px; background: transparent; }}
QCheckBox::indicator {{
    width: 17px;
    height: 17px;
    border: 1px solid {colors["line_strong"]};
    border-radius: 5px;
    background: {colors["panel_deep"]};
}}
QCheckBox::indicator:checked {{
    background: {colors["primary"]};
    border-color: {colors["primary"]};
}}

QSplitter::handle {{ background: {colors["line"]}; width: 1px; }}

QScrollBar:vertical {{
    background: transparent;
    width: 10px;
    margin: 4px 2px;
}}

QScrollBar::handle:vertical {{
    background: {colors["line_strong"]};
    border-radius: 4px;
    min-height: 28px;
}}

QScrollBar::handle:vertical:hover {{ background: {colors["text_muted"]}; }}
QScrollBar::add-line:vertical,
QScrollBar::sub-line:vertical,
QScrollBar::add-page:vertical,
QScrollBar::sub-page:vertical {{ height: 0; background: transparent; }}

QToolTip {{
    color: {colors["text"]};
    background: {colors["panel_lift"]};
    border: 1px solid {colors["line_strong"]};
    padding: 6px 8px;
}}
""".strip()


def apply_desktop_theme(app: QApplication, theme_key: str = "signal-cabin") -> QPalette:
    """Install the complete visual system before desktop widgets are built."""
    app.setStyle("Fusion")
    theme = resolve_theme(theme_key)
    palette = build_theme_palette(theme.key)
    app.setPalette(palette)
    app.setStyleSheet(desktop_stylesheet(theme.key))
    app.setProperty("zaraTheme", theme.key)
    return palette


def refresh_dynamic_style(widget) -> None:
    """Re-evaluate stylesheet selectors after a dynamic property changes."""
    style = widget.style()
    style.unpolish(widget)
    style.polish(widget)
    widget.update()


__all__ = [
    "MIN_TEXT_CONTRAST",
    "SIGNAL_CABIN_COLORS",
    "THEME_REGISTRY",
    "ThemeDefinition",
    "apply_desktop_theme",
    "apply_readable_palette",
    "build_theme_palette",
    "build_signal_cabin_palette",
    "contrast_ratio",
    "desktop_stylesheet",
    "refresh_dynamic_style",
    "relative_luminance",
    "repair_palette",
    "resolve_theme",
]
