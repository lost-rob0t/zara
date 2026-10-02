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

from PySide6.QtGui import QColor, QPalette
from PySide6.QtWidgets import QApplication

from zara.themes import (
    SEMANTIC_COLOR_KEYS,
    THEME_REGISTRY,
    ThemeDefinition,
    configure_theme_registry,
    load_theme_packages,
    register_theme,
    resolve_theme,
    theme_css_variables,
    theme_from_mapping,
    unregister_theme,
)

MIN_TEXT_CONTRAST = 4.5


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
QWidget#zaraCopilot,
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
    border: none;
    border-radius: 12px;
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
}}

QWidget#zaraConversationHistoryPanel {{
    background: {colors["panel_deep"]};
    border: none;
    border-radius: 16px;
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
}}

QFrame#zaraMessage {{
    background: transparent;
    border: none;
}}

QFrame#zaraMessageBubble {{
    background: {colors["panel"]};
    border: none;
    border-radius: 16px;
}}

QFrame#zaraMessageBubble[messageRole="user"] {{
    background: {colors["primary_deep"]};
    border-bottom-right-radius: 6px;
}}

QFrame#zaraMessageBubble[messageRole="assistant"] {{
    background: {colors["panel_lift"]};
    border-bottom-left-radius: 6px;
}}

QFrame#zaraMessageBubble[messageRole="system"] {{
    background: {colors["danger_deep"]};
}}

QFrame#zaraMessageBubble[messageKind="activity"] {{
    background: {colors["panel_deep"]};
    border: 1px solid {colors["line"]};
    border-radius: 12px;
}}

QLabel#zaraMessageRole,
QLabel#zaraMessageStatus {{
    color: {colors["text_muted"]};
    font-family: "Hack Nerd Font Mono";
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.5px;
}}

QLabel#zaraMessageStatus[messageStatus="streaming"],
QLabel#zaraMessageStatus[messageStatus="pending"] {{ color: {colors["active"]}; }}
QLabel#zaraMessageStatus[messageStatus="error"] {{ color: {colors["danger"]}; }}
QLabel#zaraMessageStatus[messageStatus="complete"] {{ color: {colors["primary"]}; }}

QFrame#zaraMessageBubble[messageRole="user"] QLabel#zaraMessageRole {{ color: {colors["primary"]}; }}

QTextBrowser#zaraMessageBody,
QTextBrowser#zaraMessageBody QWidget {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 0;
}}

QWidget#zaraMessageContent,
QWidget#zaraMessageContent QWidget {{
    background: transparent;
    border: none;
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
    border: 1px solid {colors["line"]};
    border-radius: 20px;
}}

QFrame#zaraComposerShell:focus-within {{ border-color: {colors["primary"]}; }}

QFrame#zaraComposerShell QPlainTextEdit {{
    background: transparent;
    border: none;
    border-radius: 0;
    padding: 7px 8px;
}}

QFrame#zaraConversationEmptyState {{
    background: transparent;
    border: none;
}}

QLabel#zaraEmptyStateTitle {{
    color: {colors["text"]};
    font-family: "Nimbus Sans Narrow";
    font-size: 26px;
    font-weight: 700;
}}

QLabel#zaraEmptyStateDetail {{ color: {colors["text_muted"]}; }}

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
    "SEMANTIC_COLOR_KEYS",
    "ThemeDefinition",
    "apply_desktop_theme",
    "apply_readable_palette",
    "build_theme_palette",
    "build_signal_cabin_palette",
    "configure_theme_registry",
    "contrast_ratio",
    "desktop_stylesheet",
    "load_theme_packages",
    "refresh_dynamic_style",
    "register_theme",
    "relative_luminance",
    "repair_palette",
    "resolve_theme",
    "theme_css_variables",
    "theme_from_mapping",
    "unregister_theme",
]
