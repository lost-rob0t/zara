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

MIN_TEXT_CONTRAST = 4.5

SIGNAL_CABIN_COLORS = {
    "ground": "#0A1012",
    "panel_deep": "#0D1518",
    "panel": "#111A1E",
    "panel_lift": "#172226",
    "line": "#2A393E",
    "line_strong": "#3C5358",
    "text": "#F2E9D8",
    "text_muted": "#A8B7B3",
    "ready": "#61D095",
    "ready_hover": "#7ADDA8",
    "ready_deep": "#17382B",
    "active": "#E7B84B",
    "danger": "#E6544D",
    "danger_deep": "#562727",
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


def build_signal_cabin_palette() -> QPalette:
    """Build the fixed accessible palette shared by every desktop surface."""
    colors = SIGNAL_CABIN_COLORS
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
        palette.setColor(group, QPalette.ColorRole.Highlight, QColor(colors["ready"]))
        palette.setColor(group, QPalette.ColorRole.HighlightedText, QColor(colors["ground"]))
        palette.setColor(group, QPalette.ColorRole.ToolTipBase, QColor(colors["panel_lift"]))
        palette.setColor(group, QPalette.ColorRole.ToolTipText, QColor(colors["text"]))
        palette.setColor(group, QPalette.ColorRole.Link, QColor(colors["ready"]))
        palette.setColor(group, QPalette.ColorRole.LinkVisited, QColor(colors["active"]))
    return repair_palette(palette)


def desktop_stylesheet() -> str:
    """Return Zara Desktop's complete Signal Cabin Qt stylesheet."""
    colors = SIGNAL_CABIN_COLORS
    return f"""
QWidget {{
    background: {colors["ground"]};
    color: {colors["text"]};
    font-family: "Adwaita Sans";
    font-size: 14px;
    selection-background-color: {colors["ready"]};
    selection-color: {colors["ground"]};
}}

QWidget#zaraQuickCopilot,
QWidget#zaraFullChat,
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
    background: {colors["ready"]};
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
    color: {colors["ready"]};
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
    border-color: {colors["ready"]};
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
    background: {colors["ready_deep"]};
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
    border-top-color: {colors["ready"]};
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
QLabel#zaraMessageStatus[messageStatus="complete"] {{ color: {colors["ready"]}; }}

QFrame#zaraMessage[messageRole="user"] QLabel#zaraMessageRole {{ color: {colors["ready"]}; }}

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
QPushButton:focus {{ border-color: {colors["ready"]}; }}
QPushButton:disabled {{
    color: {colors["text_muted"]};
    background: {colors["panel"]};
    border-color: {colors["line"]};
}}

QPushButton#zaraPrimaryAction {{
    color: {colors["ground"]};
    background: {colors["ready"]};
    border-color: {colors["ready"]};
}}

QPushButton#zaraPrimaryAction:hover {{
    background: {colors["ready_hover"]};
    border-color: {colors["ready_hover"]};
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


def apply_desktop_theme(app: QApplication) -> QPalette:
    """Install the complete visual system before desktop widgets are built."""
    app.setStyle("Fusion")
    palette = build_signal_cabin_palette()
    app.setPalette(palette)
    app.setStyleSheet(desktop_stylesheet())
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
    "apply_desktop_theme",
    "apply_readable_palette",
    "build_signal_cabin_palette",
    "contrast_ratio",
    "desktop_stylesheet",
    "refresh_dynamic_style",
    "relative_luminance",
    "repair_palette",
]
