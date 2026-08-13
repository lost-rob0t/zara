"""Readable system-palette handling for Zara Desktop."""

from __future__ import annotations

from collections.abc import Iterable

from PySide6.QtGui import QColor, QPalette
from PySide6.QtWidgets import QApplication

MIN_TEXT_CONTRAST = 4.5

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


__all__ = [
    "MIN_TEXT_CONTRAST",
    "apply_readable_palette",
    "contrast_ratio",
    "relative_luminance",
    "repair_palette",
]
