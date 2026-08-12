"""Multi-monitor aware window geometry helpers.

Used by the Qt overlay to restore a saved pet position without ever
landing it completely off-screen. If the saved monitor disappears, the
pet falls back to the primary screen's default position.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Sequence, Tuple


@dataclass(frozen=True)
class ScreenRect:
    """A monitor's available geometry in virtual desktop coordinates."""

    x: int
    y: int
    width: int
    height: int

    def contains(self, px: int, py: int, margin: int = 0) -> bool:
        return (
            px + margin >= self.x
            and py + margin >= self.y
            and px <= self.x + self.width - margin
            and py <= self.y + self.height - margin
        )


def recover_position(
    saved_x: Optional[int],
    saved_y: Optional[int],
    screens: Sequence[ScreenRect],
    default: Tuple[int, int] = (100, 100),
    pet_width: int = 192,
    pet_height: int = 208,
    margin: int = 8,
) -> Tuple[int, int]:
    """Return a sane on-screen position for the pet.

    If the saved position is visible on any current screen, keep it. If
    the saved monitor disappeared, fall back to ``default`` clamped to
    the first available screen. When no screens are reported, return
    ``default`` unchanged.
    """
    if saved_x is None or saved_y is None:
        return _clamp(default, screens, pet_width, pet_height, margin)
    for screen in screens:
        if screen.contains(saved_x, saved_y, margin=margin):
            return (saved_x, saved_y)
    return _clamp(default, screens, pet_width, pet_height, margin)


def _clamp(
    point: Tuple[int, int],
    screens: Sequence[ScreenRect],
    pet_width: int,
    pet_height: int,
    margin: int,
) -> Tuple[int, int]:
    if not screens:
        return point
    screen = screens[0]
    x, y = point
    x = max(screen.x + margin, min(x, screen.x + screen.width - pet_width - margin))
    y = max(screen.y + margin, min(y, screen.y + screen.height - pet_height - margin))
    return (x, y)