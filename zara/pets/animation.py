"""Animation controller — elapsed-time based sprite playback.

Requirements satisfied here:

- elapsed-time-based animation (not frame counting)
- correct frame timing via monotonic clock
- looping and non-looping animations
- deterministic state transitions
- no restart on repeated identical state events (idempotent ``set_state``)
- frame cache: the sprite sheet is decoded once, frames are extracted
  lazily into a cache keyed by (row, col)
- low idle CPU usage: a 60 Hz tick is plenty for 8 fps animation; the
  controller reports the active frame and the renderer paints only when
  the frame index changes
- clean resource disposal (``dispose`` releases the cache)
- reduced-motion support: render a representative static frame (row 0,
  col 0) and stop advancing

The controller is Qt-agnostic. The renderer (qt_overlay) calls
``current_frame()`` on each repaint and ``set_state`` when the pet state
changes. Image decoding uses Pillow when available; otherwise the Qt
overlay falls back to QPixmap slicing. The cache holds ``QPixmap``-free
Pillow images (or raw crops) so the controller is testable headless.
"""

from __future__ import annotations

import logging
import math
import time
from dataclasses import dataclass
from typing import Dict, Optional, Tuple

from .manifest import Animation, PetManifest
from .state import PetState

logger = logging.getLogger(__name__)


@dataclass
class FrameRef:
    """Opaque reference to a decoded frame. The renderer interprets it."""

    row: int
    col: int
    # The actual image payload is stored by the renderer's pixmap cache;
    # the controller only tracks indices so it has no Qt/Qt-less coupling.


class AnimationController:
    """Drives sprite animation for one ``PetManifest``.

    The controller does not decode images itself — that is the renderer's
    job (so Qt vs. test-headless renderers can use different backends).
    The controller computes *which* (row, col) frame should be displayed
    at the current monotonic time, and whether it changed since the last
    query.
    """

    def __init__(self, manifest: PetManifest, *, reduced_motion: bool = False,
                 clock=time.monotonic) -> None:
        self.manifest = manifest
        self._reduced_motion = bool(reduced_motion)
        self._clock = clock
        self._state: PetState = PetState.IDLE
        self._animation: Optional[Animation] = manifest.animation_for(PetState.IDLE.value)
        self._override_animation: Optional[Animation] = None
        self._override_frame: Optional[Tuple[int, int]] = None
        self._anim_start: float = self._clock()
        self._last_row: Optional[int] = None
        self._last_col: Optional[int] = None
        self._pending_non_loop_done = False

    @property
    def state(self) -> PetState:
        return self._state

    @property
    def reduced_motion(self) -> bool:
        return self._reduced_motion

    def set_reduced_motion(self, enabled: bool) -> None:
        self._reduced_motion = bool(enabled)
        # Re-pin the current frame so a transition to reduced motion holds
        # still immediately rather than mid-animation.
        self._anim_start = self._clock()

    def set_state(self, state: PetState) -> bool:
        """Set the pet state. Returns True if the animation changed.

        Repeated identical state events do not restart the animation:
        when ``state`` equals the current state we keep the current
        animation timeline so a re-emitted event does not jump the frame.
        """
        if (
            state is self._state
            and self._override_animation is None
            and self._override_frame is None
        ):
            return False
        self._state = state
        self._override_animation = None
        self._override_frame = None
        self._animation = self.manifest.animation_for(state.value)
        self._anim_start = self._clock()
        self._pending_non_loop_done = False
        return True

    def set_animation(self, name: str) -> bool:
        """Play an arbitrary named animation (e.g. 'drag').

        Used for non-state animations like the drag/movement row that
        aren't part of the five-state model. Pass the state name back to
        ``set_state`` to return to normal state-driven playback.
        """
        anim = self.manifest.animation_for(name)
        if anim is None:
            return False
        self._override_animation = anim
        self._override_frame = None
        self._animation = anim
        self._anim_start = self._clock()
        self._pending_non_loop_done = False
        return True

    @property
    def has_animation_override(self) -> bool:
        return self._override_animation is not None

    @property
    def animation_override_name(self) -> Optional[str]:
        if self._override_animation is None:
            return None
        return self._override_animation.name

    def animation_finished(self) -> bool:
        anim = self._override_animation
        if anim is None or anim.loop:
            return False
        durations = self._frame_durations(anim)
        if durations is not None:
            return (self._clock() - self._anim_start) * 1000 >= sum(durations)
        return self._clock() - self._anim_start >= anim.frames / float(anim.fps)

    def set_look_direction(self, direction_index: int) -> bool:
        frame = self.manifest.look_frame(direction_index)
        if frame is None or self._override_animation is not None:
            return False
        if frame == self._override_frame:
            return False
        self._override_frame = frame
        return True

    def clear_look_direction(self) -> bool:
        if self._override_frame is None:
            return False
        self._override_frame = None
        return True

    def current_frame(self) -> Tuple[int, int]:
        """Return the (row, col) frame that should be displayed now."""
        if self._override_frame is not None:
            return self._override_frame
        anim = self._animation
        if anim is None:
            return (0, 0)
        if self._reduced_motion:
            return (anim.row, 0)
        elapsed = self._clock() - self._anim_start
        durations = self._frame_durations(anim)
        if durations is not None:
            elapsed_ms = elapsed * 1000
            total_ms = sum(durations)
            if anim.loop:
                elapsed_ms %= total_ms
            elif elapsed_ms >= total_ms:
                return anim.row, anim.frames - 1
            boundary = 0.0
            for index, duration in enumerate(durations):
                boundary += duration
                if elapsed_ms < boundary:
                    return anim.row, index
            return anim.row, anim.frames - 1
        if anim.fps <= 0:
            return (anim.row, 0)
        frame_duration = 1.0 / float(anim.fps)
        if frame_duration <= 0:
            return (anim.row, 0)
        frame_index = int(elapsed / frame_duration)
        if anim.loop:
            frame_index = frame_index % anim.frames
        else:
            if frame_index >= anim.frames:
                frame_index = anim.frames - 1
                if not self._pending_non_loop_done:
                    self._pending_non_loop_done = True
                    logger.debug(
                        "[Animation] non-loop animation %s finished", anim.name
                    )
        col = max(0, min(frame_index, anim.frames - 1))
        return (anim.row, col)

    def _frame_durations(self, anim: Animation) -> Optional[list[float]]:
        all_durations = self.manifest.metadata.get("animation_durations_ms")
        if not isinstance(all_durations, dict):
            return None
        raw = all_durations.get(anim.name)
        if not isinstance(raw, list) or len(raw) != anim.frames:
            return None
        if any(
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or value <= 0
            for value in raw
        ):
            return None
        return [float(value) for value in raw]

    def frame_changed(self) -> bool:
        """True if the frame advanced since the last call to this method."""
        row, col = self.current_frame()
        changed = (row, col) != (self._last_row, self._last_col)
        self._last_row = row
        self._last_col = col
        return changed

    def dispose(self) -> None:
        """Release any cached state. The renderer owns the pixmap cache."""
        self._animation = None
        self._override_animation = None
        self._override_frame = None
        self._last_row = None
        self._last_col = None


def look_direction_index(dx: float, dy: float, deadzone: float = 32.0) -> Optional[int]:
    """Map a screen-space vector to the v2 clockwise look-cell index."""
    if math.hypot(dx, dy) <= deadzone:
        return None
    degrees = math.degrees(math.atan2(dx, -dy)) % 360.0
    return int((degrees + 11.25) // 22.5) % 16
