"""Qt/PySide6 pet overlay window — lazy imported so headless tests need no Qt.

The overlay is a frameless, transparent, always-on-top window that paints
the current animation frame. Dragging uses a movement threshold so clicks
and right-clicks are distinguished from drags. Transparent regions allow
click-through where the platform supports it; where it does not, the
window is still interactive on visible pet pixels (documented caveat).

PySide6 is imported lazily inside ``run_overlay`` so importing this
module in a headless environment does not pull in Qt.
"""

from __future__ import annotations

import logging
import os
import time
from pathlib import Path
from typing import Callable, Optional

from . import runtime_bridge
from .animation import AnimationController
from .geometry import ScreenRect, recover_position
from .manifest import PetManifest
from .settings import PetSettings
from .state import PetState
from .storage import list_pets, load_pet, sprite_path_for

logger = logging.getLogger(__name__)

DRAG_THRESHOLD = 5  # pixels


def _screen_rects(qt_screens) -> list[ScreenRect]:
    rects: list[ScreenRect] = []
    for screen in qt_screens:
        geo = screen.availableGeometry()
        rects.append(ScreenRect(geo.x(), geo.y(), geo.width(), geo.height()))
    return rects


def _reduced_motion_enabled(preference: str) -> bool:
    """Resolve the reduced-motion preference against the OS setting."""
    if preference == "on":
        return True
    if preference == "off":
        return False
    # "system": check the environment. Linux exposes this via gsettings or
    # GTK_TOOLKIT_PORTAL; we check a couple of well-known env heuristics.
    if os.getenv("GTK_DISABLE animations") == "1":
        return True
    try:
        import subprocess
        result = subprocess.run(
            ["gsettings", "get", "org.gnome.desktop.interface", "enable-animations"],
            capture_output=True, text=True, timeout=2,
        )
        if result.returncode == 0 and "false" in result.stdout.lower():
            return True
    except (OSError, FileNotFoundError, subprocess.TimeoutException):
        pass
    return False


def run_overlay(
    settings: PetSettings,
    pet_manifest: PetManifest,
    on_state_change: Optional[Callable[[PetState, list[str]], None]] = None,
    on_request_focus: Optional[Callable[[], None]] = None,
) -> int:
    """Launch the Qt pet overlay. Returns the Qt exit code.

    This function is the only place PySide6 is imported. Callers in
    headless contexts (tests, CLI without ``--pets``) never reach it.
    """
    from PySide6.QtCore import Qt, QTimer, QPoint, QEvent
    from PySide6.QtGui import QPixmap, QPainter, QImage
    from PySide6.QtWidgets import QApplication, QWidget, QMenu

    app = QApplication.instance() or QApplication([])
    app.setQuitOnLastWindowClosed(False)

    screens = _screen_rects(app.screens())
    saved_x = settings.state.x if settings.state.x is not None else 100
    saved_y = settings.state.y if settings.state.y is not None else 100
    x, y = recover_position(saved_x, saved_y, screens, default=(100, 100))
    scale = float(settings.state.scale or 1.0)
    reduced = _reduced_motion_enabled(settings.state.reduced_motion)

    sprite_path = sprite_path_for(pet_manifest)
    sprite_image = QImage(str(sprite_path))
    if sprite_image.isNull():
        logger.error("[PetOverlay] cannot load sprite %s", sprite_path)
        return 1

    cell_w = pet_manifest.frame_geometry.width
    cell_h = pet_manifest.frame_geometry.height

    # Frame pixmap cache keyed by (row, col) so we never re-extract a cell.
    cache: dict[tuple[int, int], QPixmap] = {}

    def _frame_pixmap(row: int, col: int) -> QPixmap:
        key = (row, col)
        if key not in cache:
            cropped = sprite_image.copy(col * cell_w, row * cell_h, cell_w, cell_h)
            cache[key] = QPixmap.fromImage(cropped)
        return cache[key]

    controller = AnimationController(pet_manifest, reduced_motion=reduced)

    class PetWindow(QWidget):
        def __init__(self) -> None:
            super().__init__()
            self.setWindowFlags(
                Qt.FramelessWindowHint
                | Qt.WindowStaysOnTopHint
                | Qt.Tool
            )
            self.setAttribute(Qt.WA_TranslucentBackground, True)
            self.setAttribute(Qt.WA_ShowWithoutActivating, True)
            self.setFocusPolicy(Qt.NoFocus)
            self.setFixedSize(int(cell_w * scale), int(cell_h * scale))
            self.move(x, y)
            self._drag_offset: Optional[QPoint] = None
            self._drag_origin: Optional[QPoint] = None
            self._moved = False
            self._current_pixmap: Optional[QPixmap] = None
            self._update_frame()

        def _update_frame(self) -> None:
            row, col = controller.current_frame()
            self._current_pixmap = _frame_pixmap(row, col)
            self.update()

        def paintEvent(self, event) -> None:
            painter = QPainter(self)
            painter.setRenderHint(QPainter.SmoothPixmapTransform, True)
            if self._current_pixmap is not None:
                painter.drawPixmap(self.rect(), self._current_pixmap)

        def mousePressEvent(self, event) -> None:
            if event.button() == Qt.LeftButton:
                self._drag_offset = event.globalPos() - self.frameGeometry().topLeft()
                self._drag_origin = event.globalPos()
                self._moved = False
            elif event.button() == Qt.RightButton:
                self._show_context_menu(event.globalPos())

        def mouseMoveEvent(self, event) -> None:
            if self._drag_offset is not None and (event.buttons() & Qt.LeftButton):
                if (event.globalPos() - self._drag_origin).manhattanLength() > DRAG_THRESHOLD:
                    self._moved = True
                    self.move(event.globalPos() - self._drag_offset)

        def mouseReleaseEvent(self, event) -> None:
            if event.button() == Qt.LeftButton:
                if self._moved:
                    # Persist the new position.
                    settings.update(x=self.x(), y=self.y())
                    settings.save()
                else:
                    # Treat as a click — focus/open Zarathura.
                    if on_request_focus is not None:
                        on_request_focus()
                self._drag_offset = None
                self._moved = False

        def _show_context_menu(self, pos) -> None:
            menu = QMenu(self)
            open_action = menu.addAction("Open Zarathushtra")
            menu.addSeparator()
            tuck_action = menu.addAction("Tuck Away")
            action = menu.exec_(pos)
            if action is open_action and on_request_focus is not None:
                on_request_focus()
            elif action is tuck_action:
                self.hide()
                settings.update(enabled=False)
                settings.save()

        def _on_state(self, state: PetState, labels: list[str]) -> None:
            controller.set_state(state)
            self._update_frame()

    window = PetWindow()
    window.show()

    # Subscribe the window to pet state changes from the actor.
    from .actor import PetStateActor
    actor = PetStateActor.start(subscriber=window._on_state)
    runtime_bridge.register_actor(actor)

    # Animation tick: 60 Hz is plenty for 8 fps; the controller reports
    # whether the frame advanced so we only repaint when it changed.
    def _tick() -> None:
        if controller.frame_changed():
            window._update_frame()

    timer = QTimer()
    timer.timeout.connect(_tick)
    timer.start(int(1000 / 60))

    def _cleanup() -> None:
        timer.stop()
        runtime_bridge.unregister_actor()
        actor.stop()
        controller.dispose()
        cache.clear()

    app.aboutToQuit.connect(_cleanup)
    if on_state_change is not None:
        on_state_change(controller.state, [])
    return app.exec()