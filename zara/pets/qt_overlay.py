"""Qt/PySide6 pet overlay window + system tray.

The overlay is a frameless, transparent, always-on-top window that
paints the current animation frame. Dragging switches the pet to the
``running`` animation (the companion "runs" while you carry her) and
persists the new position on release. A system tray icon offers
Show/Hide, Open Zarathushtra, and Quit.

Cross-process events arrive over ZMQ PUB/SUB (see ``ipc.py``); the
subscriber polls from a Qt timer and forwards payloads to the
PetStateActor. In-process events go directly via the actor ref.

PySide6 is imported lazily inside ``run_overlay``.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Callable, Optional

from . import events, runtime_bridge
from .animation import AnimationController
from .geometry import ScreenRect, recover_position
from .ipc import PetSubscriber
from .manifest import PetManifest
from .settings import PetSettings
from .state import PetState
from .storage import sprite_path_for

logger = logging.getLogger(__name__)

DRAG_THRESHOLD = 5  # pixels


# ZMQ payload -> PetEvent instance. Mirrors zara.pets.events.
_PAYLOAD_MAP = {
    "ModelStarted": lambda p: events.ModelStarted(label=p.get("label")),
    "ModelStreaming": lambda p: events.ModelStreaming(label=p.get("label")),
    "ModelCompleted": lambda p: events.ModelCompleted(
        success=p.get("success", True), label=p.get("label")),
    "ModelFailed": lambda p: events.ModelFailed(
        reason=p.get("reason", ""), label=p.get("label")),
    "ToolStarted": lambda p: events.ToolStarted(label=p.get("label")),
    "ToolCompleted": lambda p: events.ToolCompleted(
        success=p.get("success", True), label=p.get("label")),
    "ToolFailed": lambda p: events.ToolFailed(
        reason=p.get("reason", ""), label=p.get("label")),
    "AgentStarted": lambda p: events.AgentStarted(label=p.get("label")),
    "AgentCompleted": lambda p: events.AgentCompleted(
        success=p.get("success", True), label=p.get("label")),
    "AgentFailed": lambda p: events.AgentFailed(
        reason=p.get("reason", ""), label=p.get("label")),
    "UserInputRequired": lambda p: events.UserInputRequired(
        kind=p.get("kind", "approval"), label=p.get("label")),
    "UserResponded": lambda p: events.UserResponded(label=p.get("label")),
    "OutputReady": lambda p: events.OutputReady(label=p.get("label")),
    "ResponseText": lambda p: events.ResponseText(
        text=p.get("text", ""), truncated=p.get("truncated", False),
        label=p.get("label")),
    "OutputSeen": lambda p: events.OutputSeen(label=p.get("label")),
    "TaskCancelled": lambda p: events.TaskCancelled(label=p.get("label")),
    "RuntimeIdle": lambda p: events.RuntimeIdle(label=p.get("label")),
    "ProviderUnavailable": lambda p: events.ProviderUnavailable(
        reason=p.get("reason", ""), label=p.get("label")),
}

# Human-readable emotion strings shown in the tray tooltip / balloon.
_EMOTION_LABELS = {
    PetState.IDLE: "resting",
    PetState.RUNNING: "working",
    PetState.NEEDS_INPUT: "needs your input",
    PetState.READY: "has something ready",
    PetState.BLOCKED: "is stuck",
}


def _screen_rects(qt_screens) -> list[ScreenRect]:
    rects: list[ScreenRect] = []
    for screen in qt_screens:
        geo = screen.availableGeometry()
        rects.append(ScreenRect(geo.x(), geo.y(), geo.width(), geo.height()))
    return rects


def _reduced_motion_enabled(preference: str) -> bool:
    if preference == "on":
        return True
    if preference == "off":
        return False
    import os
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
    on_request_focus: Optional[Callable[[], None]] = None,
) -> int:
    from PySide6.QtCore import Qt, QTimer, QPoint
    from PySide6.QtGui import QPixmap, QPainter, QImage, QIcon, QAction
    from PySide6.QtWidgets import QApplication, QWidget, QMenu, QSystemTrayIcon

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

    cache: dict[tuple[int, int], QPixmap] = {}

    def _frame_pixmap(row: int, col: int) -> QPixmap:
        key = (row, col)
        if key not in cache:
            cropped = sprite_image.copy(col * cell_w, row * cell_h, cell_w, cell_h)
            cache[key] = QPixmap.fromImage(cropped)
        return cache[key]

    controller = AnimationController(pet_manifest, reduced_motion=reduced)

    # Shared state so the tray and window can coordinate show/hide and the
    # drag handler can remember the pre-drag state.
    overlay_state = {
        "pre_drag_state": PetState.IDLE,
        "dragging": False,
        "last_emotion": PetState.IDLE,
    }

    class PetWindow(QWidget):
        def __init__(self) -> None:
            super().__init__()
            # Frameless + always-on-top. We do NOT use Qt.Tool because on
            # tiling WMs (qtile) it constrains the window to one screen
            # and prevents dragging across monitors. Instead we set the
            # X11 utility window type via the attribute, which makes the
            # WM show the window on all workspaces without the screen
            # constraint that Qt.Tool imposes.
            self.setWindowFlags(
                Qt.FramelessWindowHint
                | Qt.WindowStaysOnTopHint
            )
            self.setAttribute(Qt.WA_TranslucentBackground, True)
            self.setAttribute(Qt.WA_ShowWithoutActivating, True)
            self.setAttribute(Qt.WA_X11NetWmWindowTypeUtility, True)
            self.setFocusPolicy(Qt.StrongFocus)
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
            # Compositing: clear to transparent first so stale pixels from
            # the previous frame don't smear on compositors that don't
            # auto-clear translucent windows.
            painter.setCompositionMode(QPainter.CompositionMode_Source)
            painter.fillRect(self.rect(), Qt.transparent)
            painter.setCompositionMode(QPainter.CompositionMode_SourceOver)
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
                    if not overlay_state["dragging"]:
                        overlay_state["dragging"] = True
                        overlay_state["pre_drag_state"] = controller.state
                        self._apply_drag_animation(event.globalPos())
                        self._update_frame()
                    else:
                        new_pos = event.globalPos()
                        if new_pos.x() != self._drag_origin.x():
                            moving_right = new_pos.x() > self._drag_origin.x()
                            want = "drag" if moving_right else "drag-left"
                            current = controller._override_animation.name if controller._override_animation else None
                            if current != want and pet_manifest.animation_for(want) is not None:
                                self._apply_drag_animation(new_pos)
                                self._update_frame()
                        self._drag_origin = new_pos
                    self.move(event.globalPos() - self._drag_offset)

        def _apply_drag_animation(self, global_pos) -> None:
            moving_right = global_pos.x() > self._drag_origin.x()
            name = "drag" if moving_right else "drag-left"
            if pet_manifest.animation_for(name) is not None:
                controller.set_animation(name)
            elif pet_manifest.animation_for("drag") is not None:
                controller.set_animation("drag")

        def mouseReleaseEvent(self, event) -> None:
            if event.button() == Qt.LeftButton:
                if overlay_state["dragging"]:
                    settings.update(x=self.x(), y=self.y())
                    settings.save()
                    overlay_state["dragging"] = False
                    base_x[0] = self.x()
                    base_y[0] = self.y()
                    controller.set_state(overlay_state["pre_drag_state"])
                    self._update_frame()
                else:
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
            if action is open_action:
                if on_request_focus is not None:
                    on_request_focus()
            elif action is tuck_action:
                window.hide()
                settings.update(enabled=False)
                settings.save()

    window = PetWindow()
    window.show()
    base_x = [window.x()]
    base_y = [window.y()]

    # --- Show/hide + settings helpers (defined before the tray uses them) -
    def _toggle_visibility() -> None:
        if window.isVisible():
            window.hide()
        else:
            window.show()
            window.raise_()

    def _open_settings() -> None:
        from .qt_settings import run_settings_dialog
        run_settings_dialog(settings)

    # --- System tray ----------------------------------------------------
    tray = QSystemTrayIcon()
    tray_icon = _make_tray_icon(cell_w, cell_h, sprite_image, scale)
    tray.setIcon(tray_icon)
    tray.setToolTip(f"{pet_manifest.name} — resting")

    def _tray_activated(reason) -> None:
        if reason == QSystemTrayIcon.Trigger:
            _toggle_visibility()

    tray.activated.connect(_tray_activated)

    def _tray_menu() -> QMenu:
        menu = QMenu()
        show_action = QAction("Show Mara" if window.isVisible() else "Hide Mara", menu)
        show_action.triggered.connect(_toggle_visibility)
        menu.addAction(show_action)
        open_action = QAction("Open Zarathushtra", menu)
        open_action.triggered.connect(lambda: on_request_focus() if on_request_focus else None)
        menu.addAction(open_action)
        menu.addSeparator()
        settings_action = QAction("Pet Settings...", menu)
        settings_action.triggered.connect(_open_settings)
        menu.addAction(settings_action)
        menu.addSeparator()
        quit_action = QAction("Quit", menu)
        quit_action.triggered.connect(app.quit)
        menu.addAction(quit_action)
        return menu

    tray.setContextMenu(_tray_menu())

    # --- Pet state actor + ZMQ subscriber -------------------------------
    from .actor import PetStateActor

    def _on_state(state: PetState, labels: list[str]) -> None:
        # During a drag, keep the running animation regardless of incoming
        # state; we'll restore on release.
        if overlay_state["dragging"]:
            return
        controller.set_state(state)
        window._update_frame()
        emotion = _EMOTION_LABELS.get(state, state.value)
        tray.setToolTip(f"{pet_manifest.name} — {emotion}")
        # Surface emotion changes that need the user's attention via a tray
        # balloon so the user notices even if the window is hidden.
        if state is PetState.NEEDS_INPUT and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                pet_manifest.name, f"{pet_manifest.name} needs your input",
                QSystemTrayIcon.Information, 4000)
        elif state is PetState.BLOCKED and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                pet_manifest.name, f"{pet_manifest.name} is stuck",
                QSystemTrayIcon.Warning, 4000)
        elif state is PetState.READY and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                pet_manifest.name, f"{pet_manifest.name} has something ready",
                QSystemTrayIcon.Information, 3000)
        overlay_state["last_emotion"] = state

    actor = PetStateActor.start(subscriber=_on_state)
    runtime_bridge.register_actor(actor)

    subscriber = PetSubscriber(on_event=lambda p: _dispatch_payload(p, actor))
    subscriber.start()

    def _dispatch_payload(payload, actor_ref) -> None:
        event_name = payload.get("event")
        factory = _PAYLOAD_MAP.get(event_name)
        if factory is None:
            logger.warning("[PetOverlay] unknown event: %s", event_name)
            return
        logger.info("[PetOverlay] event received: %s", event_name)
        try:
            actor_ref.tell(factory(payload))
        except Exception:
            logger.debug("[PetOverlay] dispatch failed for %s", event_name, exc_info=True)

    # --- Timers ---------------------------------------------------------
    import math
    bob_phase = [0.0]

    def _animation_tick() -> None:
        if overlay_state["dragging"]:
            return
        # Physical movement while "running" (thinking/responding): a gentle
        # vertical bob so the sprite is visibly active, not just cycling
        # sprite frames. needs-input gets a smaller anxious jitter.
        state = controller.state
        if state is PetState.RUNNING or state is PetState.NEEDS_INPUT:
            bob_phase[0] += 0.15
            amplitude = 6 if state is PetState.RUNNING else 3
            bob = int(amplitude * math.sin(bob_phase[0]))
            window.move(base_x[0], base_y[0] + bob)
        else:
            if window.y() != base_y[0] or window.x() != base_x[0]:
                window.move(base_x[0], base_y[0])
        if controller.frame_changed():
            window._update_frame()

    anim_timer = QTimer()
    anim_timer.timeout.connect(_animation_tick)
    anim_timer.start(int(1000 / 60))

    def _ipc_tick() -> None:
        subscriber.poll()

    ipc_timer = QTimer()
    ipc_timer.timeout.connect(_ipc_tick)
    ipc_timer.start(33)  # ~30 Hz ZMQ drain

    # --- Ctrl-C / SIGTERM handler ---------------------------------------
    # Qt absorbs SIGINT into timer callbacks, producing traceback noise
    # instead of a clean exit. Install a signal handler that routes
    # through QCoreApplication.quit so cleanup runs normally.
    import signal as _signal
    _orig_sigint = _signal.getsignal(_signal.SIGINT)
    _orig_sigterm = _signal.getsignal(_signal.SIGTERM)

    def _quit_on_signal(signum, frame) -> None:
        app.quit()

    def _restore_signals() -> None:
        _signal.signal(_signal.SIGINT, _orig_sigint)
        _signal.signal(_signal.SIGTERM, _orig_sigterm)

    # A short wakeup timer lets Python check for pending signals
    # (signal handlers only run in the main thread between bytecodes).
    wakeup = QTimer()
    wakeup.timeout.connect(lambda: None)
    wakeup.start(200)
    _signal.signal(_signal.SIGINT, _quit_on_signal)
    _signal.signal(_signal.SIGTERM, _quit_on_signal)

    def _cleanup() -> None:
        anim_timer.stop()
        ipc_timer.stop()
        wakeup.stop()
        _restore_signals()
        runtime_bridge.unregister_actor()
        actor.stop()
        subscriber.stop()
        controller.dispose()
        cache.clear()
        tray.hide()

    app.aboutToQuit.connect(_cleanup)
    tray.show()
    _on_state(PetState.IDLE, [])
    return app.exec()


def _build_context_menu(parent, pos):
    from PySide6.QtGui import QAction
    from PySide6.QtWidgets import QMenu
    menu = QMenu(parent)
    menu.addAction("Open Zarathushtra")
    menu.addSeparator()
    menu.addAction("Tuck Away")
    return menu


def _make_tray_icon(cell_w: int, cell_h: int, sprite_image, scale: float):
    """Build a small tray icon from the pet's idle frame."""
    from PySide6.QtCore import Qt, QSize
    from PySide6.QtGui import QPixmap, QIcon, QImage, QPainter
    idle = sprite_image.copy(0, 0, cell_w, cell_h)
    pix = QPixmap.fromImage(idle)
    icon = QIcon(pix)
    return icon