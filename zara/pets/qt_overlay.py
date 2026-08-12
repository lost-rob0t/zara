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
from typing import Callable, Optional

from . import events, runtime_bridge
from .animation import AnimationController, look_direction_index
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


def _overlay_window_flags(Qt, platform_name: str):
    flags = Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint
    if platform_name.lower() == "xcb":
        return flags | Qt.X11BypassWindowManagerHint
    return flags | Qt.Tool


def _queued_state_dispatcher(callback):
    from PySide6.QtCore import QObject, Signal, Qt

    class StateDispatcher(QObject):
        state_changed = Signal(object, object)

    dispatcher = StateDispatcher()
    dispatcher.state_changed.connect(callback, Qt.QueuedConnection)
    return dispatcher


def run_overlay(
    settings: PetSettings,
    pet_manifest: PetManifest,
    on_request_focus: Optional[Callable[[], None]] = None,
) -> int:
    from PySide6.QtCore import Qt, QTimer, QPoint
    from PySide6.QtGui import QPixmap, QPainter, QImage, QAction, QCursor
    from PySide6.QtWidgets import (
        QApplication,
        QLabel,
        QMenu,
        QSystemTrayIcon,
        QWidget,
    )

    app = QApplication.instance() or QApplication([])
    app.setQuitOnLastWindowClosed(False)

    screens = _screen_rects(app.screens())
    saved_x = settings.state.x if settings.state.x is not None else 100
    saved_y = settings.state.y if settings.state.y is not None else 100
    x, y = recover_position(saved_x, saved_y, screens, default=(100, 100))
    scale = float(settings.state.scale or 1.0)
    reduced = _reduced_motion_enabled(settings.state.reduced_motion)
    assistant_name = settings.state.assistant_name

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
            self.setWindowFlags(_overlay_window_flags(Qt, app.platformName()))
            self.setAttribute(Qt.WA_TranslucentBackground, True)
            self.setAttribute(Qt.WA_ShowWithoutActivating, True)
            self.setFocusPolicy(Qt.NoFocus)
            self.setFixedSize(int(cell_w * scale), int(cell_h * scale))
            self.move(x, y)
            self._drag_offset: Optional[QPoint] = None
            self._press_origin: Optional[QPoint] = None
            self._last_drag_pos: Optional[QPoint] = None
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
                self._press_origin = event.globalPos()
                self._last_drag_pos = event.globalPos()
            elif event.button() == Qt.RightButton:
                self._show_context_menu(event.globalPos())

        def mouseMoveEvent(self, event) -> None:
            if self._drag_offset is None or not (event.buttons() & Qt.LeftButton):
                return
            global_pos = event.globalPos()
            if not overlay_state["dragging"]:
                if self._press_origin is None:
                    return
                if (global_pos - self._press_origin).manhattanLength() <= DRAG_THRESHOLD:
                    return
                overlay_state["dragging"] = True
                overlay_state["pre_drag_state"] = controller.state
            self._apply_drag_animation(global_pos)
            self._last_drag_pos = global_pos
            self.move(global_pos - self._drag_offset)
            self.raise_()
            self._update_frame()

        def _apply_drag_animation(self, global_pos) -> None:
            previous = self._last_drag_pos or global_pos
            moving_right = global_pos.x() >= previous.x()
            name = "drag" if moving_right else "drag-left"
            current = controller.animation_override_name
            if current == name:
                return
            if not controller.set_animation(name):
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
                    if controller.state is PetState.IDLE:
                        controller.set_animation("jump")
                    self._update_frame()
                else:
                    controller.set_animation("wave")
                    self._update_frame()
                self._drag_offset = None
                self._press_origin = None
                self._last_drag_pos = None

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
    window.raise_()
    base_x = [window.x()]
    base_y = [window.y()]

    response_bubble = QLabel()
    response_bubble.setWindowFlags(_overlay_window_flags(Qt, app.platformName()))
    response_bubble.setAttribute(Qt.WA_ShowWithoutActivating, True)
    response_bubble.setAttribute(Qt.WA_TransparentForMouseEvents, True)
    response_bubble.setWordWrap(True)
    response_bubble.setMaximumWidth(360)
    response_bubble.setMargin(12)
    response_bubble.setStyleSheet(
        "QLabel { background: rgba(24, 28, 36, 235); color: white; "
        "border: 1px solid rgba(255, 255, 255, 90); border-radius: 12px; "
        "font-size: 13px; }"
    )
    response_bubble.hide()
    response_timer = QTimer()
    response_timer.setSingleShot(True)
    response_timer.timeout.connect(response_bubble.hide)

    def _show_response(text: str) -> None:
        clean = " ".join(text.split())
        if not clean:
            return
        response_bubble.setText(clean)
        response_bubble.adjustSize()
        screen = app.screenAt(window.frameGeometry().center()) or app.primaryScreen()
        available = screen.availableGeometry()
        bubble_x = window.x() + (window.width() - response_bubble.width()) // 2
        bubble_x = max(
            available.left(),
            min(bubble_x, available.right() - response_bubble.width() + 1),
        )
        bubble_y = window.y() - response_bubble.height() - 10
        if bubble_y < available.top():
            bubble_y = window.y() + window.height() + 10
        response_bubble.move(bubble_x, bubble_y)
        response_bubble.show()
        response_bubble.raise_()
        response_timer.start(8000)

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
    tray.setToolTip(f"{assistant_name} — resting")

    def _tray_activated(reason) -> None:
        if reason == QSystemTrayIcon.Trigger:
            _toggle_visibility()

    tray.activated.connect(_tray_activated)

    def _tray_menu() -> QMenu:
        menu = QMenu()
        verb = "Hide" if window.isVisible() else "Show"
        show_action = QAction(f"{verb} {assistant_name}", menu)
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
        tray.setToolTip(f"{assistant_name} — {emotion}")
        # Surface emotion changes that need the user's attention via a tray
        # balloon so the user notices even if the window is hidden.
        if state is PetState.NEEDS_INPUT and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                assistant_name, f"{assistant_name} needs your input",
                QSystemTrayIcon.Information, 4000)
        elif state is PetState.BLOCKED and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                assistant_name, f"{assistant_name} is stuck",
                QSystemTrayIcon.Warning, 4000)
        elif state is PetState.READY and state is not overlay_state["last_emotion"]:
            tray.showMessage(
                assistant_name, f"{assistant_name} has something ready",
                QSystemTrayIcon.Information, 3000)
        overlay_state["last_emotion"] = state

    state_dispatcher = _queued_state_dispatcher(_on_state)
    actor = PetStateActor.start(subscriber=state_dispatcher.state_changed.emit)
    runtime_bridge.register_actor(actor)

    subscriber = PetSubscriber(on_event=lambda p: _dispatch_payload(p, actor))
    subscriber.start()

    def _dispatch_payload(payload, actor_ref) -> None:
        event_name = payload.get("event")
        if event_name == "ResponseText":
            _show_response(str(payload.get("text", "")))
            return
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
    def _animation_tick() -> None:
        if overlay_state["dragging"]:
            return
        if controller.animation_finished():
            controller.set_state(controller.state)
        if (
            controller.state is PetState.IDLE
            and not controller.has_animation_override
            and not controller.reduced_motion
        ):
            center = window.frameGeometry().center()
            cursor = QCursor.pos()
            direction = look_direction_index(
                cursor.x() - center.x(),
                cursor.y() - center.y(),
            )
            if direction is None:
                controller.clear_look_direction()
            else:
                controller.set_look_direction(direction)
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
        response_timer.stop()
        response_bubble.hide()
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
