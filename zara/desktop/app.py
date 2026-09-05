"""Native PySide6 Zara desktop application entry point."""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Optional, Sequence

from PySide6.QtWidgets import QApplication

from zara.client import InProcessZaraClient, ZaraClient
from zara.config import ZaraConfig, get_config
from zara.desktop.control import (
    DesktopControlAlreadyRunning,
    DesktopControlServer,
    send_desktop_control,
)
from zara.desktop.controller import DesktopController
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.desktop.theme import apply_desktop_theme
from zara.runtime.host import RuntimeHost
from zara.server import ServerLease, default_zmq_endpoint
from zara.zmq_transport import ZmqZaraClient

_CONTROLLER_ATTR = "_zara_desktop_controller"
_CONTROL_ATTR = "_zara_desktop_control_server"


def _default_daemon_endpoint() -> str:
    """Resolve the same owner-private IPC endpoint used by ``zara-server``."""
    return default_zmq_endpoint(ServerLease()._runtime_dir())


def _desktop_control_runtime_dir() -> Path:
    """Reuse Zara's owner-private XDG/UID runtime-directory policy."""
    return ServerLease()._runtime_dir()


def _default_desktop_client() -> ZaraClient:
    """Construct the canonical daemon-backed client for normal desktop startup."""
    return ZmqZaraClient(_default_daemon_endpoint())


def create_application(
    argv: Optional[Sequence[str]] = None,
    *,
    client: Optional[ZaraClient] = None,
    host: Optional[RuntimeHost] = None,
    config: Optional[ZaraConfig] = None,
) -> tuple[QApplication, DesktopController]:
    """Create or reuse the one canonical Zara QApplication/controller."""
    if client is not None and host is not None:
        raise ValueError("choose either client or legacy host, not both")

    instance = QApplication.instance()
    if instance is None:
        app = QApplication(list(argv) if argv is not None else sys.argv)
    elif isinstance(instance, QApplication):
        app = instance
    else:  # pragma: no cover - defensive for embedders that made QCoreApplication
        raise RuntimeError("Zara Desktop requires QApplication, not QCoreApplication")

    app.setApplicationName("Zara")
    app.setOrganizationName("Zara")
    app.setQuitOnLastWindowClosed(False)
    active_config = config or get_config()
    apply_desktop_theme(app, str(active_config.get("desktop", "theme", "signal-cabin")))

    existing = getattr(app, _CONTROLLER_ATTR, None)
    if existing is not None:
        return app, existing

    # ``host`` is retained only as a compatibility injection seam for existing
    # standalone tests/embedders. Normal desktop construction always owns a
    # ZaraClient, so transport selection remains outside Qt surfaces.
    service = client if client is not None else host
    if service is None:
        service = _default_desktop_client()
    bridge = QtRuntimeBridge(service, parent=app)
    controller = DesktopController(app, service, bridge)
    setattr(app, _CONTROLLER_ATTR, controller)
    return app, controller


def _install_desktop_control(
    app: QApplication,
    controller: DesktopController,
    *,
    runtime_dir: Optional[Path | str] = None,
) -> DesktopControlServer:
    """Reserve the single desktop owner before its Zara client starts."""
    existing = getattr(app, _CONTROL_ATTR, None)
    if existing is not None:
        return existing
    target_dir = _desktop_control_runtime_dir() if runtime_dir is None else Path(runtime_dir)
    server = DesktopControlServer(target_dir, controller.desktop_control_requested.emit)
    server.start()
    app.aboutToQuit.connect(server.close)
    setattr(app, _CONTROL_ATTR, server)
    return server


def start_desktop(
    argv: Optional[Sequence[str]] = None,
    *,
    client: Optional[ZaraClient] = None,
    host: Optional[RuntimeHost] = None,
    config: Optional[ZaraConfig] = None,
    summon_quick: bool = True,
) -> tuple[QApplication, DesktopController]:
    """Start the canonical desktop client and expose a visible UI surface."""
    # Preserve the historical host-only call shape when no explicit client is
    # supplied. This keeps compatibility embedders/mocks valid while the normal
    # path is now ZaraClient-owned.
    config_args = {"config": config} if config is not None else {}
    if client is None:
        app, controller = create_application(argv, host=host, **config_args)
    else:
        app, controller = create_application(argv, client=client, host=host, **config_args)
    controller.start()
    if summon_quick:
        controller.show_quick_copilot()
    return app, controller


def main(
    argv: Optional[Sequence[str]] = None,
    *,
    initial_command: str = "show",
) -> int:
    """Own one desktop process, or relay to the owner that won a startup race."""
    app, controller = create_application(argv)
    runtime_dir = _desktop_control_runtime_dir()
    try:
        _install_desktop_control(app, controller, runtime_dir=runtime_dir)
    except DesktopControlAlreadyRunning:
        send_desktop_control(initial_command, runtime_dir=runtime_dir)
        return 0

    controller.start()
    controller.apply_desktop_control(initial_command)
    return int(app.exec())


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
