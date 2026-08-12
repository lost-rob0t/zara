"""Native PySide6 Zara desktop application entry point."""

from __future__ import annotations

import sys
from typing import Optional, Sequence

from PySide6.QtWidgets import QApplication

from zara.desktop.controller import DesktopController
from zara.desktop.qt_bridge import QtRuntimeBridge
from zara.runtime.host import RuntimeHost

_CONTROLLER_ATTR = "_zara_desktop_controller"


def create_application(
    argv: Optional[Sequence[str]] = None,
    *,
    host: Optional[RuntimeHost] = None,
) -> tuple[QApplication, DesktopController]:
    """Create or reuse the one canonical Zara QApplication/controller."""
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

    existing = getattr(app, _CONTROLLER_ATTR, None)
    if existing is not None:
        return app, existing

    runtime_host = host or RuntimeHost()
    bridge = QtRuntimeBridge(runtime_host, parent=app)
    controller = DesktopController(app, runtime_host, bridge)
    setattr(app, _CONTROLLER_ATTR, controller)
    return app, controller


def main(argv: Optional[Sequence[str]] = None) -> int:
    app, controller = create_application(argv)
    controller.start()
    return int(app.exec())


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
