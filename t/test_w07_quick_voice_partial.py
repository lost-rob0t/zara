from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QSettings
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.state import DesktopRuntimeState, DesktopStatus
from zara.desktop.windows import QuickCopilotWindow


class _Bridge:
    def submit(self, command):
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


def test_voice_partial_status_is_rendered_in_quick_copilot(tmp_path):
    app = QApplication.instance() or QApplication([])
    app.setQuitOnLastWindowClosed(False)
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "voice-status.db"))
    )
    window = QuickCopilotWindow(
        _Bridge(),
        service,
        settings=QSettings(str(tmp_path / "settings.ini"), QSettings.Format.IniFormat),
    )
    partial = "Partial transcript: open roam daily…"
    try:
        window.set_status(DesktopStatus(DesktopRuntimeState.LISTENING, partial))
        window.show()
        app.processEvents()

        assert window.runtime_status_label.text() == "Listening"
        assert window.runtime_detail_label.text() == partial
        assert window.runtime_detail_label.isVisible()
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        app.processEvents()
