from __future__ import annotations

from test_quick_copilot import dispose_controller, make_controller


def test_tray_primary_activation_toggles_one_quick_copilot(tmp_path):
    qt_app, controller, _, tray, _, _ = make_controller(tmp_path)
    quick = controller.quick_window
    assert quick is not None
    try:
        original = quick

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is original
        assert quick.isVisible()
        assert quick.composer.hasFocus()

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is original
        assert quick.isVisible() is False

        tray.quick_requested.emit()
        qt_app.processEvents()
        assert controller.quick_window is original
        assert quick.isVisible()
        assert quick.composer.hasFocus()
    finally:
        dispose_controller(controller)
