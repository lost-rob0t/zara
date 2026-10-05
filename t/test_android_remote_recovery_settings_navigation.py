from pathlib import Path


RECOVERY_ACCEPTANCE = Path("android/integration/device_remote_recovery_acceptance.py")


def test_remote_recovery_returns_to_visible_settings_overview_before_asserting_runtime():
    text = RECOVERY_ACCEPTANCE.read_text(encoding="utf-8")
    guarded_return = (
        'device.press_back()\n'
        '        device.reveal("Runtime & local AI")\n'
        '        device.await_label("Runtime & local AI")'
    )
    guarded_return_top_level = (
        'device.press_back()\n'
        '    device.reveal("Runtime & local AI")\n'
        '    device.await_label("Runtime & local AI")'
    )

    # Recovery enters Settings from multiple scroll positions. Every path that
    # asserts the overview's top Runtime card must first make it reachable,
    # otherwise a valid lower scroll position creates a false installed-E2E RED.
    assert guarded_return in text
    assert text.count(guarded_return_top_level) >= 2
