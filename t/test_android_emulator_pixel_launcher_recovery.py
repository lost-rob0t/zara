from pathlib import Path


EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_emulator_gate_quiesces_only_pixel_launcher_before_zara_acceptance() -> None:
    text = EMULATOR_GATE.read_text(encoding="utf-8")
    package = "com.google.android.apps.nexuslauncher"
    package_probe = f'adb -s "$serial" shell pm path {package}'
    force_stop = f'adb -s "$serial" shell am force-stop {package}'
    acceptance = 'python3 "$repo_root/android/integration/device_acceptance.py"'

    assert package_probe in text
    assert force_stop in text
    assert acceptance in text
    assert text.index(package_probe) < text.index(force_stop) < text.index(acceptance)
    assert 'settings put global hide_error_dialogs' not in text
    assert 'am force-stop ai.zara.app' not in text.split(acceptance, 1)[0]
