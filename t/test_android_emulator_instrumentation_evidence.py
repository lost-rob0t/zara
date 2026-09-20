from pathlib import Path


EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_connected_instrumentation_failure_is_retained_as_android_evidence() -> None:
    source = EMULATOR_GATE.read_text(encoding="utf-8")
    evidence_dir = 'evidence_dir="android/app/build/reports/device"'
    create_evidence = 'mkdir -p "$evidence_dir"'
    connected_test = ":app:connectedDebugAndroidTest"

    assert evidence_dir in source
    assert create_evidence in source
    assert source.index(create_evidence) < source.index(connected_test)
    assert 'instrumentation_log="$evidence_dir/connected-debug-android-test.log"' in source
    assert 'instrumentation_status=${PIPESTATUS[0]}' in source
    assert "copy_connected_test_diagnostics" in source
    assert 'instrumentation-failure.txt' in source


def test_connected_instrumentation_reinstalls_exact_phone_apk_before_ui_acceptance() -> None:
    source = EMULATOR_GATE.read_text(encoding="utf-8")
    connected_test = ":app:connectedDebugAndroidTest"
    reinstall = 'adb -s "$serial" install -r "$phone_apk"'
    package_check = 'adb -s "$serial" shell cmd package path ai.zara.app | grep -Fq "package:"'
    device_acceptance = "python android/integration/device_acceptance.py"

    connected_index = source.index(connected_test)
    reinstall_index = source.index(reinstall, connected_index)
    package_index = source.index(package_check, reinstall_index)
    acceptance_index = source.index(device_acceptance)

    assert connected_index < reinstall_index < package_index < acceptance_index
