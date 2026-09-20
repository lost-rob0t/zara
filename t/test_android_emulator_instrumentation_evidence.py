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
