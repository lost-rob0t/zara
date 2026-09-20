from pathlib import Path


REMOTE_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_emulator_gate_packages_remote_acceptance_helper() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'python3 "$repo_root/android/integration/device_remote_acceptance.py"' in gate
    assert 'PYTHONPATH="$repo_root/android/integration:$repo_root' in gate
    assert '--fixture-file "$interop_fixture"' in gate
    assert '--output "$repo_root/$evidence_dir"' in gate
    assert REMOTE_ACCEPTANCE.is_file(), (
        "emulator gate invokes device_remote_acceptance.py but the exact source tree "
        "does not package that helper"
    )

    remote = REMOTE_ACCEPTANCE.read_text(encoding="utf-8")
    assert "SecurityAdminClient" in remote
    assert 'device.tap("Remote")' in remote
    assert "signal_turn_acceptance(fixture)" in remote
    assert 'device.await_contains("stock server response"' in remote
    assert '"remote_turn_completed": True' in remote
