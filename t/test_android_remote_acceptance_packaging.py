from pathlib import Path


REMOTE_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_emulator_gate_packages_its_remote_acceptance_helper() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert "device_remote_acceptance.py" in gate
    assert REMOTE_ACCEPTANCE.is_file(), (
        "emulator gate references device_remote_acceptance.py but the exact branch "
        "does not package that helper"
    )

    remote = REMOTE_ACCEPTANCE.read_text(encoding="utf-8")
    assert "SecurityAdminClient" in remote
    assert 'device.tap("Remote")' in remote
    assert "signal_turn_acceptance(fixture)" in remote
    assert 'device.await_contains("stock server response"' in remote
    assert '"remote_turn_completed": True' in remote
