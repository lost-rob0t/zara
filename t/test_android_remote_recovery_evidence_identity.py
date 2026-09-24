from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
RECOVERY = INTEGRATION / "device_remote_recovery_acceptance.py"
INSTALL_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"


def _load_recovery_module():
    sys.path.insert(0, str(INTEGRATION))
    try:
        spec = importlib.util.spec_from_file_location(
            "zara_device_remote_recovery_acceptance",
            RECOVERY,
        )
        assert spec is not None
        assert spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.remove(str(INTEGRATION))


def test_recovery_acceptance_binds_exact_source_apk_and_device_identity(
    tmp_path: Path,
    monkeypatch,
) -> None:
    module = _load_recovery_module()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    apk_sha256 = "ab" * 32
    output = tmp_path / "evidence"
    fixture = tmp_path / "fixture.env"
    fixture.write_text("unused=fixture\n", encoding="utf-8")
    observed_devices = []

    class FakeDevice:
        def __init__(self, serial: str, evidence_dir: Path) -> None:
            self.serial = serial
            self.output = evidence_dir
            self.screenshots = []
            self.scenario_evidence = []
            self.source_sha = None
            self.apk_sha256 = None
            self.current_profile = None
            self.device_api = None

        def adb(self, *arguments: str):
            if arguments == ("shell", "getprop", "ro.build.version.sdk"):
                return "35\n"
            if arguments == ("shell", "am", "force-stop", module.APP_PACKAGE):
                return ""
            raise AssertionError(f"unexpected adb call: {arguments!r}")

    def fake_exercise(device, fixture_values):
        observed_devices.append(device)
        assert fixture_values == {"fixture": "synthetic"}
        assert device.source_sha == source_sha
        assert device.apk_sha256 == apk_sha256
        assert device.current_profile == "default"
        assert device.device_api == "35"
        return {"recovery_completed": True}

    monkeypatch.setattr(module, "Device", FakeDevice)
    monkeypatch.setattr(module, "read_fixture", lambda _path: {"fixture": "synthetic"})
    monkeypatch.setattr(module, "exercise_recovery", fake_exercise)
    monkeypatch.setattr(module, "collect_app_diagnostics", lambda _device, _output: {})
    monkeypatch.setattr(module, "verified_source_sha", lambda claimed: source_sha, raising=False)
    monkeypatch.setattr(module, "candidate_apk_sha256", lambda claimed: apk_sha256, raising=False)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            str(RECOVERY),
            "--serial",
            "emulator-5554",
            "--fixture-file",
            str(fixture),
            "--source-sha",
            source_sha,
            "--apk-sha256",
            apk_sha256,
            "--output",
            str(output),
        ],
    )

    module.main()

    assert len(observed_devices) == 1
    manifest = json.loads((output / "recovery-manifest.json").read_text(encoding="utf-8"))
    assert manifest["source_sha"] == source_sha
    assert manifest["apk_sha256"] == apk_sha256
    assert manifest["device"] == {"api": "35"}
    assert manifest["scenarios"] == []
    assert manifest["passed"] is True
    assert manifest["recovery_completed"] is True


def test_installed_recovery_gate_passes_exact_candidate_identity() -> None:
    script = INSTALL_GATE.read_text(encoding="utf-8")
    marker = 'python3 "$repo_root/android/integration/device_remote_recovery_acceptance.py"'
    start = script.index(marker)
    command = script[start : start + 600]

    assert '--source-sha "$source_sha"' in command
    assert '--apk-sha256 "$phone_apk_sha256"' in command
