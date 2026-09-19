from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"
WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _png_bytes(label: str) -> bytes:
    return b"\x89PNG\r\n\x1a\n" + label.encode("utf-8") + (b"x" * 96)


def _write_desktop_evidence(root: Path, source_sha: str) -> Path:
    evidence = root / "desktop"
    evidence.mkdir(parents=True)
    payload = _png_bytes("desktop")
    screenshot = evidence / "copilot-empty-compact.png"
    screenshot.write_bytes(payload)
    manifest = {
        "schema": 1,
        "fixtures": [
            {
                "state": "empty-compact",
                "path": screenshot.name,
                "width": 680,
                "height": 460,
                "theme": "signal-cabin",
                "source_commit": source_sha,
                "sha256": hashlib.sha256(payload).hexdigest(),
            }
        ],
    }
    manifest_path = evidence / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def _write_android_evidence(root: Path, source_sha: str, *, passed: bool = True) -> Path:
    evidence = root / "android"
    evidence.mkdir(parents=True)
    payload = _png_bytes("android")
    screenshot = evidence / "empty-shell.png"
    screenshot.write_bytes(payload)
    manifest = {
        "source_sha": source_sha,
        "serial": "emulator-5554",
        "passed": passed,
        "screenshots": [
            {
                "state": "empty-shell",
                "file": screenshot.name,
                "sha256": hashlib.sha256(payload).hexdigest(),
            }
        ],
    }
    manifest_path = evidence / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def _run_validator(source_sha: str, desktop: Path, android: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            sys.executable,
            str(VALIDATOR),
            "--source-sha",
            source_sha,
            "--desktop",
            str(desktop),
            "--android",
            str(android),
        ],
        text=True,
        capture_output=True,
        check=False,
    )


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location("zara_device_acceptance_test", DEVICE_ACCEPTANCE)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_dual_surface_validator_accepts_exact_sha_png_evidence(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, source_sha)

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode == 0, result.stderr
    assert "desktop" in result.stdout
    assert "android" in result.stdout


def test_dual_surface_validator_rejects_wrong_sha(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, "fedcba9876543210fedcba9876543210fedcba98")

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode != 0
    assert "source" in result.stderr.lower()


def test_dual_surface_validator_rejects_failed_android_acceptance(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, source_sha, passed=False)

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode != 0
    assert "passed" in result.stderr.lower()


def test_dual_surface_validator_rejects_tampered_desktop_png(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, source_sha)
    (desktop.parent / "copilot-empty-compact.png").write_bytes(_png_bytes("tampered"))

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode != 0
    assert "desktop screenshot hash mismatch" in result.stderr.lower()


def test_dual_surface_validator_rejects_tampered_android_png(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, source_sha)
    (android.parent / "empty-shell.png").write_bytes(_png_bytes("tampered"))

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode != 0
    assert "hash mismatch" in result.stderr.lower()


def test_dual_surface_validator_rejects_manifest_path_escape(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    desktop = _write_desktop_evidence(tmp_path, source_sha)
    android = _write_android_evidence(tmp_path, source_sha)
    manifest = json.loads(desktop.read_text(encoding="utf-8"))
    manifest["fixtures"][0]["path"] = "../outside.png"
    desktop.write_text(json.dumps(manifest), encoding="utf-8")
    (desktop.parent.parent / "outside.png").write_bytes(_png_bytes("outside"))

    result = _run_validator(source_sha, desktop, android)

    assert result.returncode != 0
    assert "escapes" in result.stderr.lower()


def test_ci_generates_android_screenshots_and_validates_both_surfaces() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "reactivecircus/android-emulator-runner@v2" in workflow
    assert "Capture Android screenshot evidence" in workflow
    assert "script: bash -euo pipefail -c '" in workflow
    assert "android/integration/device_acceptance.py" in workflow
    assert '--source-sha "$SOURCE_SHA"' in workflow
    assert "--output android/app/build/reports/device" in workflow
    assert "android-ui-evidence" in workflow
    assert "Validate dual-surface screenshot evidence" in workflow
    assert "scripts/validate-ui-evidence.py" in workflow


def test_ci_refreshes_android_command_line_tools_before_emulator_provisioning() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert 'android_cli_tools_build="15859902"' in workflow
    assert 'android_cli_tools_sha256="4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583"' in workflow
    assert 'commandlinetools-linux-${android_cli_tools_build}_latest.zip' in workflow
    assert "sha256sum --check" in workflow
    assert 'rm -rf "$sdk_root/cmdline-tools/latest"' in workflow
    assert 'sdkmanager_bin="$sdk_root/cmdline-tools/latest/bin/sdkmanager"' in workflow
    assert 'sdkmanager_bin" --sdk_root="$sdk_root" --install emulator --channel=0' in workflow


def test_ci_targets_the_action_managed_emulator_explicitly() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert 'serial="emulator-5554"' in workflow
    assert 'adb -s "$serial" wait-for-device' in workflow
    assert 'adb -s "$serial" get-state' in workflow
    assert 'adb -s "$serial" install -r' in workflow
    assert 'adb devices | awk' not in workflow


def test_ci_adds_independent_deep_regression_matrix() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "deep-regression:" in workflow
    assert "PYTHONHASHSEED" in workflow
    assert "PYTHONASYNCIODEBUG" in workflow
    assert "31337" in workflow


def test_ci_checks_out_exact_reviewed_source_and_success_gates_candidate_apks() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    exact_ref = "ref: ${{ github.event.pull_request.head.sha || github.sha }}"

    assert workflow.count(exact_ref) >= 5
    assert "Verify exact source checkout" in workflow
    assert "name: Upload exact-SHA phone debug APK\n        if: success()" in workflow
    assert "name: Upload Wear debug APK\n        if: success()" in workflow


def test_android_acceptance_dismisses_only_pixel_launcher_anr(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    launcher_anr = module.ET.fromstring(
        '<node text="Pixel Launcher isn\'t responding" bounds="[10,10][90,90]" />'
    )
    wait = module.ET.fromstring('<node text="Wait" bounds="[20,30][80,70]" />')
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: launcher_anr
        if fragment == "Pixel Launcher isn't responding"
        else None,
    )
    monkeypatch.setattr(device, "find", lambda label: wait if label == "Wait" else None)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_pixel_launcher_anr() is True
    assert adb_calls == [("shell", "input", "tap", "50", "50")]

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    adb_calls.clear()
    assert device.dismiss_pixel_launcher_anr() is False
    assert adb_calls == []


def test_android_acceptance_dismisses_release_notes_before_surface_assertion(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][500,90]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: release_notes if fragment == "What's new in Zara " else None,
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: continue_button if label == "Continue" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes() is True
    assert adb_calls == [("shell", "input", "tap", "600", "1550")]

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    adb_calls.clear()
    assert device.dismiss_release_notes() is False
    assert adb_calls == []


def test_android_acceptance_waits_for_delayed_release_notes_button_semantics(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][500,90]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    attempts = {"continue": 0}
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: release_notes if fragment == "What's new in Zara " else None,
    )

    def delayed_find(label: str):
        if label != "Continue":
            return None
        attempts["continue"] += 1
        return continue_button if attempts["continue"] >= 3 else None

    monkeypatch.setattr(device, "find", delayed_find)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes(timeout=1.0) is True
    assert attempts["continue"] == 3
    assert adb_calls == [("shell", "input", "tap", "600", "1550")]


def test_android_acceptance_release_notes_button_timeout_still_fails_closed(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][500,90]" />'
    )
    monotonic_values = iter((0.0, 0.0, 0.5, 1.1))

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: release_notes if fragment == "What's new in Zara " else None,
    )
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(module.time, "monotonic", lambda: next(monotonic_values))
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(AssertionError, match="did not expose Continue"):
        device.dismiss_release_notes(timeout=1.0)


def test_android_acceptance_launch_surface_clears_release_notes_before_waiting(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    events: list[str] = []

    monkeypatch.setattr(device, "adb", lambda *args, **kwargs: events.append("launch") or "")
    monkeypatch.setattr(
        device,
        "dismiss_release_notes",
        lambda: events.append("dismiss-release-notes") or True,
    )
    monkeypatch.setattr(
        device,
        "await_label",
        lambda label: events.append(f"await:{label}"),
    )

    device.launch_surface("ai.zara.app/.MainActivity", "Chat")

    assert events == ["launch", "dismiss-release-notes", "await:Chat"]


def test_android_acceptance_recreate_relaunches_saved_launcher_task(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.recreate()

    assert adb_calls == [
        ("shell", "input", "keyevent", "3"),
        ("shell", "am", "kill", "ai.zara.app"),
        (
            "shell",
            "am",
            "start",
            "-W",
            "-a",
            "android.intent.action.MAIN",
            "-c",
            "android.intent.category.LAUNCHER",
            "-f",
            "0x10200000",
            "-n",
            "ai.zara.app/.MainActivity",
        ),
    ]


def test_android_acceptance_rejects_claimed_sha_mismatch(monkeypatch: pytest.MonkeyPatch) -> None:
    module = _load_device_acceptance_module()
    actual_source_sha = "a" * 40
    claimed_source_sha = "b" * 40
    monkeypatch.setattr(
        module.subprocess,
        "check_output",
        lambda *args, **kwargs: actual_source_sha,
    )

    with pytest.raises(RuntimeError, match="does not match the checked-out repository"):
        module.verified_source_sha(claimed_source_sha)
