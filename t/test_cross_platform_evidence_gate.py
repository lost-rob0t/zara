from __future__ import annotations

import hashlib
import json
from pathlib import Path
import subprocess
import sys


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


def test_ci_generates_android_screenshots_and_validates_both_surfaces() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "reactivecircus/android-emulator-runner@v2" in workflow
    assert "Capture Android screenshot evidence" in workflow
    assert "android-ui-evidence" in workflow
    assert "Validate dual-surface screenshot evidence" in workflow
    assert "scripts/validate-ui-evidence.py" in workflow


def test_ci_adds_independent_deep_regression_matrix() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")

    assert "deep-regression:" in workflow
    assert "PYTHONHASHSEED" in workflow
    assert "PYTHONASYNCIODEBUG" in workflow
    assert "31337" in workflow


def test_android_acceptance_can_stamp_reviewed_head_sha() -> None:
    source = DEVICE_ACCEPTANCE.read_text(encoding="utf-8")

    assert 'parser.add_argument("--source-sha"' in source
    assert '"source_sha": source_sha' in source
