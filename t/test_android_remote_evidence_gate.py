from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"


def _load_validator():
    spec = importlib.util.spec_from_file_location("zara_ui_evidence_validator", VALIDATOR)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _png_bytes(label: str) -> bytes:
    return b"\x89PNG\r\n\x1a\n" + label.encode("utf-8") + (b"x" * 96)


def _write_android_manifest(
    evidence: Path,
    *,
    manifest_name: str,
    state: str,
    source_sha: str,
    apk_sha256: str = "b" * 64,
    device_api: str = "35",
) -> Path:
    evidence.mkdir(parents=True, exist_ok=True)
    screenshot = evidence / f"{state}.png"
    screenshot.write_bytes(_png_bytes(state))
    screenshot_sha256 = hashlib.sha256(screenshot.read_bytes()).hexdigest()
    runtime = {
        "mode": "remote" if state.startswith("remote-") else "local",
        "runtime_id": "stock-zara-server" if state.startswith("remote-") else "local-zara-server",
        "model": None,
        "quantization": None,
        "phase": "connected" if state.startswith("remote-") else "ready",
    }
    actions = [f"capture:{state}"]
    assertions = [
        {
            "name": "screenshot-png",
            "passed": True,
            "detail": "device returned PNG screenshot evidence",
        }
    ]
    text_path = evidence / f"{state}.ui.txt"
    text_path.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(runtime, sort_keys=True, separators=(',', ':'))}\n"
        f"ACTION 1 capture:{state}\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n"
        f'class="android.widget.TextView" text="{state}" content_desc="" '
        "enabled=true clickable=false selected=true focused=false bounds=[20,40][220,96]\n",
        encoding="utf-8",
    )
    assertion_path = evidence / f"{state}.assertions.txt"
    assertion_path.write_text(
        f"ACTION 1 capture:{state}\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n",
        encoding="utf-8",
    )
    scenario = {
        "scenario_id": f"android.ui.{state}",
        "source_sha": source_sha,
        "apk_sha256": apk_sha256,
        "device_api": device_api,
        "profile": "default",
        "route": "chat",
        "runtime": runtime,
        "actions": actions,
        "assertions": assertions,
        "screenshot": {
            "file": screenshot.name,
            "sha256": screenshot_sha256,
        },
        "text_evidence": {
            "file": text_path.name,
            "sha256": hashlib.sha256(text_path.read_bytes()).hexdigest(),
        },
        "assertion_evidence": {
            "file": assertion_path.name,
            "sha256": hashlib.sha256(assertion_path.read_bytes()).hexdigest(),
        },
    }
    (evidence / f"{state}.json").write_text(
        json.dumps(scenario, sort_keys=True),
        encoding="utf-8",
    )
    manifest = {
        "source_sha": source_sha,
        "apk_sha256": apk_sha256,
        "serial": "emulator-5554",
        "passed": True,
        "device": {"api": device_api},
        "screenshots": [
            {
                "state": state,
                "file": screenshot.name,
                "sha256": screenshot_sha256,
            }
        ],
        "scenarios": [scenario],
    }
    manifest_path = evidence / manifest_name
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def _rename_supplemental_evidence_files(
    evidence: Path,
    manifest_path: Path,
    *,
    prefix: str,
) -> None:
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    scenario = manifest["scenarios"][0]
    for manifest_key, scenario_key, suffix in (
        ("screenshots", "screenshot", ".png"),
        (None, "text_evidence", ".ui.txt"),
        (None, "assertion_evidence", ".assertions.txt"),
    ):
        evidence_ref = scenario[scenario_key]
        old_path = evidence / evidence_ref["file"]
        new_path = evidence / f"{prefix}{suffix}"
        new_path.write_bytes(old_path.read_bytes())
        evidence_ref["file"] = new_path.name
        evidence_ref["sha256"] = hashlib.sha256(new_path.read_bytes()).hexdigest()
        if manifest_key == "screenshots":
            manifest["screenshots"][0]["file"] = new_path.name
            manifest["screenshots"][0]["sha256"] = evidence_ref["sha256"]
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")


def test_primary_android_gate_rejects_tampered_remote_rendered_evidence(
    tmp_path: Path,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    _write_android_manifest(
        evidence,
        manifest_name="remote-manifest.json",
        state="remote-text-turn",
        source_sha=source_sha,
    )
    (evidence / "remote-text-turn.ui.txt").write_text(
        "tampered after manifest hashing\n",
        encoding="utf-8",
    )

    with pytest.raises(validator.EvidenceError, match="hash mismatch"):
        validator.validate_android(primary, source_sha)


@pytest.mark.parametrize(
    ("manifest_name", "state"),
    (
        ("remote-manifest.json", "remote-text-turn"),
        ("recovery-manifest.json", "recovery-text-turn"),
    ),
)
def test_primary_android_gate_rejects_supplemental_evidence_from_different_apk(
    tmp_path: Path,
    manifest_name: str,
    state: str,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
        apk_sha256="b" * 64,
    )
    _write_android_manifest(
        evidence,
        manifest_name=manifest_name,
        state=state,
        source_sha=source_sha,
        apk_sha256="c" * 64,
    )

    with pytest.raises(validator.EvidenceError, match="supplemental apk_sha256 mismatch"):
        validator.validate_android(primary, source_sha)


@pytest.mark.parametrize(
    ("manifest_name", "state"),
    (
        ("remote-manifest.json", "remote-text-turn"),
        ("recovery-manifest.json", "recovery-text-turn"),
    ),
)
def test_primary_android_gate_rejects_supplemental_evidence_from_different_device_api(
    tmp_path: Path,
    manifest_name: str,
    state: str,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
        apk_sha256="b" * 64,
        device_api="35",
    )
    _write_android_manifest(
        evidence,
        manifest_name=manifest_name,
        state=state,
        source_sha=source_sha,
        apk_sha256="b" * 64,
        device_api="34",
    )

    with pytest.raises(validator.EvidenceError, match="supplemental device API mismatch"):
        validator.validate_android(primary, source_sha)

@pytest.mark.parametrize(
    "manifest_name",
    ("remote-manifest.json", "recovery-manifest.json"),
)
def test_primary_android_gate_rejects_cross_manifest_duplicate_scenario_state(
    tmp_path: Path,
    manifest_name: str,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    supplemental = _write_android_manifest(
        evidence,
        manifest_name=manifest_name,
        state="empty-shell",
        source_sha=source_sha,
    )
    _rename_supplemental_evidence_files(
        evidence,
        supplemental,
        prefix=manifest_name.removesuffix("-manifest.json") + "-empty-shell",
    )

    with pytest.raises(validator.EvidenceError, match="duplicate scenario"):
        validator.validate_android(primary, source_sha)


def test_primary_android_gate_rejects_remote_recovery_duplicate_scenario_state(
    tmp_path: Path,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    _write_android_manifest(
        evidence,
        manifest_name="remote-manifest.json",
        state="shared-supplemental",
        source_sha=source_sha,
    )
    recovery = _write_android_manifest(
        evidence,
        manifest_name="recovery-manifest.json",
        state="shared-supplemental",
        source_sha=source_sha,
    )
    _rename_supplemental_evidence_files(
        evidence,
        recovery,
        prefix="recovery-shared-supplemental",
    )

    with pytest.raises(validator.EvidenceError, match="duplicate scenario"):
        validator.validate_android(primary, source_sha)


def test_primary_android_gate_rejects_cross_manifest_screenshot_filename_alias(
    tmp_path: Path,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    supplemental = _write_android_manifest(
        evidence,
        manifest_name="remote-manifest.json",
        state="remote-text-turn",
        source_sha=source_sha,
    )
    primary_manifest = json.loads(primary.read_text(encoding="utf-8"))
    supplemental_manifest = json.loads(supplemental.read_text(encoding="utf-8"))
    primary_screenshot = primary_manifest["screenshots"][0]
    supplemental_manifest["screenshots"][0]["file"] = primary_screenshot["file"]
    supplemental_manifest["screenshots"][0]["sha256"] = primary_screenshot["sha256"]
    scenario = supplemental_manifest["scenarios"][0]
    scenario["screenshot"] = dict(primary_screenshot)
    (evidence / "remote-text-turn.json").write_text(
        json.dumps(scenario, sort_keys=True),
        encoding="utf-8",
    )
    supplemental.write_text(json.dumps(supplemental_manifest), encoding="utf-8")

    with pytest.raises(
        validator.EvidenceError,
        match="evidence filename is duplicated",
    ):
        validator.validate_android(primary, source_sha)


@pytest.mark.parametrize(
    "alias_field",
    ("text_evidence", "assertion_evidence"),
)
def test_primary_android_gate_rejects_cross_manifest_sidecar_filename_alias(
    tmp_path: Path,
    alias_field: str,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    supplemental = _write_android_manifest(
        evidence,
        manifest_name="remote-manifest.json",
        state="secondary-shell",
        source_sha=source_sha,
    )
    primary_manifest = json.loads(primary.read_text(encoding="utf-8"))
    supplemental_manifest = json.loads(supplemental.read_text(encoding="utf-8"))
    primary_scenario = primary_manifest["scenarios"][0]
    scenario = supplemental_manifest["scenarios"][0]
    scenario["actions"] = list(primary_scenario["actions"])
    scenario["assertions"] = list(primary_scenario["assertions"])
    trace = (
        "ACTION 1 capture:empty-shell\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n"
    )
    own_assertions = evidence / "secondary-shell.assertions.txt"
    own_assertions.write_text(trace, encoding="utf-8")
    scenario["assertion_evidence"]["sha256"] = hashlib.sha256(
        own_assertions.read_bytes()
    ).hexdigest()
    own_text = evidence / "secondary-shell.ui.txt"
    own_text.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(scenario['runtime'], sort_keys=True, separators=(',', ':'))}\n"
        + trace
        + 'class="android.widget.TextView" text="secondary-shell" content_desc="" '
        "enabled=true clickable=false selected=true focused=false bounds=[20,40][220,96]\n",
        encoding="utf-8",
    )
    scenario["text_evidence"]["sha256"] = hashlib.sha256(
        own_text.read_bytes()
    ).hexdigest()
    scenario[alias_field] = dict(primary_scenario[alias_field])
    (evidence / "secondary-shell.json").write_text(
        json.dumps(scenario, sort_keys=True),
        encoding="utf-8",
    )
    supplemental.write_text(json.dumps(supplemental_manifest), encoding="utf-8")

    with pytest.raises(
        validator.EvidenceError,
        match="evidence filename is duplicated",
    ):
        validator.validate_android(primary, source_sha)


def test_primary_android_gate_accepts_noncolliding_supplemental_namespaces(
    tmp_path: Path,
) -> None:
    validator = _load_validator()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    evidence = tmp_path / "android"
    primary = _write_android_manifest(
        evidence,
        manifest_name="manifest.json",
        state="empty-shell",
        source_sha=source_sha,
    )
    _write_android_manifest(
        evidence,
        manifest_name="remote-manifest.json",
        state="remote-text-turn",
        source_sha=source_sha,
    )
    _write_android_manifest(
        evidence,
        manifest_name="recovery-manifest.json",
        state="recovery-text-turn",
        source_sha=source_sha,
    )

    assert validator.validate_android(primary, source_sha) == 3
