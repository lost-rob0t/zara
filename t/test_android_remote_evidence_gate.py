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
) -> Path:
    evidence.mkdir(parents=True, exist_ok=True)
    apk_sha256 = "b" * 64
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
        "device_api": "35",
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
        "device": {"api": "35"},
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
