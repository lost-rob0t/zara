from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"


def _load_validator_module():
    spec = importlib.util.spec_from_file_location("zara_ui_evidence_validator", VALIDATOR)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _write_android_evidence_with_failed_assertion(root: Path, source_sha: str) -> Path:
    evidence = root / "android"
    evidence.mkdir()

    screenshot_bytes = b"\x89PNG\r\n\x1a\n" + (b"x" * 96)
    screenshot_path = evidence / "failed-state.png"
    screenshot_path.write_bytes(screenshot_bytes)
    screenshot_sha = hashlib.sha256(screenshot_bytes).hexdigest()

    runtime = {
        "mode": None,
        "runtime_id": None,
        "model": None,
        "quantization": None,
        "phase": None,
    }
    actions = ["capture:failed-state"]
    assertions = [
        {
            "name": "label-visible",
            "passed": False,
            "detail": "label='Expected' was absent",
        },
        {
            "name": "screenshot-png",
            "passed": True,
            "detail": "device returned PNG screenshot evidence",
        },
    ]

    text_path = evidence / "failed-state.ui.txt"
    text_path.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(runtime, sort_keys=True, separators=(',', ':'))}\n"
        "ACTION 1 capture:failed-state\n"
        "ASSERT FAIL label-visible label='Expected' was absent\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n"
        'class="android.widget.TextView" text="Chat" content_desc="" '
        'enabled=true clickable=false selected=true focused=false bounds=[20,40][180,96]\n',
        encoding="utf-8",
    )

    assertion_path = evidence / "failed-state.assertions.txt"
    assertion_path.write_text(
        "ACTION 1 capture:failed-state\n"
        "ASSERT FAIL label-visible label='Expected' was absent\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n",
        encoding="utf-8",
    )

    apk_sha256 = "b" * 64
    scenario = {
        "scenario_id": "android.ui.failed-state",
        "source_sha": source_sha,
        "apk_sha256": apk_sha256,
        "device_api": "35",
        "profile": "default",
        "route": "chat",
        "runtime": runtime,
        "actions": actions,
        "assertions": assertions,
        "screenshot": {
            "file": screenshot_path.name,
            "sha256": screenshot_sha,
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
    (evidence / "failed-state.json").write_text(
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
                "state": "failed-state",
                "file": screenshot_path.name,
                "sha256": screenshot_sha,
            }
        ],
        "scenarios": [scenario],
    }
    manifest_path = evidence / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def test_android_validator_rejects_passed_manifest_with_failed_scenario_assertion(
    tmp_path: Path,
) -> None:
    module = _load_validator_module()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    manifest = _write_android_evidence_with_failed_assertion(tmp_path, source_sha)

    with pytest.raises(module.EvidenceError, match="failed assertion"):
        module.validate_android(manifest, source_sha)
