from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
EVIDENCE_VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"
SOURCE_SHA = "a" * 40
APK_SHA256 = "b" * 64
PNG = b"\x89PNG\r\n\x1a\n" + b"w10-semantic-binding"
RUNTIME = {
    "mode": None,
    "runtime_id": None,
    "model": None,
    "quantization": None,
    "phase": None,
}
UI_NODE = (
    'class="android.widget.TextView" text="Chat" content_desc="" '
    "enabled=true clickable=false selected=true focused=false "
    "bounds=[20,40][180,96]\n"
)
TRACE = (
    "ACTION 1 capture:state\n"
    "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n"
)


def _load_validator():
    spec = importlib.util.spec_from_file_location(
        "zara_w10_scenario_semantic_validator", EVIDENCE_VALIDATOR
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _persist_manifest_and_scenario(path: Path, manifest: dict) -> None:
    path.write_text(json.dumps(manifest), encoding="utf-8")
    scenario = manifest["scenarios"][0]
    (path.parent / "state.json").write_text(
        json.dumps(scenario, sort_keys=True),
        encoding="utf-8",
    )


def _write_bundle(tmp_path: Path) -> Path:
    screenshot = tmp_path / "state.png"
    screenshot.write_bytes(PNG)
    screenshot_sha = hashlib.sha256(PNG).hexdigest()

    text = tmp_path / "state.ui.txt"
    text.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(RUNTIME, sort_keys=True, separators=(',', ':'))}\n"
        + UI_NODE
        + TRACE,
        encoding="utf-8",
    )
    assertions = tmp_path / "state.assertions.txt"
    assertions.write_text(TRACE, encoding="utf-8")
    scenario = {
        "scenario_id": "android.ui.state",
        "source_sha": SOURCE_SHA,
        "apk_sha256": APK_SHA256,
        "device_api": "35",
        "profile": "default",
        "route": "chat",
        "runtime": dict(RUNTIME),
        "actions": ["capture:state"],
        "assertions": [
            {
                "name": "screenshot-png",
                "passed": True,
                "detail": "device returned PNG screenshot evidence",
            }
        ],
        "screenshot": {"file": screenshot.name, "sha256": screenshot_sha},
        "text_evidence": {
            "file": text.name,
            "sha256": hashlib.sha256(text.read_bytes()).hexdigest(),
        },
        "assertion_evidence": {
            "file": assertions.name,
            "sha256": hashlib.sha256(assertions.read_bytes()).hexdigest(),
        },
    }
    manifest = {
        "source_sha": SOURCE_SHA,
        "apk_sha256": APK_SHA256,
        "passed": True,
        "device": {"api": "35"},
        "screenshots": [
            {"state": "state", "file": screenshot.name, "sha256": screenshot_sha}
        ],
        "scenarios": [scenario],
    }
    manifest_path = tmp_path / "manifest.json"
    _persist_manifest_and_scenario(manifest_path, manifest)
    return manifest_path


def test_validator_requires_manifest_candidate_apk_identity(tmp_path: Path) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    del manifest["apk_sha256"]
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="apk_sha256"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_scenario_apk_identity_drift(tmp_path: Path) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    manifest["scenarios"][0]["apk_sha256"] = "c" * 64
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="apk_sha256"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_text_twin_route_drift(tmp_path: Path) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    text = tmp_path / "state.ui.txt"
    text.write_text(
        'route="settings"\n'
        f"runtime={json.dumps(RUNTIME, sort_keys=True, separators=(',', ':'))}\n"
        + UI_NODE
        + TRACE,
        encoding="utf-8",
    )
    manifest["scenarios"][0]["text_evidence"]["sha256"] = hashlib.sha256(
        text.read_bytes()
    ).hexdigest()
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="text.*route"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_assertion_trace_semantic_drift(tmp_path: Path) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    assertions = tmp_path / "state.assertions.txt"
    assertions.write_text(
        "ACTION 1 tap:unrelated-control\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n",
        encoding="utf-8",
    )
    manifest["scenarios"][0]["assertion_evidence"]["sha256"] = hashlib.sha256(
        assertions.read_bytes()
    ).hexdigest()
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="assertion.*trace"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_text_twin_missing_ui_semantics(tmp_path: Path) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    text = tmp_path / "state.ui.txt"
    text.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(RUNTIME, sort_keys=True, separators=(',', ':'))}\n"
        + TRACE,
        encoding="utf-8",
    )
    manifest["scenarios"][0]["text_evidence"]["sha256"] = hashlib.sha256(
        text.read_bytes()
    ).hexdigest()
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="text.*UI semantics"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_text_twin_missing_action_assertion_trace(
    tmp_path: Path,
) -> None:
    validator = _load_validator()
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    text = tmp_path / "state.ui.txt"
    text.write_text(
        'route="chat"\n'
        f"runtime={json.dumps(RUNTIME, sort_keys=True, separators=(',', ':'))}\n"
        + UI_NODE,
        encoding="utf-8",
    )
    manifest["scenarios"][0]["text_evidence"]["sha256"] = hashlib.sha256(
        text.read_bytes()
    ).hexdigest()
    _persist_manifest_and_scenario(manifest_path, manifest)

    with pytest.raises(validator.EvidenceError, match="text.*action/assertion trace"):
        validator.validate_android(manifest_path, SOURCE_SHA)
