from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"
EMULATOR_GATE = ROOT / "scripts" / "test-android-emulator-install.sh"
EVIDENCE_VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"
SOURCE_SHA = "a" * 40
APK_SHA256 = "b" * 64
PNG = b"\x89PNG\r\n\x1a\n" + b"w10-metadata"
RUNTIME = {
    "mode": None,
    "runtime_id": None,
    "model": None,
    "quantization": None,
    "phase": None,
}


def _load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_capture_binds_exact_apk_route_and_typed_runtime_metadata(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load(DEVICE_ACCEPTANCE, "zara_w10_metadata_device")
    device = module.Device("emulator-5554", tmp_path)
    device.source_sha = SOURCE_SHA
    device.device_api = "35"
    device.current_profile = "default"
    device.apk_sha256 = APK_SHA256
    device.current_route = "chat"
    device.runtime_evidence = dict(RUNTIME)
    hierarchy = (
        '<hierarchy><node text="Chat" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" /></hierarchy>'
    )

    def fake_adb(*arguments: str, binary: bool = False):
        if arguments[:2] == ("exec-out", "screencap"):
            assert binary is True
            return PNG
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(device, "_hierarchy_text", lambda: hierarchy)

    device.capture("metadata-state")

    record = device.scenario_evidence[0]
    assert record["apk_sha256"] == APK_SHA256
    assert record["route"] == "chat"
    assert record["runtime"] == RUNTIME
    text = (tmp_path / "metadata-state.ui.txt").read_text(encoding="utf-8")
    assert 'route="chat"' in text
    assert "runtime=" in text
    for field in RUNTIME:
        assert field in text


def test_installed_acceptance_hashes_and_passes_the_exact_candidate_apk() -> None:
    acceptance = DEVICE_ACCEPTANCE.read_text(encoding="utf-8")
    emulator_gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'parser.add_argument("--apk-sha256", required=True)' in acceptance
    assert 'sha256sum "$phone_apk"' in emulator_gate
    assert '--apk-sha256 "$phone_apk_sha256"' in emulator_gate


def _write_bundle(tmp_path: Path) -> Path:
    screenshot = tmp_path / "state.png"
    screenshot.write_bytes(PNG)
    screenshot_sha = hashlib.sha256(PNG).hexdigest()
    text = tmp_path / "state.ui.txt"
    text.write_text('route="chat" runtime={}\n', encoding="utf-8")
    assertions = tmp_path / "state.assertions.txt"
    assertions.write_text(
        "ACTION 1 capture:state\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n",
        encoding="utf-8",
    )
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
    (tmp_path / "state.json").write_text(
        json.dumps(scenario, sort_keys=True),
        encoding="utf-8",
    )
    manifest = {
        "source_sha": SOURCE_SHA,
        "passed": True,
        "device": {"api": "35"},
        "screenshots": [
            {"state": "state", "file": screenshot.name, "sha256": screenshot_sha}
        ],
        "scenarios": [scenario],
    }
    manifest_path = tmp_path / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


@pytest.mark.parametrize("field", ["apk_sha256", "route", "runtime"])
def test_validator_rejects_missing_required_scenario_metadata(
    tmp_path: Path,
    field: str,
) -> None:
    validator = _load(EVIDENCE_VALIDATOR, f"zara_w10_metadata_validator_{field}")
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    del manifest["scenarios"][0][field]
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    (tmp_path / "state.json").write_text(
        json.dumps(manifest["scenarios"][0], sort_keys=True),
        encoding="utf-8",
    )

    with pytest.raises(validator.EvidenceError, match=field):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_validator_rejects_incomplete_runtime_shape(tmp_path: Path) -> None:
    validator = _load(EVIDENCE_VALIDATOR, "zara_w10_metadata_validator_runtime")
    manifest_path = _write_bundle(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    del manifest["scenarios"][0]["runtime"]["quantization"]
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    (tmp_path / "state.json").write_text(
        json.dumps(manifest["scenarios"][0], sort_keys=True),
        encoding="utf-8",
    )

    with pytest.raises(validator.EvidenceError, match="runtime.*quantization"):
        validator.validate_android(manifest_path, SOURCE_SHA)
