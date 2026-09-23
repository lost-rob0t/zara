import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import sys

import pytest


EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")
EVIDENCE_VALIDATOR = Path("scripts/validate-ui-evidence.py")
SOURCE_SHA = "a" * 40


def visual_manifest_verifier() -> str:
    source = EMULATOR_GATE.read_text(encoding="utf-8")
    marker = (
        'python3 - "$visual_manifest" '
        '"$repo_root/android/app/build/reports/device" "$source_sha" <<\'PY\'\n'
    )
    start = source.index(marker) + len(marker)
    end = source.index("\nPY\n", start)
    return source[start:end]


def evidence_validator_module():
    spec = importlib.util.spec_from_file_location(
        "zara_ui_evidence_validator_test",
        EVIDENCE_VALIDATOR,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_visual_bundle(tmp_path: Path, *, twin_xml: str) -> Path:
    screenshot = tmp_path / "drawer-conversation-overflow.png"
    screenshot.write_bytes(b"\x89PNG\r\n\x1a\nsynthetic screenshot bytes")
    screenshot_sha = hashlib.sha256(screenshot.read_bytes()).hexdigest()
    text_twin = tmp_path / "drawer-conversation-overflow.xml"
    text_twin.write_text(twin_xml, encoding="utf-8")

    scenario_text = tmp_path / "drawer-conversation-overflow.ui.txt"
    scenario_text.write_text(
        'class="android.widget.TextView" text="Rename" enabled=true clickable=false '
        'bounds=[10,30][70,50]\n',
        encoding="utf-8",
    )
    scenario_assertions = tmp_path / "drawer-conversation-overflow.assertions.txt"
    scenario_assertions.write_text(
        "ACTION 1 capture:drawer-conversation-overflow\n"
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence\n",
        encoding="utf-8",
    )

    actions = [
        {
            "label": "Pin",
            "bounds": "[10,10][50,30]",
            "content_inset_px": 2,
            "luma_span": 64,
            "occupied_luma_bins": 12,
        },
        {
            "label": "Rename",
            "bounds": "[10,30][70,50]",
            "content_inset_px": 2,
            "luma_span": 72,
            "occupied_luma_bins": 16,
        },
        {
            "label": "Move to project",
            "bounds": "[10,50][110,70]",
            "content_inset_px": 2,
            "luma_span": 80,
            "occupied_luma_bins": 20,
        },
    ]
    scenario = {
        "scenario_id": "android.ui.drawer-conversation-overflow",
        "source_sha": SOURCE_SHA,
        "device_api": "35",
        "profile": "default",
        "actions": ["capture:drawer-conversation-overflow"],
        "assertions": [
            {
                "name": "screenshot-png",
                "passed": True,
                "detail": "device returned PNG screenshot evidence",
            }
        ],
        "screenshot": {
            "file": screenshot.name,
            "sha256": screenshot_sha,
        },
        "text_evidence": {
            "file": scenario_text.name,
            "sha256": hashlib.sha256(scenario_text.read_bytes()).hexdigest(),
        },
        "assertion_evidence": {
            "file": scenario_assertions.name,
            "sha256": hashlib.sha256(scenario_assertions.read_bytes()).hexdigest(),
        },
    }
    scenario_path = tmp_path / "drawer-conversation-overflow.json"
    scenario_path.write_text(json.dumps(scenario, sort_keys=True), encoding="utf-8")

    manifest = {
        "passed": True,
        "source_sha": SOURCE_SHA,
        "device": {"api": "35"},
        "screenshots": [
            {
                "state": "drawer-conversation-overflow",
                "file": screenshot.name,
                "sha256": screenshot_sha,
            }
        ],
        "scenarios": [scenario],
        "visual_checks": [
            {
                "state": "drawer-conversation-overflow",
                "source_sha": SOURCE_SHA,
                "device_api": "35",
                "profile": "default",
                "trigger_bounds": [90, 10, 110, 30],
                "action_union": [10, 10, 110, 70],
                "viewport": [120, 100],
                "screenshot_file": screenshot.name,
                "screenshot_sha256": screenshot_sha,
                "text_twin_file": text_twin.name,
                "text_twin_sha256": hashlib.sha256(text_twin.read_bytes()).hexdigest(),
                "actions": actions,
            }
        ],
    }
    manifest_path = tmp_path / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def run_existing_visual_verifier(
    tmp_path: Path,
    manifest_path: Path,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, "-", str(manifest_path), str(tmp_path), SOURCE_SHA],
        input=visual_manifest_verifier(),
        text=True,
        capture_output=True,
        check=False,
    )


def run_visual_verifier(tmp_path: Path, *, twin_xml: str) -> subprocess.CompletedProcess[str]:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=twin_xml)
    return run_existing_visual_verifier(tmp_path, manifest_path)


def matching_twin() -> str:
    return """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[10,30][70,50]" />
<node text="Move to project" bounds="[10,50][110,70]" />
</hierarchy>"""


def test_visual_manifest_verifier_accepts_matching_text_twin(tmp_path: Path) -> None:
    result = run_visual_verifier(tmp_path, twin_xml=matching_twin())
    assert result.returncode == 0, result.stderr


def test_visual_manifest_verifier_rejects_text_twin_missing_action(tmp_path: Path) -> None:
    stale_twin = """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[10,30][70,50]" />
</hierarchy>"""

    result = run_visual_verifier(tmp_path, twin_xml=stale_twin)

    assert result.returncode != 0
    assert "text twin is missing action" in result.stderr


def test_visual_manifest_verifier_rejects_text_twin_bounds_drift(tmp_path: Path) -> None:
    stale_twin = """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[80,80][140,100]" />
<node text="Move to project" bounds="[10,50][110,70]" />
</hierarchy>"""

    result = run_visual_verifier(tmp_path, twin_xml=stale_twin)

    assert result.returncode != 0
    assert "text twin action bounds differ" in result.stderr


def test_visual_manifest_verifier_rejects_missing_geometry_receipt(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    del data["visual_checks"][0]["trigger_bounds"]
    manifest_path.write_text(json.dumps(data), encoding="utf-8")

    result = run_existing_visual_verifier(tmp_path, manifest_path)

    assert result.returncode != 0
    assert "overflow visual receipt omitted valid trigger_bounds" in result.stderr


def test_visual_manifest_verifier_rejects_missing_pixel_evidence(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    del data["visual_checks"][0]["actions"][1]["luma_span"]
    manifest_path.write_text(json.dumps(data), encoding="utf-8")

    result = run_existing_visual_verifier(tmp_path, manifest_path)

    assert result.returncode != 0
    assert "overflow visual receipt action omitted valid luma_span: Rename" in result.stderr


def test_visual_manifest_verifier_rejects_union_that_disagrees_with_actions(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    data["visual_checks"][0]["action_union"] = [10, 10, 70, 50]
    manifest_path.write_text(json.dumps(data), encoding="utf-8")

    result = run_existing_visual_verifier(tmp_path, manifest_path)

    assert result.returncode != 0
    assert "overflow visual receipt action_union differs from action bounds" in result.stderr


def test_canonical_android_validator_accepts_complete_scenario_evidence(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    validator = evidence_validator_module()

    assert validator.validate_android(manifest_path, SOURCE_SHA) == 1


def test_visual_manifest_verifier_rejects_missing_scenario_evidence(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    del data["scenarios"]
    manifest_path.write_text(json.dumps(data), encoding="utf-8")
    validator = evidence_validator_module()

    with pytest.raises(validator.EvidenceError, match="omitted per-scenario evidence"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_visual_manifest_verifier_rejects_missing_scenario_assertion_file(
    tmp_path: Path,
) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    (tmp_path / "drawer-conversation-overflow.assertions.txt").unlink()
    validator = evidence_validator_module()

    with pytest.raises(validator.EvidenceError, match="scenario evidence file is missing"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_visual_manifest_verifier_rejects_duplicate_scenario_id(tmp_path: Path) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    data["scenarios"].append(dict(data["scenarios"][0]))
    manifest_path.write_text(json.dumps(data), encoding="utf-8")
    validator = evidence_validator_module()

    with pytest.raises(validator.EvidenceError, match="duplicate scenario id"):
        validator.validate_android(manifest_path, SOURCE_SHA)


def test_visual_manifest_verifier_rejects_scenario_screenshot_hash_drift(
    tmp_path: Path,
) -> None:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=matching_twin())
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    data["scenarios"][0]["screenshot"]["sha256"] = "0" * 64
    manifest_path.write_text(json.dumps(data), encoding="utf-8")
    validator = evidence_validator_module()

    with pytest.raises(
        validator.EvidenceError,
        match="android scenario screenshot .* hash mismatch",
    ):
        validator.validate_android(manifest_path, SOURCE_SHA)
