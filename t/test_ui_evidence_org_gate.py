from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


SOURCE_SHA = "0123456789abcdef0123456789abcdef01234567"
PNG = b"\x89PNG\r\n\x1a\n"
ORG_FIXTURES = (
    ("editor", "org-editor.png"),
    ("todo", "org-todo.png"),
    ("sync", "org-sync.png"),
    ("notebook", "org-notebook.png"),
)


def _validator_module():
    script = Path(__file__).resolve().parents[1] / "scripts" / "validate-ui-evidence.py"
    spec = importlib.util.spec_from_file_location("zara_ui_evidence_validator", script)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _write_png(path: Path, label: str) -> str:
    payload = PNG + label.encode("utf-8")
    path.write_bytes(payload)
    return hashlib.sha256(payload).hexdigest()


def _write_desktop_evidence(root: Path) -> Path:
    root.mkdir(parents=True, exist_ok=True)
    screenshot = "assistant-empty.png"
    screenshot_sha = _write_png(root / screenshot, "desktop")
    manifest = {
        "fixtures": [
            {
                "state": "assistant-empty",
                "path": screenshot,
                "source_commit": SOURCE_SHA,
                "width": 1100,
                "height": 760,
                "sha256": screenshot_sha,
            }
        ]
    }
    manifest_path = root / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    org_fixtures = []
    for mode, filename in ORG_FIXTURES:
        org_fixtures.append(
            {
                "mode": mode,
                "path": filename,
                "width": 1100,
                "height": 760,
                "theme": "signal-cabin",
                "source_commit": SOURCE_SHA,
                "sha256": _write_png(root / filename, mode),
            }
        )
    (root / "org-manifest.json").write_text(
        json.dumps({"schema": 1, "fixtures": org_fixtures}),
        encoding="utf-8",
    )
    return manifest_path


def test_final_desktop_gate_accepts_complete_exact_head_org_evidence(tmp_path: Path):
    validator = _validator_module()
    manifest_path = _write_desktop_evidence(tmp_path)

    assert validator.validate_desktop(manifest_path, SOURCE_SHA) == 1


def test_final_desktop_gate_rejects_tampered_org_screenshot(tmp_path: Path):
    validator = _validator_module()
    manifest_path = _write_desktop_evidence(tmp_path)
    with (tmp_path / "org-todo.png").open("ab") as handle:
        handle.write(b"tampered")

    with pytest.raises(validator.EvidenceError, match="Org evidence|sha256"):
        validator.validate_desktop(manifest_path, SOURCE_SHA)


def test_final_desktop_gate_rejects_missing_org_manifest(tmp_path: Path):
    validator = _validator_module()
    manifest_path = _write_desktop_evidence(tmp_path)
    (tmp_path / "org-manifest.json").unlink()

    with pytest.raises(validator.EvidenceError, match="Org evidence|manifest"):
        validator.validate_desktop(manifest_path, SOURCE_SHA)


def _write_android_evidence(root: Path, *, nested: bool = True) -> Path:
    metadata = {"file": "launcher.png", "sha256": _write_png(root / "launcher.png", "android")}
    entry = {"state": "launcher"}
    if nested:
        entry["screenshot"] = metadata
    else:
        entry.update(metadata)
    path = root / "manifest.json"
    path.write_text(json.dumps({"source_sha": SOURCE_SHA, "passed": True, "screenshots": [entry]}))
    return path


@pytest.mark.parametrize("nested", [False, True])
def test_android_gate_accepts_capture_and_legacy_metadata(tmp_path: Path, nested: bool):
    validator = _validator_module()
    path = _write_android_evidence(tmp_path, nested=nested)
    assert validator.validate_android(path, SOURCE_SHA) == 1


@pytest.mark.parametrize("mutation, message", [
    ("tampered", "hash mismatch"),
    ("traversal", "escapes"),
    ("missing", "screenshot is missing"),
    ("malformed", "metadata must be an object"),
    ("conflicting", "conflicting"),
    ("source", "source SHA mismatch"),
    ("failed", "passed=true"),
])
def test_android_capture_metadata_preserves_integrity_checks(tmp_path: Path, mutation: str, message: str):
    validator = _validator_module()
    path = _write_android_evidence(tmp_path)
    manifest = json.loads(path.read_text())
    entry = manifest["screenshots"][0]
    if mutation == "tampered":
        (tmp_path / "launcher.png").write_bytes(PNG + b"tampered")
    elif mutation == "traversal":
        entry["screenshot"]["file"] = "../launcher.png"
    elif mutation == "missing":
        (tmp_path / "launcher.png").unlink()
    elif mutation == "malformed":
        entry["screenshot"] = None
    elif mutation == "conflicting":
        entry["file"] = "other.png"
    elif mutation == "source":
        manifest["source_sha"] = "stale"
    elif mutation == "failed":
        manifest["passed"] = False
    path.write_text(json.dumps(manifest))
    with pytest.raises(validator.EvidenceError, match=message):
        validator.validate_android(path, SOURCE_SHA)
