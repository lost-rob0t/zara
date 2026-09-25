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
