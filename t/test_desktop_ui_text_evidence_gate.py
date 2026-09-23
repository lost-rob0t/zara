from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"


def _load_validator_module():
    spec = importlib.util.spec_from_file_location(
        "zara_validate_ui_evidence_desktop_test",
        VALIDATOR,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_desktop_validator_rejects_screenshot_only_fixture(tmp_path: Path) -> None:
    module = _load_validator_module()
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    screenshot = tmp_path / "copilot-error.png"
    payload = b"\x89PNG\r\n\x1a\n" + (b"desktop-error" * 16)
    screenshot.write_bytes(payload)
    manifest_path = tmp_path / "manifest.json"
    manifest_path.write_text(
        json.dumps(
            {
                "schema": 1,
                "fixtures": [
                    {
                        "state": "error",
                        "path": screenshot.name,
                        "width": 680,
                        "height": 460,
                        "theme": "signal-cabin",
                        "source_commit": source_sha,
                        "sha256": hashlib.sha256(payload).hexdigest(),
                    }
                ],
            }
        ),
        encoding="utf-8",
    )

    with pytest.raises(module.EvidenceError, match="desktop fixture text evidence is missing"):
        module.validate_desktop(manifest_path, source_sha)
