from __future__ import annotations

import hashlib
import importlib.util
import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from zara.desktop.ui_fixtures import render_copilot_fixtures


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "scripts" / "validate-ui-evidence.py"


def _load_validator_module():
    spec = importlib.util.spec_from_file_location(
        "zara_validate_ui_evidence_desktop_integration_test",
        VALIDATOR,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rendered_desktop_evidence_round_trips_through_canonical_validator(tmp_path: Path) -> None:
    source_sha = "0123456789abcdef0123456789abcdef01234567"
    output_dir = tmp_path / "desktop-ui"
    manifest = render_copilot_fixtures(output_dir, source_commit=source_sha)
    validator = _load_validator_module()

    assert manifest["schema"] == 2
    assert validator.validate_desktop(output_dir / "manifest.json", source_sha) == len(
        manifest["fixtures"]
    )

    for entry in manifest["fixtures"]:
        text_path = output_dir / entry["text_evidence"]["file"]
        assertions_path = output_dir / entry["assertion_evidence"]["file"]
        assert hashlib.sha256(text_path.read_bytes()).hexdigest() == entry["text_evidence"]["sha256"]
        assert (
            hashlib.sha256(assertions_path.read_bytes()).hexdigest()
            == entry["assertion_evidence"]["sha256"]
        )
        text = text_path.read_text(encoding="utf-8")
        assert f'"source_commit":"{source_sha}"' in text
        assert f'ACTION 1 render:{entry["state"]}' in text
        assert "ASSERT PASS same-state-semantics" in text
        assert "ASSERT PASS screenshot-png" in text

    error_entry = next(entry for entry in manifest["fixtures"] if entry["state"] == "error")
    error_text = (output_dir / error_entry["text_evidence"]["file"]).read_text(encoding="utf-8")
    assert "The runtime rejected this turn. Nothing was executed." in error_text
