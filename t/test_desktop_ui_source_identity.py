from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DESKTOP_EVIDENCE = ROOT / "scripts" / "desktop_ui_evidence.py"
SOURCE_SHA = "a" * 40
CLAIMED_SHA = "b" * 40


def _load():
    spec = importlib.util.spec_from_file_location(
        "zara_w10_desktop_source_identity",
        DESKTOP_EVIDENCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_desktop_cli_verifies_claimed_source_before_render(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load()
    verified_claims: list[str] = []
    rendered_sources: list[str] = []

    def verify(claimed: str) -> str:
        verified_claims.append(claimed)
        return SOURCE_SHA

    def render(_output: Path, *, source_commit: str):
        rendered_sources.append(source_commit)
        return {}

    monkeypatch.setattr(module, "verified_source_sha", verify, raising=False)
    monkeypatch.setattr(module, "render_desktop_ui_evidence", render)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "desktop_ui_evidence.py",
            "--output",
            str(tmp_path),
            "--source-sha",
            CLAIMED_SHA,
        ],
    )

    assert module.main() == 0
    assert verified_claims == [CLAIMED_SHA]
    assert rendered_sources == [SOURCE_SHA]
