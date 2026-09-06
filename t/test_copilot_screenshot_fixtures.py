from __future__ import annotations

import json
import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from zara.desktop.ui_fixtures import render_copilot_fixtures


REQUIRED_SCREENSHOTS = {
    "copilot-empty-compact.png",
    "copilot-short-chat-compact.png",
    "copilot-long-wrap-compact.png",
    "copilot-streaming.png",
    "copilot-error.png",
    "copilot-disconnected.png",
    "copilot-tool-running.png",
    "copilot-tool-approval.png",
    "copilot-voice-listening.png",
    "copilot-voice-partial.png",
    "copilot-expanded.png",
    "copilot-history.png",
    "copilot-smallest-supported.png",
}


def test_copilot_fixture_renderer_emits_bounded_manifest_and_required_pngs(tmp_path):
    output_dir = tmp_path / "ui"
    manifest = render_copilot_fixtures(
        output_dir,
        source_commit="0123456789abcdef0123456789abcdef01234567",
    )

    manifest_path = output_dir / "manifest.json"
    assert manifest_path.is_file()
    persisted = json.loads(manifest_path.read_text(encoding="utf-8"))
    assert persisted == manifest

    entries = manifest["fixtures"]
    assert isinstance(entries, list)
    assert len(entries) == len(REQUIRED_SCREENSHOTS)
    assert {Path(entry["path"]).name for entry in entries} == REQUIRED_SCREENSHOTS
    assert len({entry["state"] for entry in entries}) == len(entries)

    for entry in entries:
        assert entry["source_commit"] == "0123456789abcdef0123456789abcdef01234567"
        assert isinstance(entry["theme"], str) and entry["theme"]
        assert entry["width"] > 0
        assert entry["height"] > 0
        path = output_dir / Path(entry["path"]).name
        assert path.is_file()
        assert path.stat().st_size > 64
        assert path.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")


def test_copilot_fixture_renderer_isolated_from_user_state(tmp_path, monkeypatch):
    output_dir = tmp_path / "ui"
    forbidden = tmp_path / "user-xdg"
    forbidden.mkdir()
    monkeypatch.setenv("XDG_DATA_HOME", str(forbidden))

    render_copilot_fixtures(output_dir, source_commit="test-source")

    assert list(forbidden.iterdir()) == []
    assert set(path.name for path in output_dir.glob("*.png")) == REQUIRED_SCREENSHOTS
