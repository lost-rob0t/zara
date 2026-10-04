from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from zara.desktop.org_fixtures import render_org_fixtures


REQUIRED_SCREENSHOTS = {
    "org-editor.png",
    "org-todo.png",
    "org-sync.png",
    "org-notebook.png",
}


def test_org_fixture_renderer_emits_exact_head_manifest_and_priority_surfaces(tmp_path):
    output_dir = tmp_path / "ui"
    manifest = render_org_fixtures(
        output_dir,
        source_commit="0123456789abcdef0123456789abcdef01234567",
    )

    manifest_path = output_dir / "org-manifest.json"
    assert manifest_path.is_file()
    persisted = json.loads(manifest_path.read_text(encoding="utf-8"))
    assert persisted == manifest

    entries = manifest["fixtures"]
    assert isinstance(entries, list)
    assert len(entries) == len(REQUIRED_SCREENSHOTS)
    assert {Path(entry["path"]).name for entry in entries} == REQUIRED_SCREENSHOTS
    assert {entry["mode"] for entry in entries} == {
        "editor",
        "todo",
        "sync",
        "notebook",
    }

    for entry in entries:
        assert entry["source_commit"] == "0123456789abcdef0123456789abcdef01234567"
        assert entry["width"] > 0
        assert entry["height"] > 0
        path = output_dir / Path(entry["path"]).name
        assert path.is_file()
        data = path.read_bytes()
        assert len(data) > 64
        assert data.startswith(b"\x89PNG\r\n\x1a\n")
        assert entry["sha256"] == hashlib.sha256(data).hexdigest()


def test_org_fixture_renderer_never_uses_user_xdg_state(tmp_path, monkeypatch):
    output_dir = tmp_path / "ui"
    forbidden = tmp_path / "user-xdg"
    forbidden.mkdir()
    monkeypatch.setenv("XDG_DATA_HOME", str(forbidden))
    monkeypatch.delenv("ZARA_ORG_ROOT", raising=False)

    render_org_fixtures(output_dir, source_commit="test-source")

    assert list(forbidden.iterdir()) == []
    assert {path.name for path in output_dir.glob("org-*.png")} == REQUIRED_SCREENSHOTS
