from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path

import pytest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtGui import QPalette
from PySide6.QtWidgets import QApplication, QStyleFactory

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
    "settings-appearance.png",
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
        data = path.read_bytes()
        assert len(data) > 64
        assert data.startswith(b"\x89PNG\r\n\x1a\n")
        assert entry["sha256"] == hashlib.sha256(data).hexdigest()


def test_fixture_manifest_binds_actual_theme_and_settings_semantic_twin(tmp_path):
    manifest = render_copilot_fixtures(tmp_path / "ui", source_commit="theme-provenance")
    entries = {entry["state"]: entry for entry in manifest["fixtures"]}

    settings = entries["settings-appearance"]
    assert settings["theme"] == "dotfiles-outrun"
    assert settings["text_twin"] == {
        "window_title": "Zara Settings",
        "category": "Appearance",
        "theme_label": "Dotfiles Outrun",
    }
    assert settings["actions"] == [
        {"id": "save-settings", "text": "Save settings", "enabled": True},
        {"id": "restart-zara", "text": "Restart Zara", "enabled": True},
    ]

    copilot_entries = [
        entry for state, entry in entries.items() if state != "settings-appearance"
    ]
    assert copilot_entries
    assert {entry["theme"] for entry in copilot_entries} == {"signal-cabin"}


def test_copilot_fixture_renderer_isolated_from_user_state(tmp_path, monkeypatch):
    output_dir = tmp_path / "ui"
    forbidden = tmp_path / "user-xdg"
    forbidden.mkdir()
    monkeypatch.setenv("XDG_DATA_HOME", str(forbidden))

    render_copilot_fixtures(output_dir, source_commit="test-source")

    assert list(forbidden.iterdir()) == []
    assert set(path.name for path in output_dir.glob("*.png")) == REQUIRED_SCREENSHOTS


def test_copilot_fixture_renderer_restores_application_theme_state(tmp_path):
    qt_app = QApplication.instance() or QApplication([])
    original_palette = QPalette(qt_app.palette())
    caller_stylesheet = qt_app.styleSheet()
    caller_theme = qt_app.property("zaraTheme")
    original_stylesheet = "QWidget { color: #123456; }"
    original_theme = "fixture-caller-theme"
    qt_app.setStyleSheet("")
    original_style_name = qt_app.style().objectName()
    caller_style_name = next(
        (name for name in QStyleFactory.keys() if name.casefold() != "fusion"),
        None,
    )
    if caller_style_name is None:
        pytest.skip("Qt provides no non-Fusion style for restoration testing")
    qt_app.setStyle(caller_style_name)
    caller_style_name = qt_app.style().objectName()
    qt_app.setStyleSheet(original_stylesheet)
    qt_app.setProperty("zaraTheme", original_theme)

    try:
        render_copilot_fixtures(tmp_path / "ui", source_commit="theme-isolation")

        assert qt_app.palette() == original_palette
        assert qt_app.styleSheet() == original_stylesheet
        assert qt_app.property("zaraTheme") == original_theme
        qt_app.setStyleSheet("")
        assert qt_app.style().objectName() == caller_style_name
        qt_app.setStyleSheet(original_stylesheet)
    finally:
        qt_app.setStyle(original_style_name)
        qt_app.setPalette(original_palette)
        qt_app.setStyleSheet(caller_stylesheet)
        qt_app.setProperty("zaraTheme", caller_theme)
