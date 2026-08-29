from __future__ import annotations

import tomllib

import pytest
from PySide6.QtWidgets import QApplication

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.preferences import SettingsDocument, SettingsValidationError
from zara.desktop.theme import apply_desktop_theme


def make_config(tmp_path, extra: str = "") -> ZaraConfig:
    path = tmp_path / "config.toml"
    path.write_text(DEFAULT_CONFIG_TOML + extra, encoding="utf-8")
    return ZaraConfig(str(path))


def test_settings_document_updates_known_values_without_erasing_comments_or_unknown_tables(tmp_path):
    config = make_config(
        tmp_path,
        "\n# keep this user note\n[custom]\nanswer = 42\n",
    )
    document = SettingsDocument(config)

    document.update(
        {
            "desktop.theme": "nord",
            "llm.model": "mistral-small",
            "agent.max_steps": 14,
            "tools.file_tools": True,
        }
    )

    text = config.config_file.read_text(encoding="utf-8")
    parsed = tomllib.loads(text)
    assert parsed["desktop"]["theme"] == "nord"
    assert parsed["llm"]["model"] == "mistral-small"
    assert parsed["agent"]["max_steps"] == 14
    assert parsed["tools"]["file_tools"] is True
    assert parsed["custom"]["answer"] == 42
    assert "# keep this user note" in text
    assert config.get("desktop", "theme") == "nord"


def test_invalid_theme_or_value_preserves_the_previous_file(tmp_path):
    config = make_config(tmp_path)
    document = SettingsDocument(config)
    before = config.config_file.read_bytes()

    with pytest.raises(SettingsValidationError, match="desktop.theme"):
        document.update({"desktop.theme": "not-a-theme"})
    assert config.config_file.read_bytes() == before

    with pytest.raises(SettingsValidationError, match="agent.max_steps"):
        document.update({"agent.max_steps": 0})
    assert config.config_file.read_bytes() == before


def test_string_serialization_cannot_escape_its_toml_value(tmp_path):
    config = make_config(tmp_path)
    document = SettingsDocument(config)
    value = 'model"\n[tools]\nfile_tools = true'

    document.update({"llm.model": value})

    parsed = tomllib.loads(config.config_file.read_text(encoding="utf-8"))
    assert parsed["llm"]["model"] == value
    assert parsed["tools"]["file_tools"] is False


def test_unknown_legacy_theme_loads_and_visually_falls_back(tmp_path):
    path = tmp_path / "config.toml"
    path.write_text(
        DEFAULT_CONFIG_TOML.replace('theme = "signal-cabin"', 'theme = "old-custom-theme"'),
        encoding="utf-8",
    )
    config = ZaraConfig(str(path))

    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    qt_app = instance or QApplication([])
    apply_desktop_theme(qt_app, config.get("desktop", "theme"))

    assert qt_app.property("zaraTheme") == "signal-cabin"
