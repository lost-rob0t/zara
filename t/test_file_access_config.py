"""Config validation for file access modes."""

from pathlib import Path

import pytest

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig, ConfigError


def _config_with(tmp_path: Path, body: str) -> ZaraConfig:
    base = DEFAULT_CONFIG_TOML.replace(
        "max_bytes = 20000\n",
        "max_bytes = 20000\n" + body,
        1,
    )
    config_path = tmp_path / "config.toml"
    config_path.write_text(base)
    return ZaraConfig(str(config_path))


def test_default_file_mode_is_sandbox_with_readonly_guest_ceiling(tmp_path):
    config = _config_with(tmp_path, "")
    file_config = config.get_file_tool_config(Path(tmp_path))
    assert file_config["mode"] == "sandbox"
    access = config.get_file_access_config()
    assert access["default_mode"] == "sandbox"
    assert access["guest_ceiling"] == "readonly"


def test_file_mode_and_guest_ceiling_accept_valid_values(tmp_path):
    config = _config_with(
        tmp_path,
        'mode = "ask"\nguest_ceiling = "sandbox"\n',
    )
    assert config.get_file_tool_config(Path(tmp_path))["mode"] == "ask"
    access = config.get_file_access_config()
    assert access["default_mode"] == "ask"
    assert access["guest_ceiling"] == "sandbox"


@pytest.mark.parametrize(
    "setting",
    ['mode = "danger"', 'mode = 1', 'guest_ceiling = "yolo"', 'guest_ceiling = ""'],
)
def test_invalid_mode_settings_are_rejected(tmp_path, setting):
    with pytest.raises(ConfigError, match="file_tools"):
        _config_with(tmp_path, f"{setting}\n")
