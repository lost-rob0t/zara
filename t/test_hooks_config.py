from pathlib import Path

import pytest

from zara.config import ConfigError, ZaraConfig


def write_config(path: Path, hooks_toml: str) -> Path:
    config_path = path / "config.toml"
    config_path.write_text(hooks_toml, encoding="utf-8")
    return config_path


def test_hooks_config_defaults_disabled(tmp_path: Path) -> None:
    config = ZaraConfig(str(write_config(tmp_path, "")))

    assert config.get_hooks_config() == {
        "enabled": False,
        "allow_override": False,
    }


def test_hooks_config_accepts_explicit_policy(tmp_path: Path) -> None:
    config = ZaraConfig(
        str(
            write_config(
                tmp_path,
                "[hooks]\nenabled = true\nallow_override = false\n",
            )
        )
    )

    assert config.get_hooks_config() == {
        "enabled": True,
        "allow_override": False,
    }


@pytest.mark.parametrize(
    "body, expected",
    [
        ("[hooks]\nenabled = \"yes\"\n", "hooks.enabled must be true or false"),
        (
            "[hooks]\nallow_override = 1\n",
            "hooks.allow_override must be true or false",
        ),
    ],
)
def test_hooks_config_rejects_non_boolean_policy(
    tmp_path: Path,
    body: str,
    expected: str,
) -> None:
    with pytest.raises(ConfigError, match=expected):
        ZaraConfig(str(write_config(tmp_path, body)))
