from __future__ import annotations

import pytest

from zara.config import ConfigError, ZaraConfig


def _write(tmp_path, body: str):
    path = tmp_path / "config.toml"
    path.write_text(body)
    return path


def test_context_and_skills_defaults_are_available(tmp_path):
    config = ZaraConfig(str(_write(tmp_path, '[tts]\nprovider = "qwen3"\n')))

    assert config.get_section("context") == {}
    assert config.get_section("skills") == {}


@pytest.mark.parametrize(
    ("setting", "match"),
    [
        ('strategy = "drop-randomly"', "context.strategy"),
        ("max_tokens = 0", "context.max_tokens"),
        ("max_tokens = false", "context.max_tokens"),
        ("preserve_recent_turns = -1", "context.preserve_recent_turns"),
        ("preserve_recent_turns = false", "context.preserve_recent_turns"),
        ("summary_max_tokens = 0", "context.summary_max_tokens"),
        ("skill_max_tokens = 0", "context.skill_max_tokens"),
    ],
)
def test_context_config_is_validated(tmp_path, setting, match):
    path = _write(
        tmp_path,
        f'[tts]\nprovider = "qwen3"\n\n[context]\n{setting}\n',
    )

    with pytest.raises(ConfigError, match=match):
        ZaraConfig(str(path))


def test_context_sub_budgets_must_fit_total_budget(tmp_path):
    path = _write(
        tmp_path,
        '[tts]\nprovider = "qwen3"\n\n'
        '[context]\nmax_tokens = 100\nsummary_max_tokens = 101\n',
    )

    with pytest.raises(ConfigError, match="summary_max_tokens"):
        ZaraConfig(str(path))


@pytest.mark.parametrize(
    ("setting", "match"),
    [
        ('enabled = "yes"', "skills.enabled"),
        ('search_paths = "~/skills"', "skills.search_paths"),
        ('search_paths = [""]', "skills.search_paths"),
        ('search_paths = [1]', "skills.search_paths"),
    ],
)
def test_skill_config_is_validated(tmp_path, setting, match):
    path = _write(
        tmp_path,
        f'[tts]\nprovider = "qwen3"\n\n[skills]\n{setting}\n',
    )

    with pytest.raises(ConfigError, match=match):
        ZaraConfig(str(path))
