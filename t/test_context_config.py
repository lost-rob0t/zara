from __future__ import annotations

import pytest

from zara.agent import AgentManager
from zara.agent.context import ContextConfig


@pytest.mark.parametrize(
    ("kwargs", "match"),
    [
        ({"strategy": "drop-randomly"}, "strategy"),
        ({"max_tokens": 0}, "max_tokens"),
        ({"max_tokens": False}, "max_tokens"),
        ({"preserve_recent_turns": -1}, "preserve_recent_turns"),
        ({"preserve_recent_turns": False}, "preserve_recent_turns"),
        ({"summary_max_tokens": 0}, "summary_max_tokens"),
        ({"skill_max_tokens": 0}, "skill_max_tokens"),
    ],
)
def test_context_config_is_validated_at_context_owner(kwargs, match):
    with pytest.raises(ValueError, match=match):
        ContextConfig(**kwargs)


def test_context_sub_budgets_must_fit_total_budget():
    with pytest.raises(ValueError, match="summary_max_tokens"):
        ContextConfig(max_tokens=100, summary_max_tokens=101, skill_max_tokens=50)

    with pytest.raises(ValueError, match="skill_max_tokens"):
        ContextConfig(max_tokens=100, summary_max_tokens=50, skill_max_tokens=101)


@pytest.mark.parametrize(
    ("config", "match"),
    [
        ({"enabled": "yes"}, "skills.enabled"),
        ({"search_paths": "~/skills"}, "skills.search_paths"),
        ({"search_paths": [""]}, "skills.search_paths"),
        ({"search_paths": [1]}, "skills.search_paths"),
    ],
)
def test_skill_search_config_is_validated_at_agent_owner(tmp_path, monkeypatch, config, match):
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path))
    manager = AgentManager.__new__(AgentManager)

    with pytest.raises(ValueError, match=match):
        manager._skill_roots(config)


def test_skill_search_roots_include_xdg_and_extra_paths(tmp_path, monkeypatch):
    xdg = tmp_path / "xdg"
    extra = tmp_path / "extra"
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg))
    manager = AgentManager.__new__(AgentManager)

    roots = manager._skill_roots({"enabled": True, "search_paths": [str(extra)]})

    assert xdg / "zarathushtra" / "skills" in roots
    assert extra in roots
