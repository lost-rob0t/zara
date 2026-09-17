from __future__ import annotations

from pathlib import Path

from zara.agent.skills import SkillRegistry


ROOT = Path(__file__).resolve().parents[1]


def test_existing_repository_skills_are_first_class_runtime_skills():
    registry = SkillRegistry([ROOT / "skills"])
    discovered = registry.discover()
    names = {skill.name for skill in discovered}

    assert {"rage", "zara-android-release"} <= names
    for skill in discovered:
        assert skill.description.strip()
        assert skill.path.name == "SKILL.md"


def test_every_builtin_skill_has_browser_metadata_without_loading_instruction_body():
    registry = SkillRegistry([ROOT / "skills"])
    registry.discover()

    entries = registry.browser_entries()
    assert entries
    assert {entry.name for entry in entries} == {skill.name for skill in registry.skills}
    assert all(entry.description.strip() for entry in entries)
    assert all(not hasattr(entry, "body") for entry in entries)
