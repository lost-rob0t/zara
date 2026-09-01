from __future__ import annotations

from pathlib import Path

import pytest

from zara.agent.skills import (
    SkillBudgetError,
    SkillConfigError,
    SkillConflictError,
    SkillRegistry,
)


def _write_skill(
    root: Path,
    name: str,
    *,
    description: str,
    metadata: dict[str, str] | None = None,
    body: str = "Follow the workflow.",
    allowed_tools: str | None = None,
) -> Path:
    skill_dir = root / name
    skill_dir.mkdir(parents=True, exist_ok=True)
    lines = ["---", f"name: {name}", f"description: {description}"]
    if allowed_tools is not None:
        lines.append(f"allowed-tools: {allowed_tools}")
    if metadata:
        lines.append("metadata:")
        lines.extend(f"  {key}: {value!r}" for key, value in metadata.items())
    lines.extend(["---", body, ""])
    path = skill_dir / "SKILL.md"
    path.write_text("\n".join(lines))
    return path


def test_discovers_standard_agent_skill_without_zara_metadata(tmp_path):
    _write_skill(
        tmp_path,
        "pdf-processing",
        description="Extract and inspect PDF documents when the user asks about PDFs.",
    )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    registry.discover()

    skill = registry.get("pdf-processing")
    assert skill is not None
    assert skill.name == "pdf-processing"
    assert skill.description.startswith("Extract and inspect")
    assert skill.schema_version == 1
    assert skill.body == "Follow the workflow."


def test_parses_zara_metadata_and_selects_python_prolog_skills(tmp_path):
    _write_skill(
        tmp_path,
        "python-runtime",
        description="Zara Python runtime architecture. Use for Python runtime changes.",
        metadata={
            "zara-schema": "1",
            "zara-domain": "python",
            "zara-language": "python",
            "zara-selectors": "python runtime .py",
            "zara-priority": "80",
            "zara-max-tokens": "100",
            "zara-paths": "zara/ t/",
            "zara-always-on": "false",
        },
    )
    _write_skill(
        tmp_path,
        "prolog-intents",
        description="Zara Prolog intent conventions. Use for intent and predicate changes.",
        metadata={
            "zara-schema": "1",
            "zara-domain": "prolog",
            "zara-language": "prolog",
            "zara-selectors": "prolog intent predicate .pl",
            "zara-priority": "70",
            "zara-max-tokens": "100",
            "zara-paths": "kb/ modules/",
            "zara-always-on": "false",
        },
    )
    _write_skill(
        tmp_path,
        "python-prolog",
        description="Python to Prolog boundary rules. Use when changing both runtimes.",
        metadata={
            "zara-schema": "1",
            "zara-domain": "integration",
            "zara-selectors": "python prolog boundary pyswip",
            "zara-priority": "90",
            "zara-max-tokens": "100",
            "zara-dependencies": "python-runtime prolog-intents",
        },
    )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    registry.discover()
    selected = registry.select(
        "Change the Python Prolog boundary for intent results",
        source_paths=["zara/prolog_engine.py", "modules/intent_resolver.pl"],
        max_tokens=300,
    )

    assert [skill.name for skill in selected.skills] == [
        "python-prolog",
        "python-runtime",
        "prolog-intents",
    ]
    assert selected.omitted == ()


def test_duplicate_names_across_roots_fail_explicitly(tmp_path):
    first = tmp_path / "first"
    second = tmp_path / "second"
    _write_skill(first, "same-skill", description="First definition for the same skill.")
    _write_skill(second, "same-skill", description="Second definition for the same skill.")

    registry = SkillRegistry([first, second])
    with pytest.raises(SkillConfigError, match="same-skill"):
        registry.discover()


@pytest.mark.parametrize(
    ("directory", "frontmatter_name", "match"),
    [
        ("valid-dir", "Different", "name"),
        ("valid-dir", "bad--name", "name"),
    ],
)
def test_invalid_standard_names_fail(tmp_path, directory, frontmatter_name, match):
    skill_dir = tmp_path / directory
    skill_dir.mkdir()
    (skill_dir / "SKILL.md").write_text(
        f"---\nname: {frontmatter_name}\ndescription: invalid name fixture\n---\nbody\n"
    )

    with pytest.raises(SkillConfigError, match=match):
        SkillRegistry([tmp_path]).discover()


def test_unknown_zara_schema_fails(tmp_path):
    _write_skill(
        tmp_path,
        "future-skill",
        description="Future schema fixture.",
        metadata={"zara-schema": "99"},
    )

    with pytest.raises(SkillConfigError, match="schema"):
        SkillRegistry([tmp_path]).discover()


def test_dependency_cycle_fails(tmp_path):
    _write_skill(
        tmp_path,
        "skill-a",
        description="A cycle fixture selected by cycle.",
        metadata={"zara-dependencies": "skill-b", "zara-selectors": "cycle"},
    )
    _write_skill(
        tmp_path,
        "skill-b",
        description="B cycle fixture selected by cycle.",
        metadata={"zara-dependencies": "skill-a", "zara-selectors": "cycle"},
    )

    registry = SkillRegistry([tmp_path])
    with pytest.raises(SkillConfigError, match="cycle"):
        registry.discover()


def test_selected_conflicts_fail_instead_of_silently_winning(tmp_path):
    _write_skill(
        tmp_path,
        "alpha-skill",
        description="Alpha workflow for conflict-demo work.",
        metadata={"zara-selectors": "conflict-demo", "zara-conflicts": "beta-skill"},
    )
    _write_skill(
        tmp_path,
        "beta-skill",
        description="Beta workflow for conflict-demo work.",
        metadata={"zara-selectors": "conflict-demo"},
    )

    registry = SkillRegistry([tmp_path])
    registry.discover()
    with pytest.raises(SkillConflictError, match="alpha-skill"):
        registry.select("run conflict-demo", max_tokens=1000)


def test_per_skill_body_budget_fails_without_truncating_instructions(tmp_path):
    _write_skill(
        tmp_path,
        "bounded-skill",
        description="Bounded skill fixture.",
        metadata={"zara-max-tokens": "2", "zara-selectors": "bounded"},
        body="one two three",
    )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    with pytest.raises(SkillBudgetError, match="bounded-skill"):
        registry.discover()


def test_total_skill_budget_omits_lower_priority_whole_skill(tmp_path):
    for name, priority in (("high-skill", "100"), ("low-skill", "10")):
        _write_skill(
            tmp_path,
            name,
            description=f"{name} handles shared-topic tasks.",
            metadata={
                "zara-selectors": "shared-topic",
                "zara-priority": priority,
                "zara-max-tokens": "10",
            },
            body="one two three four",
        )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    registry.discover()
    selected = registry.select("shared-topic", max_tokens=4)

    assert [skill.name for skill in selected.skills] == ["high-skill"]
    assert selected.omitted == ("low-skill",)


def test_allowed_tools_is_compatibility_metadata_not_runtime_authority(tmp_path):
    _write_skill(
        tmp_path,
        "shell-helper",
        description="Shell helper workflow.",
        allowed_tools="Bash(git:*) Read",
    )

    registry = SkillRegistry([tmp_path])
    registry.discover()
    skill = registry.get("shell-helper")

    assert skill is not None
    assert skill.allowed_tools == "Bash(git:*) Read"
    assert not hasattr(skill, "approved_tools")
