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
    path = _write_skill(
        tmp_path,
        "pdf-processing",
        description="Extract and inspect PDF documents when the user asks about PDFs.",
    )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    registry.discover()

    skill = registry.get("pdf-processing")
    assert skill is not None
    assert skill.name == "pdf-processing"
    assert skill.path == path.resolve()
    assert skill.schema_version == 1
    assert skill.body == "Follow the workflow."


def test_selects_relevant_skill_with_transitive_dependencies(tmp_path):
    _write_skill(
        tmp_path,
        "python-runtime",
        description="Zara Python runtime architecture.",
        metadata={
            "zara-domain": "python",
            "zara-language": "python",
            "zara-selectors": "python runtime .py",
            "zara-priority": "80",
            "zara-max-tokens": "100",
            "zara-paths": "zara/ t/",
        },
    )
    _write_skill(
        tmp_path,
        "prolog-modules",
        description="Zara Prolog module and predicate conventions.",
        metadata={
            "zara-domain": "prolog",
            "zara-language": "prolog",
            "zara-selectors": "prolog predicate module .pl",
            "zara-priority": "70",
            "zara-max-tokens": "100",
            "zara-paths": "kb/ modules/",
        },
    )
    _write_skill(
        tmp_path,
        "python-prolog",
        description="Python to Prolog boundary rules.",
        metadata={
            "zara-domain": "integration",
            "zara-selectors": "python prolog boundary",
            "zara-priority": "90",
            "zara-max-tokens": "100",
            "zara-dependencies": "python-runtime prolog-modules",
        },
    )

    registry = SkillRegistry([tmp_path], token_counter=lambda text: len(text.split()))
    registry.discover()
    selected = registry.select(
        "Change the Python Prolog boundary",
        source_paths=["zara/prolog_engine.py", "modules/intent_resolver.pl"],
        max_tokens=300,
    )

    assert [skill.name for skill in selected.skills] == [
        "python-prolog",
        "python-runtime",
        "prolog-modules",
    ]
    assert selected.omitted == ()


def test_duplicate_names_across_roots_fail_explicitly(tmp_path):
    first = tmp_path / "first"
    second = tmp_path / "second"
    _write_skill(first, "same-skill", description="First definition.")
    _write_skill(second, "same-skill", description="Second definition.")

    with pytest.raises(SkillConfigError, match="same-skill"):
        SkillRegistry([first, second]).discover()


def test_dependency_cycle_fails_explicitly(tmp_path):
    _write_skill(
        tmp_path,
        "skill-a",
        description="Cycle A.",
        metadata={"zara-dependencies": "skill-b", "zara-selectors": "cycle"},
    )
    _write_skill(
        tmp_path,
        "skill-b",
        description="Cycle B.",
        metadata={"zara-dependencies": "skill-a", "zara-selectors": "cycle"},
    )

    with pytest.raises(SkillConfigError, match="cycle"):
        SkillRegistry([tmp_path]).discover()


def test_selected_conflicts_fail_instead_of_silently_winning(tmp_path):
    _write_skill(
        tmp_path,
        "alpha-skill",
        description="Alpha conflict workflow.",
        metadata={"zara-selectors": "conflict-demo", "zara-conflicts": "beta-skill"},
    )
    _write_skill(
        tmp_path,
        "beta-skill",
        description="Beta conflict workflow.",
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


def test_allowed_tools_is_context_metadata_not_runtime_authority(tmp_path):
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
    assert not hasattr(skill, "granted_capabilities")


def test_skill_browser_projection_contains_safe_metadata_but_not_instruction_body(tmp_path):
    path = _write_skill(
        tmp_path,
        "android-tools",
        description="Use typed Android Prolog and LLM phone capabilities.",
        metadata={
            "zara-schema": "1",
            "zara-domain": "android",
            "zara-language": "prolog",
            "zara-selectors": "android sms contacts flashlight wifi location maps",
            "zara-priority": "90",
            "zara-max-tokens": "500",
            "zara-dependencies": "",
            "zara-conflicts": "",
            "zara-always-on": "false",
        },
        body="Private operational instructions stay in selected model context, not browser metadata.",
        allowed_tools="android_sms_compose android_contacts_list android_location_current",
    )

    registry = SkillRegistry([tmp_path])
    registry.discover()
    entries = registry.browser_entries()

    assert len(entries) == 1
    entry = entries[0]
    assert entry.name == "android-tools"
    assert entry.description.startswith("Use typed Android")
    assert entry.source == str(path.resolve())
    assert entry.domain == "android"
    assert entry.language == "prolog"
    assert entry.priority == 90
    assert entry.max_tokens == 500
    assert entry.always_on is False
    assert entry.allowed_tools == "android_sms_compose android_contacts_list android_location_current"
    assert not hasattr(entry, "body")
    assert "Private operational instructions" not in repr(entry)
