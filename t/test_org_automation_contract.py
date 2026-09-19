from __future__ import annotations

import pytest

from zara.org_automation import (
    AutomationCompileError,
    AutomationSymbol,
    OrgAutomationHeading,
    compile_automation,
    inspect_automation,
)


def heading(
    title: str,
    properties: dict[str, str] | None = None,
    *children: OrgAutomationHeading,
    source: str = "/srv/custom-org/workflows/automations.org",
) -> OrgAutomationHeading:
    return OrgAutomationHeading(
        title=title,
        properties=properties or {},
        children=tuple(children),
        source=source,
    )


def recipe_heading(*, action_workspace: str = "main") -> OrgAutomationHeading:
    return heading(
        "Sync notes after editing",
        {
            "ID": "recipe-5f",
            "ZARA_AUTOMATION": "t",
            "ZARA_ENABLED": "t",
            "OWNER_NOTE": "preserve me",
        },
        heading("WHEN", {"EVENT": "org.file.saved"}),
        heading(
            "IF",
            {"PREDICATE": "zara:condition/workspace-clean", "WORKSPACE": "main"},
        ),
        heading(
            "THEN",
            {"COMMAND": "org.sync.push", "WORKSPACE": action_workspace},
        ),
    )


def symbol_catalog() -> dict[str, AutomationSymbol]:
    return {
        "org.file.saved": AutomationSymbol(
            symbol="org.file.saved",
            kind="event",
            owner="org-core",
        ),
        "zara:condition/workspace-clean": AutomationSymbol(
            symbol="zara:condition/workspace-clean",
            kind="condition",
            owner="org-runtime",
        ),
        "org.sync.push": AutomationSymbol(
            symbol="org.sync.push",
            kind="command",
            owner="org-sync",
            capabilities=("org.sync.write",),
        ),
    }


def test_compile_ordinary_org_projection_without_path_assumptions() -> None:
    recipe = compile_automation(recipe_heading())

    assert recipe.automation_id == "recipe-5f"
    assert recipe.title == "Sync notes after editing"
    assert recipe.enabled is True
    assert recipe.source == "/srv/custom-org/workflows/automations.org"
    assert recipe.trigger.symbol == "org.file.saved"
    assert recipe.condition is not None
    assert recipe.condition.symbol == "zara:condition/workspace-clean"
    assert recipe.actions[0].symbol == "org.sync.push"
    assert dict(recipe.actions[0].arguments) == {"WORKSPACE": "main"}
    assert dict(recipe.metadata) == {"OWNER_NOTE": "preserve me"}
    assert "Documents/Notes/org" not in repr(recipe)


def test_semantic_hash_is_deterministic_but_tracks_definition_changes() -> None:
    original = recipe_heading()
    reordered = OrgAutomationHeading(
        title=original.title,
        properties={
            "OWNER_NOTE": "preserve me",
            "ZARA_ENABLED": "t",
            "ID": "recipe-5f",
            "ZARA_AUTOMATION": "t",
        },
        children=original.children,
        source=original.source,
    )

    first = compile_automation(original)
    second = compile_automation(reordered)
    changed = compile_automation(recipe_heading(action_workspace="archive"))

    assert first.definition_hash == second.definition_hash
    assert first.definition_hash != changed.definition_hash


def test_multiple_actions_are_ordered_inspectable_primitives() -> None:
    source = recipe_heading()
    with_two_actions = OrgAutomationHeading(
        title=source.title,
        properties=source.properties,
        children=source.children
        + (
            heading(
                "THEN",
                {"COMMAND": "org.todo.update", "STATE": "DONE"},
            ),
        ),
        source=source.source,
    )

    recipe = compile_automation(with_two_actions)

    assert [step.symbol for step in recipe.actions] == [
        "org.sync.push",
        "org.todo.update",
    ]
    assert dict(recipe.actions[1].arguments) == {"STATE": "DONE"}


@pytest.mark.parametrize(
    ("property_name", "property_value"),
    [
        ("SHELL", "/bin/sh -c whoami"),
        ("RAW_INTENT", "android.intent.action.VIEW"),
        ("PYTHON_EVAL", "__import__('os').system('id')"),
        ("PROLOG_GOAL", "call(X)"),
        ("KOTLIN_CLASS", "example.EscapeHatch"),
        ("EXEC", "anything"),
    ],
)
def test_synced_org_cannot_embed_ambient_execution_authority(
    property_name: str,
    property_value: str,
) -> None:
    source = recipe_heading()
    unsafe_then = heading(
        "THEN",
        {
            "COMMAND": "org.sync.push",
            "WORKSPACE": "main",
            property_name: property_value,
        },
    )
    malicious = OrgAutomationHeading(
        title=source.title,
        properties=source.properties,
        children=source.children[:-1] + (unsafe_then,),
        source=source.source,
    )

    with pytest.raises(AutomationCompileError):
        compile_automation(malicious)


@pytest.mark.parametrize(
    "symbol",
    [
        "org.sync.push --force",
        "org.sync.push\ncall(evil)",
        "",
    ],
)
def test_symbol_references_are_bounded_identifiers_not_code(symbol: str) -> None:
    source = recipe_heading()
    invalid = OrgAutomationHeading(
        title=source.title,
        properties=source.properties,
        children=source.children[:-1]
        + (heading("THEN", {"COMMAND": symbol}),),
        source=source.source,
    )

    with pytest.raises(AutomationCompileError):
        compile_automation(invalid)


def test_recipe_shape_fails_closed() -> None:
    with pytest.raises(AutomationCompileError):
        compile_automation(
            heading(
                "not a recipe",
                {"ID": "x", "ZARA_AUTOMATION": "t"},
                heading("THEN", {"COMMAND": "org.sync.push"}),
            )
        )

    with pytest.raises(AutomationCompileError):
        compile_automation(
            heading(
                "duplicate trigger",
                {"ID": "x", "ZARA_AUTOMATION": "t"},
                heading("WHEN", {"EVENT": "org.file.saved"}),
                heading("WHEN", {"EVENT": "org.todo.changed"}),
                heading("THEN", {"COMMAND": "org.sync.push"}),
            )
        )


def test_registry_projection_is_inspected_without_executing_any_symbol() -> None:
    catalog = symbol_catalog()
    calls: list[str] = []

    def describe(symbol: str) -> AutomationSymbol | None:
        calls.append(symbol)
        return catalog.get(symbol)

    inspection = inspect_automation(
        compile_automation(recipe_heading()),
        describe,
        platform="desktop",
    )

    assert inspection.status == "ready"
    assert calls == [
        "org.file.saved",
        "zara:condition/workspace-clean",
        "org.sync.push",
    ]
    assert [dependency.symbol for dependency in inspection.dependencies] == calls
    assert inspection.dependencies[-1].capabilities == ("org.sync.write",)


def test_missing_or_unloaded_symbol_degrades_without_corrupting_recipe() -> None:
    catalog = symbol_catalog()
    del catalog["org.sync.push"]
    recipe = compile_automation(recipe_heading())

    inspection = inspect_automation(recipe, catalog.get, platform="android")

    assert inspection.status == "degraded"
    assert inspection.reason == "missing_symbol:org.sync.push"
    assert recipe.actions[0].symbol == "org.sync.push"


def test_recipe_reference_only_surfaces_authority_requirements() -> None:
    inspection = inspect_automation(
        compile_automation(recipe_heading()),
        symbol_catalog().get,
        platform="android",
    )

    assert inspection.status == "ready"
    action = inspection.dependencies[-1]
    assert action.symbol == "org.sync.push"
    assert action.capabilities == ("org.sync.write",)
    assert not hasattr(inspection, "granted_capabilities")
    assert not hasattr(inspection, "execute")


def test_unavailable_symbol_reports_degraded() -> None:
    catalog = symbol_catalog()
    catalog["org.sync.push"] = AutomationSymbol(
        symbol="org.sync.push",
        kind="command",
        owner="org-sync",
        capabilities=("org.sync.write",),
        available=False,
    )

    inspection = inspect_automation(
        compile_automation(recipe_heading()),
        catalog.get,
        platform="android",
    )

    assert inspection.status == "degraded"
    assert inspection.reason == "unavailable_symbol:org.sync.push"


def test_platform_specific_symbol_reports_unsupported() -> None:
    catalog = symbol_catalog()
    catalog["org.sync.push"] = AutomationSymbol(
        symbol="org.sync.push",
        kind="command",
        owner="org-sync",
        platforms=("android",),
    )

    inspection = inspect_automation(
        compile_automation(recipe_heading()),
        catalog.get,
        platform="desktop",
    )

    assert inspection.status == "unsupported"
    assert inspection.reason == "unsupported_platform:org.sync.push:desktop"


def test_disabled_recipe_does_not_query_registry() -> None:
    source = recipe_heading()
    disabled = OrgAutomationHeading(
        title=source.title,
        properties={**source.properties, "ZARA_ENABLED": "nil"},
        children=source.children,
        source=source.source,
    )

    def should_not_run(_symbol: str) -> AutomationSymbol | None:
        raise AssertionError("disabled recipe must not resolve runtime symbols")

    inspection = inspect_automation(
        compile_automation(disabled),
        should_not_run,
        platform="desktop",
    )

    assert inspection.status == "disabled"
    assert inspection.dependencies == ()
