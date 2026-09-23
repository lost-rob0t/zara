from __future__ import annotations

from dataclasses import replace

import pytest

from zara.org_automation import (
    AutomationDependency,
    AutomationInspection,
    AutomationSymbol,
    OrgAutomationHeading,
    compile_automation,
    inspect_automation,
)
from zara.org_automation_run import (
    AutomationPlanError,
    fence_automation_generation,
    plan_automation_run,
)


def heading(
    title: str,
    properties: dict[str, str] | None = None,
    *children: OrgAutomationHeading,
    source: str = "/worktrees/custom/org/automation.org",
) -> OrgAutomationHeading:
    return OrgAutomationHeading(
        title=title,
        properties=properties or {},
        children=tuple(children),
        source=source,
    )


def recipe_heading(*, workspace: str = "main", enabled: str = "t") -> OrgAutomationHeading:
    return heading(
        "Sync notes after editing",
        {
            "ID": "recipe-5f",
            "ZARA_AUTOMATION": "t",
            "ZARA_ENABLED": enabled,
        },
        heading("WHEN", {"EVENT": "org.file.saved", "WORKSPACE": workspace}),
        heading(
            "IF",
            {"PREDICATE": "zara:condition/workspace-clean", "WORKSPACE": workspace},
        ),
        heading(
            "THEN",
            {"COMMAND": "org.sync.push", "WORKSPACE": workspace},
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


def ready_recipe_and_inspection(*, workspace: str = "main"):
    recipe = compile_automation(recipe_heading(workspace=workspace))
    inspection = inspect_automation(recipe, symbol_catalog().get, platform="android")
    assert inspection.status == "ready"
    return recipe, inspection


def plan(*, run_id: str = "run-1", event_id: str = "event-1", generation: int = 7):
    recipe, inspection = ready_recipe_and_inspection()
    return plan_automation_run(
        recipe,
        inspection,
        run_id=run_id,
        trigger_event_id=event_id,
        trigger_generation=generation,
        principal="user:local",
        workspace="main",
        platform="android",
        deadline_unix_ms=1_800_000_000_000,
        cancellation_token="cancel-1",
    )


def test_run_plan_is_inert_correlated_and_keeps_runtime_authority_external() -> None:
    run = plan()

    assert run.automation_id == "recipe-5f"
    assert len(run.definition_hash) == 64
    assert run.run_id == "run-1"
    assert run.trigger_event_id == "event-1"
    assert run.trigger_generation == 7
    assert run.principal == "user:local"
    assert run.workspace == "main"
    assert run.platform == "android"
    assert run.deadline_unix_ms == 1_800_000_000_000
    assert run.cancellation_token == "cancel-1"
    assert [item.role for item in run.invocations] == ["WHEN", "IF", "THEN"]
    assert [item.symbol for item in run.invocations] == [
        "org.file.saved",
        "zara:condition/workspace-clean",
        "org.sync.push",
    ]
    assert run.invocations[-1].required_capabilities == ("org.sync.write",)
    assert dict(run.invocations[-1].arguments) == {"WORKSPACE": "main"}
    assert not hasattr(run, "execute")
    assert not hasattr(run, "granted_capabilities")
    assert "Documents/Notes/org" not in repr(run)


def test_replay_key_is_stable_per_definition_and_trigger_not_per_attempt() -> None:
    first = plan(run_id="run-a", event_id="event-44", generation=3)
    retry = plan(run_id="run-b", event_id="event-44", generation=3)
    next_event = plan(run_id="run-c", event_id="event-45", generation=4)

    changed_recipe, changed_inspection = ready_recipe_and_inspection(workspace="archive")
    changed_definition = plan_automation_run(
        changed_recipe,
        changed_inspection,
        run_id="run-d",
        trigger_event_id="event-44",
        trigger_generation=3,
        principal="user:local",
        workspace="main",
        platform="android",
        deadline_unix_ms=1_800_000_000_000,
        cancellation_token="cancel-2",
    )

    assert first.replay_key == retry.replay_key
    assert first.replay_key != next_event.replay_key
    assert first.replay_key != changed_definition.replay_key


def test_non_ready_or_disabled_recipe_cannot_be_planned() -> None:
    recipe, inspection = ready_recipe_and_inspection()
    degraded = replace(inspection, status="degraded", reason="missing_symbol:org.sync.push")

    with pytest.raises(AutomationPlanError):
        plan_automation_run(
            recipe,
            degraded,
            run_id="run-1",
            trigger_event_id="event-1",
            trigger_generation=1,
            principal="user:local",
            workspace="main",
            platform="android",
            deadline_unix_ms=1_800_000_000_000,
            cancellation_token="cancel-1",
        )

    disabled_recipe = compile_automation(recipe_heading(enabled="nil"))
    disabled = inspect_automation(disabled_recipe, symbol_catalog().get, platform="android")
    assert disabled.status == "disabled"
    with pytest.raises(AutomationPlanError):
        plan_automation_run(
            disabled_recipe,
            disabled,
            run_id="run-2",
            trigger_event_id="event-2",
            trigger_generation=2,
            principal="user:local",
            workspace="main",
            platform="android",
            deadline_unix_ms=1_800_000_000_000,
            cancellation_token="cancel-2",
        )


def test_plan_rejects_stale_or_forged_dependency_projection() -> None:
    recipe, inspection = ready_recipe_and_inspection()
    forged = AutomationInspection(
        status="ready",
        dependencies=(
            AutomationDependency(
                role="WHEN",
                symbol="org.todo.changed",
                kind="event",
                owner="org-core",
                capabilities=(),
            ),
        )
        + inspection.dependencies[1:],
    )

    with pytest.raises(AutomationPlanError):
        plan_automation_run(
            recipe,
            forged,
            run_id="run-1",
            trigger_event_id="event-1",
            trigger_generation=1,
            principal="user:local",
            workspace="main",
            platform="android",
            deadline_unix_ms=1_800_000_000_000,
            cancellation_token="cancel-1",
        )


def test_generation_fence_blocks_changed_or_disabled_definition_before_mutation() -> None:
    run = plan()
    current = compile_automation(recipe_heading())
    changed = compile_automation(recipe_heading(workspace="archive"))
    disabled = compile_automation(recipe_heading(enabled="nil"))

    allowed = fence_automation_generation(run, current)
    stale = fence_automation_generation(run, changed)
    blocked = fence_automation_generation(run, disabled)

    assert allowed.allowed is True
    assert allowed.status == "current"
    assert stale.allowed is False
    assert stale.status == "stale_definition"
    assert blocked.allowed is False
    assert blocked.status == "disabled"


def test_generation_fence_rejects_different_automation_identity() -> None:
    run = plan()
    other = recipe_heading()
    other = OrgAutomationHeading(
        title=other.title,
        properties={**other.properties, "ID": "recipe-other"},
        children=other.children,
        source=other.source,
    )

    decision = fence_automation_generation(run, compile_automation(other))

    assert decision.allowed is False
    assert decision.status == "automation_mismatch"


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("run_id", ""),
        ("run_id", "run\n2"),
        ("trigger_event_id", "event 1"),
        ("principal", "user\x00root"),
        ("workspace", "main\nother"),
        ("cancellation_token", "cancel\t1"),
    ],
)
def test_run_correlation_fields_fail_closed(field: str, value: str) -> None:
    recipe, inspection = ready_recipe_and_inspection()
    kwargs = {
        "run_id": "run-1",
        "trigger_event_id": "event-1",
        "trigger_generation": 1,
        "principal": "user:local",
        "workspace": "main",
        "platform": "android",
        "deadline_unix_ms": 1_800_000_000_000,
        "cancellation_token": "cancel-1",
    }
    kwargs[field] = value

    with pytest.raises(AutomationPlanError):
        plan_automation_run(recipe, inspection, **kwargs)


@pytest.mark.parametrize("generation", [-1, True, 1.5])
def test_trigger_generation_must_be_non_negative_integer(generation: object) -> None:
    recipe, inspection = ready_recipe_and_inspection()

    with pytest.raises(AutomationPlanError):
        plan_automation_run(
            recipe,
            inspection,
            run_id="run-1",
            trigger_event_id="event-1",
            trigger_generation=generation,
            principal="user:local",
            workspace="main",
            platform="android",
            deadline_unix_ms=1_800_000_000_000,
            cancellation_token="cancel-1",
        )


@pytest.mark.parametrize("deadline", [0, -1, True, 1.5])
def test_deadline_must_be_positive_integer(deadline: object) -> None:
    recipe, inspection = ready_recipe_and_inspection()

    with pytest.raises(AutomationPlanError):
        plan_automation_run(
            recipe,
            inspection,
            run_id="run-1",
            trigger_event_id="event-1",
            trigger_generation=1,
            principal="user:local",
            workspace="main",
            platform="android",
            deadline_unix_ms=deadline,
            cancellation_token="cancel-1",
        )
