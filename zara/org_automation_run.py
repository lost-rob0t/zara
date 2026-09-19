"""Inert run planning primitives for Org-backed Zara automation.

This module does not execute commands, own a scheduler, grant capabilities, or
persist run state. It turns an already-compiled recipe plus an already-ready
projection from Zara's canonical #986 symbol registry into a bounded correlated
run envelope. The real runtime remains authoritative for principal, capability,
permission, approval, cancellation, deadline, and command execution semantics.

The generation fence is intentionally pure. Executors must call it against the
latest canonical Org projection immediately before any mutation; it cannot make
stale work safe by itself.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass

from .org_automation import (
    AutomationDependency,
    AutomationInspection,
    AutomationRecipe,
    AutomationStep,
)


_MAX_CORRELATION_LENGTH = 160
_MAX_PLATFORM_LENGTH = 48
_MAX_OWNER_LENGTH = 160
_MAX_CAPABILITY_LENGTH = 128
_MAX_CAPABILITIES = 64
_TOKEN_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.:/@-]*$")


class AutomationPlanError(ValueError):
    """A run envelope cannot be planned from the supplied inert projections."""


@dataclass(frozen=True)
class AutomationInvocation:
    """One ordered reference to the canonical programmable runtime.

    ``required_capabilities`` is descriptive only. It does not grant, acquire,
    or cache authority and must never replace the runtime's live authorization.
    """

    role: str
    symbol: str
    arguments: tuple[tuple[str, str], ...]
    owner: str
    required_capabilities: tuple[str, ...]


@dataclass(frozen=True)
class AutomationRunPlan:
    automation_id: str
    definition_hash: str
    replay_key: str
    run_id: str
    trigger_event_id: str
    trigger_generation: int
    principal: str
    workspace: str
    platform: str
    deadline_unix_ms: int
    cancellation_token: str
    invocations: tuple[AutomationInvocation, ...]


@dataclass(frozen=True)
class AutomationGenerationFence:
    status: str
    allowed: bool


def plan_automation_run(
    recipe: AutomationRecipe,
    inspection: AutomationInspection,
    *,
    run_id: str,
    trigger_event_id: str,
    trigger_generation: int,
    principal: str,
    workspace: str,
    platform: str,
    deadline_unix_ms: int,
    cancellation_token: str,
) -> AutomationRunPlan:
    """Create an inert, correlated plan without invoking any symbol.

    ``inspection`` must be the ready projection of the canonical registry for
    ``recipe``. The plan copies only descriptive owner/capability metadata and
    ordered symbolic references. Runtime authority is deliberately absent.
    """

    if not isinstance(recipe, AutomationRecipe):
        raise AutomationPlanError("recipe must be an AutomationRecipe")
    if not isinstance(inspection, AutomationInspection):
        raise AutomationPlanError("inspection must be an AutomationInspection")
    if not recipe.enabled:
        raise AutomationPlanError("disabled automation cannot be planned")
    if inspection.status != "ready":
        raise AutomationPlanError(f"automation inspection is not ready: {inspection.status}")

    run_id = _bounded_token(run_id, "run_id")
    trigger_event_id = _bounded_token(trigger_event_id, "trigger_event_id")
    principal = _bounded_token(principal, "principal")
    workspace = _bounded_token(workspace, "workspace")
    platform = _bounded_token(platform, "platform", limit=_MAX_PLATFORM_LENGTH)
    cancellation_token = _bounded_token(cancellation_token, "cancellation_token")
    trigger_generation = _non_negative_int(trigger_generation, "trigger_generation")
    deadline_unix_ms = _positive_int(deadline_unix_ms, "deadline_unix_ms")

    steps = _ordered_steps(recipe)
    dependencies = inspection.dependencies
    if len(dependencies) != len(steps):
        raise AutomationPlanError("inspection dependency count does not match recipe")

    invocations: list[AutomationInvocation] = []
    for step, dependency in zip(steps, dependencies):
        invocations.append(_project_invocation(step, dependency))

    replay_key = _replay_key(
        recipe.automation_id,
        recipe.definition_hash,
        trigger_event_id,
        trigger_generation,
    )
    return AutomationRunPlan(
        automation_id=recipe.automation_id,
        definition_hash=recipe.definition_hash,
        replay_key=replay_key,
        run_id=run_id,
        trigger_event_id=trigger_event_id,
        trigger_generation=trigger_generation,
        principal=principal,
        workspace=workspace,
        platform=platform,
        deadline_unix_ms=deadline_unix_ms,
        cancellation_token=cancellation_token,
        invocations=tuple(invocations),
    )


def fence_automation_generation(
    plan: AutomationRunPlan,
    current_recipe: AutomationRecipe,
) -> AutomationGenerationFence:
    """Compare a planned run with the latest canonical Org definition.

    ``allowed=True`` means only that the definition generation is current. It
    does not authorize an action. The canonical runtime must still perform its
    normal principal/capability/permission/approval/deadline/cancellation gates.
    """

    if not isinstance(plan, AutomationRunPlan):
        raise AutomationPlanError("plan must be an AutomationRunPlan")
    if not isinstance(current_recipe, AutomationRecipe):
        raise AutomationPlanError("current_recipe must be an AutomationRecipe")
    if current_recipe.automation_id != plan.automation_id:
        return AutomationGenerationFence(status="automation_mismatch", allowed=False)
    if not current_recipe.enabled:
        return AutomationGenerationFence(status="disabled", allowed=False)
    if current_recipe.definition_hash != plan.definition_hash:
        return AutomationGenerationFence(status="stale_definition", allowed=False)
    return AutomationGenerationFence(status="current", allowed=True)


def _ordered_steps(recipe: AutomationRecipe) -> tuple[AutomationStep, ...]:
    condition = (recipe.condition,) if recipe.condition is not None else ()
    return (recipe.trigger,) + condition + recipe.actions


def _project_invocation(
    step: AutomationStep,
    dependency: AutomationDependency,
) -> AutomationInvocation:
    if not isinstance(dependency, AutomationDependency):
        raise AutomationPlanError("inspection dependency has invalid type")
    if dependency.role != step.role or dependency.symbol != step.symbol:
        raise AutomationPlanError(
            f"inspection dependency does not match recipe step: {step.role}:{step.symbol}"
        )

    owner = _bounded_text(dependency.owner, "dependency owner", _MAX_OWNER_LENGTH, allow_empty=True)
    capabilities = _bounded_capabilities(dependency.capabilities)
    return AutomationInvocation(
        role=step.role,
        symbol=step.symbol,
        arguments=step.arguments,
        owner=owner,
        required_capabilities=capabilities,
    )


def _replay_key(
    automation_id: str,
    definition_hash: str,
    trigger_event_id: str,
    trigger_generation: int,
) -> str:
    payload = json.dumps(
        {
            "automation_id": automation_id,
            "definition_hash": definition_hash,
            "trigger_event_id": trigger_event_id,
            "trigger_generation": trigger_generation,
        },
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _bounded_capabilities(values: tuple[str, ...]) -> tuple[str, ...]:
    if not isinstance(values, tuple):
        raise AutomationPlanError("dependency capabilities must be a tuple")
    if len(values) > _MAX_CAPABILITIES:
        raise AutomationPlanError("too many dependency capabilities")
    result: list[str] = []
    for value in values:
        result.append(
            _bounded_token(
                value,
                "dependency capability",
                limit=_MAX_CAPABILITY_LENGTH,
            )
        )
    if len(result) != len(set(result)):
        raise AutomationPlanError("duplicate dependency capabilities")
    return tuple(result)


def _bounded_token(value: object, name: str, *, limit: int = _MAX_CORRELATION_LENGTH) -> str:
    value = _bounded_text(value, name, limit, allow_empty=False)
    if not _TOKEN_RE.fullmatch(value):
        raise AutomationPlanError(f"{name} must be a bounded identifier")
    return value


def _bounded_text(value: object, name: str, limit: int, *, allow_empty: bool) -> str:
    if not isinstance(value, str):
        raise AutomationPlanError(f"{name} must be a string")
    if len(value) > limit:
        raise AutomationPlanError(f"{name} exceeds the {limit}-character bound")
    if not allow_empty and not value:
        raise AutomationPlanError(f"{name} must not be empty")
    if any(ord(character) < 32 or ord(character) == 127 for character in value):
        raise AutomationPlanError(f"{name} contains a control character")
    return value


def _non_negative_int(value: object, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise AutomationPlanError(f"{name} must be a non-negative integer")
    return value


def _positive_int(value: object, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise AutomationPlanError(f"{name} must be a positive integer")
    return value


__all__ = [
    "AutomationGenerationFence",
    "AutomationInvocation",
    "AutomationPlanError",
    "AutomationRunPlan",
    "fence_automation_generation",
    "plan_automation_run",
]
