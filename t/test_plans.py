from __future__ import annotations

import dataclasses

import pytest

from zara.runtime.frames import DurationValue, RefValue, TextValue
from zara.runtime.plans import (
    DeviceAdvertisement,
    ExecutionPlan,
    PlanArgument,
    PlanEnvironment,
    PlanLocation,
    PlanSideEffect,
    PlanStatus,
    PreferDevice,
    PreferLocation,
)


def ready_plan(**overrides) -> ExecutionPlan:
    fields = dict(
        intent_ns="app",
        intent_name="open",
        provider="open_app",
        location=PlanLocation.DEVICE,
        device="droid",
        side_effect=PlanSideEffect.EXTERNAL,
        requires_auth=None,
        status=PlanStatus.READY,
        reason=None,
        alternatives=(),
        arguments=(PlanArgument("app", RefValue("app_alias", "firefox")),),
        evidence=("prio(100)",),
    )
    fields.update(overrides)
    return ExecutionPlan(**fields)


def test_ready_plan_valid():
    plan = ready_plan()
    assert plan.status is PlanStatus.READY


def test_ready_plan_requires_provider():
    with pytest.raises(ValueError, match="provider"):
        ready_plan(provider=None)


def test_ready_plan_requires_location():
    with pytest.raises(ValueError, match="location"):
        ready_plan(location=None)


def test_device_plan_requires_device():
    with pytest.raises(ValueError, match="device"):
        ready_plan(device=None)


def test_server_plan_forbids_device():
    with pytest.raises(ValueError, match="server"):
        ready_plan(location=PlanLocation.SERVER, device="droid")


def test_ready_plan_forbids_reason_and_alternatives():
    with pytest.raises(ValueError, match="reason"):
        ready_plan(reason="no_provider")
    with pytest.raises(ValueError, match="alternatives"):
        ready_plan(alternatives=("open_app@d1",))


def test_unavailable_plan_forbids_provider_selection_and_arguments():
    plan = ExecutionPlan(
        intent_ns="app",
        intent_name="open",
        provider=None,
        location=None,
        device=None,
        side_effect=PlanSideEffect.NONE,
        requires_auth=None,
        status=PlanStatus.UNAVAILABLE,
        reason="provider_unavailable",
    )
    assert plan.arguments == ()
    with pytest.raises(ValueError, match="provider"):
        ready_plan(status=PlanStatus.UNAVAILABLE, reason="provider_unavailable")
    with pytest.raises(ValueError, match="arguments"):
        ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            provider=None,
            location=None,
            device=None,
            side_effect=PlanSideEffect.NONE,
            requires_auth=None,
            status=PlanStatus.UNAVAILABLE,
            reason="provider_unavailable",
            arguments=(PlanArgument("app", RefValue("app_alias", "x")),),
        )


def test_unavailable_plan_requires_reason():
    with pytest.raises(ValueError, match="reason"):
        ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            status=PlanStatus.UNAVAILABLE,
        )


def test_denied_plan_requires_reason():
    with pytest.raises(ValueError, match="reason"):
        ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            status=PlanStatus.DENIED,
        )


def test_ambiguous_plan_requires_alternatives_and_forbids_reason():
    plan = ExecutionPlan(
        intent_ns="app",
        intent_name="open",
        status=PlanStatus.AMBIGUOUS,
        alternatives=("open_app@d1", "open_app@d2"),
    )
    assert plan.alternatives == ("open_app@d1", "open_app@d2")
    with pytest.raises(ValueError, match="alternatives"):
        ExecutionPlan(intent_ns="app", intent_name="open", status=PlanStatus.AMBIGUOUS)
    with pytest.raises(ValueError, match="reason"):
        ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            status=PlanStatus.AMBIGUOUS,
            reason="no_provider",
            alternatives=("open_app@d1",),
        )


def test_text_argument_bounded():
    oversized = TextValue(text="a" * 513)
    with pytest.raises(ValueError, match="bound"):
        ready_plan(arguments=(PlanArgument("app", oversized),))
    exactly = TextValue(text="a" * 512)
    ready_plan(arguments=(PlanArgument("app", exactly),))


def test_invalid_slot_value_rejected():
    with pytest.raises(ValueError, match="empty_id"):
        ready_plan(arguments=(PlanArgument("app", RefValue(kind="app_alias", id=" ")),))


def test_duration_argument_accepted():
    plan = ExecutionPlan(
        intent_ns="device",
        intent_name="timer.set",
        provider="timer_device",
        location=PlanLocation.DEVICE,
        device="droid",
        side_effect=PlanSideEffect.LOCAL,
        status=PlanStatus.READY,
        arguments=(PlanArgument("duration", DurationValue(seconds=120)),),
    )
    assert plan.arguments[0].value == DurationValue(seconds=120)


def test_evidence_must_be_strings():
    with pytest.raises(ValueError, match="evidence"):
        ready_plan(evidence=(100,))


def test_requires_auth_must_be_non_empty():
    with pytest.raises(ValueError, match="requires_auth"):
        ready_plan(requires_auth="  ")


def test_plans_are_frozen():
    plan = ready_plan()
    with pytest.raises(dataclasses.FrozenInstanceError):
        plan.provider = "other"


def test_environment_validates_and_bounds():
    assert PlanEnvironment(principal="alice").principal == "alice"
    with pytest.raises(ValueError, match="principal"):
        PlanEnvironment(principal="")
    with pytest.raises(ValueError, match="device_id"):
        PlanEnvironment(
            principal="alice",
            devices=(DeviceAdvertisement(" ", "alice", ("app.open",)),),
        )
    with pytest.raises(ValueError, match="capabilities"):
        PlanEnvironment(
            principal="alice",
            devices=(DeviceAdvertisement("d1", "alice", ("",)),),
        )
    with pytest.raises(ValueError, match="bound"):
        PlanEnvironment(
            principal="alice",
            devices=(
                DeviceAdvertisement(
                    "d1", "alice", tuple(f"cap{i}" for i in range(65))
                ),
            ),
        )
    with pytest.raises(ValueError, match="bound"):
        PlanEnvironment(
            principal="alice",
            providers=tuple(f"p{index}" for index in range(65)),
        )
    with pytest.raises(ValueError, match="bound"):
        PlanEnvironment(
            principal="alice",
            aliases=tuple((f"p{index}", "alias") for index in range(257)),
        )
    with pytest.raises(ValueError, match="bound"):
        PlanEnvironment(
            principal="alice",
            policies=tuple(PreferLocation(PlanLocation.SERVER) for _ in range(9)),
        )
    with pytest.raises(ValueError, match="PreferLocation"):
        PlanEnvironment(principal="alice", policies=("prefer(location(server))",))
    with pytest.raises(ValueError, match="prefer-device"):
        PlanEnvironment(principal="alice", policies=(PreferDevice("  "),))
    with pytest.raises(ValueError, match="alias"):
        PlanEnvironment(principal="alice", aliases=(("open_desktop", ""),))
