from __future__ import annotations

import pathlib

import pytest

from zara.prolog_engine import PrologEngine
from zara.runtime.frames import (
    FilledSlot,
    FrameStatus,
    IntentFrame,
    RefValue,
    SlotOrigin,
    TextValue,
)
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

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

_ENGINE = None


def get_engine() -> PrologEngine:
    global _ENGINE
    if _ENGINE is not None:
        return _ENGINE
    engine = PrologEngine()
    engine.consult(REPO_ROOT / "kb" / "capabilities.pl")
    engine.consult(REPO_ROOT / "modules" / "capability_plans.pl")
    _ENGINE = engine
    return _ENGINE


def open_frame(target: str) -> IntentFrame:
    return IntentFrame(
        intent_ns="app",
        intent_name="open",
        slots=(
            FilledSlot(
                name="target",
                value=RefValue(kind="app_alias", id=target),
                origin=SlotOrigin.UTTERANCE,
            ),
        ),
        status=FrameStatus.COMPLETE,
    )


ANDROID_ENV = PlanEnvironment(
    principal="alice",
    devices=(DeviceAdvertisement("droid", "alice", ("app.open", "timer.set")),),
)

DESKTOP_ENV = PlanEnvironment(
    principal="alice",
    providers=("open_desktop", "search_server", "timer_server"),
    aliases=(("open_desktop", "firefox"),),
)


def test_plan_for_frame_decodes_ready_device_plan():
    plan = get_engine().plan_for_frame(open_frame("firefox"), ANDROID_ENV)
    assert plan == ExecutionPlan(
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
        evidence=("prio(100)", "cap(app.open)", "dev(droid)"),
    )


def test_plan_for_frame_decodes_unavailable_alias_unsupported():
    plan = get_engine().plan_for_frame(open_frame("termux"), DESKTOP_ENV)
    assert plan.status is PlanStatus.UNAVAILABLE
    assert plan.reason == "alias_unsupported"
    assert plan.provider is None
    assert plan.arguments == ()


def test_plan_for_frame_decodes_denied_without_authorization():
    env = PlanEnvironment(principal="alice", providers=("admin_restart",))
    frame = IntentFrame(
        intent_ns="skill",
        intent_name="admin.restart",
        status=FrameStatus.COMPLETE,
    )
    plan = get_engine().plan_for_frame(frame, env)
    assert plan.status is PlanStatus.DENIED
    assert plan.reason == "insufficient_authorization"
    assert "requires(daemon.admin)" in plan.evidence


def test_plan_for_frame_decodes_ambiguous_with_alternatives():
    env = PlanEnvironment(
        principal="alice",
        devices=(
            DeviceAdvertisement("d1", "alice", ("app.open",)),
            DeviceAdvertisement("d2", "alice", ("app.open",)),
        ),
    )
    plan = get_engine().plan_for_frame(open_frame("firefox"), env)
    assert plan.status is PlanStatus.AMBIGUOUS
    assert plan.alternatives == ("open_app@d1", "open_app@d2")


def test_plan_for_frame_requires_complete_frame():
    open_frame_incomplete = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        status=FrameStatus.MISSING,
        missing=("duration",),
    )
    with pytest.raises(ValueError, match="complete"):
        get_engine().plan_for_frame(open_frame_incomplete, ANDROID_ENV)


def test_plan_for_frame_survives_quote_bearing_alias():
    env = PlanEnvironment(
        principal="o'brien",
        providers=("open_desktop",),
        aliases=(("open_desktop", "o'brien's app"),),
    )
    frame = open_frame("o'brien's app")
    plan = get_engine().plan_for_frame(frame, env)
    assert plan.status is PlanStatus.READY
    assert plan.provider == "open_desktop"


def test_plan_for_frame_policy_prefer_device_decodes():
    env = PlanEnvironment(
        principal="alice",
        devices=(DeviceAdvertisement("droid", "alice", ("timer.set",)),),
        providers=("timer_server",),
        policies=(PreferLocation(PlanLocation.DEVICE),),
    )
    frame = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        slots=(
            FilledSlot(
                name="duration",
                value=TextValue(text="120"),
                origin=SlotOrigin.UTTERANCE,
            ),
        ),
        status=FrameStatus.COMPLETE,
    )
    plan = get_engine().plan_for_frame(frame, env)
    assert plan.status is PlanStatus.READY
    assert plan.provider == "timer_device"
    assert plan.location is PlanLocation.DEVICE


def test_plan_for_frame_policy_prefer_named_device_decodes():
    env = PlanEnvironment(
        principal="alice",
        devices=(
            DeviceAdvertisement("d1", "alice", ("timer.set",)),
            DeviceAdvertisement("d2", "alice", ("timer.set",)),
        ),
        policies=(PreferDevice("d2"),),
    )
    frame = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        slots=(
            FilledSlot(
                name="duration",
                value=TextValue(text="120"),
                origin=SlotOrigin.UTTERANCE,
            ),
        ),
        status=FrameStatus.COMPLETE,
    )
    plan = get_engine().plan_for_frame(frame, env)
    assert plan.status is PlanStatus.READY
    assert plan.device == "d2"


def test_plan_environment_rejects_empty_principal():
    with pytest.raises(ValueError, match="principal"):
        PlanEnvironment(principal="  ")


def test_plan_environment_rejects_oversized_device_list():
    devices = tuple(
        DeviceAdvertisement(f"d{index}", "alice", ("app.open",))
        for index in range(65)
    )
    with pytest.raises(ValueError, match="bound"):
        PlanEnvironment(principal="alice", devices=devices)
