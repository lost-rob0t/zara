"""RuntimeHost wiring for the api_service plan execution service (issue #158)."""

import asyncio

import pytest

from zara.runtime.api_service import PlanOutcomeStatus
from zara.runtime.frames import (
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    RefValue,
    SlotOrigin,
    TextValue,
)
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.runtime.plans import (
    ExecutionPlan,
    PlanArgument,
    PlanLocation,
    PlanSideEffect,
    PlanStatus,
)

from test_runtime_host import ImmediateBackend


class FakeConfig:
    def __init__(self, *, enabled: bool, disabled_providers=()) -> None:
        self._api_service = {
            "enabled": enabled,
            "disabled_providers": tuple(disabled_providers),
        }

    def get_api_service_config(self):
        return self._api_service


def complete_frame(intent_ns: str, intent_name: str, *slots) -> IntentFrame:
    return IntentFrame(
        intent_ns=intent_ns,
        intent_name=intent_name,
        slots=tuple(slots),
        status=FrameStatus.COMPLETE,
    )


def start_host(config) -> RuntimeHost:
    host = RuntimeHost(ImmediateBackend, config=config)
    host.start().result(timeout=10)
    assert host.state is RuntimeHostState.RUNNING
    return host


@pytest.fixture(autouse=True)
def isolated_user_config(tmp_path, monkeypatch):
    config_home = tmp_path / "config"
    (config_home / "zarathushtra").mkdir(parents=True)
    monkeypatch.setenv("XDG_CONFIG_HOME", str(config_home))


def test_enabled_host_builds_plan_service_and_executes_search():
    host = start_host(FakeConfig(enabled=True))
    try:
        service = host.plan_service
        assert service is not None
        frame = complete_frame(
            "web", "search",
            FilledSlot(name="query", value=TextValue(text="prolog test"),
                       origin=SlotOrigin.UTTERANCE),
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="host-req-1")
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
    finally:
        host.shutdown().result(timeout=10)
    assert host.plan_service is None


def test_disabled_host_has_no_plan_service():
    host = start_host(FakeConfig(enabled=False))
    try:
        assert host.plan_service is None
    finally:
        host.shutdown().result(timeout=10)


def test_default_config_disables_api_service():
    host = start_host(FakeConfig(enabled=False))
    try:
        assert host.plan_service is None
    finally:
        host.shutdown().result(timeout=10)


def test_failed_api_service_startup_keeps_host_healthy():
    class BrokenConfig(FakeConfig):
        def get_api_service_config(self):
            raise ValueError("malformed [api_service] configuration")

    host = RuntimeHost(ImmediateBackend, config=BrokenConfig(enabled=True))
    host.start().result(timeout=10)
    try:
        assert host.state is RuntimeHostState.RUNNING
        assert host.plan_service is None
    finally:
        host.shutdown().result(timeout=10)


def test_headless_host_never_becomes_desktop_execution_target():
    host = start_host(FakeConfig(enabled=True))
    try:
        service = host.plan_service
        assert service is not None
        frame = complete_frame(
            "app", "open",
            FilledSlot(
                name="target",
                value=RefValue(kind="app_alias", id="firefox"),
                origin=SlotOrigin.UTTERANCE,
            ),
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="host-open-1")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "plan_not_ready"
        assert "unavailable" in outcome.response

        hand_built = ExecutionPlan(
            intent_ns="app",
            intent_name="open",
            provider="open_app",
            location=PlanLocation.DEVICE,
            device="phone",
            side_effect=PlanSideEffect.EXTERNAL,
            status=PlanStatus.READY,
            arguments=(PlanArgument("app", RefValue(kind="app_alias", id="firefox")),),
        )
        refused = asyncio.run(
            service.execute_plan(
                hand_built, principal="alice", request_id="host-open-2"
            )
        )
        assert refused.status is PlanOutcomeStatus.REFUSED
        assert refused.detail == "unknown_provider"
    finally:
        host.shutdown().result(timeout=10)


def test_timer_service_work_survives_across_host_reuse():
    host = start_host(FakeConfig(enabled=True))
    try:
        service = host.plan_service
        assert service is not None
        frame = complete_frame(
            "device", "timer.set",
            FilledSlot(
                name="duration",
                value=DurationValue(seconds=45),
                origin=SlotOrigin.UTTERANCE,
            ),
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="host-timer-1")
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
        assert len(service.timers.pending()) == 1
    finally:
        host.shutdown().result(timeout=10)
