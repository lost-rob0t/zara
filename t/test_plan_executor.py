from __future__ import annotations

import pytest

from zara.runtime.frames import RefValue
from zara.runtime.plans import (
    ExecutionPlan,
    PlanArgument,
    PlanLocation,
    PlanOutcome,
    PlanOutcomeStatus,
    PlanSideEffect,
    PlanStatus,
    PlanExecutor,
)


def ready_open_plan(provider: str = "open_app") -> ExecutionPlan:
    return ExecutionPlan(
        intent_ns="app",
        intent_name="open",
        provider=provider,
        location=PlanLocation.DEVICE,
        device="droid",
        side_effect=PlanSideEffect.EXTERNAL,
        status=PlanStatus.READY,
        arguments=(PlanArgument("app", RefValue(kind="app_alias", id="firefox")),),
    )


def unavailable_plan() -> ExecutionPlan:
    return ExecutionPlan(
        intent_ns="app",
        intent_name="open",
        status=PlanStatus.UNAVAILABLE,
        reason="provider_unavailable",
    )


class RecordingAdapter:
    def __init__(self) -> None:
        self.calls: list[ExecutionPlan] = []

    def __call__(self, plan: ExecutionPlan) -> str:
        self.calls.append(plan)
        return f"ran {plan.provider}"


def test_executes_ready_plan_once():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    outcome = executor.execute(ready_open_plan(), "req-1")
    assert outcome == PlanOutcome(
        status=PlanOutcomeStatus.EXECUTED, detail="ok", response="ran open_app"
    )
    assert len(adapter.calls) == 1


def test_adapter_receives_typed_values_not_strings():
    seen: dict[str, object] = {}

    def adapter(plan: ExecutionPlan) -> str:
        seen["argument"] = plan.arguments[0].value
        return "ok"

    executor = PlanExecutor({"open_app": adapter})
    executor.execute(ready_open_plan(), "req-1")
    assert isinstance(seen["argument"], RefValue)
    assert seen["argument"] == RefValue(kind="app_alias", id="firefox")


def test_replay_same_request_id_does_not_reexecute():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    plan = ready_open_plan()
    assert executor.execute(plan, "req-1").status is PlanOutcomeStatus.EXECUTED
    outcome = executor.execute(plan, "req-1")
    assert outcome.status is PlanOutcomeStatus.REPLAYED
    assert outcome.detail == "replay"
    assert len(adapter.calls) == 1


def test_new_request_id_executes_again():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    plan = ready_open_plan()
    executor.execute(plan, "req-1")
    assert executor.execute(plan, "req-2").status is PlanOutcomeStatus.EXECUTED
    assert len(adapter.calls) == 2


def test_refuses_unregistered_provider():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    outcome = executor.execute(ready_open_plan(provider="open_ghost"), "req-1")
    assert outcome.status is PlanOutcomeStatus.REFUSED
    assert outcome.detail == "unknown_provider"
    assert adapter.calls == []


def test_refuses_non_ready_plan():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    outcome = executor.execute(unavailable_plan(), "req-1")
    assert outcome.status is PlanOutcomeStatus.REFUSED
    assert outcome.detail == "plan_not_ready"
    assert adapter.calls == []


def test_refused_plans_do_not_consume_request_id():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter})
    executor.execute(unavailable_plan(), "req-1")
    outcome = executor.execute(ready_open_plan(), "req-1")
    assert outcome.status is PlanOutcomeStatus.EXECUTED


def test_adapter_failure_is_typed_refusal():
    def failing_adapter(plan: ExecutionPlan) -> str:
        raise RuntimeError("device gone")

    executor = PlanExecutor({"open_app": failing_adapter})
    outcome = executor.execute(ready_open_plan(), "req-1")
    assert outcome.status is PlanOutcomeStatus.REFUSED
    assert outcome.detail == "adapter_failed"
    assert "device gone" in outcome.response


def test_replay_window_bounded():
    adapter = RecordingAdapter()
    executor = PlanExecutor({"open_app": adapter}, max_tracked=2)
    plan = ready_open_plan()
    executor.execute(plan, "req-1")
    executor.execute(plan, "req-2")
    executor.execute(plan, "req-3")
    assert len(adapter.calls) == 3
    replayed = executor.execute(plan, "req-1")
    assert replayed.status is PlanOutcomeStatus.EXECUTED
    assert len(adapter.calls) == 4


def test_request_id_required():
    executor = PlanExecutor({"open_app": RecordingAdapter()})
    with pytest.raises(ValueError, match="request_id"):
        executor.execute(ready_open_plan(), " ")
    with pytest.raises(ValueError, match="request_id"):
        executor.execute(ready_open_plan(), "")


def test_non_execution_plan_rejected():
    executor = PlanExecutor({"open_app": RecordingAdapter()})
    with pytest.raises(TypeError, match="ExecutionPlan"):
        executor.execute("open firefox", "req-1")


def test_adapter_must_be_callable():
    with pytest.raises(TypeError, match="callable"):
        PlanExecutor({"open_app": "not-callable"})
