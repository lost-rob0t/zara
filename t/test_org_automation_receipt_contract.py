from __future__ import annotations

from dataclasses import replace

import pytest

from zara.org_automation_receipt import (
    AutomationReceiptError,
    AutomationRunEvent,
    reduce_automation_run,
)
from zara.org_automation_run import AutomationRunPlan


def run_plan() -> AutomationRunPlan:
    return AutomationRunPlan(
        automation_id="recipe-5f",
        definition_hash="a" * 64,
        replay_key="b" * 64,
        run_id="run-1",
        trigger_event_id="event-1",
        trigger_generation=7,
        principal="user:local",
        workspace="main",
        platform="android",
        deadline_unix_ms=1_800_000_000_000,
        cancellation_token="cancel-1",
        invocations=(),
    )


def event(
    sequence: int,
    kind: str,
    *,
    attempt: int = 0,
    timestamp_unix_ms: int = 1_000,
    retry_at_unix_ms: int | None = None,
    result_ref: str = "",
    error_kind: str = "",
) -> AutomationRunEvent:
    plan = run_plan()
    return AutomationRunEvent(
        sequence=sequence,
        kind=kind,
        run_id=plan.run_id,
        replay_key=plan.replay_key,
        definition_hash=plan.definition_hash,
        timestamp_unix_ms=timestamp_unix_ms,
        attempt=attempt,
        retry_at_unix_ms=retry_at_unix_ms,
        result_ref=result_ref,
        error_kind=error_kind,
    )


def test_empty_event_stream_is_planned_and_rebuildable() -> None:
    projection = reduce_automation_run(run_plan(), ())

    assert projection.automation_id == "recipe-5f"
    assert projection.status == "planned"
    assert projection.attempts == 0
    assert projection.last_sequence is None
    assert projection.terminal_sequence is None
    assert projection.retry_at_unix_ms is None
    assert projection.result_ref == ""
    assert projection.error_kind == ""
    assert not hasattr(projection, "execute")
    assert not hasattr(projection, "granted_capabilities")


def test_completed_run_reduces_to_one_inspectable_terminal_receipt() -> None:
    projection = reduce_automation_run(
        run_plan(),
        (
            event(0, "accepted"),
            event(1, "started", attempt=1, timestamp_unix_ms=1_010),
            event(
                2,
                "completed",
                attempt=1,
                timestamp_unix_ms=1_020,
                result_ref="result:bounded-42",
            ),
        ),
    )

    assert projection.status == "completed"
    assert projection.attempts == 1
    assert projection.last_sequence == 2
    assert projection.terminal_sequence == 2
    assert projection.retry_at_unix_ms is None
    assert projection.result_ref == "result:bounded-42"
    assert projection.error_kind == ""


def test_retry_backoff_is_explicit_and_next_attempt_is_monotonic() -> None:
    projection = reduce_automation_run(
        run_plan(),
        (
            event(0, "accepted"),
            event(1, "started", attempt=1, timestamp_unix_ms=1_010),
            event(
                2,
                "retry_scheduled",
                attempt=1,
                timestamp_unix_ms=1_020,
                retry_at_unix_ms=2_000,
                error_kind="transient_error",
            ),
            event(3, "started", attempt=2, timestamp_unix_ms=2_000),
            event(
                4,
                "completed",
                attempt=2,
                timestamp_unix_ms=2_050,
                result_ref="result:retry-ok",
            ),
        ),
    )

    assert projection.status == "completed"
    assert projection.attempts == 2
    assert projection.retry_at_unix_ms is None
    assert projection.result_ref == "result:retry-ok"


def test_conflicting_or_late_terminal_events_fail_closed() -> None:
    completed = (
        event(0, "accepted"),
        event(1, "started", attempt=1),
        event(2, "completed", attempt=1, result_ref="result:ok"),
    )

    with pytest.raises(AutomationReceiptError, match="terminal"):
        reduce_automation_run(run_plan(), completed + (event(3, "failed", attempt=1, error_kind="late"),))

    with pytest.raises(AutomationReceiptError, match="terminal"):
        reduce_automation_run(run_plan(), completed + (event(3, "started", attempt=2),))


@pytest.mark.parametrize(
    "mutated",
    [
        replace(event(0, "accepted"), run_id="run-other"),
        replace(event(0, "accepted"), replay_key="c" * 64),
        replace(event(0, "accepted"), definition_hash="d" * 64),
    ],
)
def test_stale_or_foreign_run_identity_is_rejected(mutated: AutomationRunEvent) -> None:
    with pytest.raises(AutomationReceiptError, match="identity"):
        reduce_automation_run(run_plan(), (mutated,))


def test_event_sequence_is_exact_and_replay_duplicates_do_not_double_apply() -> None:
    with pytest.raises(AutomationReceiptError, match="sequence"):
        reduce_automation_run(
            run_plan(),
            (
                event(0, "accepted"),
                event(2, "started", attempt=1),
            ),
        )

    with pytest.raises(AutomationReceiptError, match="sequence"):
        reduce_automation_run(
            run_plan(),
            (
                event(0, "accepted"),
                event(0, "accepted"),
            ),
        )


def test_retry_requires_future_backoff_and_new_attempt() -> None:
    bad_backoff = (
        event(0, "accepted"),
        event(1, "started", attempt=1, timestamp_unix_ms=1_000),
        event(
            2,
            "retry_scheduled",
            attempt=1,
            timestamp_unix_ms=1_100,
            retry_at_unix_ms=1_100,
            error_kind="transient_error",
        ),
    )
    with pytest.raises(AutomationReceiptError, match="retry"):
        reduce_automation_run(run_plan(), bad_backoff)

    repeated_attempt = (
        event(0, "accepted"),
        event(1, "started", attempt=1, timestamp_unix_ms=1_000),
        event(
            2,
            "retry_scheduled",
            attempt=1,
            timestamp_unix_ms=1_100,
            retry_at_unix_ms=2_000,
            error_kind="transient_error",
        ),
        event(3, "started", attempt=1, timestamp_unix_ms=2_000),
    )
    with pytest.raises(AutomationReceiptError, match="attempt"):
        reduce_automation_run(run_plan(), repeated_attempt)


@pytest.mark.parametrize(
    "bad_event",
    [
        event(0, "mystery"),
        replace(event(0, "accepted"), result_ref="raw\nprovider-payload"),
        replace(event(0, "accepted"), error_kind="provider error with spaces"),
        replace(event(0, "accepted"), attempt=-1),
        replace(event(0, "accepted"), timestamp_unix_ms=0),
    ],
)
def test_receipt_boundary_rejects_unbounded_or_ambiguous_event_data(
    bad_event: AutomationRunEvent,
) -> None:
    with pytest.raises(AutomationReceiptError):
        reduce_automation_run(run_plan(), (bad_event,))


def test_completed_and_failed_payload_shapes_are_closed() -> None:
    with pytest.raises(AutomationReceiptError, match="completed"):
        reduce_automation_run(
            run_plan(),
            (
                event(0, "accepted"),
                event(1, "completed", result_ref="", attempt=0),
            ),
        )

    failed = reduce_automation_run(
        run_plan(),
        (
            event(0, "accepted"),
            event(1, "failed", error_kind="runtime_error", attempt=0),
        ),
    )
    assert failed.status == "failed"
    assert failed.error_kind == "runtime_error"
    assert failed.result_ref == ""


def test_completed_result_must_be_a_host_owned_reference_not_secret_shaped_text() -> None:
    with pytest.raises(AutomationReceiptError, match="result_ref"):
        reduce_automation_run(
            run_plan(),
            (
                event(0, "accepted"),
                event(1, "started", attempt=1),
                event(
                    2,
                    "completed",
                    attempt=1,
                    result_ref="sk-live-provider-secret",
                ),
            ),
        )
