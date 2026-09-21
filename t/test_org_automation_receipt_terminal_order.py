from __future__ import annotations

import pytest

from zara.org_automation_receipt import (
    AutomationReceiptError,
    AutomationRunEvent,
    reduce_automation_run,
)
from zara.org_automation_run import AutomationRunPlan


def _plan() -> AutomationRunPlan:
    return AutomationRunPlan(
        automation_id="recipe-order",
        definition_hash="a" * 64,
        replay_key="b" * 64,
        run_id="run-order",
        trigger_event_id="event-order",
        trigger_generation=1,
        principal="user:local",
        workspace="main",
        platform="android",
        deadline_unix_ms=1_800_000_000_000,
        cancellation_token="cancel-order",
        invocations=(),
    )


def _event(sequence: int, kind: str, *, attempt: int = 0, result_ref: str = "") -> AutomationRunEvent:
    plan = _plan()
    return AutomationRunEvent(
        sequence=sequence,
        kind=kind,
        run_id=plan.run_id,
        replay_key=plan.replay_key,
        definition_hash=plan.definition_hash,
        timestamp_unix_ms=1_000 + sequence,
        attempt=attempt,
        result_ref=result_ref,
    )


def test_completed_receipt_requires_a_started_attempt() -> None:
    with pytest.raises(AutomationReceiptError, match="completed"):
        reduce_automation_run(
            _plan(),
            (
                _event(0, "accepted"),
                _event(1, "completed", result_ref="result:impossible-success"),
            ),
        )
