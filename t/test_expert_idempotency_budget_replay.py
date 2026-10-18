"""Durable accounting when completed expert work exceeds the admitted budget."""

from __future__ import annotations

import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pytest

from zara.database import DatabaseManager
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertBudgetExceededError,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/budget-replay"
PRINCIPAL = "user:budget-replay"
WORKSPACE = "workspace:budget-replay"
IDEMPOTENCY_KEY = "idempotency:budget-replay"
REQUEST_ID = "request:budget-replay"
RECEIPT = {
    "effect_id": "effect:budget-replay:write",
    "verified_outcome_ref": "zara.verified-outcome/v2:1:outcome:budget-write",
}


@dataclass
class OverBudgetHandler:
    delay_seconds: float = 0.0
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "route.diagnose"
        assert payload == {"symptom_id": "budget"}
        self.calls += 1
        if self.delay_seconds:
            time.sleep(self.delay_seconds)
        return {
            "verdict": "succeeded",
            "data": {"summary": "executed work exceeded admitted budget"},
            "evidence_refs": ["evidence:budget-replay:v1"],
            "usage": {"model_calls": 2},
            "effect_receipts": [RECEIPT],
        }


@dataclass
class CountingHandler:
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": "replacement must never run"},
            "evidence_refs": [],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


def _descriptor(*, timeout_ms: int = 30_000) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:budget-replay-v1",
            "name": "Budget Replay Fixture",
            "description": "Post-dispatch budget rejection and durable replay fixture.",
            "source_reference": "t/test_expert_idempotency_budget_replay.py",
            "reasoning_kind": "hybrid",
            "operations": [
                {
                    "operation_id": "route.diagnose",
                    "input_schema": {
                        "fields": [
                            {"name": "symptom_id", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {"name": "summary", "type": "string", "required": True}
                        ]
                    },
                }
            ],
            "applicability": {"keywords": ["budget", "restart"]},
            "required_capabilities": [],
            "possible_effects": ["model_inference"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {"timeout_ms": timeout_ms, "max_model_calls": 2},
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _activate(
    database: DatabaseManager,
    handler: Any,
    *,
    timeout_ms: int = 30_000,
) -> tuple[ExpertRegistry, Any]:
    registry = ExpertRegistry(database=database)
    registry.reload([(_descriptor(timeout_ms=timeout_ms), handler)])
    handle, _ = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
    return registry, handle


def _invoke(
    registry: ExpertRegistry,
    handle: Any,
    *,
    max_model_calls: int,
    timeout_ms: int = 30_000,
):
    return registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "budget"},
        limits=ExpertLimits(
            timeout_ms=timeout_ms,
            max_model_calls=max_model_calls,
        ),
        idempotency_key=IDEMPOTENCY_KEY,
        request_id=REQUEST_ID,
    )


def _assert_preserved_budget_terminal(
    path: Path,
    *,
    timeout_ms: int,
) -> None:
    restarted_database = DatabaseManager(path)
    replacement = CountingHandler()
    restarted_registry, restarted_handle = _activate(
        restarted_database,
        replacement,
        timeout_ms=timeout_ms,
    )

    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(
            restarted_registry,
            restarted_handle,
            max_model_calls=0,
            timeout_ms=timeout_ms,
        )

    replay = _invoke(
        restarted_registry,
        restarted_handle,
        max_model_calls=2,
        timeout_ms=timeout_ms,
    )
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.BUDGET_EXCEEDED
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 2}
    assert replay.effect_receipts == (RECEIPT,)
    assert replacement.calls == 0, "post-dispatch budget replay must never redispatch"
    restarted_database.close()


def test_over_budget_completion_preserves_usage_and_effect_receipt_across_restart(
    tmp_path: Path,
) -> None:
    path = tmp_path / "budget-replay.db"
    database = DatabaseManager(path)
    handler = OverBudgetHandler()
    registry, handle = _activate(database, handler)

    with pytest.raises(
        ExpertBudgetExceededError,
        match="expert aggregate usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(registry, handle, max_model_calls=1)

    assert handler.calls == 1
    database.close()
    _assert_preserved_budget_terminal(path, timeout_ms=30_000)


def test_deadline_late_over_budget_completion_keeps_observed_accounting(
    tmp_path: Path,
) -> None:
    path = tmp_path / "budget-deadline-replay.db"
    database = DatabaseManager(path)
    handler = OverBudgetHandler(delay_seconds=0.05)
    registry, handle = _activate(database, handler, timeout_ms=5)

    with pytest.raises(
        ExpertBudgetExceededError,
        match="expert aggregate usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(registry, handle, max_model_calls=1, timeout_ms=5)

    assert handler.calls == 1
    database.close()
    _assert_preserved_budget_terminal(path, timeout_ms=5)
