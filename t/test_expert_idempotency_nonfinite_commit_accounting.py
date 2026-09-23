"""Durable accounting when a completed expert result is not canonical JSON."""

from __future__ import annotations

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
    ExpertInvalidInputError,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/nonfinite-commit-accounting"
PRINCIPAL = "user:nonfinite-commit-accounting"
WORKSPACE = "workspace:nonfinite-commit-accounting"
IDEMPOTENCY_KEY = "idempotency:nonfinite-commit-accounting"
REQUEST_ID = "request:nonfinite-commit-accounting"
RECEIPT = {
    "effect_id": "effect:nonfinite-commit-accounting:score",
    "verified_outcome_ref": "zara.verified-outcome/v2:1:outcome:score-1",
}


@dataclass
class NonFiniteModelHandler:
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "score.compute"
        assert payload == {"subject": "fixture"}
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"score": float("nan")},
            "evidence_refs": ["evidence:nonfinite-commit-accounting:v1"],
            "usage": {"model_calls": 1},
            "effect_receipts": [RECEIPT],
        }


@dataclass
class CountingReplacementHandler:
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"score": 1.0},
            "evidence_refs": ["evidence:replacement:v1"],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:nonfinite-commit-accounting-v1",
            "name": "Nonfinite Commit Accounting Fixture",
            "description": "Reject noncanonical terminal data without losing durable accounting.",
            "source_reference": "t/test_expert_idempotency_nonfinite_commit_accounting.py",
            "reasoning_kind": "model",
            "operations": [
                {
                    "operation_id": "score.compute",
                    "input_schema": {
                        "fields": [
                            {"name": "subject", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {"name": "score", "type": "number", "required": True}
                        ]
                    },
                }
            ],
            "applicability": {"keywords": ["restart", "accounting"]},
            "required_capabilities": [],
            "possible_effects": ["model_inference"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {"max_model_calls": 1},
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _activate(
    database: DatabaseManager,
    handler: Any,
) -> tuple[ExpertRegistry, Any]:
    registry = ExpertRegistry(database=database)
    registry.reload([(_descriptor(), handler)])
    handle, _ = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
    return registry, handle


def _invoke(
    registry: ExpertRegistry,
    handle: Any,
    *,
    max_model_calls: int,
):
    return registry.invoke(
        handle,
        "score.compute",
        {"subject": "fixture"},
        limits=ExpertLimits(max_model_calls=max_model_calls),
        idempotency_key=IDEMPOTENCY_KEY,
        request_id=REQUEST_ID,
    )


def test_nonfinite_terminal_preserves_known_usage_and_effect_receipt_across_restart(
    tmp_path: Path,
) -> None:
    path = tmp_path / "nonfinite-terminal.db"
    database = DatabaseManager(path)
    handler = NonFiniteModelHandler()
    registry, handle = _activate(database, handler)

    with pytest.raises(ExpertInvalidInputError, match="canonical JSON"):
        _invoke(registry, handle, max_model_calls=1)

    assert handler.calls == 1
    database.close()

    restarted_database = DatabaseManager(path)
    replacement = CountingReplacementHandler()
    restarted_registry, restarted_handle = _activate(restarted_database, replacement)

    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(restarted_registry, restarted_handle, max_model_calls=0)

    assert replacement.calls == 0, "known prior work must never be redispatched"

    replay = _invoke(restarted_registry, restarted_handle, max_model_calls=1)
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INVALID_INPUT
    assert replay.request_id == REQUEST_ID
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 1}
    assert replay.effect_receipts == (RECEIPT,)
    assert replacement.calls == 0, "fail-closed terminal replay must never redispatch"

    replay_again = _invoke(restarted_registry, restarted_handle, max_model_calls=1)
    assert replay_again.invocation_id == replay.invocation_id
    assert replay_again.request_id == replay.request_id
    assert replay_again.usage == replay.usage
    assert replay_again.effect_receipts == replay.effect_receipts
    assert replacement.calls == 0
    restarted_database.close()
