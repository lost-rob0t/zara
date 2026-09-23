"""Durable fail-closed accounting when expert completion crosses its deadline."""

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
    ExpertDeadlineExceededError,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/deadline-replay"
PRINCIPAL = "user:deadline-replay"
WORKSPACE = "workspace:deadline-replay"
IDEMPOTENCY_KEY = "idempotency:deadline-replay"
REQUEST_ID = "request:deadline-replay"
RECEIPT = {
    "effect_id": "effect:deadline-replay:write",
    "verified_outcome_ref": "zara.verified-outcome/v2:1:outcome:deadline-write",
}


@dataclass
class SlowModelHandler:
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "route.diagnose"
        assert payload == {"symptom_id": "deadline"}
        self.calls += 1
        time.sleep(0.05)
        return {
            "verdict": "succeeded",
            "data": {"summary": "late completion must not become current success"},
            "evidence_refs": ["evidence:deadline-replay:v1"],
            "usage": {"model_calls": 1},
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


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:deadline-replay-v1",
            "name": "Deadline Replay Fixture",
            "description": "Deadline completion and durable replay accounting fixture.",
            "source_reference": "t/test_expert_idempotency_deadline_replay.py",
            "reasoning_kind": "symbolic",
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
            "applicability": {"keywords": ["deadline", "restart"]},
            "required_capabilities": [],
            "possible_effects": ["model_inference"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {"timeout_ms": 5, "max_model_calls": 1},
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _activate(database: DatabaseManager, handler: Any) -> tuple[ExpertRegistry, Any]:
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
        "route.diagnose",
        {"symptom_id": "deadline"},
        limits=ExpertLimits(timeout_ms=5, max_model_calls=max_model_calls),
        idempotency_key=IDEMPOTENCY_KEY,
        request_id=REQUEST_ID,
    )


def test_late_completion_fails_closed_and_replays_without_redispatch(tmp_path: Path) -> None:
    path = tmp_path / "deadline-replay.db"
    database = DatabaseManager(path)
    handler = SlowModelHandler()
    registry, handle = _activate(database, handler)

    with pytest.raises(
        ExpertDeadlineExceededError,
        match="completion crossed admitted deadline",
    ):
        _invoke(registry, handle, max_model_calls=1)

    invocation_ids = registry.snapshot().invocation_ids
    assert len(invocation_ids) == 1
    invocation_id = invocation_ids[0]
    assert handler.calls == 1
    database.close()

    restarted_database = DatabaseManager(path)
    replacement = CountingHandler()
    restarted_registry, restarted_handle = _activate(restarted_database, replacement)

    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(restarted_registry, restarted_handle, max_model_calls=0)

    replay = _invoke(restarted_registry, restarted_handle, max_model_calls=1)
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.DEADLINE_EXCEEDED
    assert replay.error_message == "expert completion crossed admitted deadline"
    assert replay.invocation_id == invocation_id
    assert replay.request_id == REQUEST_ID
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 1}
    assert replay.effect_receipts == (RECEIPT,)
    assert replacement.calls == 0, "deadline terminal replay must never redispatch"
    restarted_database.close()
