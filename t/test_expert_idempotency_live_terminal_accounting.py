"""Durable accounting for cancellation and stale-generation terminal fences."""

from __future__ import annotations

import threading
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
    ExpertStaleGenerationError,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/live-terminal-accounting"
PRINCIPAL = "user:live-terminal-accounting"
WORKSPACE = "workspace:live-terminal-accounting"
IDEMPOTENCY_KEY = "idempotency:live-terminal-accounting"
REQUEST_ID = "request:live-terminal-accounting"
RECEIPT = {
    "effect_id": "effect:live-terminal-accounting:timer",
    "verified_outcome_ref": "zara.verified-outcome/v2:1:outcome:timer-600",
}


@dataclass
class BlockingModelHandler:
    calls: int = 0

    def __post_init__(self) -> None:
        self.entered = threading.Event()
        self.release = threading.Event()

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "route.diagnose"
        assert payload == {"symptom_id": "timer"}
        self.calls += 1
        self.entered.set()
        assert self.release.wait(timeout=5.0), "test failed to release blocking handler"
        return {
            "verdict": "succeeded",
            "data": {"summary": "handler completed after fence"},
            "evidence_refs": ["evidence:live-terminal-accounting:v1"],
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
            "data": {"summary": "replacement"},
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
            "manifest_digest": "sha256:live-terminal-accounting-v1",
            "name": "Live Terminal Accounting Fixture",
            "description": "Durable cancellation and stale-generation accounting fixture.",
            "source_reference": "t/test_expert_idempotency_live_terminal_accounting.py",
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
        "route.diagnose",
        {"symptom_id": "timer"},
        limits=ExpertLimits(max_model_calls=max_model_calls),
        idempotency_key=IDEMPOTENCY_KEY,
        request_id=REQUEST_ID,
    )


def _start_blocked(
    registry: ExpertRegistry,
    handle: Any,
    handler: BlockingModelHandler,
) -> tuple[dict[str, Any], threading.Thread]:
    outcome: dict[str, Any] = {}

    def invoke() -> None:
        try:
            outcome["result"] = _invoke(registry, handle, max_model_calls=1)
        except BaseException as error:  # pragma: no cover - asserted by caller
            outcome["error"] = error

    worker = threading.Thread(target=invoke, daemon=True)
    worker.start()
    assert handler.entered.wait(timeout=1.0), "handler did not enter dispatch"
    return outcome, worker


def _only_invocation_id(registry: ExpertRegistry) -> str:
    invocation_ids = registry.snapshot().invocation_ids
    assert len(invocation_ids) == 1
    return invocation_ids[0]


def test_cancelled_model_backed_terminal_preserves_usage_and_receipt_across_restart(
    tmp_path: Path,
) -> None:
    path = tmp_path / "cancelled-model-terminal.db"
    database = DatabaseManager(path)
    handler = BlockingModelHandler()
    registry, handle = _activate(database, handler)
    outcome, worker = _start_blocked(registry, handle, handler)
    invocation_id = _only_invocation_id(registry)

    cancellation = registry.cancel(invocation_id)
    assert cancellation["cancelled"] is True
    handler.release.set()
    worker.join(timeout=1.0)

    assert not worker.is_alive()
    assert "error" not in outcome
    terminal = outcome["result"]
    assert terminal.verdict is ExpertVerdict.CANCELLED
    assert terminal.data == {}
    assert terminal.evidence_refs == ()
    assert terminal.usage == {"model_calls": 1}
    assert terminal.effect_receipts == (RECEIPT,)
    assert terminal.invocation_id == invocation_id
    assert terminal.request_id == REQUEST_ID
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

    assert replacement.calls == 0, "cancelled terminal replay must never redispatch"
    restarted_database.close()


def test_stale_generation_terminal_preserves_consumed_usage_and_receipt_for_restart(
    tmp_path: Path,
) -> None:
    path = tmp_path / "stale-generation-terminal.db"
    database = DatabaseManager(path)
    handler = BlockingModelHandler()
    registry, handle = _activate(database, handler)
    outcome, worker = _start_blocked(registry, handle, handler)
    invocation_id = _only_invocation_id(registry)

    replacement = CountingHandler()
    registry.reload([(_descriptor(), replacement)])
    handler.release.set()
    worker.join(timeout=1.0)

    assert not worker.is_alive()
    assert isinstance(outcome.get("error"), ExpertStaleGenerationError)
    assert handler.calls == 1
    assert replacement.calls == 0
    database.close()

    restarted_database = DatabaseManager(path)
    replay_handler = CountingHandler()
    restarted_registry, restarted_handle = _activate(restarted_database, replay_handler)
    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        _invoke(restarted_registry, restarted_handle, max_model_calls=0)

    replay = _invoke(restarted_registry, restarted_handle, max_model_calls=1)
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INTERRUPTED
    assert replay.activation_id == handle.activation_id
    assert replay.invocation_id == invocation_id
    assert replay.request_id == REQUEST_ID
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 1}
    assert replay.effect_receipts == (RECEIPT,)
    assert replay_handler.calls == 0, "stale terminal replay must never redispatch"
    restarted_database.close()
