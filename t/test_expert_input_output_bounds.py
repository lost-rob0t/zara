"""Strict expert input JSON and live/replay output-budget regressions."""

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

TARGET_ID = "zara:expert/input-output-bounds"
PRINCIPAL = "user:input-output-bounds"
WORKSPACE = "workspace:input-output-bounds"
PERMISSIVE_OUTPUT_BYTES = 4096
TIGHT_OUTPUT_BYTES = 256


@dataclass
class CountingHandler:
    output_text: str = "ok"
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "route.diagnose"
        assert "score" in payload
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": self.output_text},
            "evidence_refs": [],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:input-output-bounds-v1",
            "name": "Input Output Bounds Fixture",
            "description": "Strict input and result-budget regression fixture.",
            "source_reference": "t/test_expert_input_output_bounds.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "route.diagnose",
                    "input_schema": {
                        "fields": [
                            {"name": "score", "type": "number", "required": True},
                            {"name": "metadata", "type": "object", "required": False},
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {"name": "summary", "type": "string", "required": True}
                        ]
                    },
                }
            ],
            "applicability": {"keywords": ["bounds", "idempotency"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {
                "max_output_bytes": PERMISSIVE_OUTPUT_BYTES,
                "max_model_calls": 0,
            },
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _runtime(
    database: DatabaseManager,
    handler: CountingHandler,
) -> tuple[ExpertRegistry, Any]:
    registry = ExpertRegistry(database=database)
    registry.reload([(_descriptor(), handler)])
    handle, _receipt = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
    return registry, handle


def _invoke(
    registry: ExpertRegistry,
    handle: Any,
    *,
    input_payload: dict[str, Any],
    idempotency_key: str,
    max_output_bytes: int,
):
    return registry.invoke(
        handle,
        "route.diagnose",
        input_payload,
        limits=ExpertLimits(
            max_output_bytes=max_output_bytes,
            max_model_calls=0,
        ),
        idempotency_key=idempotency_key,
        request_id=f"request:{idempotency_key.rsplit(':', 1)[-1]}",
    )


def _journal_row_count(database: DatabaseManager) -> int:
    table = database.fetch_one(
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?",
        ("expert_idempotency_v1",),
    )
    if table is None:
        return 0
    row = database.fetch_one("SELECT COUNT(*) AS count FROM expert_idempotency_v1")
    assert row is not None
    return int(row["count"])


@pytest.mark.parametrize(
    ("case", "bad_payload"),
    [
        ("nan", {"score": float("nan")}),
        ("positive-infinity", {"score": float("inf")}),
        ("negative-infinity", {"score": float("-inf")}),
        ("nested-nan", {"score": 1.0, "metadata": {"nested": [float("nan")]}}),
    ],
)
def test_nonfinite_input_fails_before_dispatch_or_durable_claim_and_does_not_poison_key(
    tmp_path: Path,
    case: str,
    bad_payload: dict[str, Any],
) -> None:
    database = DatabaseManager(tmp_path / f"{case}.db")
    handler = CountingHandler()
    registry, handle = _runtime(database, handler)
    idempotency_key = f"idempotency:{case}"

    with pytest.raises(ExpertInvalidInputError, match="non-finite|bounded JSON"):
        _invoke(
            registry,
            handle,
            input_payload=bad_payload,
            idempotency_key=idempotency_key,
            max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
        )

    assert handler.calls == 0
    assert _journal_row_count(database) == 0

    finite = _invoke(
        registry,
        handle,
        input_payload={"score": 1.25, "metadata": {"nested": [1.0]}},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert finite.verdict is ExpertVerdict.SUCCEEDED
    assert finite.replayed is False
    assert handler.calls == 1
    assert _journal_row_count(database) == 1
    database.close()


def test_live_result_over_output_budget_fails_closed_and_preserves_no_redispatch(
    tmp_path: Path,
) -> None:
    database = DatabaseManager(tmp_path / "live-output-budget.db")
    handler = CountingHandler(output_text="x" * 1024)
    registry, handle = _runtime(database, handler)
    idempotency_key = "idempotency:live-output-budget"

    with pytest.raises(ExpertBudgetExceededError, match="max_output_bytes"):
        _invoke(
            registry,
            handle,
            input_payload={"score": 1.0},
            idempotency_key=idempotency_key,
            max_output_bytes=TIGHT_OUTPUT_BYTES,
        )

    assert handler.calls == 1
    replay = _invoke(
        registry,
        handle,
        input_payload={"score": 1.0},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.BUDGET_EXCEEDED
    assert replay.data == {}
    assert handler.calls == 1
    database.close()


def test_tighter_same_process_replay_rejects_without_rewrite_or_redispatch(
    tmp_path: Path,
) -> None:
    database = DatabaseManager(tmp_path / "same-process-output-budget.db")
    handler = CountingHandler(output_text="x" * 1024)
    registry, handle = _runtime(database, handler)
    idempotency_key = "idempotency:same-process-output-budget"

    first = _invoke(
        registry,
        handle,
        input_payload={"score": 1.0},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert handler.calls == 1

    with pytest.raises(ExpertBudgetExceededError, match="max_output_bytes"):
        _invoke(
            registry,
            handle,
            input_payload={"score": 1.0},
            idempotency_key=idempotency_key,
            max_output_bytes=TIGHT_OUTPUT_BYTES,
        )

    permissive_replay = _invoke(
        registry,
        handle,
        input_payload={"score": 1.0},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert permissive_replay.replayed is True
    assert permissive_replay.verdict is ExpertVerdict.SUCCEEDED
    assert permissive_replay.invocation_id == first.invocation_id
    assert permissive_replay.request_id == first.request_id
    assert permissive_replay.data == first.data
    assert handler.calls == 1
    database.close()


def test_tighter_restart_replay_rejects_without_rewrite_or_redispatch(
    tmp_path: Path,
) -> None:
    path = tmp_path / "restart-output-budget.db"
    first_database = DatabaseManager(path)
    first_handler = CountingHandler(output_text="x" * 1024)
    first_registry, first_handle = _runtime(first_database, first_handler)
    idempotency_key = "idempotency:restart-output-budget"

    first = _invoke(
        first_registry,
        first_handle,
        input_payload={"score": 1.0},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first_handler.calls == 1
    first_database.close()

    restarted_database = DatabaseManager(path)
    replacement_handler = CountingHandler(output_text="replacement must never run")
    restarted_registry, restarted_handle = _runtime(
        restarted_database,
        replacement_handler,
    )

    with pytest.raises(ExpertBudgetExceededError, match="max_output_bytes"):
        _invoke(
            restarted_registry,
            restarted_handle,
            input_payload={"score": 1.0},
            idempotency_key=idempotency_key,
            max_output_bytes=TIGHT_OUTPUT_BYTES,
        )
    assert replacement_handler.calls == 0

    permissive_replay = _invoke(
        restarted_registry,
        restarted_handle,
        input_payload={"score": 1.0},
        idempotency_key=idempotency_key,
        max_output_bytes=PERMISSIVE_OUTPUT_BYTES,
    )
    assert permissive_replay.replayed is True
    assert permissive_replay.verdict is ExpertVerdict.SUCCEEDED
    assert permissive_replay.invocation_id == first.invocation_id
    assert permissive_replay.request_id == first.request_id
    assert permissive_replay.data == first.data
    assert replacement_handler.calls == 0
    restarted_database.close()
