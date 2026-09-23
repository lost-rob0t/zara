"""Restart corruption bounds for the canonical durable expert journal (#1449)."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Callable

import pytest

from zara.database import DatabaseManager
from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertLimits,
    ExpertRegistry,
    ExpertRequest,
    ExpertVerdict,
)


class DispatchCounter:
    def __init__(self) -> None:
        self.calls = 0

    def handler(self, **_payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": "bounded durable fixture"},
            "evidence_refs": ["evidence:restart-bounds:v1"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": "zara:expert/restart-bounds",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:restart-bounds-v1",
            "name": "Restart Bounds Fixture",
            "description": "Deterministic fixture for durable terminal bounds.",
            "source_reference": "t/test_expert_idempotency_restart_bounds.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "route.diagnose",
                    "input_schema": {
                        "fields": [
                            {
                                "name": "symptom_id",
                                "type": "string",
                                "required": True,
                            }
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {
                                "name": "summary",
                                "type": "string",
                                "required": True,
                            }
                        ]
                    },
                }
            ],
            "applicability": {"keywords": ["restart", "bounds"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _runtime(
    counter: DispatchCounter,
    database: DatabaseManager,
) -> tuple[ExpertRegistry, CanonicalExpertInvocationPort, Any]:
    registry = ExpertRegistry(database=database)
    registry.reload([(_descriptor(), counter.handler)])
    handle, _receipt = registry.activate(
        "user:restart-bounds",
        "workspace:restart-bounds",
        "zara:expert/restart-bounds",
    )
    return registry, CanonicalExpertInvocationPort(registry), handle


def _request(registry: ExpertRegistry, handle: Any) -> ExpertRequest:
    return ExpertRequest(
        request_id="request:restart-bounds",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id=handle.expert_id,
        expert_operation="route.diagnose",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
        input={"symptom_id": "timer"},
        limits=ExpertLimits(max_model_calls=0),
        idempotency_key="idempotency:restart-bounds",
    )


def _oversized_receipt_count(wire: dict[str, Any]) -> None:
    wire["effect_receipts"] = [{"effect_id": f"effect:{index}"} for index in range(33)]


def _oversized_receipt_entry(wire: dict[str, Any]) -> None:
    wire["effect_receipts"] = [{"effect_id": "effect:oversized", "evidence": "x" * 2100}]


def _oversized_evidence_count(wire: dict[str, Any]) -> None:
    wire["evidence_refs"] = [f"evidence:{index}" for index in range(33)]


def _oversized_evidence_entry(wire: dict[str, Any]) -> None:
    wire["evidence_refs"] = ["e" * 129]


def _oversized_data_mapping(wire: dict[str, Any]) -> None:
    wire["data"] = {f"field_{index}": index for index in range(17)}


def _oversized_usage_mapping(wire: dict[str, Any]) -> None:
    wire["usage"] = {"model_calls": 0, **{f"metric_{index}": index for index in range(16)}}


def _oversized_error_message(wire: dict[str, Any]) -> None:
    wire["error_message"] = "x" * 257


def _non_string_error_message(wire: dict[str, Any]) -> None:
    wire["error_message"] = {"message": "corrupt"}


@pytest.mark.parametrize(
    "mutate",
    [
        _oversized_receipt_count,
        _oversized_receipt_entry,
        _oversized_evidence_count,
        _oversized_evidence_entry,
        _oversized_data_mapping,
        _oversized_usage_mapping,
        _oversized_error_message,
        _non_string_error_message,
    ],
    ids=[
        "receipt-count",
        "receipt-size",
        "evidence-count",
        "evidence-length",
        "data-mapping",
        "usage-mapping",
        "error-message-length",
        "error-message-type",
    ],
)
def test_out_of_contract_durable_terminal_fails_closed_without_redispatch(
    tmp_path: Path,
    mutate: Callable[[dict[str, Any]], None],
) -> None:
    counter = DispatchCounter()
    path = tmp_path / "restart-bounds.db"
    first_db = DatabaseManager(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first = first_port.invoke(_request(first_registry, first_handle))
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 0}
    assert counter.calls == 1

    row = first_db.fetch_one(
        "SELECT result_json FROM expert_idempotency_v1 WHERE invocation_id = ?",
        (first.invocation_id,),
    )
    assert row is not None
    wire = json.loads(row["result_json"])
    mutate(wire)
    first_db.execute(
        "UPDATE expert_idempotency_v1 SET result_json = ? WHERE invocation_id = ?",
        (json.dumps(wire, sort_keys=True, separators=(",", ":")), first.invocation_id),
    )
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry, restarted_port, restarted_handle = _runtime(counter, restarted_db)
    recovered = restarted_port.invoke(_request(restarted_registry, restarted_handle))

    assert counter.calls == 1, "corrupt durable terminal must never redispatch"
    assert recovered.replayed is True
    assert recovered.verdict is ExpertVerdict.UNKNOWN
    assert recovered.error_code is ExpertErrorCode.INTERRUPTED
    assert recovered.invocation_id == first.invocation_id
    assert recovered.request_id == first.request_id
    assert recovered.data == {}
    assert recovered.evidence_refs == ()
    assert recovered.usage == {"model_calls": 0}
    assert recovered.effect_receipts == ()
    restarted_db.close()
