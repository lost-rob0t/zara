"""Restart/concurrency contract for durable ZARA-EXPERT idempotency (#1449)."""

from __future__ import annotations

import threading
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import pytest

from zara.database import DatabaseManager
from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertInvalidInputError,
    ExpertLimits,
    ExpertRegistry,
    ExpertRequest,
    ExpertStaleGenerationError,
    ExpertVerdict,
)


@dataclass
class DispatchCounter:
    calls: int = 0

    def handler(self, **_payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": "symbolic restart fixture"},
            "evidence_refs": ["evidence:restart-fixture:v1"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


@dataclass
class BlockingDispatchCounter(DispatchCounter):
    entered: threading.Event = field(default_factory=threading.Event)
    release: threading.Event = field(default_factory=threading.Event)

    def handler(self, **payload: Any) -> dict[str, Any]:
        self.calls += 1
        self.entered.set()
        if not self.release.wait(timeout=10):
            raise RuntimeError("test handler release timed out")
        return {
            "verdict": "succeeded",
            "data": {"summary": f"symbolic restart fixture:{payload['symptom_id']}"},
            "evidence_refs": ["evidence:restart-fixture:v1"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _descriptor(*, manifest_digest: str = "sha256:restart-fixture-v1") -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": "zara:expert/restart-fixture",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": manifest_digest,
            "name": "Restart Fixture",
            "description": "Deterministic symbolic fixture for restart replay semantics.",
            "source_reference": "t/test_expert_idempotency_restart_contract.py",
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
            "applicability": {"keywords": ["restart", "idempotency"]},
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
        "user:restart-contract",
        "workspace:restart-contract",
        "zara:expert/restart-fixture",
    )
    return registry, CanonicalExpertInvocationPort(registry), handle


def _request(
    registry: ExpertRegistry,
    handle: Any,
    *,
    symptom_id: str = "timer",
) -> ExpertRequest:
    return ExpertRequest(
        request_id="request:restart-contract",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id=handle.expert_id,
        expert_operation="route.diagnose",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
        input={"symptom_id": symptom_id},
        limits=ExpertLimits(max_model_calls=0),
        idempotency_key="idempotency:restart-contract",
    )


def _fresh_database(path: Path) -> DatabaseManager:
    return DatabaseManager(path)


def test_terminal_idempotent_retry_after_process_recreation_replays_exact_result(
    tmp_path: Path,
) -> None:
    counter = DispatchCounter()
    path = tmp_path / "restart.db"

    first_db = _fresh_database(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first = first_port.invoke(_request(first_registry, first_handle))
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.replayed is False
    assert first.usage == {"model_calls": 0}
    assert first.effect_receipts == ()
    assert counter.calls == 1
    first_db.close()

    restarted_db = _fresh_database(path)
    restarted_registry, restarted_port, restarted_handle = _runtime(counter, restarted_db)
    assert restarted_handle.activation_id != first_handle.activation_id
    replay = restarted_port.invoke(_request(restarted_registry, restarted_handle))

    assert counter.calls == 1, "process recreation silently redispatched an idempotent expert request"
    assert replay.replayed is True
    assert replay.activation_id == first.activation_id
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.verdict is first.verdict
    assert replay.data == first.data
    assert replay.evidence_refs == first.evidence_refs
    assert replay.usage == first.usage == {"model_calls": 0}
    assert replay.effect_receipts == first.effect_receipts


def test_restart_same_key_changed_input_is_conflict_not_dispatch(tmp_path: Path) -> None:
    counter = DispatchCounter()
    path = tmp_path / "conflict.db"
    first_db = _fresh_database(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first_port.invoke(_request(first_registry, first_handle))
    first_db.close()

    second_db = _fresh_database(path)
    second_registry, second_port, second_handle = _runtime(counter, second_db)
    with pytest.raises(ExpertInvalidInputError, match="idempotency conflict"):
        second_port.invoke(
            _request(second_registry, second_handle, symptom_id="different-input")
        )
    assert counter.calls == 1


def test_corrupt_durable_terminal_returns_explicit_unknown_without_redispatch(
    tmp_path: Path,
) -> None:
    counter = DispatchCounter()
    path = tmp_path / "corrupt.db"
    first_db = _fresh_database(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first = first_port.invoke(_request(first_registry, first_handle))
    first_db.execute(
        "UPDATE expert_idempotency_v1 SET result_json = ? WHERE invocation_id = ?",
        ("{", first.invocation_id),
    )
    first_db.close()

    second_db = _fresh_database(path)
    second_registry, second_port, second_handle = _runtime(counter, second_db)
    recovered = second_port.invoke(_request(second_registry, second_handle))

    assert counter.calls == 1
    assert recovered.replayed is True
    assert recovered.verdict is ExpertVerdict.UNKNOWN
    assert recovered.error_code is ExpertErrorCode.INTERRUPTED
    assert recovered.invocation_id == first.invocation_id
    assert recovered.request_id == first.request_id
    assert recovered.usage == {"model_calls": 0}
    assert recovered.evidence_refs == ()
    assert recovered.effect_receipts == ()


def test_concurrent_recreated_registry_returns_unknown_instead_of_double_dispatch(
    tmp_path: Path,
) -> None:
    counter = BlockingDispatchCounter()
    path = tmp_path / "concurrent.db"
    first_registry, first_port, first_handle = _runtime(counter, _fresh_database(path))
    second_registry, second_port, second_handle = _runtime(counter, _fresh_database(path))
    outcome: dict[str, Any] = {}

    def invoke_first() -> None:
        try:
            outcome["result"] = first_port.invoke(_request(first_registry, first_handle))
        except BaseException as error:  # pragma: no cover - asserted below
            outcome["error"] = error

    thread = threading.Thread(target=invoke_first)
    thread.start()
    assert counter.entered.wait(timeout=10)

    unresolved = second_port.invoke(_request(second_registry, second_handle))
    assert unresolved.replayed is True
    assert unresolved.verdict is ExpertVerdict.UNKNOWN
    assert unresolved.error_code is ExpertErrorCode.INTERRUPTED
    assert unresolved.usage == {"model_calls": 0}
    assert counter.calls == 1

    counter.release.set()
    thread.join(timeout=10)
    assert not thread.is_alive()
    assert "error" not in outcome
    first = outcome["result"]
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 0}
    assert counter.calls == 1

    third_registry, third_port, third_handle = _runtime(counter, _fresh_database(path))
    replay = third_port.invoke(_request(third_registry, third_handle))
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.SUCCEEDED
    assert replay.invocation_id == first.invocation_id
    assert replay.evidence_refs == first.evidence_refs
    assert replay.usage == {"model_calls": 0}
    assert counter.calls == 1


def test_stale_generation_completion_is_interrupted_durably_and_never_replayed_as_success(
    tmp_path: Path,
) -> None:
    counter = BlockingDispatchCounter()
    path = tmp_path / "stale.db"
    database = _fresh_database(path)
    registry, port, handle = _runtime(counter, database)
    outcome: dict[str, Any] = {}

    def invoke() -> None:
        try:
            outcome["result"] = port.invoke(_request(registry, handle))
        except BaseException as error:  # pragma: no branch - asserted below
            outcome["error"] = error

    thread = threading.Thread(target=invoke)
    thread.start()
    assert counter.entered.wait(timeout=10)

    registry.reload(
        [
            (
                _descriptor(manifest_digest="sha256:restart-fixture-v2"),
                counter.handler,
            )
        ]
    )
    counter.release.set()
    thread.join(timeout=10)
    assert not thread.is_alive()
    assert isinstance(outcome.get("error"), ExpertStaleGenerationError)
    assert "result" not in outcome
    assert counter.calls == 1
    database.close()

    restarted_registry, restarted_port, restarted_handle = _runtime(
        counter,
        _fresh_database(path),
    )
    recovered = restarted_port.invoke(_request(restarted_registry, restarted_handle))
    assert recovered.replayed is True
    assert recovered.verdict is ExpertVerdict.UNKNOWN
    assert recovered.error_code is ExpertErrorCode.INTERRUPTED
    assert recovered.usage == {"model_calls": 0}
    assert recovered.evidence_refs == ()
    assert recovered.effect_receipts == ()
    assert counter.calls == 1
