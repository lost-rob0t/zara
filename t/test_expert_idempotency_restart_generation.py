"""RED: durable expert replay must respect current registry/runtime generation."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

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


@dataclass
class DispatchCounter:
    calls: int = 0

    def handler(self, **_payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": "generation-bound symbolic result"},
            "evidence_refs": ["evidence:restart-generation:v1"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


@dataclass
class CancelledDispatchCounter(DispatchCounter):
    def handler(self, **_payload: Any) -> dict[str, Any]:
        self.calls += 1
        return {
            "verdict": "cancelled",
            "data": {},
            "evidence_refs": [],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _descriptor(expert_id: str, manifest_digest: str) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": expert_id,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": manifest_digest,
            "name": "Restart Generation Fixture",
            "description": "Deterministic symbolic fixture for generation-fenced replay.",
            "source_reference": "t/test_expert_idempotency_restart_generation.py",
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
            "applicability": {"keywords": ["restart", "generation"]},
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


def _request(registry: ExpertRegistry, activation_id: str, expert_id: str) -> ExpertRequest:
    return ExpertRequest(
        request_id="request:restart-generation",
        operation="expert.invoke",
        activation_id=activation_id,
        expert_id=expert_id,
        expert_operation="route.diagnose",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
        input={"symptom_id": "timer"},
        limits=ExpertLimits(max_model_calls=0),
        idempotency_key="idempotency:restart-generation",
    )


def _bump_generation(registry: ExpertRegistry) -> None:
    registry.register(
        _descriptor(
            "zara:expert/restart-generation-peer",
            "sha256:restart-generation-peer-v1",
        ),
        lambda **_payload: {
            "verdict": "succeeded",
            "data": {"summary": "unused peer"},
            "evidence_refs": ["evidence:restart-generation-peer:v1"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        },
    )


def test_terminal_restart_replay_with_changed_generation_is_explicit_unknown(
    tmp_path: Path,
) -> None:
    target_id = "zara:expert/restart-generation"
    target = _descriptor(target_id, "sha256:restart-generation-v1")
    counter = DispatchCounter()
    path = tmp_path / "restart-generation.db"

    first_db = DatabaseManager(path)
    first_registry = ExpertRegistry(database=first_db)
    first_registry.reload([(target, counter.handler)])
    first_handle, _ = first_registry.activate(
        "user:restart-generation",
        "workspace:restart-generation",
        target_id,
    )
    first = CanonicalExpertInvocationPort(first_registry).invoke(
        _request(first_registry, first_handle.activation_id, target_id)
    )
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 0}
    assert counter.calls == 1
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry = ExpertRegistry(database=restarted_db)
    restarted_registry.reload([(target, counter.handler)])
    _bump_generation(restarted_registry)
    restarted_handle, _ = restarted_registry.activate(
        "user:restart-generation",
        "workspace:restart-generation",
        target_id,
    )
    assert restarted_registry.generation != first.resolved_registry_generation
    assert restarted_registry.runtime_generation != first.resolved_runtime_generation

    replay = CanonicalExpertInvocationPort(restarted_registry).invoke(
        _request(restarted_registry, restarted_handle.activation_id, target_id)
    )

    assert counter.calls == 1, "changed-generation retry must not redispatch"
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INTERRUPTED
    assert replay.activation_id == first.activation_id
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.usage == {"model_calls": 0}
    assert replay.evidence_refs == ()
    assert replay.effect_receipts == ()


def test_cancelled_terminal_replay_stays_cancelled_across_generation_change(
    tmp_path: Path,
) -> None:
    target_id = "zara:expert/restart-generation"
    target = _descriptor(target_id, "sha256:restart-generation-v1")
    counter = CancelledDispatchCounter()
    path = tmp_path / "restart-generation-cancelled.db"

    first_db = DatabaseManager(path)
    first_registry = ExpertRegistry(database=first_db)
    first_registry.reload([(target, counter.handler)])
    first_handle, _ = first_registry.activate(
        "user:restart-generation",
        "workspace:restart-generation",
        target_id,
    )
    first = CanonicalExpertInvocationPort(first_registry).invoke(
        _request(first_registry, first_handle.activation_id, target_id)
    )
    assert first.verdict is ExpertVerdict.CANCELLED
    assert first.usage == {"model_calls": 0}
    assert counter.calls == 1
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry = ExpertRegistry(database=restarted_db)
    restarted_registry.reload([(target, counter.handler)])
    _bump_generation(restarted_registry)
    restarted_handle, _ = restarted_registry.activate(
        "user:restart-generation",
        "workspace:restart-generation",
        target_id,
    )

    replay = CanonicalExpertInvocationPort(restarted_registry).invoke(
        _request(restarted_registry, restarted_handle.activation_id, target_id)
    )

    assert counter.calls == 1, "cancelled terminal retry must not redispatch"
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.CANCELLED
    assert replay.error_code is first.error_code
    assert replay.activation_id == first.activation_id
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.usage == first.usage == {"model_calls": 0}
