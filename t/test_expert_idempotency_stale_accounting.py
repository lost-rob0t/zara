"""RED: freshness downgrade must preserve durable expert accounting."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

import pytest

from zara.database import DatabaseManager
from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertBudgetExceededError,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertLimits,
    ExpertRegistry,
    ExpertRequest,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/stale-accounting"
PRINCIPAL = "user:stale-accounting"
WORKSPACE = "workspace:stale-accounting"


@dataclass
class DispatchCounter:
    model_calls: int
    effectful: bool
    calls: int = 0

    def handler(self, **_payload: Any) -> dict[str, Any]:
        self.calls += 1
        receipts = []
        if self.effectful:
            receipts = [
                {
                    "effect_id": "effect:stale-accounting:timer",
                    "verified_outcome_ref": (
                        "zara.verified-outcome/v2:1:outcome:postcondition/timer-600"
                    ),
                }
            ]
        return {
            "verdict": "succeeded",
            "data": {"summary": "historical terminal result"},
            "evidence_refs": ["evidence:stale-accounting:v1"],
            "usage": {"model_calls": self.model_calls},
            "effect_receipts": receipts,
        }


def _descriptor(manifest_digest: str) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": manifest_digest,
            "name": "Stale Accounting Fixture",
            "description": "Deterministic durable replay accounting fixture.",
            "source_reference": "t/test_expert_idempotency_stale_accounting.py",
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
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _activate(
    database: DatabaseManager,
    counter: DispatchCounter,
    descriptor: ExpertDescriptor,
) -> tuple[ExpertRegistry, CanonicalExpertInvocationPort, str]:
    registry = ExpertRegistry(database=database)
    registry.reload([(descriptor, counter.handler)])
    handle, _ = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
    return registry, CanonicalExpertInvocationPort(registry), handle.activation_id


def _request(
    registry: ExpertRegistry,
    activation_id: str,
    *,
    max_model_calls: int,
) -> ExpertRequest:
    return ExpertRequest(
        request_id="request:stale-accounting",
        operation="expert.invoke",
        activation_id=activation_id,
        expert_id=TARGET_ID,
        expert_operation="route.diagnose",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
        input={"symptom_id": "timer"},
        limits=ExpertLimits(max_model_calls=max_model_calls),
        idempotency_key="idempotency:stale-accounting",
    )


def _bump_generation(registry: ExpertRegistry) -> None:
    peer = ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": "zara:expert/stale-accounting-peer",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:stale-accounting-peer-v1",
            "name": "Stale Accounting Peer",
            "description": "Generation bump fixture.",
            "source_reference": "t/test_expert_idempotency_stale_accounting.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "noop.inspect",
                    "input_schema": {"fields": []},
                    "output_schema": {"fields": []},
                }
            ],
            "applicability": {"keywords": ["peer"]},
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
    registry.register(
        peer,
        lambda **_payload: {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": [],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        },
    )


def _restart_with_drift(
    path: Path,
    counter: DispatchCounter,
    *,
    drift: Literal["generation", "build"],
) -> tuple[ExpertRegistry, CanonicalExpertInvocationPort, str]:
    database = DatabaseManager(path)
    descriptor = _descriptor(
        "sha256:stale-accounting-v2"
        if drift == "build"
        else "sha256:stale-accounting-v1"
    )
    registry, port, activation_id = _activate(database, counter, descriptor)
    if drift == "generation":
        _bump_generation(registry)
        handle, _ = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
        activation_id = handle.activation_id
    return registry, port, activation_id


@pytest.mark.parametrize("drift", ["generation", "build"])
def test_stale_model_backed_success_keeps_usage_for_current_zero_budget(
    tmp_path: Path,
    drift: Literal["generation", "build"],
) -> None:
    counter = DispatchCounter(model_calls=1, effectful=False)
    path = tmp_path / f"stale-model-{drift}.db"

    first_db = DatabaseManager(path)
    first_registry, first_port, first_activation = _activate(
        first_db,
        counter,
        _descriptor("sha256:stale-accounting-v1"),
    )
    first = first_port.invoke(
        _request(first_registry, first_activation, max_model_calls=1)
    )
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 1}
    assert counter.calls == 1
    first_db.close()

    restarted_registry, restarted_port, restarted_activation = _restart_with_drift(
        path,
        counter,
        drift=drift,
    )
    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        restarted_port.invoke(
            _request(restarted_registry, restarted_activation, max_model_calls=0)
        )

    assert counter.calls == 1, "stale durable success must never be redispatched"


@pytest.mark.parametrize("drift", ["generation", "build"])
def test_stale_effectful_success_fails_closed_without_losing_historical_receipt(
    tmp_path: Path,
    drift: Literal["generation", "build"],
) -> None:
    counter = DispatchCounter(model_calls=0, effectful=True)
    path = tmp_path / f"stale-effect-{drift}.db"

    first_db = DatabaseManager(path)
    first_registry, first_port, first_activation = _activate(
        first_db,
        counter,
        _descriptor("sha256:stale-accounting-v1"),
    )
    first = first_port.invoke(
        _request(first_registry, first_activation, max_model_calls=0)
    )
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 0}
    assert first.effect_receipts
    assert counter.calls == 1
    first_db.close()

    restarted_registry, restarted_port, restarted_activation = _restart_with_drift(
        path,
        counter,
        drift=drift,
    )
    replay = restarted_port.invoke(
        _request(restarted_registry, restarted_activation, max_model_calls=0)
    )

    assert counter.calls == 1, "stale effectful success must never repeat the prior effect"
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INTERRUPTED
    assert replay.activation_id == first.activation_id
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.data == first.data
    assert replay.evidence_refs == first.evidence_refs
    assert replay.usage == first.usage
    assert replay.effect_receipts == first.effect_receipts
