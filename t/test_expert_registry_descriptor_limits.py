"""Descriptor-owned budget ceilings for ZARA-EXPERT/1 pure-symbolic execution.

Caller-provided limits may only narrow an expert descriptor's declared limits.
In particular, a caller must never widen a descriptor with max_model_calls=0.
"""

from __future__ import annotations

from typing import Any

import pytest

from zara.experts import (
    EffectClass,
    ExpertBudgetExceededError,
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ReasoningKind,
)


def _descriptor(*, model_required: bool) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": "zara:expert/descriptor-budget-fixture",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:descriptor.budget.fixture.v1",
            "name": "Descriptor Budget Fixture",
            "description": "Pins descriptor-owned model-call ceilings.",
            "source_reference": "t/test_expert_registry_descriptor_limits.py",
            "reasoning_kind": "model" if model_required else "symbolic",
            "operations": [
                {
                    "operation_id": "parse",
                    "input_schema": {
                        "fields": [
                            {"name": "text", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {"fields": []},
                }
            ],
            "applicability": {"keywords": ["budget"]},
            "required_capabilities": [],
            "possible_effects": ["model_inference"] if model_required else ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {"max_model_calls": 0},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class CountingHandler:
    def __init__(self, *, reported_model_calls: int) -> None:
        self.calls = 0
        self.reported_model_calls = reported_model_calls

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        self.calls += 1
        assert expert_operation == "parse"
        assert payload == {"text": "hello"}
        return {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": [],
            "usage": {"model_calls": self.reported_model_calls},
            "effect_receipts": [],
        }


def _activate(
    registry: ExpertRegistry,
    descriptor: ExpertDescriptor,
    handler: CountingHandler,
) -> Any:
    registry.reload([(descriptor, handler)])
    handle, _ = registry.activate(
        "user:alice",
        "ws:main",
        descriptor.expert_id,
    )
    return handle


def test_caller_cannot_widen_descriptor_zero_model_budget_before_dispatch() -> None:
    registry = ExpertRegistry()
    descriptor = _descriptor(model_required=True)
    handler = CountingHandler(reported_model_calls=1)
    handle = _activate(registry, descriptor, handler)

    assert descriptor.reasoning_kind is ReasoningKind.MODEL
    assert EffectClass.MODEL_INFERENCE in descriptor.possible_effects

    with pytest.raises(ExpertBudgetExceededError, match="max_model_calls=0"):
        registry.invoke(
            handle,
            "parse",
            {"text": "hello"},
            limits=ExpertLimits(max_model_calls=64),
        )

    assert handler.calls == 0


def test_success_usage_is_checked_against_descriptor_zero_model_ceiling() -> None:
    registry = ExpertRegistry()
    descriptor = _descriptor(model_required=False)
    handler = CountingHandler(reported_model_calls=1)
    handle = _activate(registry, descriptor, handler)

    with pytest.raises(ExpertBudgetExceededError, match="model_calls"):
        registry.invoke(
            handle,
            "parse",
            {"text": "hello"},
            limits=ExpertLimits(max_model_calls=64),
        )

    assert handler.calls == 1
    assert registry.snapshot().invocation_ids == ()
