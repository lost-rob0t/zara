"""RED: nested terminal expert results must report exact model usage.

A failed child can still have consumed model calls.  The canonical shared budget
therefore cannot treat an omitted ``usage.model_calls`` field as zero merely
because the verdict is non-success.  This regression keeps the zero-model
acceptance path fail-closed without adding another ledger or dispatcher.
"""

from __future__ import annotations

from typing import Any

from zara.experts import (
    ExpertDescriptor,
    ExpertInvalidInputError,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)


def _descriptor(expert_id: str, *, delegation_policy: str) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": expert_id,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": f"sha256:{expert_id.rsplit('/', 1)[-1]}.v1",
            "name": expert_id,
            "description": "Missing terminal usage regression fixture.",
            "source_reference": "t/test_expert_registry_delegation_missing_usage.py",
            "reasoning_kind": "symbolic",
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
            "applicability": {"keywords": [expert_id.rsplit('/', 1)[-1]]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": delegation_policy,
            "resource_limits": {"max_model_calls": 0},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class _FailedWithoutUsage:
    def __init__(self) -> None:
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        del expert_operation, payload
        self.calls += 1
        return {
            "verdict": "failed",
            "data": {},
            "evidence_refs": ["ev:failed-without-usage"],
            "effect_receipts": [],
        }


def test_failed_nested_child_without_usage_fails_closed_before_parent_acceptance() -> None:
    registry = ExpertRegistry()
    child = _FailedWithoutUsage()
    handles: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            rejected = False
            try:
                registry.invoke(
                    handles["child"],
                    "parse",
                    {"text": payload["text"]},
                    limits=ExpertLimits(max_model_calls=0),
                )
            except ExpertInvalidInputError:
                rejected = True
            return {
                "verdict": "succeeded",
                "data": {"missing_usage_rejected": rejected},
                "evidence_refs": ["ev:parent"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/parent-missing-usage",
                    delegation_policy="children",
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-missing-usage",
                    delegation_policy="never",
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/parent-missing-usage"
    )
    handles["child"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child-missing-usage"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "pure symbolic"},
        limits=ExpertLimits(max_model_calls=0),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"missing_usage_rejected": True}
    assert type(result.usage["model_calls"]) is int
    assert result.usage["model_calls"] == 0
    assert child.calls == 1
