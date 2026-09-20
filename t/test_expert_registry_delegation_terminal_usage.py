"""RED regression: terminal child verdicts cannot escape shared model accounting."""

from __future__ import annotations

from typing import Any

from zara.experts import (
    ExpertBudgetExceededError,
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)


def _descriptor(
    expert_id: str,
    *,
    delegation_policy: str,
    max_model_calls: int,
    model_effect: bool = False,
) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": expert_id,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": f"sha256:{expert_id.rsplit('/', 1)[-1]}.v1",
            "name": expert_id,
            "description": "Terminal usage accounting fixture.",
            "source_reference": "t/test_expert_registry_delegation_terminal_usage.py",
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
            "possible_effects": ["model_inference"] if model_effect else ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": delegation_policy,
            "resource_limits": {"max_model_calls": max_model_calls},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class _FailedOneModelCallHandler:
    def __init__(self) -> None:
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        del expert_operation, payload
        self.calls += 1
        return {
            "verdict": "failed",
            "data": {},
            "evidence_refs": ["ev:failed-after-model"],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


def test_failed_child_still_consumes_parent_shared_model_budget() -> None:
    registry = ExpertRegistry()
    child = _FailedOneModelCallHandler()
    handles: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            first = registry.invoke(
                handles["child"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=1),
            )
            blocked_second = False
            try:
                registry.invoke(
                    handles["child"],
                    "parse",
                    {"text": payload["text"]},
                    limits=ExpertLimits(max_model_calls=1),
                )
            except ExpertBudgetExceededError:
                blocked_second = True
            return {
                "verdict": "succeeded",
                "data": {
                    "first": first.verdict.value,
                    "second_blocked": blocked_second,
                },
                "evidence_refs": ["ev:parent"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/parent-terminal-budget",
                    delegation_policy="children",
                    max_model_calls=1,
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-terminal-budget",
                    delegation_policy="never",
                    max_model_calls=1,
                    model_effect=True,
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/parent-terminal-budget"
    )
    handles["child"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child-terminal-budget"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "try twice"},
        limits=ExpertLimits(max_model_calls=1),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"first": "failed", "second_blocked": True}
    assert result.usage["model_calls"] == 1
    assert child.calls == 1
