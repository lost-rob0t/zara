"""Canonical expert-to-expert delegation fences for ZARA-EXPERT/1.

These tests intentionally exercise nested calls through the one public
``ExpertRegistry``.  A handler may delegate, but it must inherit the caller's
identity and remaining model-call budget, and the parent descriptor must allow
that delegation.  No adapter-local registry or budget ledger is involved.
"""

from __future__ import annotations

from typing import Any

from zara.experts import (
    ExpertBudgetExceededError,
    ExpertDeniedError,
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
    possible_effects: list[str] | None = None,
) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": expert_id,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": f"sha256:{expert_id.rsplit('/', 1)[-1]}.v1",
            "name": expert_id,
            "description": "Delegation budget acceptance fixture.",
            "source_reference": "t/test_expert_registry_delegation_budget.py",
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
            "applicability": {"keywords": [expert_id.rsplit("/", 1)[-1]]},
            "required_capabilities": [],
            "possible_effects": possible_effects or ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": delegation_policy,
            "resource_limits": {"max_model_calls": max_model_calls},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class _ZeroHandler:
    def __init__(self) -> None:
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        del expert_operation, payload
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": ["ev:zero"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


class _OneModelCallHandler:
    def __init__(self) -> None:
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        del expert_operation, payload
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": ["ev:model-call"],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


def test_delegation_policy_never_blocks_nested_dispatch() -> None:
    registry = ExpertRegistry()
    child = _ZeroHandler()
    child_handle_box: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            blocked = False
            try:
                registry.invoke(
                    child_handle_box["handle"],
                    "parse",
                    {"text": payload["text"]},
                    limits=ExpertLimits(max_model_calls=0),
                )
            except ExpertDeniedError:
                blocked = True
            return {
                "verdict": "succeeded",
                "data": {"delegation_blocked": blocked},
                "evidence_refs": ["ev:parent"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/parent-never",
                    delegation_policy="never",
                    max_model_calls=0,
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-zero",
                    delegation_policy="never",
                    max_model_calls=0,
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate("user:alice", "ws:main", "zara:expert/parent-never")
    child_handle_box["handle"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child-zero"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "delegate"},
        limits=ExpertLimits(max_model_calls=0),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"delegation_blocked": True}
    assert result.usage["model_calls"] == 0
    assert child.calls == 0


def test_nested_delegation_cannot_cross_workspace_identity() -> None:
    registry = ExpertRegistry()
    child = _ZeroHandler()
    child_handle_box: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            blocked = False
            try:
                registry.invoke(
                    child_handle_box["handle"],
                    "parse",
                    {"text": payload["text"]},
                    limits=ExpertLimits(max_model_calls=0),
                )
            except ExpertDeniedError:
                blocked = True
            return {
                "verdict": "succeeded",
                "data": {"identity_blocked": blocked},
                "evidence_refs": ["ev:parent"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/parent-children",
                    delegation_policy="children",
                    max_model_calls=0,
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-other-workspace",
                    delegation_policy="never",
                    max_model_calls=0,
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate("user:alice", "ws:main", "zara:expert/parent-children")
    child_handle_box["handle"], _ = registry.activate(
        "user:alice", "ws:other", "zara:expert/child-other-workspace"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "delegate"},
        limits=ExpertLimits(max_model_calls=0),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"identity_blocked": True}
    assert child.calls == 0


def test_nested_delegates_share_one_model_call_budget() -> None:
    registry = ExpertRegistry()
    child = _OneModelCallHandler()
    child_handle_box: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            first = registry.invoke(
                child_handle_box["handle"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=1),
            )
            blocked_second = False
            try:
                registry.invoke(
                    child_handle_box["handle"],
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
                    "zara:expert/parent-budget",
                    delegation_policy="children",
                    max_model_calls=1,
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-model",
                    delegation_policy="never",
                    max_model_calls=1,
                    possible_effects=["model_inference"],
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate("user:alice", "ws:main", "zara:expert/parent-budget")
    child_handle_box["handle"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child-model"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "delegate twice"},
        limits=ExpertLimits(max_model_calls=1),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"first": "succeeded", "second_blocked": True}
    assert result.usage["model_calls"] == 1
    assert child.calls == 1


def test_zero_model_parent_and_child_preserve_exact_zero_shared_usage() -> None:
    registry = ExpertRegistry()
    child = _ZeroHandler()
    child_handle_box: dict[str, Any] = {}

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            nested = registry.invoke(
                child_handle_box["handle"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=64),
            )
            return {
                "verdict": "succeeded",
                "data": {"nested": nested.verdict.value},
                "evidence_refs": ["ev:parent"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/parent-zero",
                    delegation_policy="children",
                    max_model_calls=0,
                ),
                Parent(),
            ),
            (
                _descriptor(
                    "zara:expert/child-zero-shared",
                    delegation_policy="never",
                    max_model_calls=0,
                ),
                child,
            ),
        ]
    )
    parent, _ = registry.activate("user:alice", "ws:main", "zara:expert/parent-zero")
    child_handle_box["handle"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child-zero-shared"
    )

    result = registry.invoke(
        parent,
        "parse",
        {"text": "pure symbolic"},
        limits=ExpertLimits(max_model_calls=0),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"nested": "succeeded"}
    assert type(result.usage["model_calls"]) is int
    assert result.usage["model_calls"] == 0
    assert child.calls == 1
