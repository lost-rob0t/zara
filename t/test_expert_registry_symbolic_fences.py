"""Pure-symbolic authority fences for ZARA-EXPERT/1.

These regressions pin host-owned operation dispatch and exact model-call
accounting. They intentionally exercise the public ExpertRegistry surface so
product/plugin adapters cannot paper over authority or usage gaps downstream.
"""

from __future__ import annotations

import threading
from dataclasses import replace
from typing import Any

import pytest

from zara.experts import (
    ExpertBudgetExceededError,
    ExpertDescriptor,
    ExpertInvalidInputError,
    ExpertLimits,
    ExpertRegistry,
    ExpertVerdict,
)


def _descriptor(*, operations: list[dict[str, Any]] | None = None) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": "zara:expert/dispatch-fixture",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:dispatch.fixture.v1",
            "name": "Dispatch Fixture",
            "description": "Pins trusted host dispatch and symbolic accounting semantics.",
            "source_reference": "t/test_expert_registry_symbolic_fences.py",
            "reasoning_kind": "symbolic",
            "operations": operations
            or [
                {
                    "operation_id": "parse",
                    "input_schema": {
                        "fields": [
                            {"name": "text", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {"fields": []},
                },
                {
                    "operation_id": "style.rules",
                    "input_schema": {
                        "fields": [
                            {"name": "text", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {"fields": []},
                },
            ],
            "applicability": {"keywords": ["dispatch"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "children",
            "resource_limits": {"max_model_calls": 0},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class OperationAwareHandler:
    def __init__(self) -> None:
        self.calls: list[tuple[str, dict[str, Any]]] = []

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        self.calls.append((expert_operation, payload))
        return {
            "verdict": "succeeded",
            "data": {"selected_operation": expert_operation},
            "evidence_refs": [f"ev:{expert_operation}"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _activate(registry: ExpertRegistry, handler: Any) -> Any:
    registry.reload([(_descriptor(), handler)])
    handle, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/dispatch-fixture"
    )
    return handle


def test_host_selected_operation_is_forwarded_to_multi_operation_handler() -> None:
    registry = ExpertRegistry()
    handler = OperationAwareHandler()
    handle = _activate(registry, handler)

    first = registry.invoke(handle, "parse", {"text": "same-input"})
    second = registry.invoke(handle, "style.rules", {"text": "same-input"})

    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert second.verdict is ExpertVerdict.SUCCEEDED
    assert first.data["selected_operation"] == "parse"
    assert second.data["selected_operation"] == "style.rules"
    assert handler.calls == [
        ("parse", {"text": "same-input"}),
        ("style.rules", {"text": "same-input"}),
    ]


def test_user_input_cannot_spoof_host_owned_operation_dispatch() -> None:
    registry = ExpertRegistry()
    handler = OperationAwareHandler()
    handle = _activate(registry, handler)

    with pytest.raises(ExpertInvalidInputError, match="unknown input field"):
        registry.invoke(
            handle,
            "parse",
            {"text": "same-input", "expert_operation": "style.rules"},
        )

    assert handler.calls == []


@pytest.mark.parametrize("bad_model_calls", [False, "0", 0.0, None])
def test_symbolic_usage_model_calls_must_be_exact_builtin_integer(
    bad_model_calls: Any,
) -> None:
    class Handler:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation, payload
            return {
                "verdict": "succeeded",
                "data": {},
                "evidence_refs": [],
                "usage": {"model_calls": bad_model_calls},
                "effect_receipts": [],
            }

    registry = ExpertRegistry()
    handle = _activate(registry, Handler())

    with pytest.raises(ExpertInvalidInputError, match="model_calls"):
        registry.invoke(
            handle,
            "parse",
            {"text": "x"},
            limits=ExpertLimits(max_model_calls=0),
        )


def test_symbolic_usage_must_explicitly_report_model_calls() -> None:
    class Handler:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation, payload
            return {
                "verdict": "succeeded",
                "data": {},
                "evidence_refs": [],
                "usage": {},
                "effect_receipts": [],
            }

    registry = ExpertRegistry()
    handle = _activate(registry, Handler())

    with pytest.raises(ExpertInvalidInputError, match="model_calls"):
        registry.invoke(
            handle,
            "parse",
            {"text": "x"},
            limits=ExpertLimits(max_model_calls=0),
        )


def test_symbolic_usage_cannot_exceed_admitted_model_budget() -> None:
    class Handler:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation, payload
            return {
                "verdict": "succeeded",
                "data": {},
                "evidence_refs": [],
                "usage": {"model_calls": 1},
                "effect_receipts": [],
            }

    registry = ExpertRegistry()
    handle = _activate(registry, Handler())

    with pytest.raises(ExpertBudgetExceededError, match="model_calls"):
        registry.invoke(
            handle,
            "parse",
            {"text": "x"},
            limits=ExpertLimits(max_model_calls=0),
        )


def test_zero_model_usage_remains_exact_zero_on_success() -> None:
    registry = ExpertRegistry()
    handler = OperationAwareHandler()
    handle = _activate(registry, handler)

    result = registry.invoke(
        handle,
        "parse",
        {"text": "x"},
        limits=ExpertLimits(max_model_calls=0),
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert type(result.usage["model_calls"]) is int
    assert result.usage["model_calls"] == 0


def test_nested_symbolic_expert_delegation_does_not_deadlock_registry() -> None:
    """A handler may synchronously invoke another admitted expert on this registry."""

    registry = ExpertRegistry()
    child_handler = OperationAwareHandler()
    child_descriptor = replace(
        _descriptor(),
        expert_id="zara:expert/dispatch-child",
        manifest_digest="sha256:dispatch.child.v1",
        name="Dispatch Child",
        applicability_keywords=("child",),
    )
    child_handle_box: dict[str, Any] = {}

    class ParentHandler:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            assert expert_operation == "parse"
            child = registry.invoke(
                child_handle_box["handle"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=0),
            )
            return {
                "verdict": "succeeded",
                "data": {"child_verdict": child.verdict.value},
                "evidence_refs": ["ev:delegation"],
                "usage": {"model_calls": 0},
                "effect_receipts": [],
            }

    registry.reload(
        [
            (_descriptor(), ParentHandler()),
            (child_descriptor, child_handler),
        ]
    )
    parent_handle, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/dispatch-fixture"
    )
    child_handle_box["handle"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/dispatch-child"
    )

    outcome: dict[str, Any] = {}

    def invoke_parent() -> None:
        try:
            outcome["result"] = registry.invoke(
                parent_handle,
                "parse",
                {"text": "delegate"},
                limits=ExpertLimits(max_model_calls=0),
            )
        except BaseException as error:  # pragma: no cover - surfaced below
            outcome["error"] = error

    worker = threading.Thread(target=invoke_parent, daemon=True)
    worker.start()
    worker.join(timeout=2.0)

    assert not worker.is_alive(), "nested expert delegation deadlocked the registry lock"
    assert "error" not in outcome
    result = outcome["result"]
    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"child_verdict": "succeeded"}
    assert child_handler.calls == [("parse", {"text": "delegate"})]
