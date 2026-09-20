"""Regression for model usage lost across stale expert completion fences."""

from __future__ import annotations

import threading
from typing import Any

import pytest

import zara.experts as experts
from zara.experts import (
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ExpertStaleGenerationError,
)


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": "zara:expert/stale-usage",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:stale.usage.v1",
            "name": "Stale Usage",
            "description": "Pins usage accounting across generation fences.",
            "source_reference": "t/test_expert_registry_stale_usage_budget.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "query",
                    "input_schema": {
                        "fields": [
                            {"name": "text", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {"fields": []},
                }
            ],
            "applicability": {"keywords": ["stale"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "children",
            "resource_limits": {"max_model_calls": 1},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class BlockingOneCallHandler:
    def __init__(self) -> None:
        self.entered = threading.Event()
        self.release = threading.Event()

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "query"
        assert payload == {"text": "hold"}
        self.entered.set()
        assert self.release.wait(timeout=5.0), "test failed to release blocking handler"
        return {
            "verdict": "succeeded",
            "data": {"late": True},
            "evidence_refs": ["ev:stale-model-call"],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


def test_stale_completion_charges_actual_usage_to_parent_budget() -> None:
    registry = ExpertRegistry()
    handler = BlockingOneCallHandler()
    descriptor = _descriptor()
    registry.reload([(descriptor, handler)])
    handle, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/stale-usage"
    )

    parent = experts._DelegationFrame(
        expert_id="zara:expert/parent",
        principal="user:alice",
        workspace="ws:main",
        delegation_policy=experts.DelegationPolicy.CHILDREN,
        limits=ExpertLimits(max_model_calls=1),
    )
    registry._delegation_stack().append(parent)

    reload_finished = threading.Event()

    def reload_while_child_runs() -> None:
        assert handler.entered.wait(timeout=1.0), "child handler did not enter dispatch"
        registry.reload([(descriptor, handler)])
        reload_finished.set()
        handler.release.set()

    reloader = threading.Thread(target=reload_while_child_runs, daemon=True)
    reloader.start()
    try:
        with pytest.raises(ExpertStaleGenerationError):
            registry.invoke(
                handle,
                "query",
                {"text": "hold"},
                limits=ExpertLimits(max_model_calls=1),
            )
    finally:
        handler.release.set()
        reloader.join(timeout=1.0)
        popped = registry._delegation_stack().pop()
        assert popped is parent

    assert reload_finished.is_set()
    assert parent.remaining_model_calls == 0
    assert parent.delegated_model_calls == 1
