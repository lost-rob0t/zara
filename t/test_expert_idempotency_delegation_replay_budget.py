"""Durable child replay must charge the canonical parent delegation ledger."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pytest

from zara.database import DatabaseManager
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
) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": expert_id,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": f"sha256:{expert_id.rsplit('/', 1)[-1]}.v1",
            "name": expert_id,
            "description": "Durable delegation replay accounting fixture.",
            "source_reference": "t/test_expert_idempotency_delegation_replay_budget.py",
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
            "applicability": {"keywords": ["delegation", "idempotency"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": delegation_policy,
            "resource_limits": {"max_model_calls": max_model_calls},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


class _OneModelCallChild:
    def __init__(self) -> None:
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "parse"
        assert payload == {"text": "model-backed child"}
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": ["ev:durable-child-model-call"],
            "usage": {"model_calls": 1},
            "effect_receipts": [],
        }


class _DelegatingParent:
    def __init__(self, registry: ExpertRegistry, child_handle_box: dict[str, Any]) -> None:
        self.registry = registry
        self.child_handle_box = child_handle_box
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "parse"
        assert payload == {"text": "parent"}
        self.calls += 1
        first = self.registry.invoke(
            self.child_handle_box["handle"],
            "parse",
            {"text": "model-backed child"},
            limits=ExpertLimits(max_model_calls=1),
            idempotency_key="idempotency:delegated-child",
        )
        blocked_second = False
        try:
            self.registry.invoke(
                self.child_handle_box["handle"],
                "parse",
                {"text": "model-backed child"},
                limits=ExpertLimits(max_model_calls=1),
                idempotency_key="idempotency:delegated-child",
            )
        except ExpertBudgetExceededError:
            blocked_second = True
        return {
            "verdict": "succeeded",
            "data": {
                "child_replayed": first.replayed,
                "second_blocked": blocked_second,
            },
            "evidence_refs": ["ev:delegating-parent"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _runtime(
    database: DatabaseManager,
    child: _OneModelCallChild,
) -> tuple[ExpertRegistry, _DelegatingParent, Any, Any]:
    registry = ExpertRegistry(database=database)
    child_handle_box: dict[str, Any] = {}
    parent_handler = _DelegatingParent(registry, child_handle_box)
    registry.reload(
        [
            (
                _descriptor(
                    "zara:expert/delegating-parent",
                    delegation_policy="children",
                    max_model_calls=1,
                ),
                parent_handler,
            ),
            (
                _descriptor(
                    "zara:expert/durable-child",
                    delegation_policy="never",
                    max_model_calls=1,
                ),
                child,
            ),
        ]
    )
    parent_handle, _ = registry.activate(
        "user:delegation-replay",
        "workspace:delegation-replay",
        "zara:expert/delegating-parent",
    )
    child_handle, _ = registry.activate(
        "user:delegation-replay",
        "workspace:delegation-replay",
        "zara:expert/durable-child",
    )
    child_handle_box["handle"] = child_handle
    return registry, parent_handler, parent_handle, child_handle


def test_durable_child_replay_charges_parent_and_parent_replay_keeps_usage(
    tmp_path: Path,
) -> None:
    path = tmp_path / "delegation-replay.db"
    child = _OneModelCallChild()
    first_db = DatabaseManager(path)
    registry, parent_handler, parent_handle, child_handle = _runtime(first_db, child)

    seeded_child = registry.invoke(
        child_handle,
        "parse",
        {"text": "model-backed child"},
        limits=ExpertLimits(max_model_calls=1),
        idempotency_key="idempotency:delegated-child",
    )
    assert seeded_child.verdict is ExpertVerdict.SUCCEEDED
    assert seeded_child.usage == {"model_calls": 1}
    assert child.calls == 1

    parent = registry.invoke(
        parent_handle,
        "parse",
        {"text": "parent"},
        limits=ExpertLimits(max_model_calls=1),
        idempotency_key="idempotency:delegating-parent",
    )

    assert child.calls == 1, "durable child replay must not redispatch the child"
    assert parent_handler.calls == 1
    assert parent.data == {"child_replayed": True, "second_blocked": True}
    assert parent.usage == {"model_calls": 1}
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry, restarted_parent_handler, restarted_parent_handle, _ = _runtime(
        restarted_db,
        child,
    )
    with pytest.raises(
        ExpertBudgetExceededError,
        match="durable replay usage.model_calls exceeds admitted max_model_calls",
    ):
        restarted_registry.invoke(
            restarted_parent_handle,
            "parse",
            {"text": "parent"},
            limits=ExpertLimits(max_model_calls=0),
            idempotency_key="idempotency:delegating-parent",
        )

    assert child.calls == 1
    assert parent_handler.calls == 1
    assert restarted_parent_handler.calls == 0
    restarted_db.close()
