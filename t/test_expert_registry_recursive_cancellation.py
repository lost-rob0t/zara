"""Recursive cancellation acceptance for canonical ZARA-EXPERT/1 delegation."""

from __future__ import annotations

import threading
from typing import Any

from zara.experts import (
    ExpertDeniedError,
    ExpertDescriptor,
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
            "manifest_digest": f"sha256:{expert_id.rsplit('/', 1)[-1]}.root-cancel.v1",
            "name": expert_id,
            "description": "Recursive cancellation acceptance fixture.",
            "source_reference": "t/test_expert_registry_recursive_cancellation.py",
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


def _terminal(verdict: str = "succeeded") -> dict[str, Any]:
    return {
        "verdict": verdict,
        "data": {},
        "evidence_refs": [] if verdict == "cancelled" else ["ev:symbolic"],
        "usage": {"model_calls": 0},
        "effect_receipts": [],
    }


class _BlockingLeaf:
    def __init__(self) -> None:
        self.entered = threading.Event()
        self.release = threading.Event()
        self.calls = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        del expert_operation, payload
        self.calls += 1
        self.entered.set()
        if not self.release.wait(timeout=5.0):
            raise AssertionError("leaf backend was not released")
        return _terminal()


def _install_three_level_chain(
    registry: ExpertRegistry,
    leaf: _BlockingLeaf,
) -> tuple[Any, dict[str, Any]]:
    handles: dict[str, Any] = {}

    class Middle:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            child = registry.invoke(
                handles["leaf"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=0),
            )
            return _terminal(child.verdict.value)

    class Root:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            child = registry.invoke(
                handles["middle"],
                "parse",
                {"text": payload["text"]},
                limits=ExpertLimits(max_model_calls=0),
            )
            return _terminal(child.verdict.value)

    registry.reload(
        [
            (
                _descriptor("zara:expert/root", delegation_policy="children"),
                Root(),
            ),
            (
                _descriptor("zara:expert/middle", delegation_policy="children"),
                Middle(),
            ),
            (
                _descriptor("zara:expert/leaf", delegation_policy="never"),
                leaf,
            ),
        ]
    )
    for name in ("root", "middle", "leaf"):
        handles[name], _ = registry.activate(
            "user:alice",
            "ws:main",
            f"zara:expert/{name}",
        )
    return handles["root"], handles


def test_cancelling_root_recursively_fences_live_descendants() -> None:
    registry = ExpertRegistry()
    leaf = _BlockingLeaf()
    root_handle, _handles = _install_three_level_chain(registry, leaf)
    outcome: dict[str, Any] = {}

    def invoke_root() -> None:
        outcome["result"] = registry.invoke(
            root_handle,
            "parse",
            {"text": "nested cancellation"},
            limits=ExpertLimits(max_model_calls=0),
        )

    worker = threading.Thread(target=invoke_root, daemon=True)
    worker.start()
    assert leaf.entered.wait(timeout=5.0)

    invocation_ids = registry.snapshot().invocation_ids
    assert len(invocation_ids) == 3
    root_invocation_id = next(
        invocation_id
        for invocation_id in invocation_ids
        if registry.explain(invocation_id)["expert_id"] == "zara:expert/root"
    )
    receipt = registry.cancel(root_invocation_id)
    assert receipt["cancelled"] is True
    assert receipt["committed"] is False

    leaf.release.set()
    worker.join(timeout=5.0)
    assert not worker.is_alive()

    result = outcome["result"]
    assert result.verdict is ExpertVerdict.CANCELLED
    assert result.data == {}
    assert result.evidence_refs == ()
    assert result.effect_receipts == ()
    assert type(result.usage["model_calls"]) is int
    assert result.usage["model_calls"] == 0

    traces = [registry.explain(invocation_id) for invocation_id in invocation_ids]
    assert {trace["expert_id"] for trace in traces} == {
        "zara:expert/root",
        "zara:expert/middle",
        "zara:expert/leaf",
    }
    for trace in traces:
        assert trace["verdict"] == "cancelled"
        assert trace["evidence_refs"] == []
        assert type(trace["usage"]["model_calls"]) is int
        assert trace["usage"]["model_calls"] == 0
    assert leaf.calls == 1


def test_cancelled_parent_cannot_dispatch_a_later_child() -> None:
    registry = ExpertRegistry()
    child_calls = 0
    parent_ready = threading.Event()
    parent_release = threading.Event()
    blocked: dict[str, bool] = {"value": False}
    handles: dict[str, Any] = {}

    class Child:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            nonlocal child_calls
            del expert_operation, payload
            child_calls += 1
            return _terminal()

    class Parent:
        def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
            del expert_operation
            parent_ready.set()
            if not parent_release.wait(timeout=5.0):
                raise AssertionError("parent backend was not released")
            try:
                registry.invoke(
                    handles["child"],
                    "parse",
                    {"text": payload["text"]},
                    limits=ExpertLimits(max_model_calls=0),
                )
            except ExpertDeniedError:
                blocked["value"] = True
            return _terminal()

    registry.reload(
        [
            (
                _descriptor("zara:expert/parent", delegation_policy="children"),
                Parent(),
            ),
            (
                _descriptor("zara:expert/child", delegation_policy="never"),
                Child(),
            ),
        ]
    )
    handles["parent"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/parent"
    )
    handles["child"], _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/child"
    )
    outcome: dict[str, Any] = {}

    def invoke_parent() -> None:
        outcome["result"] = registry.invoke(
            handles["parent"],
            "parse",
            {"text": "cancel before child"},
            limits=ExpertLimits(max_model_calls=0),
        )

    worker = threading.Thread(target=invoke_parent, daemon=True)
    worker.start()
    assert parent_ready.wait(timeout=5.0)

    invocation_ids = registry.snapshot().invocation_ids
    assert len(invocation_ids) == 1
    receipt = registry.cancel(invocation_ids[0])
    assert receipt["cancelled"] is True
    parent_release.set()
    worker.join(timeout=5.0)

    assert not worker.is_alive()
    assert blocked["value"] is True
    assert child_calls == 0
    assert outcome["result"].verdict is ExpertVerdict.CANCELLED
    assert registry.snapshot().invocation_ids == invocation_ids
