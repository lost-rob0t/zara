"""Live cancellation and stale-generation fences for ZARA-EXPERT/1."""

from __future__ import annotations

import threading
from typing import Any

import pytest

from zara.experts import (
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ExpertStaleGenerationError,
    ExpertVerdict,
)


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": "zara:expert/live-fence",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:live.fence.v1",
            "name": "Live Fence",
            "description": "Exercises cancellation and stale-generation fencing.",
            "source_reference": "t/test_expert_registry_live_fences.py",
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
            "applicability": {"keywords": ["fence"]},
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


class BlockingHandler:
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
            "evidence_refs": ["ev:late"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


class ReplacementHandler:
    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "query"
        return {
            "verdict": "succeeded",
            "data": {"source": "replacement", "text": payload["text"]},
            "evidence_refs": ["ev:replacement"],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def _start_blocked_invocation(
    registry: ExpertRegistry,
    handler: BlockingHandler,
) -> tuple[Any, dict[str, Any], threading.Thread]:
    registry.reload([(_descriptor(), handler)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/live-fence")
    outcome: dict[str, Any] = {}

    def invoke() -> None:
        try:
            outcome["result"] = registry.invoke(
                handle,
                "query",
                {"text": "hold"},
                limits=ExpertLimits(max_model_calls=0),
            )
        except BaseException as error:  # pragma: no cover - asserted by caller
            outcome["error"] = error

    worker = threading.Thread(target=invoke, daemon=True)
    worker.start()
    assert handler.entered.wait(timeout=1.0), "handler did not enter dispatch"
    return handle, outcome, worker


def test_cancel_fences_live_handler_before_late_success_commit() -> None:
    registry = ExpertRegistry()
    handler = BlockingHandler()
    _, outcome, worker = _start_blocked_invocation(registry, handler)
    cancel_outcome: dict[str, Any] = {}

    def cancel_live() -> None:
        invocation_ids = registry.snapshot().invocation_ids
        assert len(invocation_ids) == 1
        cancel_outcome.update(registry.cancel(invocation_ids[0]))

    canceller = threading.Thread(target=cancel_live, daemon=True)
    canceller.start()
    canceller.join(timeout=0.5)
    if canceller.is_alive():
        handler.release.set()
        worker.join(timeout=1.0)
        canceller.join(timeout=1.0)
        pytest.fail("live cancellation was blocked behind the running handler")

    assert cancel_outcome["cancelled"] is True
    assert cancel_outcome["committed"] is False
    handler.release.set()
    worker.join(timeout=1.0)

    assert not worker.is_alive()
    assert "error" not in outcome
    result = outcome["result"]
    assert result.verdict is ExpertVerdict.CANCELLED
    assert result.data == {}


def test_reload_fences_stale_completion_and_does_not_restore_old_handler() -> None:
    registry = ExpertRegistry()
    handler = BlockingHandler()
    _, outcome, worker = _start_blocked_invocation(registry, handler)
    replacement = ReplacementHandler()
    reloaded = threading.Event()

    def reload_registry() -> None:
        registry.reload([(_descriptor(), replacement)])
        reloaded.set()

    reloader = threading.Thread(target=reload_registry, daemon=True)
    reloader.start()
    if not reloaded.wait(timeout=0.5):
        handler.release.set()
        worker.join(timeout=1.0)
        reloader.join(timeout=1.0)
        pytest.fail("registry reload was blocked behind the running handler")

    handler.release.set()
    worker.join(timeout=1.0)

    assert not worker.is_alive()
    assert isinstance(outcome.get("error"), ExpertStaleGenerationError)

    fresh_handle, _ = registry.activate(
        "user:alice", "ws:main", "zara:expert/live-fence"
    )
    fresh = registry.invoke(
        fresh_handle,
        "query",
        {"text": "fresh"},
        limits=ExpertLimits(max_model_calls=0),
    )
    assert fresh.verdict is ExpertVerdict.SUCCEEDED
    assert fresh.data == {"source": "replacement", "text": "fresh"}
