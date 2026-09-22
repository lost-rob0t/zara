"""Canonical consumer-only expert invocation port tests for #1238.

The port is a narrow view over the existing ZARA-EXPERT/1 registry. It must not
issue activations, register experts, own budgets, or create another dispatcher.
"""

from __future__ import annotations

from typing import Any

import pytest

from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertDeniedError,
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ExpertRequest,
    ExpertVerdict,
)


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": "zara:expert/port-fixture",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:port-fixture-v1",
            "name": "Port Fixture",
            "description": "Deterministic fixture for the canonical invocation port.",
            "source_reference": "t/test_expert_invocation_port.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "route.explain",
                    "input_schema": {"fields": []},
                    "output_schema": {"fields": []},
                }
            ],
            "applicability": {"keywords": ["port", "fixture"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux", "android"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {
                "timeout_ms": 1000,
                "max_results": 4,
                "max_output_bytes": 4096,
                "max_model_calls": 0,
            },
            "availability": "ready",
        }
    )


def _handler(**_kwargs: Any) -> dict[str, object]:
    return {
        "verdict": "succeeded",
        "data": {},
        "evidence_refs": ["ev:canonical-port-fixture"],
        "usage": {"model_calls": 0},
        "effect_receipts": [],
    }


def _registry() -> ExpertRegistry:
    registry = ExpertRegistry()
    registry.reload([(_descriptor(), _handler)])
    return registry


def test_port_returns_exact_existing_activation_without_issuing_one() -> None:
    registry = _registry()
    handle, _ = registry.activate(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    )
    port = CanonicalExpertInvocationPort(registry)

    assert port.active_activation(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    ) == handle
    assert port.current_registry_generation() == registry.generation
    assert port.current_runtime_generation() == registry.runtime_generation

    # Consumer composition must not gain registry/lifecycle mutation authority.
    assert not hasattr(port, "activate")
    assert not hasattr(port, "register")
    assert not hasattr(port, "reload")
    assert not hasattr(port, "deactivate")


def test_port_does_not_project_stale_activation_after_generation_advance() -> None:
    registry = _registry()
    handle, _ = registry.activate(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    )
    port = CanonicalExpertInvocationPort(registry)

    assert port.active_activation(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    ) == handle

    # Registry reload is the canonical authority cutover. The registry deliberately
    # keeps old activation records for lifecycle/audit semantics, so the consumer
    # projection must not surface a handle minted under the superseded generations.
    registry.reload([])

    assert registry.generation != handle.registry_generation
    assert registry.runtime_generation != handle.runtime_generation
    assert port.active_activation(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    ) is None


def test_port_missing_or_ambiguous_activation_fails_closed() -> None:
    registry = _registry()
    port = CanonicalExpertInvocationPort(registry)

    assert port.active_activation(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    ) is None

    registry.activate("user:alice", "ws:main", "zara:expert/port-fixture")
    registry.activate("user:alice", "ws:main", "zara:expert/port-fixture")

    with pytest.raises(ExpertDeniedError, match="ambiguous"):
        port.active_activation(
            "user:alice",
            "ws:main",
            "zara:expert/port-fixture",
        )


def test_port_invokes_only_through_canonical_request_path_with_zero_model_budget() -> None:
    registry = _registry()
    handle, _ = registry.activate(
        "user:alice",
        "ws:main",
        "zara:expert/port-fixture",
    )
    port = CanonicalExpertInvocationPort(registry)
    request = ExpertRequest(
        request_id="req:canonical-port-1",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id=handle.expert_id,
        expert_operation="route.explain",
        expected_registry_generation=handle.registry_generation,
        expected_runtime_generation=handle.runtime_generation,
        input={},
        limits=ExpertLimits(
            timeout_ms=1000,
            max_results=4,
            max_output_bytes=4096,
            max_model_calls=0,
        ),
        idempotency_key="idem:canonical-port-1",
    )

    result = port.invoke(request)

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.activation_id == handle.activation_id
    assert result.resolved_registry_generation == registry.generation
    assert result.resolved_runtime_generation == registry.runtime_generation
    assert result.usage == {"model_calls": 0}
    assert result.evidence_refs == ("ev:canonical-port-fixture",)
