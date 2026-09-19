from __future__ import annotations

import csv
from dataclasses import replace
from pathlib import Path

import pytest

from zara.runtime.registry import (
    ControlOwner,
    IncompatibleRuntimeProtocol,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeLocality,
    RuntimeRegistry,
    RuntimeTransport,
    RuntimeUnavailable,
    ZARA_RUNTIME_PROTOCOL,
)


FIXTURE = Path(__file__).resolve().parents[1] / "contracts" / "zara-runtime-v1" / "descriptors.tsv"


def _truth(value: str) -> bool:
    if value == "true":
        return True
    if value == "false":
        return False
    raise AssertionError(f"invalid fixture boolean: {value!r}")


def _items(value: str) -> tuple[str, ...]:
    if not value or value == "-":
        return ()
    return tuple(part for part in value.split(",") if part)


def fixture_descriptors() -> list[RuntimeDescriptor]:
    with FIXTURE.open(newline="", encoding="utf-8") as handle:
        rows = csv.DictReader(handle, delimiter="\t")
        return [
            RuntimeDescriptor(
                id=row["id"],
                display_name=row["display_name"],
                protocol=row["protocol"],
                runtime_version=row["runtime_version"],
                implementation_version=row["implementation_version"],
                installed=_truth(row["installed"]),
                available=_truth(row["available"]),
                health=RuntimeHealth(row["health"]),
                locality=RuntimeLocality(row["locality"]),
                transport=RuntimeTransport(row["transport"]),
                capabilities=_items(row["capabilities"]),
                profiles=_items(row["profiles"]),
                provider_control=ControlOwner(row["provider_control"]),
                model_control=ControlOwner(row["model_control"]),
                supports_streaming=_truth(row["supports_streaming"]),
                supports_cancel=_truth(row["supports_cancel"]),
                supports_context_handles=_truth(row["supports_context_handles"]),
                supports_host_tools=_truth(row["supports_host_tools"]),
                provenance=row["provenance"],
            )
            for row in rows
        ]


def test_shared_fixture_exposes_only_installed_compatible_runtime_as_selectable() -> None:
    registry = RuntimeRegistry()
    snapshot = registry.refresh(fixture_descriptors())

    assert [item.id for item in snapshot.descriptors] == [
        "future-runtime",
        "prolog-rlm",
        "zara-python",
    ]
    assert [item.id for item in registry.selectable()] == ["zara-python"]
    assert registry.capabilities("zara-python").protocol == ZARA_RUNTIME_PROTOCOL
    assert registry.capabilities("prolog-rlm").selectable is False


def test_absent_optional_runtime_is_not_selectable() -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())

    with pytest.raises(RuntimeUnavailable, match="prolog-rlm"):
        registry.select("prolog-rlm")


def test_unknown_protocol_major_fails_closed() -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())

    with pytest.raises(IncompatibleRuntimeProtocol, match="ZARA-RUNTIME/2"):
        registry.select("future-runtime")


def test_auto_is_routing_policy_not_runtime_identity() -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())

    with pytest.raises(RuntimeUnavailable, match="routing policy"):
        registry.select("auto")


def test_duplicate_discovery_fails_atomically() -> None:
    registry = RuntimeRegistry()
    original = registry.refresh(fixture_descriptors())
    duplicate = fixture_descriptors()[0]

    with pytest.raises(ValueError, match="duplicate runtime id"):
        registry.refresh([*fixture_descriptors(), duplicate])

    assert registry.snapshot() == original


def test_identical_discovery_refresh_is_generation_noop_and_preserves_binding() -> None:
    registry = RuntimeRegistry()
    fixture = fixture_descriptors()
    registry.refresh(fixture)
    registry.select("zara-python")
    binding = registry.bind_invocation("ctx:turn-1", capability_refs=("cap:tool-42",))
    before = registry.snapshot()

    after = registry.refresh(list(reversed(fixture)))

    assert after == before
    assert registry.accepts_binding(binding, context_ref="ctx:turn-1")


def test_refresh_invalidates_selection_generation_when_runtime_disappears() -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())
    selected = registry.select("zara-python")

    assert registry.accepts_generation("zara-python", selected.generation)

    registry.refresh([item for item in fixture_descriptors() if item.id != "zara-python"])

    assert registry.current() is None
    assert registry.accepts_generation("zara-python", selected.generation) is False
    assert registry.snapshot().generation > selected.generation


def test_runtime_death_invalidates_selection_and_stale_generation() -> None:
    registry = RuntimeRegistry()
    fixture = fixture_descriptors()
    registry.refresh(fixture)
    selected = registry.select("zara-python")

    dead = [
        replace(item, available=False, health=RuntimeHealth.FAILED)
        if item.id == "zara-python"
        else item
        for item in fixture
    ]
    snapshot = registry.refresh(dead)

    assert registry.current() is None
    assert registry.health("zara-python") is RuntimeHealth.FAILED
    assert registry.accepts_generation("zara-python", selected.generation) is False
    assert snapshot.generation > selected.generation


def test_invocation_binding_requires_selected_runtime_and_exact_host_context() -> None:
    registry = RuntimeRegistry()
    fixture = fixture_descriptors()
    registry.refresh(fixture)

    with pytest.raises(RuntimeUnavailable, match="selected"):
        registry.bind_invocation("ctx:turn-1")

    selected = registry.select("zara-python")
    binding = registry.bind_invocation(
        "ctx:turn-1",
        capability_refs=("cap:tool-42", "cap:memory-read"),
    )

    assert binding.runtime_id == selected.runtime_id
    assert binding.generation == selected.generation
    assert registry.accepts_binding(binding, context_ref="ctx:turn-1")
    assert registry.accepts_binding(binding, context_ref="ctx:turn-2") is False

    changed = [
        replace(item, health=RuntimeHealth.DEGRADED)
        if item.id == "zara-python"
        else item
        for item in fixture
    ]
    registry.refresh(changed)

    assert registry.accepts_binding(binding, context_ref="ctx:turn-1") is False


def test_invocation_binding_rejects_wrong_runtime_identity() -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())
    registry.select("zara-python")
    binding = registry.bind_invocation("ctx:turn-1")

    forged = replace(binding, runtime_id="prolog-rlm")

    assert registry.accepts_binding(forged, context_ref="ctx:turn-1") is False


@pytest.mark.parametrize(
    "unsafe_context_ref",
    [
        "",
        "turn-1",
        "principal:user-1",
        "ctx:turn?api_key=secret",
        "ctx:../../secrets",
    ],
)
def test_invocation_binding_rejects_unbounded_or_authority_bearing_context_refs(
    unsafe_context_ref: str,
) -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())
    registry.select("zara-python")

    with pytest.raises(ValueError, match="context_ref"):
        registry.bind_invocation(unsafe_context_ref)


@pytest.mark.parametrize(
    "unsafe_capability_refs",
    [
        ("shell:exec",),
        ("filesystem:read",),
        ("secret:openai",),
        ("principal:admin",),
        ("plugin:registry",),
        ("cap:tool?token=secret",),
        ("cap:tool-42", "cap:tool-42"),
    ],
)
def test_invocation_binding_carries_only_opaque_host_capability_refs(
    unsafe_capability_refs: tuple[str, ...],
) -> None:
    registry = RuntimeRegistry()
    registry.refresh(fixture_descriptors())
    registry.select("zara-python")

    with pytest.raises(ValueError, match="capability_refs"):
        registry.bind_invocation("ctx:turn-1", capability_refs=unsafe_capability_refs)


def test_descriptor_wire_projection_is_bounded_and_transport_neutral() -> None:
    descriptor = next(item for item in fixture_descriptors() if item.id == "zara-python")

    wire = descriptor.to_wire()

    assert wire["protocol"] == ZARA_RUNTIME_PROTOCOL
    assert wire["capabilities"] == [
        "chat",
        "streaming",
        "cancel",
        "context_handles",
        "host_tools",
    ]
    assert "provider_payload" not in wire
    assert "secret" not in wire


@pytest.mark.parametrize(
    "unsafe_provenance",
    [
        "https://user:secret@host/runtime",
        "provider:token=abc123",
        "env:OPENAI_API_KEY=secret",
        "runtime:source?api_key=secret",
    ],
)
def test_descriptor_rejects_secret_bearing_provenance(unsafe_provenance: str) -> None:
    descriptor = next(item for item in fixture_descriptors() if item.id == "zara-python")

    with pytest.raises(ValueError, match="provenance"):
        replace(descriptor, provenance=unsafe_provenance)
