"""ZARA-EXPERT/1 expert registry contract tests (issue #1233 phase 1)."""

from __future__ import annotations

import csv
import threading
from dataclasses import replace
from pathlib import Path
from typing import Any

import pytest

from zara.experts import (
    ACTIVATION_TRANSITIONS,
    EXPERT_OPERATIONS,
    HOST_CEILINGS,
    ZARA_EXPERT_PROTOCOL,
    ExpertAmbiguityError,
    ExpertBudgetExceededError,
    ExpertContractError,
    ExpertDescriptor,
    ExpertErrorCode,
    ExpertIncompatibleProtocolError,
    ExpertInvalidInputError,
    ExpertLimits,
    ExpertRegistry,
    ExpertRequest,
    ExpertStaleGenerationError,
    ExpertUnavailableError,
    ExpertUnsupportedBackendError,
    ExpertUnsupportedOperationError,
    ExpertVerdict,
    LifecycleState,
    activation_transition,
)


FIXTURE = Path(__file__).resolve().parents[1] / "contracts" / "zara-expert-v1" / "descriptors.tsv"


def _items(value: str) -> tuple[str, ...]:
    if not value or value == "-":
        return ()
    return tuple(part for part in value.split(",") if part)


def _scalar(value: str) -> str:
    return "" if value == "-" else value


def fixture_rows() -> list[dict[str, str]]:
    with FIXTURE.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def hydrate_row(row: dict[str, str]) -> dict[str, Any]:
    wire: dict[str, Any] = {
        "protocol": row["protocol"],
        "expert_id": row["expert_id"],
        "expert_version": row["expert_version"],
        "package_namespace": row["package_namespace"],
        "manifest_digest": row["manifest_digest"],
        "name": row["name"],
        "description": row["description"],
        "source_reference": row["source_reference"],
        "reasoning_kind": row["reasoning_kind"],
        "operations": [
            {
                "operation_id": operation_id,
                "input_schema": {"fields": []},
                "output_schema": {"fields": []},
            }
            for operation_id in _items(row["operations"])
        ],
        "applicability": {"keywords": list(_items(row["applicability_keywords"]))},
        "required_capabilities": list(_items(row["required_capabilities"])),
        "possible_effects": list(_items(row["possible_effects"])),
        "supported_engines": list(_items(row["supported_engines"])),
        "supported_platforms": list(_items(row["supported_platforms"])),
        "fallback_policy": row["fallback_policy"],
        "delegation_policy": row["delegation_policy"],
        "registry_generation": 1,
        "availability": row["availability"],
    }
    reason = _scalar(row["unavailable_reason"])
    if reason:
        wire["unavailable_reason"] = reason
    return wire


def compatible_fixture_wires() -> list[dict[str, Any]]:
    return [
        hydrate_row(row)
        for row in fixture_rows()
        if row["protocol"] == ZARA_EXPERT_PROTOCOL
    ]


def expert_wire(**overrides: Any) -> dict[str, Any]:
    wire: dict[str, Any] = {
        "protocol": ZARA_EXPERT_PROTOCOL,
        "expert_id": "zara:expert/todo",
        "expert_version": "1.0.0",
        "package_namespace": "zara",
        "manifest_digest": "sha256:todo.expert.v1",
        "name": "Todo Expert",
        "description": "Diagnoses todo state and explains blocked routing decisions.",
        "source_reference": "contracts/zara-expert-v1/descriptors.tsv",
        "reasoning_kind": "symbolic",
        "operations": [
            {
                "operation_id": "route.diagnose",
                "input_schema": {
                    "fields": [
                        {"name": "symptom_id", "type": "string", "required": True},
                        {
                            "name": "mode",
                            "type": "enum",
                            "required": False,
                            "enum_values": ["quick", "deep"],
                        },
                    ]
                },
                "output_schema": {
                    "fields": [{"name": "summary", "type": "string", "required": True}]
                },
            },
            {
                "operation_id": "route.explain",
                "input_schema": {"fields": []},
                "output_schema": {"fields": []},
            },
        ],
        "applicability": {"keywords": ["todo", "tasks", "reminders"]},
        "required_capabilities": [],
        "possible_effects": ["none"],
        "supported_engines": [],
        "supported_platforms": ["linux"],
        "fallback_policy": "fail_closed",
        "delegation_policy": "never",
        "registry_generation": 1,
        "availability": "ready",
    }
    wire.update(overrides)
    return wire


def descriptor(**overrides: Any) -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(expert_wire(**overrides))


class RecordingHandler:
    def __init__(
        self,
        *,
        effect_receipts: tuple[dict[str, Any], ...] = (),
        raises: bool = False,
        returns: Any = None,
    ) -> None:
        self.calls: list[dict[str, Any]] = []
        self.effect_receipts = effect_receipts
        self.raises = raises
        self.returns = returns

    def __call__(self, **kwargs: Any) -> Any:
        self.calls.append(kwargs)
        if self.raises:
            raise RuntimeError("expert handler exploded")
        if self.returns is not None:
            return self.returns
        return {
            "verdict": "succeeded",
            "data": {"summary": "todo routing explained"},
            "evidence_refs": ["ev:todo-fixture"],
            "usage": {"model_calls": 0},
            "effect_receipts": list(self.effect_receipts),
        }


def fixture_registry() -> ExpertRegistry:
    registry = ExpertRegistry()
    staged = [
        (ExpertDescriptor.from_wire(wire), RecordingHandler())
        for wire in compatible_fixture_wires()
        if wire["availability"] != "absent"
    ]
    staged.extend(
        (ExpertDescriptor.from_wire(wire), None)
        for wire in compatible_fixture_wires()
        if wire["availability"] == "absent"
    )
    registry.reload(staged)
    return registry


def todo_registry() -> tuple[ExpertRegistry, RecordingHandler, Any]:
    registry = ExpertRegistry()
    handler = RecordingHandler()
    registry.reload([(descriptor(), handler)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")
    return registry, handler, handle


def test_operation_vocabulary_is_closed() -> None:
    assert EXPERT_OPERATIONS == frozenset(
        {
            "expert.list",
            "expert.describe",
            "expert.match",
            "expert.activate",
            "expert.status",
            "expert.invoke",
            "expert.explain",
            "expert.cancel",
            "expert.deactivate",
        }
    )
    assert ZARA_EXPERT_PROTOCOL == "ZARA-EXPERT/1"


def test_shared_fixture_loads_and_catalog_excludes_absent_expert() -> None:
    rows = fixture_rows()
    assert len(rows) == 4
    registry = fixture_registry()

    listing = registry.list_experts("user:alice")

    assert listing["total"] == 2
    assert [item["expert_id"] for item in listing["experts"]] == [
        "zara:expert/android-troubleshooting",
        "zara:expert/todo",
    ]
    assert all(item["availability"] != "absent" for item in listing["experts"])


def test_unknown_protocol_major_fails_closed() -> None:
    future_row = next(row for row in fixture_rows() if row["protocol"] == "ZARA-EXPERT/2")

    with pytest.raises(ExpertIncompatibleProtocolError, match="ZARA-EXPERT/2") as excinfo:
        ExpertDescriptor.from_wire(hydrate_row(future_row))

    assert excinfo.value.code is ExpertErrorCode.INCOMPATIBLE_PROTOCOL


def test_from_wire_fails_closed_on_unknown_keys_and_unknown_enums() -> None:
    unknown_key = expert_wire(handler="module:callable")
    with pytest.raises(ExpertInvalidInputError, match="handler"):
        ExpertDescriptor.from_wire(unknown_key)

    unknown_enum = expert_wire(reasoning_kind="neural")
    with pytest.raises(ExpertInvalidInputError, match="reasoning_kind"):
        ExpertDescriptor.from_wire(unknown_enum)


def test_reload_with_invalid_staged_set_fails_atomically_retaining_last_good() -> None:
    registry = fixture_registry()
    before = registry.snapshot()
    staged = [
        (ExpertDescriptor.from_wire(wire), RecordingHandler())
        for wire in compatible_fixture_wires()
    ]
    duplicate = ExpertDescriptor.from_wire(compatible_fixture_wires()[0])

    with pytest.raises(ExpertInvalidInputError, match="duplicate expert id"):
        registry.reload([*staged, (duplicate, RecordingHandler())])

    assert registry.snapshot() == before


def test_reload_with_semantic_change_bumps_generation_and_fences_prior_handles() -> None:
    registry = fixture_registry()
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")
    generation_before = registry.generation

    surviving = [
        (ExpertDescriptor.from_wire(wire), RecordingHandler())
        for wire in compatible_fixture_wires()
        if wire["expert_id"] != "zara:expert/todo"
    ]
    registry.reload(surviving)

    assert registry.generation > generation_before
    with pytest.raises(ExpertStaleGenerationError) as excinfo:
        registry.invoke(handle, "route.explain", {})
    assert excinfo.value.code is ExpertErrorCode.STALE_GENERATION


def test_identical_reload_is_generation_noop_and_preserves_handles() -> None:
    registry = ExpertRegistry()
    staged = [
        (ExpertDescriptor.from_wire(wire), RecordingHandler())
        for wire in compatible_fixture_wires()
        if wire["availability"] != "absent"
    ]
    registry.reload(staged)
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")
    generation_before = registry.generation
    snapshot_before = registry.snapshot()

    registry.reload(list(reversed(staged)))

    assert registry.generation == generation_before
    assert registry.snapshot() == snapshot_before
    result = registry.invoke(handle, "route.explain", {})
    assert result.verdict is ExpertVerdict.SUCCEEDED


def test_alias_collision_across_package_namespaces_is_typed_error() -> None:
    registry = ExpertRegistry()
    registry.register(descriptor(), RecordingHandler())
    colliding = descriptor(
        expert_id="other:expert/todo",
        package_namespace="other",
        manifest_digest="sha256:other.todo.v1",
    )

    with pytest.raises(ExpertInvalidInputError, match="alias") as excinfo:
        registry.register(colliding, RecordingHandler())

    assert excinfo.value.code is ExpertErrorCode.INVALID_INPUT
    assert registry.snapshot().expert_ids == ("zara:expert/todo",)


def test_duplicate_expert_id_registration_fails() -> None:
    registry = ExpertRegistry()
    registry.register(descriptor(), RecordingHandler())

    with pytest.raises(ExpertInvalidInputError, match="duplicate expert id"):
        registry.register(descriptor(), RecordingHandler())


def test_activate_returns_bound_handle_and_receipt() -> None:
    registry = fixture_registry()
    generation = registry.generation

    handle, receipt = registry.activate(
        "user:alice", "ws:main", "zara:expert/android-troubleshooting"
    )

    assert handle.principal == "user:alice"
    assert handle.workspace == "ws:main"
    assert handle.expert_id == "zara:expert/android-troubleshooting"
    assert handle.registry_generation == generation
    assert handle.runtime_generation == registry.runtime_generation
    assert receipt["state"] == "active"
    assert receipt["activation_id"] == handle.activation_id


def test_invoke_completes_with_domain_verdict() -> None:
    registry, handler, handle = todo_registry()

    result = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck", "mode": "deep"},
        limits=ExpertLimits(),
        idempotency_key="diagnose-1",
    )

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.expert_id == "zara:expert/todo"
    assert result.expert_version == "1.0.0"
    assert result.manifest_digest == "sha256:todo.expert.v1"
    assert result.resolved_registry_generation == registry.generation
    assert result.expert_operation == "route.diagnose"
    assert result.error_code is None
    assert handler.calls == [{"symptom_id": "todo-stuck", "mode": "deep"}]


def test_request_envelope_round_trip_and_validation() -> None:
    registry, _, handle = todo_registry()

    request = ExpertRequest(
        request_id="req-42",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id="zara:expert/todo",
        expert_operation="route.explain",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
        input={},
    )

    result = registry.invoke_request(request)

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.request_id == "req-42"

    with pytest.raises(ExpertInvalidInputError, match="operation"):
        ExpertRequest(
            request_id="req-43",
            operation="expert.teleport",
            activation_id=handle.activation_id,
            expert_id="zara:expert/todo",
            expert_operation="route.explain",
            expected_registry_generation=None,
            expected_runtime_generation=None,
            input={},
        )
    with pytest.raises(ExpertIncompatibleProtocolError):
        ExpertRequest(
            protocol="ZARA-EXPERT/2",
            request_id="req-44",
            operation="expert.invoke",
            activation_id=handle.activation_id,
            expert_id="zara:expert/todo",
            expert_operation="route.explain",
            expected_registry_generation=None,
            expected_runtime_generation=None,
            input={},
        )


def test_invoke_request_fences_expected_generations() -> None:
    registry, _, handle = todo_registry()

    stale_request = ExpertRequest(
        request_id="req-45",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id="zara:expert/todo",
        expert_operation="route.explain",
        expected_registry_generation=registry.generation + 7,
        expected_runtime_generation=None,
        input={},
    )

    with pytest.raises(ExpertStaleGenerationError):
        registry.invoke_request(stale_request)


def test_forged_handle_principal_mismatch_is_denied() -> None:
    registry, _, handle = todo_registry()

    forged_principal = replace(handle, principal="user:eve")
    with pytest.raises(ExpertContractError, match="denied") as denied:
        registry.invoke(forged_principal, "route.explain", {})
    assert denied.value.code is ExpertErrorCode.DENIED

    forged_digest = replace(handle, manifest_digest="sha256:evil")
    with pytest.raises(ExpertContractError) as digest_error:
        registry.invoke(forged_digest, "route.explain", {})
    assert digest_error.value.code is ExpertErrorCode.DENIED

    forged_activation = replace(handle, activation_id="act:" + "0" * 32)
    with pytest.raises(ExpertContractError) as activation_error:
        registry.invoke(forged_activation, "route.explain", {})
    assert activation_error.value.code is ExpertErrorCode.DENIED


@pytest.mark.parametrize(
    ("current", "target"),
    [
        (LifecycleState.INACTIVE, LifecycleState.ACTIVATING),
        (LifecycleState.ACTIVATING, LifecycleState.ACTIVE),
        (LifecycleState.ACTIVATING, LifecycleState.FAILED),
        (LifecycleState.ACTIVE, LifecycleState.DRAINING),
        (LifecycleState.ACTIVE, LifecycleState.UNAVAILABLE),
        (LifecycleState.ACTIVE, LifecycleState.FAILED),
        (LifecycleState.DRAINING, LifecycleState.INACTIVE),
        (LifecycleState.UNAVAILABLE, LifecycleState.ACTIVE),
        (LifecycleState.FAILED, LifecycleState.INACTIVE),
    ],
)
def test_activation_lifecycle_transition_table(
    current: LifecycleState, target: LifecycleState
) -> None:
    assert ACTIVATION_TRANSITIONS == {
        LifecycleState.INACTIVE: frozenset({LifecycleState.ACTIVATING}),
        LifecycleState.ACTIVATING: frozenset({LifecycleState.ACTIVE, LifecycleState.FAILED}),
        LifecycleState.ACTIVE: frozenset(
            {LifecycleState.DRAINING, LifecycleState.UNAVAILABLE, LifecycleState.FAILED}
        ),
        LifecycleState.DRAINING: frozenset({LifecycleState.INACTIVE}),
        LifecycleState.UNAVAILABLE: frozenset({LifecycleState.ACTIVE}),
        LifecycleState.FAILED: frozenset({LifecycleState.INACTIVE}),
    }
    assert activation_transition(current, target) is target


@pytest.mark.parametrize(
    ("current", "target"),
    [
        (LifecycleState.INACTIVE, LifecycleState.ACTIVE),
        (LifecycleState.INACTIVE, LifecycleState.INACTIVE),
        (LifecycleState.ACTIVE, LifecycleState.INACTIVE),
        (LifecycleState.ACTIVE, LifecycleState.ACTIVATING),
        (LifecycleState.DRAINING, LifecycleState.ACTIVE),
        (LifecycleState.UNAVAILABLE, LifecycleState.INACTIVE),
        (LifecycleState.UNAVAILABLE, LifecycleState.FAILED),
        (LifecycleState.FAILED, LifecycleState.ACTIVE),
    ],
)
def test_invalid_activation_transitions_fail_closed(
    current: LifecycleState, target: LifecycleState
) -> None:
    with pytest.raises(ExpertContractError):
        activation_transition(current, target)


def test_deactivate_drains_and_fences_handle() -> None:
    registry, _, handle = todo_registry()

    receipt = registry.deactivate(handle)

    assert receipt["deactivated"] is True
    assert receipt["state"] == "inactive"
    with pytest.raises(ExpertContractError) as excinfo:
        registry.invoke(handle, "route.explain", {})
    assert excinfo.value.code is ExpertErrorCode.DENIED
    with pytest.raises(ExpertContractError):
        registry.deactivate(handle)


def test_backend_loss_marks_unavailable_and_recovery_restores_activation() -> None:
    registry, _, handle = todo_registry()

    registry.mark_backend_unavailable("zara:expert/todo", "swipl crashed")

    assert registry.describe("zara:expert/todo")["availability"] == "unavailable"
    with pytest.raises(ExpertUnavailableError, match="swipl crashed") as excinfo:
        registry.invoke(handle, "route.explain", {})
    assert excinfo.value.code is ExpertErrorCode.UNAVAILABLE
    with pytest.raises(ExpertContractError, match="transition"):
        registry.deactivate(handle)

    registry.recover_backend("zara:expert/todo")

    result = registry.invoke(handle, "route.explain", {})
    assert result.verdict is ExpertVerdict.SUCCEEDED


def test_cancel_after_commit_returns_committed_receipt_without_reversal() -> None:
    registry = ExpertRegistry()
    handler = RecordingHandler(
        effect_receipts=({"effect": "filesystem_read", "receipt": "fx:1"},)
    )
    registry.reload([(descriptor(), handler)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")
    result = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck"},
        idempotency_key="write-1",
    )

    receipt = registry.cancel(result.invocation_id)

    assert receipt["cancelled"] is False
    assert receipt["committed"] is True
    assert receipt["verdict"] == "succeeded"
    assert receipt["effect_receipts"] == [{"effect": "filesystem_read", "receipt": "fx:1"}]
    replay = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck"},
        idempotency_key="write-1",
    )
    assert replay.invocation_id == result.invocation_id
    assert replay.verdict is ExpertVerdict.SUCCEEDED
    assert len(handler.calls) == 1

    with pytest.raises(ExpertInvalidInputError, match="invocation"):
        registry.cancel("inv:unknown")


def test_idempotency_key_replays_prior_result_and_conflicts_on_changed_input() -> None:
    registry, handler, handle = todo_registry()

    first = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck"},
        idempotency_key="op-1",
    )
    replay = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck"},
        idempotency_key="op-1",
    )

    assert replay.invocation_id == first.invocation_id
    assert replay.replayed is True
    assert replay.verdict is first.verdict
    assert len(handler.calls) == 1

    with pytest.raises(ExpertInvalidInputError, match="idempotency") as excinfo:
        registry.invoke(
            handle,
            "route.diagnose",
            {"symptom_id": "different-symptom"},
            idempotency_key="op-1",
        )
    assert excinfo.value.code is ExpertErrorCode.INVALID_INPUT


def test_model_inference_under_zero_model_budget_is_rejected_before_dispatch() -> None:
    registry = ExpertRegistry()
    handler = RecordingHandler()
    model_descriptor = descriptor(
        expert_id="zara:expert/hint",
        manifest_digest="sha256:hint.expert.v1",
        name="Hint Expert",
        description="Model backed hint generation.",
        reasoning_kind="model",
        possible_effects=["model_inference"],
        operations=[
            {
                "operation_id": "hint.generate",
                "input_schema": {"fields": []},
                "output_schema": {"fields": []},
            }
        ],
    )
    registry.reload([(model_descriptor, handler)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/hint")

    with pytest.raises(ExpertBudgetExceededError) as excinfo:
        registry.invoke(
            handle,
            "hint.generate",
            {},
            limits=ExpertLimits(max_model_calls=0),
        )
    assert excinfo.value.code is ExpertErrorCode.BUDGET_EXCEEDED
    assert handler.calls == []

    result = registry.invoke(
        handle,
        "hint.generate",
        {},
        limits=ExpertLimits(max_model_calls=2),
    )
    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert handler.calls == [{}]


def test_match_is_deterministic_and_reports_matched_keywords() -> None:
    registry = fixture_registry()

    best = registry.match("please look at my todo reminders routing")

    assert best is not None
    assert best["expert_id"] == "zara:expert/todo"
    assert best["matched_keywords"] == ["reminders", "todo"]
    assert registry.match("android adb wear build failure")["expert_id"] == (
        "zara:expert/android-troubleshooting"
    )
    assert registry.match("quantum chess opening theory") is None


def test_match_ambiguity_is_typed_error_with_sorted_candidates() -> None:
    registry = fixture_registry()
    registry.register(
        descriptor(
            expert_id="zara:expert/todo-clone",
            manifest_digest="sha256:todo.clone.v1",
            name="Todo Clone",
            applicability={"keywords": ["todo", "tasks"]},
        ),
        RecordingHandler(),
    )

    with pytest.raises(ExpertAmbiguityError) as excinfo:
        registry.match("todo tasks planning")

    assert excinfo.value.code is ExpertErrorCode.AMBIGUITY
    assert excinfo.value.candidates == [
        "zara:expert/todo",
        "zara:expert/todo-clone",
    ]


def test_catalog_projection_whitelist_has_no_registration_internals() -> None:
    secret_wire = expert_wire()
    secret_wire["operations"][0]["input_schema"]["fields"].append(
        {"name": "token_ref", "type": "secret_reference", "required": False}
    )
    expert = ExpertDescriptor.from_wire(secret_wire)

    projection = expert.to_catalog_projection()

    assert set(projection) == {
        "protocol",
        "expert_id",
        "expert_version",
        "package_namespace",
        "manifest_digest",
        "name",
        "description",
        "source_reference",
        "reasoning_kind",
        "operations",
        "applicability_keywords",
        "required_capabilities",
        "possible_effects",
        "supported_engines",
        "supported_platforms",
        "fallback_policy",
        "delegation_policy",
        "availability",
        "unavailable_reason",
    }
    serialized = repr(projection)
    for internal in ("handler", "registration", "aliases", "invocation", "lifecycle"):
        assert internal not in serialized
    secret_field = projection["operations"][0]["input_schema"]["fields"][2]
    assert set(secret_field) == {"name", "type", "required"}
    assert secret_field["type"] == "secret_reference"


def test_registration_rejects_wrong_handler_arity() -> None:
    registry = ExpertRegistry()

    with pytest.raises(ExpertInvalidInputError, match="handler") as excinfo:
        registry.register(descriptor(), lambda: None)
    assert excinfo.value.code is ExpertErrorCode.INVALID_INPUT

    with pytest.raises(ExpertInvalidInputError, match="callable"):
        registry.register(descriptor(), "not-a-callable")


def test_list_describe_match_perform_no_effects() -> None:
    registry = fixture_registry()
    staged_handlers = []
    for wire in compatible_fixture_wires():
        if wire["availability"] != "absent":
            staged_handlers.append(RecordingHandler())
    watcher_registry = ExpertRegistry()
    for wire, handler in zip(
        [w for w in compatible_fixture_wires() if w["availability"] != "absent"],
        staged_handlers,
    ):
        watcher_registry.register(ExpertDescriptor.from_wire(wire), handler)
    snapshot_before = watcher_registry.snapshot()

    listing = watcher_registry.list_experts("user:alice")
    described = watcher_registry.describe("zara:expert/todo")
    matched = watcher_registry.match("todo tasks")

    assert listing["total"] == 2
    assert described["expert_id"] == "zara:expert/todo"
    assert matched["expert_id"] == "zara:expert/todo"
    assert all(handler.calls == [] for handler in staged_handlers)
    assert watcher_registry.snapshot() == snapshot_before


def test_invoke_validates_input_against_operation_schema() -> None:
    registry, _, handle = todo_registry()

    with pytest.raises(ExpertInvalidInputError, match="unknown"):
        registry.invoke(handle, "route.diagnose", {"rogue_field": "x"})
    with pytest.raises(ExpertInvalidInputError, match="symptom_id"):
        registry.invoke(handle, "route.diagnose", {})
    with pytest.raises(ExpertInvalidInputError, match="symptom_id"):
        registry.invoke(handle, "route.diagnose", {"symptom_id": 42})
    with pytest.raises(ExpertInvalidInputError, match="mode"):
        registry.invoke(handle, "route.diagnose", {"symptom_id": "s", "mode": "slow"})


def test_invoke_rejects_unbounded_input_payloads() -> None:
    registry, _, handle = todo_registry()

    with pytest.raises(ExpertInvalidInputError, match="input"):
        registry.invoke(
            handle,
            "route.diagnose",
            {"symptom_id": "x" * 5000},
        )


def test_invoke_unsupported_operation_fails_closed() -> None:
    registry, _, handle = todo_registry()

    with pytest.raises(ExpertUnsupportedOperationError) as excinfo:
        registry.invoke(handle, "route.nonsense", {})
    assert excinfo.value.code is ExpertErrorCode.UNSUPPORTED_OPERATION


def test_unregistered_handler_prevents_dispatch() -> None:
    registry = ExpertRegistry()
    registry.reload([(descriptor(), None)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")

    with pytest.raises(ExpertUnsupportedOperationError):
        registry.invoke(handle, "route.explain", {})


def test_activation_gates_availability_backend_and_protocol() -> None:
    registry = fixture_registry()

    with pytest.raises(ExpertUnavailableError, match="absent:test-fixture") as absent_error:
        registry.activate("user:alice", "ws:main", "zara:expert/prolog-rlm")
    assert absent_error.value.code is ExpertErrorCode.UNAVAILABLE

    unavailable = descriptor(availability="unavailable", unavailable_reason="fixture down")
    registry.register(unavailable, RecordingHandler())
    with pytest.raises(ExpertUnavailableError, match="fixture down"):
        registry.activate("user:alice", "ws:main", "zara:expert/todo")

    future_descriptor = replace(
        ExpertDescriptor.from_wire(
            expert_wire(
                expert_id="zara:expert/future",
                manifest_digest="sha256:future.expert.v2",
                name="Future Expert",
            )
        ),
        protocol="ZARA-EXPERT/2",
    )
    registry.register(future_descriptor, RecordingHandler())
    with pytest.raises(ExpertIncompatibleProtocolError):
        registry.activate("user:alice", "ws:main", "zara:expert/future")

    exotic_engine = descriptor(
        expert_id="zara:expert/scryer-only",
        manifest_digest="sha256:scryer.expert.v1",
        name="Scryer Expert",
        supported_engines=["scryer"],
    )
    registry.register(exotic_engine, RecordingHandler())
    with pytest.raises(ExpertUnsupportedBackendError) as backend_error:
        registry.activate("user:alice", "ws:main", "zara:expert/scryer-only")
    assert backend_error.value.code is ExpertErrorCode.UNSUPPORTED_BACKEND


def test_activate_fences_expected_generations() -> None:
    registry, _, _ = todo_registry()

    with pytest.raises(ExpertStaleGenerationError):
        registry.activate(
            "user:alice",
            "ws:main",
            "zara:expert/todo",
            expected_registry_generation=registry.generation + 5,
        )
    handle, _ = registry.activate(
        "user:alice",
        "ws:main",
        "zara:expert/todo",
        expected_registry_generation=registry.generation,
        expected_runtime_generation=registry.runtime_generation,
    )
    assert handle.principal == "user:alice"


def test_limits_admission_enforces_host_ceilings() -> None:
    assert HOST_CEILINGS == {
        "timeout_ms": 600_000,
        "max_results": 10_000,
        "max_output_bytes": 10_485_760,
        "max_model_calls": 64,
    }
    assert ExpertLimits().max_model_calls == 0

    with pytest.raises(ExpertInvalidInputError):
        ExpertLimits(timeout_ms=600_001)
    with pytest.raises(ExpertInvalidInputError):
        ExpertLimits(max_model_calls=65)
    with pytest.raises(ExpertInvalidInputError):
        ExpertLimits(max_results=0)

    registry, _, handle = todo_registry()
    with pytest.raises(ExpertInvalidInputError, match="limits"):
        registry.invoke(handle, "route.explain", {}, limits="unbounded")


@pytest.mark.parametrize(
    "handler",
    [
        RecordingHandler(returns=None),
        RecordingHandler(returns="verdict-string"),
        RecordingHandler(returns={"verdict": "fabricated"}),
        RecordingHandler(raises=True),
    ],
    ids=["none", "non-mapping", "bad-verdict", "raises"],
)
def test_unknown_handler_outcomes_map_to_unknown_verdict(handler: RecordingHandler) -> None:
    registry = ExpertRegistry()
    registry.reload([(descriptor(), handler)])
    handle, _ = registry.activate("user:alice", "ws:main", "zara:expert/todo")

    result = registry.invoke(handle, "route.explain", {})

    assert result.verdict is ExpertVerdict.UNKNOWN
    assert result.error_code is ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME


def test_explain_returns_bounded_decision_trace() -> None:
    registry, _, handle = todo_registry()
    result = registry.invoke(
        handle,
        "route.diagnose",
        {"symptom_id": "todo-stuck"},
        idempotency_key="trace-1",
    )

    trace = registry.explain(result.invocation_id)

    assert trace["invocation_id"] == result.invocation_id
    assert trace["expert_id"] == "zara:expert/todo"
    assert trace["verdict"] == "succeeded"
    assert {"handle:validated", "generation:fenced", "input:schema-validated", "dispatch:completed"} <= set(
        trace["decision_refs"]
    )
    assert len(trace["decision_refs"]) <= 16
    assert all(len(ref) <= 128 for ref in trace["decision_refs"])
    assert trace["evidence_refs"] == ["ev:todo-fixture"]

    with pytest.raises(ExpertInvalidInputError, match="invocation"):
        registry.explain("inv:unknown")


def test_concurrent_activate_invoke_deactivate_remain_consistent() -> None:
    registry = ExpertRegistry()
    registry.reload([(descriptor(), RecordingHandler())])
    failures: list[BaseException] = []

    def worker(index: int) -> None:
        try:
            for _ in range(20):
                handle, _ = registry.activate(
                    f"user:w{index}", "ws:main", "zara:expert/todo"
                )
                result = registry.invoke(handle, "route.explain", {})
                assert result.verdict is ExpertVerdict.SUCCEEDED
                receipt = registry.deactivate(handle)
                assert receipt["deactivated"] is True
        except BaseException as error:  # noqa: BLE001
            failures.append(error)

    threads = [
        threading.Thread(target=worker, args=(index,)) for index in range(8)
    ]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

    assert failures == []
    assert registry.snapshot().activation_ids == ()
    assert registry.generation >= 1
