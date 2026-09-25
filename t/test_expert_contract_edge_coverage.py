from __future__ import annotations

from dataclasses import replace
from types import SimpleNamespace

import pytest

from zara import _experts_v1 as impl


def descriptor(**overrides):
    wire = {
        "protocol": impl.ZARA_EXPERT_PROTOCOL,
        "expert_id": "zara:expert/edge",
        "expert_version": "1.0.0",
        "package_namespace": "zara",
        "manifest_digest": "sha256:edge.v1",
        "name": "Edge Expert",
        "description": "Exercises fail-closed contract edges.",
        "source_reference": "t/test_expert_contract_edge_coverage.py",
        "reasoning_kind": "symbolic",
        "operations": [{
            "operation_id": "inspect",
            "input_schema": {"fields": []},
            "output_schema": {"fields": []},
        }],
        "applicability": {"keywords": ["edge"]},
        "required_capabilities": [],
        "possible_effects": ["none"],
        "supported_engines": [],
        "supported_platforms": ["linux"],
        "fallback_policy": "fail_closed",
        "delegation_policy": "never",
        "availability": "ready",
    }
    wire.update(overrides)
    return impl.ExpertDescriptor.from_wire(wire)


def activated_registry(*, engines=("swipl",), desc=None, handler=None):
    registry = impl.ExpertRegistry(engines=engines)
    desc = desc or descriptor()
    if handler is None:
        handler = lambda: {
            "verdict": "succeeded",
            "data": {},
            "evidence_refs": [],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }
    registry.register(desc, handler)
    handle, _ = registry.activate("user:alice", "ws:main", desc.expert_id)
    return registry, handle


def request_for(handle, **overrides):
    fields = dict(
        request_id="req:edge",
        operation="expert.invoke",
        activation_id=handle.activation_id,
        expert_id=handle.expert_id,
        expert_operation="inspect",
        expected_registry_generation=handle.registry_generation,
        expected_runtime_generation=handle.runtime_generation,
        input={},
    )
    fields.update(overrides)
    return impl.ExpertRequest(**fields)


def test_low_level_bounded_and_lifecycle_guards_cover_control_edges():
    with pytest.raises(TypeError):
        impl.activation_transition("active", impl.LifecycleState.DRAINING)
    with pytest.raises(TypeError):
        impl.activation_transition(impl.LifecycleState.ACTIVE, "draining")
    with pytest.raises(ValueError, match="control"):
        impl._bounded_text("bad\nvalue", field_name="value", limit=32)


def test_operation_and_descriptor_dataclasses_reject_noncanonical_shapes():
    fake_field = SimpleNamespace(name="fake")
    with pytest.raises(TypeError, match="FieldSpec"):
        impl.OperationSpec("inspect", input_fields=(fake_field,))

    valid = descriptor()
    op = valid.operations[0]
    with pytest.raises(ValueError, match="non-empty"):
        replace(valid, operations=())
    with pytest.raises(ValueError, match="duplicate"):
        replace(valid, operations=(op, op))
    with pytest.raises(TypeError, match="EffectClass"):
        replace(valid, possible_effects=("none",))
    with pytest.raises(ValueError, match="non-empty"):
        replace(valid, possible_effects=())
    with pytest.raises(TypeError, match="fallback_policy"):
        replace(valid, fallback_policy="fail_closed")
    with pytest.raises(TypeError, match="delegation_policy"):
        replace(valid, delegation_policy="never")
    with pytest.raises(TypeError, match="resource_limits"):
        replace(valid, resource_limits=object())
    with pytest.raises(ValueError, match="registry_generation"):
        replace(valid, registry_generation=True)
    with pytest.raises(TypeError, match="availability"):
        replace(valid, availability="ready")


def test_input_value_bounds_cover_nested_container_and_key_failures():
    with pytest.raises(impl.ExpertInvalidInputError, match="depth"):
        impl._validate_input_value([[[[[[[[["x"]]]]]]]]])
    with pytest.raises(impl.ExpertInvalidInputError, match="oversized string"):
        impl._validate_input_value("x" * (impl._MAX_INPUT_STRING + 1))
    with pytest.raises(impl.ExpertInvalidInputError, match="oversized list"):
        impl._validate_input_value([0] * (impl._MAX_INPUT_LIST + 1))
    with pytest.raises(impl.ExpertInvalidInputError, match="oversized object"):
        impl._validate_input_value(
            {f"k{i}": i for i in range(impl._MAX_INPUT_KEYS + 1)}
        )
    with pytest.raises(impl.ExpertInvalidInputError, match="object key"):
        impl._validate_input_value({1: "bad"})
    with pytest.raises(impl.ExpertInvalidInputError, match="object key"):
        impl._validate_input_value({"k" * 65: "bad"})
    with pytest.raises(impl.ExpertInvalidInputError, match="unsupported"):
        impl._validate_input_value(object())


@pytest.mark.parametrize(
    ("field_type", "value"),
    [
        (impl.FieldType.BOOLEAN, 1),
        (impl.FieldType.INTEGER, True),
        (impl.FieldType.NUMBER, "1"),
        (impl.FieldType.STRING, 1),
        (impl.FieldType.REFERENCE, 1),
        (impl.FieldType.SECRET_REFERENCE, 1),
        (impl.FieldType.LIST, {}),
        (impl.FieldType.OBJECT, []),
        (impl.FieldType.ENUM, "other"),
    ],
)
def test_field_type_validation_rejects_wrong_runtime_values(field_type, value):
    enum_values = ("yes",) if field_type is impl.FieldType.ENUM else ()
    spec = impl.FieldSpec("value", field_type, enum_values=enum_values)
    with pytest.raises(impl.ExpertInvalidInputError):
        impl._check_field_value(spec, value)


def test_registry_catalog_and_matching_input_guards_are_fail_closed():
    registry = impl.ExpertRegistry()
    with pytest.raises(impl.ExpertInvalidInputError, match="ExpertDescriptor"):
        registry.register(object())
    with pytest.raises(impl.ExpertInvalidInputError, match="ExpertDescriptor"):
        registry.reload([(object(), None)])

    registry.register(descriptor(), lambda: {"verdict": "succeeded"})
    with pytest.raises(impl.ExpertInvalidInputError, match="offset"):
        registry.list_experts("user:alice", offset=-1)
    with pytest.raises(impl.ExpertInvalidInputError, match="limit"):
        registry.list_experts("user:alice", limit=0)
    with pytest.raises(impl.ExpertUnavailableError, match="not registered"):
        registry.describe("zara:expert/missing")
    with pytest.raises(impl.ExpertInvalidInputError, match="goal_text"):
        registry.match(1)
    with pytest.raises(impl.ExpertInvalidInputError, match="bounded"):
        registry.match("x" * 513)
    with pytest.raises(impl.ExpertInvalidInputError, match="control"):
        registry.match("bad\nquery")


def test_activation_invocation_and_generation_guards_reject_forgery():
    registry = impl.ExpertRegistry(engines=("swipl",))
    with pytest.raises(impl.ExpertUnavailableError, match="not registered"):
        registry.activate("user:alice", "ws:main", "zara:expert/missing")

    incompatible = descriptor(supported_engines=["trealla"])
    registry.register(incompatible, lambda: {"verdict": "succeeded"})
    with pytest.raises(impl.ExpertUnsupportedBackendError):
        registry.activate("user:alice", "ws:main", incompatible.expert_id)

    registry, handle = activated_registry()
    with pytest.raises(impl.ExpertInvalidInputError, match="ActivationHandle"):
        registry.invoke(object(), "inspect")
    with pytest.raises(impl.ExpertInvalidInputError, match="ExpertRequest"):
        registry.invoke_request(object())
    fake = replace(handle, activation_id="act:" + "f" * 32)
    with pytest.raises(impl.ExpertDeniedError, match="unknown"):
        registry.invoke_request(request_for(fake))
    with pytest.raises(impl.ExpertStaleGenerationError, match="runtime generation"):
        registry.invoke_request(
            request_for(handle, expected_runtime_generation=handle.runtime_generation + 1)
        )
    with pytest.raises(impl.ExpertInvalidInputError, match="expert_operation"):
        registry.invoke(handle, "")
    with pytest.raises(impl.ExpertInvalidInputError, match="limits"):
        registry.invoke(handle, "inspect", limits=object())
    with pytest.raises(impl.ExpertUnsupportedOperationError):
        registry.invoke(handle, "missing")


def test_deactivate_and_backend_state_guards_reject_wrong_authority():
    registry = impl.ExpertRegistry()
    with pytest.raises(impl.ExpertInvalidInputError, match="ActivationHandle"):
        registry.deactivate(object())
    with pytest.raises(impl.ExpertUnavailableError):
        registry.mark_backend_unavailable("zara:expert/missing", "gone")
    with pytest.raises(impl.ExpertUnavailableError):
        registry.recover_backend("zara:expert/missing")

    desc = descriptor()
    registry.register(desc, lambda: {"verdict": "succeeded"})
    with pytest.raises(impl.ExpertInvalidInputError, match="not marked unavailable"):
        registry.recover_backend(desc.expert_id)


def test_handler_shape_and_terminal_projection_helpers_are_bounded():
    registry = impl.ExpertRegistry()
    with pytest.raises(impl.ExpertInvalidInputError, match="callable"):
        registry.register(descriptor(), handler=object())

    field = impl.FieldSpec("required", impl.FieldType.STRING, required=True)
    op = impl.OperationSpec("inspect", input_fields=(field,))
    desc = replace(descriptor(), operations=(op,))
    with pytest.raises(impl.ExpertInvalidInputError, match="does not accept"):
        registry.register(desc, handler=lambda: None)

    with pytest.raises(impl.ExpertInvalidInputError, match="mapping"):
        impl._bounded_mapping([], "data")
    with pytest.raises(impl.ExpertInvalidInputError, match="16 entries"):
        impl._bounded_mapping({str(i): i for i in range(17)}, "data")
    with pytest.raises(impl.ExpertInvalidInputError, match="must be a list"):
        impl._bounded_refs("ref")
    with pytest.raises(impl.ExpertInvalidInputError, match="32 entries"):
        impl._bounded_refs(tuple(f"ev:{i}" for i in range(33)))
    with pytest.raises(impl.ExpertInvalidInputError, match="bounded"):
        impl._bounded_refs(("",))
    with pytest.raises(impl.ExpertInvalidInputError, match="must be a list"):
        impl._bounded_receipts({})
    with pytest.raises(impl.ExpertInvalidInputError, match="32 entries"):
        impl._bounded_receipts(tuple({} for _ in range(33)))
    with pytest.raises(impl.ExpertInvalidInputError, match="mappings"):
        impl._bounded_receipts((1,))
    with pytest.raises(impl.ExpertInvalidInputError, match="bounds"):
        impl._bounded_receipts(({"payload": "x" * 2100},))
