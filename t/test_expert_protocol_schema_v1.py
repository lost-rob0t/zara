"""Structural wire contracts only; schema acceptance never grants authority."""

from __future__ import annotations

import copy
import json
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator

ROOT = Path(__file__).resolve().parents[1]
CONTRACT = ROOT / "contracts" / "zara-expert-v1"


def descriptor() -> dict:
    return {
        "protocol": "ZARA-EXPERT/1",
        "expert_id": "zara:expert/android-troubleshooting",
        "expert_version": "1.0.0",
        "package_namespace": "zara-expert",
        "manifest_digest": "sha256:" + "a" * 64,
        "name": "Android troubleshooting",
        "description": "Symbolic diagnosis over supplied observations.",
        "source_reference": "source:fixture-v1",
        "reasoning_kind": "symbolic",
        "operations": [{
            "id": "diagnose",
            "input_schema": "schema:diagnose-input-v1",
            "output_schema": "schema:diagnose-output-v1",
            "effects": [],
            "required_capabilities": [],
        }],
        "applicability_schema": "schema:diagnose-goal-v1",
        "required_observations": ["adb_state"],
        "required_capabilities": [],
        "possible_effects": [],
        "supported_engines": ["swi-prolog"],
        "supported_platforms": ["desktop"],
        "placement": {"node_id": "desktop", "runtime_id": "zara-python"},
        "fallback_policy": "none",
        "delegation_policy": "none",
        "resource_limits": limits(),
        "registry_generation": 12,
        "availability": "available",
        "unavailable_reason": None,
    }


def limits() -> dict:
    return {
        "timeout_ms": 3000,
        "max_results": 8,
        "max_output_bytes": 65536,
        "max_model_calls": 0,
    }


def invocation() -> dict:
    return {
        "protocol": "ZARA-EXPERT/1",
        "request_id": "req-42",
        "operation": "expert.invoke",
        "activation_id": "act-7",
        "expert_id": "zara:expert/android-troubleshooting",
        "expert_operation": "diagnose",
        "expected_registry_generation": 12,
        "expected_runtime_generation": 4,
        "input": {"symptom_id": "adb_no_devices"},
        "limits": limits(),
    }


def validator(name: str) -> Draft202012Validator:
    schema = json.loads((CONTRACT / f"{name}.schema.json").read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


@pytest.mark.parametrize("name,build", [
    ("expert-descriptor", descriptor), ("expert-invoke", invocation),
])
def test_valid_wire_contract(name, build):
    validator(name).validate(build())


@pytest.mark.parametrize("name,build", [
    ("expert-descriptor", descriptor), ("expert-invoke", invocation),
])
def test_each_required_field_is_required(name, build):
    valid = build()
    for field in valid:
        candidate = copy.deepcopy(valid)
        del candidate[field]
        assert not validator(name).is_valid(candidate), field


@pytest.mark.parametrize("name,build", [
    ("expert-descriptor", descriptor), ("expert-invoke", invocation),
])
@pytest.mark.parametrize("field", ["principal_id", "handler", "module", "goal", "grants"])
def test_injected_authority_and_handler_fields_rejected(name, build, field):
    candidate = build()
    candidate[field] = "user:shell(x)"
    assert not validator(name).is_valid(candidate)


@pytest.mark.parametrize("protocol", ["ZARA-EXPERT/2", "ZARA-EXPERT/01", "ZARA/1", None, 1])
@pytest.mark.parametrize("name,build", [
    ("expert-descriptor", descriptor), ("expert-invoke", invocation),
])
def test_protocol_is_exact(name, build, protocol):
    candidate = build()
    candidate["protocol"] = protocol
    assert not validator(name).is_valid(candidate)


@pytest.mark.parametrize("bad_id", [
    "", "zara://expert/test", "zara:expert/../test", "zara:expert/test/",
    " zara:expert/test", "zara:expert/test\n", "zara:expert/", "zara:expert/Test",
    "zara:expert/" + "a" * 65, "zara:expert/test?grant=all",
])
@pytest.mark.parametrize("name,build", [
    ("expert-descriptor", descriptor), ("expert-invoke", invocation),
])
def test_malformed_symbol_is_rejected(name, build, bad_id):
    candidate = build()
    candidate["expert_id"] = bad_id
    assert not validator(name).is_valid(candidate)


@pytest.mark.parametrize("field", ["expected_registry_generation", "expected_runtime_generation"])
@pytest.mark.parametrize("value", [0, -1, True, "12", 1.25, 9007199254740992])
def test_invalid_generations_rejected(field, value):
    candidate = invocation()
    candidate[field] = value
    assert not validator("expert-invoke").is_valid(candidate)


@pytest.mark.parametrize("field,minimum,maximum", [
    ("timeout_ms", 1, 3600000),
    ("max_results", 1, 1024),
    ("max_output_bytes", 1, 1048576),
    ("max_model_calls", 0, 1024),
])
def test_limit_boundaries(field, minimum, maximum):
    for value in (minimum, maximum):
        candidate = invocation()
        candidate["limits"][field] = value
        validator("expert-invoke").validate(candidate)
    for value in (minimum - 1, maximum + 1, True, None, "1", 1.5):
        candidate = invocation()
        candidate["limits"][field] = value
        assert not validator("expert-invoke").is_valid(candidate), (field, value)


@pytest.mark.parametrize("field,value", [
    ("operation", "expert.activate"),
    ("expert_operation", "user:shell"),
    ("activation_id", "act-7\n"),
    ("request_id", "a" * 129),
    ("input", []),
    ("input", "shell(x)"),
])
def test_malformed_invoke_fields_rejected(field, value):
    candidate = invocation()
    candidate[field] = value
    assert not validator("expert-invoke").is_valid(candidate)


def test_input_text_is_data_not_a_callable():
    candidate = invocation()
    candidate["input"] = {
        "literal": ":- initialization(shell('do-not-run')).",
        "case": "CamelCase/Ω.txt",
        "values": [True, False, None, -2, 1.25, "text", {"nested": []}],
    }
    validator("expert-invoke").validate(candidate)
    assert json.loads(json.dumps(candidate, ensure_ascii=False)) == candidate


@pytest.mark.parametrize("kind", ["symbolic", "hybrid", "model", "service"])
def test_declared_reasoning_kinds(kind):
    candidate = descriptor()
    candidate["reasoning_kind"] = kind
    validator("expert-descriptor").validate(candidate)


@pytest.mark.parametrize("field,value", [
    ("fallback_policy", "explicit"),
    ("resource_limits", {**limits(), "max_model_calls": 1}),
])
def test_symbolic_descriptor_cannot_enable_model_calls(field, value):
    candidate = descriptor()
    candidate[field] = value
    assert not validator("expert-descriptor").is_valid(candidate)


def test_hybrid_can_declare_explicit_model_fallback():
    candidate = descriptor()
    candidate.update(reasoning_kind="hybrid", fallback_policy="explicit")
    candidate["resource_limits"]["max_model_calls"] = 1
    validator("expert-descriptor").validate(candidate)


@pytest.mark.parametrize("state", ["unavailable", "incompatible", "stale"])
def test_unavailable_descriptor_requires_reason(state):
    candidate = descriptor()
    candidate["availability"] = state
    assert not validator("expert-descriptor").is_valid(candidate)
    candidate["unavailable_reason"] = "backend_missing"
    validator("expert-descriptor").validate(candidate)


def test_available_descriptor_has_no_failure_reason():
    candidate = descriptor()
    candidate["unavailable_reason"] = "backend_missing"
    assert not validator("expert-descriptor").is_valid(candidate)


@pytest.mark.parametrize("field", ["supported_engines", "supported_platforms", "operations"])
def test_required_catalogs_not_empty(field):
    candidate = descriptor()
    candidate[field] = []
    assert not validator("expert-descriptor").is_valid(candidate)


def test_operation_schema_is_a_registered_reference_not_a_url():
    candidate = descriptor()
    candidate["operations"][0]["input_schema"] = "https://attacker.invalid/schema"
    assert not validator("expert-descriptor").is_valid(candidate)


def test_descriptor_does_not_accept_live_principal_in_placement():
    candidate = descriptor()
    candidate["placement"]["principal_id"] = "operator"
    assert not validator("expert-descriptor").is_valid(candidate)


def test_manifest_is_an_exact_content_digest():
    for value in ("latest", "sha256:abc", "sha256:" + "A" * 64, "https://example.invalid/code"):
        candidate = descriptor()
        candidate["manifest_digest"] = value
        assert not validator("expert-descriptor").is_valid(candidate)


def test_catalog_limits():
    candidate = descriptor()
    candidate["operations"] = [{**candidate["operations"][0], "id": f"op-{i}"} for i in range(65)]
    assert not validator("expert-descriptor").is_valid(candidate)


def test_checked_in_fixtures_are_the_tested_examples():
    assert json.loads((CONTRACT / "descriptor.example.json").read_text()) == descriptor()
    assert json.loads((CONTRACT / "invoke.example.json").read_text()) == invocation()


def test_integral_json_number_generation():
    candidate = invocation()
    candidate["expected_registry_generation"] = 12.0
    validator("expert-invoke").validate(candidate)


def test_descriptor_and_request_share_primitive_contracts():
    descriptor_schema = json.loads((CONTRACT / "expert-descriptor.schema.json").read_text())
    request_schema = json.loads((CONTRACT / "expert-invoke.schema.json").read_text())
    for name in ("token", "reference", "symbol", "generation", "limits"):
        assert descriptor_schema["$defs"][name] == request_schema["$defs"][name], name
