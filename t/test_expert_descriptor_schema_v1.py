"""ZARA-EXPERT/1 expert descriptor schema conformance tests (issue #1233 phase 1)."""

from __future__ import annotations

import csv
import json
from dataclasses import replace
from pathlib import Path
from typing import Any, Callable

import pytest
from jsonschema import Draft202012Validator

from zara.experts import (
    ExpertContractError,
    ExpertDescriptor,
    ZARA_EXPERT_PROTOCOL,
)


ROOT = Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "contracts" / "zara-expert-v1" / "expert-descriptor.schema.json"
FIXTURE = ROOT / "contracts" / "zara-expert-v1" / "descriptors.tsv"


def _validator() -> Draft202012Validator:
    schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


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


def valid_wire() -> dict[str, Any]:
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
            }
        ],
        "applicability": {"keywords": ["todo", "tasks"]},
        "required_capabilities": [],
        "possible_effects": ["none"],
        "supported_engines": [],
        "supported_platforms": ["linux"],
        "fallback_policy": "fail_closed",
        "delegation_policy": "never",
        "registry_generation": 1,
        "availability": "ready",
    }
    return wire


def test_schema_is_draft_2020_12_with_canonical_id() -> None:
    schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))

    assert schema["$schema"] == "https://json-schema.org/draft/2020-12/schema"
    assert schema["$id"] == "https://zara.ai/contracts/zara-expert-v1/expert-descriptor.schema.json"
    Draft202012Validator.check_schema(schema)


def test_every_shared_fixture_row_validates_against_schema() -> None:
    validator = _validator()
    rows = fixture_rows()

    assert len(rows) == 4
    for row in rows:
        wire = hydrate_row(row)
        assert list(validator.iter_errors(wire)) == [], (
            f"shared schema rejected fixture row {row['expert_id']!r}"
        )


def test_schema_and_host_agree_on_canonical_accept() -> None:
    validator = _validator()
    wire = valid_wire()

    assert list(validator.iter_errors(wire)) == []
    expert = ExpertDescriptor.from_wire(wire)

    assert expert.to_wire() == wire


@pytest.mark.parametrize(
    ("label", "mutate"),
    [
        ("unknown_field", lambda w: w.update({"handler": "module:callable"})),
        ("protocol_zero_major", lambda w: w.update({"protocol": "ZARA-EXPERT/0"})),
        ("protocol_lowercase", lambda w: w.update({"protocol": "zara-expert/1"})),
        ("namespace_uppercase", lambda w: w.update({"package_namespace": "Zara"})),
        ("unknown_reasoning_kind", lambda w: w.update({"reasoning_kind": "neural"})),
        ("oversized_description", lambda w: w.update({"description": "x" * 2049})),
        ("unknown_effect", lambda w: w.update({"possible_effects": ["teleport"]})),
        ("expert_id_with_space", lambda w: w.update({"expert_id": "zara:expert/ todo"})),
        ("expert_id_non_printable", lambda w: w.update({"expert_id": "zara:expert/\u00e9"})),
        (
            "bad_operation_id",
            lambda w: w["operations"][0].update({"operation_id": "Route.Diagnose"}),
        ),
        (
            "unknown_field_type",
            lambda w: w["operations"][0]["input_schema"]["fields"][0].update(
                {"type": "blob"}
            ),
        ),
        (
            "unavailable_without_reason",
            lambda w: (w.update({"availability": "unavailable"}), w.pop("unavailable_reason", None)),
        ),
        (
            "unavailable_reason_control_character",
            lambda w: w.update(
                {"availability": "unavailable", "unavailable_reason": "boom\u0007"}
            ),
        ),
        (
            "oversized_resource_limit",
            lambda w: w.update({"resource_limits": {"timeout_ms": 600001}}),
        ),
        (
            "unknown_resource_limit",
            lambda w: w.update({"resource_limits": {"gpu_ms": 1}}),
        ),
        (
            "uppercase_keyword",
            lambda w: w.update({"applicability": {"keywords": ["TODO"]}}),
        ),
        (
            "unknown_operation_key",
            lambda w: w["operations"][0].update({"handler": "module:callable"}),
        ),
        (
            "unknown_field_key",
            lambda w: w["operations"][0]["input_schema"]["fields"][0].update(
                {"default": "x"}
            ),
        ),
        (
            "bad_engine_token",
            lambda w: w.update({"supported_engines": ["Swipl"]}),
        ),
        (
            "bad_digest_shape",
            lambda w: w.update({"manifest_digest": "todo-digest"}),
        ),
    ],
)
def test_schema_and_host_agree_on_reject(
    label: str, mutate: Callable[[dict[str, Any]], None]
) -> None:
    validator = _validator()
    candidate = valid_wire()
    mutate(candidate)

    assert list(validator.iter_errors(candidate)), (
        f"shared schema accepted hostile {label!r} even though the host rejects it"
    )
    with pytest.raises(ExpertContractError):
        ExpertDescriptor.from_wire(candidate)


@pytest.mark.parametrize(
    ("field", "wire_value"),
    [
        ("reasoning_kind", "symbolic"),
        ("availability", "ready"),
        ("fallback_policy", "none"),
        ("delegation_policy", "never"),
    ],
)
def test_python_descriptor_rejects_wire_enum_strings(
    field: str, wire_value: str
) -> None:
    validator = _validator()
    wire = valid_wire()
    wire[field] = wire_value
    expert = ExpertDescriptor.from_wire(valid_wire())

    assert list(validator.iter_errors(wire)) == [], (
        f"shared wire schema unexpectedly rejected canonical enum string {field}={wire_value!r}"
    )

    with pytest.raises(TypeError):
        replace(expert, **{field: wire_value})
