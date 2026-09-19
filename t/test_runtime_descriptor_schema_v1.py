from __future__ import annotations

import json
from dataclasses import replace
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator

from zara.runtime.registry import (
    ControlOwner,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeLocality,
    RuntimeTransport,
    ZARA_RUNTIME_PROTOCOL,
)


ROOT = Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "contracts" / "zara-runtime-v1" / "runtime-descriptor.schema.json"


def _validator() -> Draft202012Validator:
    schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def _valid_descriptor() -> RuntimeDescriptor:
    return RuntimeDescriptor(
        id="zara-python",
        display_name="Zara Python",
        protocol=ZARA_RUNTIME_PROTOCOL,
        runtime_version="1.0",
        implementation_version="0.2.2-alpha",
        installed=True,
        available=True,
        health=RuntimeHealth.READY,
        locality=RuntimeLocality.EMBEDDED,
        transport=RuntimeTransport.IN_PROCESS,
        capabilities=("chat", "streaming"),
        provider_control=ControlOwner.ZARA,
        model_control=ControlOwner.ZARA,
        supports_streaming=True,
        supports_cancel=True,
        supports_context_handles=True,
        supports_host_tools=True,
        provenance="builtin:test-fixture",
    )


@pytest.mark.parametrize(
    ("field", "unsafe_value"),
    [
        ("display_name", " Zara Python"),
        ("display_name", "Zara Python "),
        ("display_name", "Zara\nPython"),
        ("protocol", "ZARA-RUNTIME/" + "9" * 20),
        ("runtime_version", " 1.0"),
        ("runtime_version", "1.0\t"),
        ("implementation_version", " fixture"),
        ("implementation_version", "fixture\u007f"),
    ],
)
def test_descriptor_schema_rejects_text_hosts_reject(
    field: str,
    unsafe_value: str,
) -> None:
    validator = _validator()
    descriptor = _valid_descriptor()
    assert list(validator.iter_errors(descriptor.to_wire())) == []

    with pytest.raises(ValueError):
        replace(descriptor, **{field: unsafe_value})

    candidate = descriptor.to_wire()
    candidate[field] = unsafe_value
    assert list(validator.iter_errors(candidate)), (
        f"shared schema accepted {field}={unsafe_value!r} even though host validators reject it"
    )


@pytest.mark.parametrize(
    ("field", "unsafe_value"),
    [
        ("installed", 1),
        ("available", "true"),
        ("supports_streaming", 1),
        ("supports_cancel", "false"),
        ("supports_context_handles", 0),
        ("supports_host_tools", None),
    ],
)
def test_python_descriptor_rejects_scalar_types_shared_schema_rejects(
    field: str,
    unsafe_value: object,
) -> None:
    validator = _validator()
    descriptor = _valid_descriptor()

    candidate = descriptor.to_wire()
    candidate[field] = unsafe_value
    assert list(validator.iter_errors(candidate)), (
        f"shared schema unexpectedly accepted {field}={unsafe_value!r}"
    )

    with pytest.raises(TypeError):
        replace(descriptor, **{field: unsafe_value})


@pytest.mark.parametrize(
    ("field", "wire_value"),
    [
        ("health", "ready"),
        ("locality", "embedded"),
        ("transport", "in_process"),
        ("provider_control", "zara"),
        ("model_control", "mixed"),
    ],
)
def test_python_descriptor_rejects_wire_enum_strings(
    field: str,
    wire_value: str,
) -> None:
    validator = _validator()
    descriptor = _valid_descriptor()

    candidate = descriptor.to_wire()
    candidate[field] = wire_value
    assert list(validator.iter_errors(candidate)) == [], (
        f"shared wire schema unexpectedly rejected canonical enum string {field}={wire_value!r}"
    )

    with pytest.raises(TypeError):
        replace(descriptor, **{field: wire_value})
