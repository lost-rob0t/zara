from __future__ import annotations

import json
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator

from zara.runtime.registry import RuntimeDescriptor


ROOT = Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "contracts" / "zara-runtime-v1" / "runtime-descriptor.schema.json"


def _validator() -> Draft202012Validator:
    schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def _valid_wire_descriptor() -> dict[str, object]:
    return {
        "id": "zara-python",
        "display_name": "Zara Python",
        "protocol": "ZARA-RUNTIME/1",
        "runtime_version": "1.0",
        "implementation_version": "0.2.2-alpha",
        "installed": True,
        "available": True,
        "health": "ready",
        "locality": "embedded",
        "transport": "in_process",
        "capabilities": ["chat", "streaming"],
        "profiles": [],
        "provider_control": "zara",
        "model_control": "zara",
        "supports_streaming": True,
        "supports_cancel": True,
        "supports_context_handles": True,
        "supports_host_tools": True,
        "provenance": "builtin:test-fixture",
    }


@pytest.mark.parametrize(
    ("field", "unsafe_value"),
    [
        ("display_name", " Zara Python"),
        ("display_name", "Zara Python "),
        ("display_name", "Zara\nPython"),
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
    valid = _valid_wire_descriptor()
    assert list(validator.iter_errors(valid)) == []

    host_kwargs = {
        "id": valid["id"],
        "display_name": valid["display_name"],
        "protocol": valid["protocol"],
        "runtime_version": valid["runtime_version"],
        "implementation_version": valid["implementation_version"],
        "installed": valid["installed"],
        "available": valid["available"],
        "health": valid["health"],
        "locality": valid["locality"],
        "transport": valid["transport"],
        "capabilities": tuple(valid["capabilities"]),
        "profiles": tuple(valid["profiles"]),
        "provider_control": valid["provider_control"],
        "model_control": valid["model_control"],
        "supports_streaming": valid["supports_streaming"],
        "supports_cancel": valid["supports_cancel"],
        "supports_context_handles": valid["supports_context_handles"],
        "supports_host_tools": valid["supports_host_tools"],
        "provenance": valid["provenance"],
    }
    host_kwargs[field] = unsafe_value

    with pytest.raises((TypeError, ValueError)):
        RuntimeDescriptor(**host_kwargs)

    candidate = dict(valid)
    candidate[field] = unsafe_value
    assert list(validator.iter_errors(candidate)), (
        f"shared schema accepted {field}={unsafe_value!r} even though host validators reject it"
    )
