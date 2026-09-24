"""Reserved host metadata must never be caller-declared ZARA-EXPERT/1 input."""

from __future__ import annotations

import pytest

from zara.experts import ExpertDescriptor, ExpertInvalidInputError, ExpertRegistry


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": "ZARA-EXPERT/1",
            "expert_id": "zara:expert/reserved-host-field-fixture",
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:reserved.host.field.fixture.v1",
            "name": "Reserved Host Field Fixture",
            "description": "Pins trusted host-owned operation metadata.",
            "source_reference": "t/test_expert_registry_reserved_host_fields.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "parse",
                    "input_schema": {
                        "fields": [
                            {
                                "name": "expert_operation",
                                "type": "string",
                                "required": True,
                            }
                        ]
                    },
                    "output_schema": {"fields": []},
                }
            ],
            "applicability": {"keywords": ["reserved"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {"max_model_calls": 0},
            "registry_generation": 0,
            "availability": "ready",
        }
    )


def test_reserved_expert_operation_input_field_is_rejected_before_registration() -> None:
    registry = ExpertRegistry()

    with pytest.raises(ExpertInvalidInputError, match="reserved host metadata"):
        registry.reload([(_descriptor(), None)])

    snapshot = registry.snapshot()
    assert snapshot.expert_ids == ()
    assert snapshot.activation_ids == ()
    assert snapshot.invocation_ids == ()
