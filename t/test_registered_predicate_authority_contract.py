from __future__ import annotations

from dataclasses import fields

import pytest

from zara.plugins.predicate_authority import (
    PREDICATE_AUTHORITY_PROTOCOL,
    PredicateAuthorityError,
    PredicateExecutionProvenance,
    PredicateInvocationRequest,
    PredicateInvocationResult,
    PredicateVerdict,
)


def test_plugin_request_carries_no_executable_or_identity_authority():
    names = {field.name for field in fields(PredicateInvocationRequest)}

    assert names == {
        "request_id",
        "operation",
        "expected_generation",
        "arguments",
        "timeout_ms",
        "protocol",
    }
    assert names.isdisjoint(
        {
            "arity",
            "capability",
            "goal",
            "module",
            "namespace",
            "predicate",
            "principal",
            "query",
            "token",
        }
    )


def test_request_accepts_only_logical_selector_and_inert_arguments():
    request = PredicateInvocationRequest(
        request_id="req-1",
        operation="person.lookup",
        expected_generation=7,
        arguments={"name": "Alice Smith", "limit": 3},
        timeout_ms=500,
    )

    assert request.protocol == PREDICATE_AUTHORITY_PROTOCOL
    assert request.operation == "person.lookup"
    assert request.arguments == {"name": "Alice Smith", "limit": 3}


@pytest.mark.parametrize(
    "field",
    ["predicate", "goal", "module", "namespace", "principal", "query", "token"],
)
def test_authority_metadata_is_rejected_even_inside_argument_object(field):
    with pytest.raises(PredicateAuthorityError, match="reserved authority metadata"):
        PredicateInvocationRequest(
            request_id="req-1",
            operation="person.lookup",
            expected_generation=1,
            arguments={field: "shell"},
            timeout_ms=500,
        )


@pytest.mark.parametrize(
    "field",
    ["predicate", "goal", "module", "namespace", "principal", "query", "token"],
)
def test_nested_authority_metadata_cannot_be_smuggled_through_inert_payload(field):
    with pytest.raises(PredicateAuthorityError, match="reserved authority metadata"):
        PredicateInvocationRequest(
            request_id="req-nested",
            operation="person.lookup",
            expected_generation=1,
            arguments={"filters": {"nested": {field: "shell"}}},
            timeout_ms=500,
        )


def test_callable_or_object_argument_never_crosses_wire_contract():
    with pytest.raises(PredicateAuthorityError, match="not inert wire data"):
        PredicateInvocationRequest(
            request_id="req-1",
            operation="person.lookup",
            expected_generation=1,
            arguments={"callback": lambda: None},
            timeout_ms=500,
        )


def test_non_finite_numbers_fail_closed():
    with pytest.raises(PredicateAuthorityError, match="non-finite"):
        PredicateInvocationRequest(
            request_id="req-1",
            operation="math.inspect",
            expected_generation=1,
            arguments={"value": float("nan")},
            timeout_ms=500,
        )


def test_success_requires_fresh_execution_provenance():
    with pytest.raises(PredicateAuthorityError, match="requires execution provenance"):
        PredicateInvocationResult(
            verdict=PredicateVerdict.SUCCEEDED,
            provenance=None,
            data={"answer": 42},
        )


def test_success_provenance_binds_exact_executed_identity_and_generation():
    provenance = PredicateExecutionProvenance(
        namespace="zara.expert.todo",
        predicate="todo_lookup",
        arity=2,
        generation=9,
        request_id="req-9",
    )
    result = PredicateInvocationResult(
        verdict=PredicateVerdict.SUCCEEDED,
        provenance=provenance,
        data={"items": ["one", "two"]},
    )

    assert result.provenance == provenance
    assert result.provenance.generation == 9
    assert result.provenance.request_id == "req-9"


def test_process_completion_without_domain_evidence_can_remain_unknown():
    result = PredicateInvocationResult(
        verdict=PredicateVerdict.UNKNOWN,
        provenance=None,
        data={},
        error_code="unknown_external_outcome",
    )

    assert result.verdict is PredicateVerdict.UNKNOWN
