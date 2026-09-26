from __future__ import annotations

import json
import os
import time

import pytest

from zara.plugins.predicate_authority import (
    PREDICATE_AUTHORITY_PROTOCOL,
    PredicateInvocationRequest,
    PredicateVerdict,
)
from zara.plugins.predicate_authority_owner import (
    MAX_AUTHORITY_FRAME_BYTES,
    PredicateAuthorityProcess,
    PredicateExecutionOutcome,
    RegisteredPredicateBinding,
)


def _recording_executor(binding, arguments, timeout_ms):
    sleep_ms = int(arguments.get("sleep_ms", 0))
    if sleep_ms:
        time.sleep(sleep_ms / 1000.0)
    return PredicateExecutionOutcome(
        verdict=PredicateVerdict.SUCCEEDED,
        data={
            "child_pid": os.getpid(),
            "operation_seen": binding.operation,
            "value": arguments.get("value"),
            "timeout_ms": timeout_ms,
        },
    )


@pytest.fixture
def authority():
    owner = PredicateAuthorityProcess.start(
        bindings={
            "person.lookup": RegisteredPredicateBinding(
                operation="person.lookup",
                namespace="zara.expert.person",
                predicate="person_lookup",
                arity=2,
                generation=7,
            )
        },
        executor=_recording_executor,
    )
    try:
        yield owner
    finally:
        owner.stop()


def _request(
    *,
    request_id: str = "req-1",
    operation: str = "person.lookup",
    expected_generation: int = 7,
    arguments=None,
):
    return PredicateInvocationRequest(
        request_id=request_id,
        operation=operation,
        expected_generation=expected_generation,
        arguments={"value": "Alice"} if arguments is None else arguments,
        timeout_ms=1000,
    )


def test_authority_executes_in_separate_process_and_emits_exact_provenance(authority):
    result = authority.client.invoke(_request())

    assert result.verdict is PredicateVerdict.SUCCEEDED
    assert result.data["child_pid"] != os.getpid()
    assert result.data["operation_seen"] == "person.lookup"
    assert result.provenance is not None
    assert result.provenance.namespace == "zara.expert.person"
    assert result.provenance.predicate == "person_lookup"
    assert result.provenance.arity == 2
    assert result.provenance.generation == 7
    assert result.provenance.request_id == "req-1"


def test_plugin_client_contains_no_binding_registry_or_executor(authority):
    state = vars(authority.client)

    assert set(state) == {"_transport", "_lifecycle", "_lock"}
    assert "bindings" not in state
    assert "executor" not in state
    assert "predicate" not in state
    assert "namespace" not in state


def test_recovered_raw_transport_cannot_smuggle_predicate_identity(authority):
    raw = {
        "protocol": PREDICATE_AUTHORITY_PROTOCOL,
        "request_id": "raw-1",
        "operation": "person.lookup",
        "expected_generation": 7,
        "arguments": {"value": "Alice"},
        "timeout_ms": 1000,
        "predicate": "shell",
    }

    authority.client._transport.send_bytes(
        json.dumps(raw, separators=(",", ":"), sort_keys=True).encode("utf-8")
    )
    assert authority.client._transport.poll(1.0)
    response = json.loads(
        authority.client._transport.recv_bytes(MAX_AUTHORITY_FRAME_BYTES).decode("utf-8")
    )

    assert response["verdict"] == "error"
    assert response["error_code"] == "malformed_request"
    assert response["provenance"] is None


def test_recovered_raw_transport_cannot_smuggle_nested_authority_metadata(authority):
    raw = {
        "protocol": PREDICATE_AUTHORITY_PROTOCOL,
        "request_id": "raw-2",
        "operation": "person.lookup",
        "expected_generation": 7,
        "arguments": {"filters": {"predicate": "shell"}},
        "timeout_ms": 1000,
    }

    authority.client._transport.send_bytes(
        json.dumps(raw, separators=(",", ":"), sort_keys=True).encode("utf-8")
    )
    assert authority.client._transport.poll(1.0)
    response = json.loads(
        authority.client._transport.recv_bytes(MAX_AUTHORITY_FRAME_BYTES).decode("utf-8")
    )

    assert response["verdict"] == "error"
    assert response["error_code"] == "malformed_request"
    assert response["provenance"] is None


def test_unknown_logical_operation_cannot_select_unregistered_predicate(authority):
    result = authority.client.invoke(
        _request(request_id="req-shell", operation="shell")
    )

    assert result.verdict is PredicateVerdict.UNSUPPORTED
    assert result.error_code == "unsupported_operation"
    assert result.provenance is None


def test_stale_generation_fails_closed_before_executor(authority):
    result = authority.client.invoke(
        _request(request_id="req-stale", expected_generation=6)
    )

    assert result.verdict is PredicateVerdict.BLOCKED
    assert result.error_code == "stale_generation"
    assert result.provenance is None


def test_request_id_replay_fails_closed(authority):
    first = authority.client.invoke(_request(request_id="req-replay"))
    second = authority.client.invoke(_request(request_id="req-replay"))

    assert first.verdict is PredicateVerdict.SUCCEEDED
    assert second.verdict is PredicateVerdict.BLOCKED
    assert second.error_code == "replayed_request"
    assert second.provenance is None


def test_terminated_owner_cannot_fall_back_to_local_execution(authority):
    client = authority.client
    authority.stop()

    result = client.invoke(_request(request_id="req-after-stop"))

    assert result.verdict is PredicateVerdict.ERROR
    assert result.error_code == "authority_unavailable"
    assert result.provenance is None


def test_timeout_kills_worker_so_late_reply_cannot_poison_next_turn(authority):
    timed_out = authority.client.invoke(
        PredicateInvocationRequest(
            request_id="req-timeout",
            operation="person.lookup",
            expected_generation=7,
            arguments={"value": "Alice", "sleep_ms": 100},
            timeout_ms=10,
        )
    )
    later = authority.client.invoke(_request(request_id="req-after-timeout"))

    assert timed_out.verdict is PredicateVerdict.CANCELLED
    assert timed_out.error_code == "authority_timeout"
    assert later.verdict is PredicateVerdict.ERROR
    assert later.error_code == "authority_unavailable"
