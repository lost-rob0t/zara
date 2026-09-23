from __future__ import annotations

import os
import threading
import time

from zara.plugins.predicate_authority import (
    PredicateInvocationRequest,
    PredicateVerdict,
)
from zara.plugins.predicate_authority_owner import (
    PredicateExecutionOutcome,
    RegisteredPredicateBinding,
)
from zara.plugins.scoped_predicate_authority import (
    PredicateAuthorityScope,
    ScopedPredicateAuthorityProcess,
)


def _scoped_executor(scope, binding, arguments, timeout_ms):
    sleep_ms = int(arguments.get("sleep_ms", 0))
    if sleep_ms:
        time.sleep(sleep_ms / 1000.0)
    return PredicateExecutionOutcome(
        verdict=PredicateVerdict.SUCCEEDED,
        data={
            "principal_seen": scope.principal,
            "workspace_seen": scope.workspace,
            "activation_seen": scope.activation_id,
            "namespace_seen": binding.namespace,
            "child_pid": os.getpid(),
            "timeout_ms": timeout_ms,
        },
    )


def _binding(namespace: str, generation: int = 11) -> RegisteredPredicateBinding:
    return RegisteredPredicateBinding(
        operation="person.lookup",
        namespace=namespace,
        predicate="person_lookup",
        arity=2,
        generation=generation,
    )


def _request(*, request_id: str, generation: int = 11, sleep_ms: int = 0):
    return PredicateInvocationRequest(
        request_id=request_id,
        operation="person.lookup",
        expected_generation=generation,
        arguments={"value": "Alice", "sleep_ms": sleep_ms},
        timeout_ms=2000,
    )


def _owner(*, principal: str, workspace: str, activation_id: str, namespace: str):
    return ScopedPredicateAuthorityProcess.start(
        scope=PredicateAuthorityScope(
            principal=principal,
            workspace=workspace,
            activation_id=activation_id,
        ),
        bindings={"person.lookup": _binding(namespace)},
        executor=_scoped_executor,
    )


def test_scope_is_trusted_construction_state_not_plugin_client_state():
    owner = _owner(
        principal="alice",
        workspace="project-a",
        activation_id="act-a",
        namespace="zara.expert.person.a",
    )
    try:
        state = vars(owner.client)
        assert "scope" not in state
        assert "principal" not in state
        assert "workspace" not in state
        assert "activation_id" not in state

        result = owner.client.invoke(_request(request_id="scope-a"))
        assert result.verdict is PredicateVerdict.SUCCEEDED
        assert result.data["principal_seen"] == "alice"
        assert result.data["workspace_seen"] == "project-a"
        assert result.data["activation_seen"] == "act-a"
    finally:
        owner.stop()


def test_recovered_client_for_one_scope_cannot_cross_into_another_scope():
    owner_a = _owner(
        principal="alice",
        workspace="project-a",
        activation_id="act-a",
        namespace="zara.expert.person.a",
    )
    owner_b = _owner(
        principal="bob",
        workspace="project-b",
        activation_id="act-b",
        namespace="zara.expert.person.b",
    )
    try:
        result_a = owner_a.client.invoke(_request(request_id="scope-cross-a"))
        result_b = owner_b.client.invoke(_request(request_id="scope-cross-b"))

        assert result_a.verdict is PredicateVerdict.SUCCEEDED
        assert result_b.verdict is PredicateVerdict.SUCCEEDED
        assert result_a.data["principal_seen"] == "alice"
        assert result_b.data["principal_seen"] == "bob"
        assert result_a.provenance is not None
        assert result_b.provenance is not None
        assert result_a.provenance.namespace == "zara.expert.person.a"
        assert result_b.provenance.namespace == "zara.expert.person.b"
        assert result_a.provenance.namespace != result_b.provenance.namespace
    finally:
        owner_a.stop()
        owner_b.stop()


def test_explicit_cancellation_fences_inflight_and_late_results():
    owner = _owner(
        principal="alice",
        workspace="project-a",
        activation_id="act-a",
        namespace="zara.expert.person.a",
    )
    result_box = {}

    def invoke() -> None:
        result_box["result"] = owner.client.invoke(
            _request(request_id="cancel-inflight", sleep_ms=750)
        )

    thread = threading.Thread(target=invoke, daemon=True)
    thread.start()
    time.sleep(0.05)
    owner.cancel()
    thread.join(timeout=2.0)

    assert not thread.is_alive()
    result = result_box["result"]
    assert result.verdict is PredicateVerdict.CANCELLED
    assert result.error_code == "authority_cancelled"
    assert result.provenance is None

    later = owner.client.invoke(_request(request_id="cancel-late"))
    assert later.verdict is PredicateVerdict.CANCELLED
    assert later.error_code == "authority_cancelled"
    assert later.provenance is None
