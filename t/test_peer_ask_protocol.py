from __future__ import annotations

import time

import pytest

from zara.peer_protocol import (
    PEER_MAX_HOPS,
    PEER_RUNTIME_PROTOCOL,
    PeerAuthority,
    PeerCallAdmissionError,
    PeerCallBudget,
    PeerCallRequest,
    PeerCancelRequest,
    PeerRemoteError,
    PeerResult,
    admit_peer_call,
    project_remote_error,
)
from zara.runtime.commands import CancelTurn, SubmitTurn
from zara.security import Capability


def _authority(*, generation: int = 7) -> PeerAuthority:
    return PeerAuthority(
        principal_id="principal-owner",
        source_node_id="phone-node",
        session_id="session-7",
        enrollment_generation=generation,
        capabilities=frozenset(
            {
                Capability.TURN_SUBMIT,
                Capability.TURN_CANCEL,
                Capability.CONTEXT_READ,
            }
        ),
    )


def _request(**overrides) -> PeerCallRequest:
    values = {
        "operation": "node.ask",
        "request_id": "ask-1",
        "content": "what changed in this project?",
        "context_refs": ("ctx:project",),
        "media_refs": (),
        "requested_capabilities": frozenset({Capability.CONTEXT_READ}),
        "budget": PeerCallBudget(
            wall_time_ms=2_000,
            max_output_bytes=32_768,
            max_tokens=512,
            max_cost_microunits=0,
            max_tool_calls=0,
            max_model_calls=0,
            max_recursion_depth=1,
        ),
        "deadline_ns": time.time_ns() + 5_000_000_000,
        "hop_limit": 2,
        "visited_nodes": ("server-node",),
        "cycle_token": "cycle-1",
        "trace_id": "trace-1",
        "correlation_id": "corr-1",
        "causation_id": "cause-1",
        "expected_enrollment_generation": 7,
    }
    values.update(overrides)
    return PeerCallRequest(**values)


def test_peer_request_authority_is_session_derived_not_payload_selected() -> None:
    authority = _authority()
    request = _request()

    admitted = admit_peer_call(
        request,
        authority=authority,
        local_node_id="desktop-node",
        now_ns=time.time_ns(),
    )

    assert admitted.authority == authority
    assert admitted.source_node_id == "phone-node"
    assert admitted.target_node_id == "desktop-node"
    assert admitted.protocol == PEER_RUNTIME_PROTOCOL


@pytest.mark.parametrize(
    "field,value",
    [
        ("source_node_id", "attacker"),
        ("target_node_id", "victim"),
        ("principal_id", "other-principal"),
        ("session_id", "other-session"),
    ],
)
def test_payload_authority_fields_are_rejected(field: str, value: str) -> None:
    wire = _request().to_wire_body()
    wire[field] = value

    with pytest.raises(PeerCallAdmissionError, match="authority"):
        PeerCallRequest.from_wire("node.ask", "ask-1", wire)


def test_call_budget_and_recursive_bounds_fail_closed() -> None:
    with pytest.raises(ValueError, match="wall_time_ms"):
        PeerCallBudget(wall_time_ms=0, max_output_bytes=1)

    with pytest.raises(ValueError, match="hop_limit"):
        _request(hop_limit=PEER_MAX_HOPS + 1)

    with pytest.raises(ValueError, match="visited_nodes"):
        _request(visited_nodes=tuple(f"node-{index}" for index in range(PEER_MAX_HOPS + 2)))


def test_expired_request_is_rejected_before_runtime_dispatch() -> None:
    now_ns = time.time_ns()
    with pytest.raises(PeerCallAdmissionError, match="deadline"):
        admit_peer_call(
            _request(deadline_ns=now_ns - 1),
            authority=_authority(),
            local_node_id="desktop-node",
            now_ns=now_ns,
        )


def test_requested_capabilities_must_be_narrower_than_authenticated_authority() -> None:
    with pytest.raises(PeerCallAdmissionError, match="capability"):
        admit_peer_call(
            _request(requested_capabilities=frozenset({Capability.MEMORY_WRITE})),
            authority=_authority(),
            local_node_id="desktop-node",
            now_ns=time.time_ns(),
        )


def test_generation_fence_rejects_rotated_or_reconnected_authority() -> None:
    with pytest.raises(PeerCallAdmissionError, match="generation"):
        admit_peer_call(
            _request(expected_enrollment_generation=6),
            authority=_authority(generation=7),
            local_node_id="desktop-node",
            now_ns=time.time_ns(),
        )


def test_cycle_and_hop_guards_reject_a_to_b_to_a() -> None:
    with pytest.raises(PeerCallAdmissionError, match="cycle"):
        admit_peer_call(
            _request(visited_nodes=("desktop-node", "phone-node")),
            authority=_authority(),
            local_node_id="desktop-node",
            now_ns=time.time_ns(),
        )

    with pytest.raises(PeerCallAdmissionError, match="hop"):
        admit_peer_call(
            _request(hop_limit=0),
            authority=_authority(),
            local_node_id="desktop-node",
            now_ns=time.time_ns(),
        )


def test_ask_and_delegate_map_to_existing_runtime_submit_turn() -> None:
    for operation in ("node.ask", "node.delegate"):
        admitted = admit_peer_call(
            _request(operation=operation),
            authority=_authority(),
            local_node_id="desktop-node",
            now_ns=time.time_ns(),
        )
        command = admitted.to_runtime_command(conversation_id="conversation-1")
        assert isinstance(command, SubmitTurn)
        assert command.request_id == "ask-1"
        assert command.text == "what changed in this project?"
        assert command.conversation_id == "conversation-1"
        assert command.context_ids == ("ctx:project",)


def test_cancel_maps_to_existing_runtime_cancel_and_is_generation_fenced() -> None:
    cancel = PeerCancelRequest(
        request_id="cancel-1",
        call_id="ask-1",
        turn_id="turn-9",
        expected_enrollment_generation=7,
        reason="operator_cancelled",
    )
    command = cancel.to_runtime_command(authority=_authority())
    assert command == CancelTurn(request_id="cancel-1", turn_id="turn-9")

    with pytest.raises(PeerCallAdmissionError, match="generation"):
        cancel.to_runtime_command(authority=_authority(generation=8))


def test_result_is_bounded_and_preserves_node_runtime_provenance() -> None:
    result = PeerResult.completed(
        request_id="ask-1",
        source_node_id="desktop-node",
        runtime_id="zara-python",
        runtime_generation=13,
        text="three files changed",
        trace_id="trace-1",
    )
    wire = result.to_wire_body()
    assert wire == {
        "request_id": "ask-1",
        "status": "completed",
        "source_node_id": "desktop-node",
        "runtime_id": "zara-python",
        "runtime_generation": 13,
        "text": "three files changed",
        "trace_id": "trace-1",
    }


def test_remote_errors_are_closed_and_never_project_tracebacks() -> None:
    projected = project_remote_error(
        TimeoutError("secret traceback text /home/user/key"),
        request_id="ask-1",
        source_node_id="desktop-node",
        runtime_id="zara-python",
        runtime_generation=13,
        trace_id="trace-1",
    )
    assert isinstance(projected, PeerRemoteError)
    assert projected.code == "timeout"
    assert projected.message == "peer runtime timed out"
    assert "secret" not in projected.to_wire_body()["message"]


def test_same_request_identity_is_stable_for_existing_runtime_idempotency() -> None:
    first = admit_peer_call(
        _request(), authority=_authority(), local_node_id="desktop-node", now_ns=time.time_ns()
    ).to_runtime_command(conversation_id="conversation-1")
    second = admit_peer_call(
        _request(), authority=_authority(), local_node_id="desktop-node", now_ns=time.time_ns()
    ).to_runtime_command(conversation_id="conversation-1")
    assert first == second
    assert first.request_id == "ask-1"
