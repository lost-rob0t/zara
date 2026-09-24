"""Bounded peer ask/delegate contract over existing Zara authority/runtime seams.

This module intentionally owns no socket, authentication database, runtime,
planner, or task store.  ZARA/1 supplies the authenticated peer/session
identity; ZARA-RUNTIME/1 supplies execution and cancellation.  The helpers here
only validate the peer-call projection and map it onto those existing seams.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Mapping

from zara.runtime.commands import CancelTurn, SubmitTurn
from zara.security import Capability


PEER_RUNTIME_PROTOCOL = "ZARA-RUNTIME/1"
PEER_MAX_HOPS = 8
PEER_MAX_CONTENT_BYTES = 16 * 1024
PEER_MAX_OUTPUT_BYTES = 1024 * 1024
PEER_MAX_CONTEXT_REFS = 32
PEER_MAX_MEDIA_REFS = 16
PEER_MAX_CAPABILITIES = 32
PEER_MAX_TOKEN_BYTES = 128
PEER_MAX_ERROR_BYTES = 256
PEER_MAX_WALL_TIME_MS = 5 * 60 * 1000
PEER_MAX_TOKENS = 1_000_000
PEER_MAX_COST_MICROUNITS = 100_000_000
PEER_MAX_TOOL_CALLS = 256
PEER_MAX_MODEL_CALLS = 256
PEER_MAX_RECURSION_DEPTH = 16

_PEER_OPERATIONS = frozenset({"node.ask", "node.delegate"})
_AUTHORITY_FIELDS = frozenset(
    {"source_node_id", "target_node_id", "principal_id", "session_id"}
)
_REQUEST_FIELDS = frozenset(
    {
        "content",
        "context_refs",
        "media_refs",
        "requested_capabilities",
        "budget",
        "deadline_ns",
        "hop_limit",
        "visited_nodes",
        "cycle_token",
        "trace_id",
        "correlation_id",
        "causation_id",
        "expected_enrollment_generation",
    }
)
_CANCEL_FIELDS = frozenset(
    {"call_id", "turn_id", "expected_enrollment_generation", "reason"}
)
_RESULT_FIELDS = frozenset(
    {
        "request_id",
        "status",
        "source_node_id",
        "runtime_id",
        "runtime_generation",
        "text",
        "trace_id",
    }
)
_ERROR_FIELDS = frozenset(
    {
        "request_id",
        "status",
        "code",
        "message",
        "retryable",
        "source_node_id",
        "runtime_id",
        "runtime_generation",
        "trace_id",
    }
)

_BUDGET_FIELDS = frozenset(
    {
        "wall_time_ms",
        "max_output_bytes",
        "max_tokens",
        "max_cost_microunits",
        "max_tool_calls",
        "max_model_calls",
        "max_recursion_depth",
    }
)
_REMOTE_ERROR_CODES = frozenset(
    {
        "invalid_request",
        "unauthorized",
        "capability_denied",
        "budget_exceeded",
        "timeout",
        "cancelled",
        "unavailable",
        "runtime_error",
    }
)


class PeerCallAdmissionError(ValueError):
    """A peer call cannot cross the authenticated runtime boundary."""


class PeerCallRemoteError(RuntimeError):
    """Typed safe peer-call failure projected from a remote node."""

    def __init__(self, error: "PeerRemoteError") -> None:
        if not isinstance(error, PeerRemoteError):
            raise TypeError("error must be PeerRemoteError")
        super().__init__(f"{error.code}: {error.message}")
        self.error = error
        self.code = error.code
        self.retryable = error.retryable


def _exact_int(name: str, value: Any, minimum: int, maximum: int) -> int:
    if type(value) is not int or not minimum <= value <= maximum:
        raise ValueError(f"{name} must be an integer in [{minimum}, {maximum}]")
    return value


def _bounded_text(name: str, value: Any, *, max_bytes: int, nonempty: bool = True) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{name} must be a string")
    if nonempty and not value:
        raise ValueError(f"{name} must not be empty")
    if len(value.encode("utf-8")) > max_bytes:
        raise ValueError(f"{name} exceeds byte limit")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in value):
        raise ValueError(f"{name} contains control characters")
    return value


def _token(name: str, value: Any) -> str:
    text = _bounded_text(name, value, max_bytes=PEER_MAX_TOKEN_BYTES)
    if text.strip() != text or any(character.isspace() for character in text):
        raise ValueError(f"{name} must be a canonical token")
    return text


def _refs(name: str, value: Any, *, maximum: int) -> tuple[str, ...]:
    if value is None:
        return ()
    if not isinstance(value, (list, tuple)):
        raise ValueError(f"{name} must be a sequence")
    if len(value) > maximum:
        raise ValueError(f"{name} exceeds maximum count {maximum}")
    result = tuple(_token(f"{name}[{index}]", item) for index, item in enumerate(value))
    if len(set(result)) != len(result):
        raise ValueError(f"{name} contains duplicates")
    return result


def _capabilities(value: Any) -> frozenset[Capability]:
    if value is None:
        return frozenset()
    if not isinstance(value, (list, tuple, set, frozenset)):
        raise ValueError("requested_capabilities must be a sequence")
    if len(value) > PEER_MAX_CAPABILITIES:
        raise ValueError("requested_capabilities exceeds maximum count")
    result: set[Capability] = set()
    for item in value:
        if isinstance(item, Capability):
            capability = item
        elif isinstance(item, str):
            try:
                capability = Capability(item)
            except ValueError as error:
                raise ValueError("requested_capabilities contains an unknown capability") from error
        else:
            raise ValueError("requested_capabilities contains an invalid capability")
        result.add(capability)
    return frozenset(result)


@dataclass(frozen=True)
class PeerCallBudget:
    wall_time_ms: int
    max_output_bytes: int
    max_tokens: int = 0
    max_cost_microunits: int = 0
    max_tool_calls: int = 0
    max_model_calls: int = 0
    max_recursion_depth: int = 0

    def __post_init__(self) -> None:
        _exact_int("wall_time_ms", self.wall_time_ms, 1, PEER_MAX_WALL_TIME_MS)
        _exact_int("max_output_bytes", self.max_output_bytes, 1, PEER_MAX_OUTPUT_BYTES)
        _exact_int("max_tokens", self.max_tokens, 0, PEER_MAX_TOKENS)
        _exact_int("max_cost_microunits", self.max_cost_microunits, 0, PEER_MAX_COST_MICROUNITS)
        _exact_int("max_tool_calls", self.max_tool_calls, 0, PEER_MAX_TOOL_CALLS)
        _exact_int("max_model_calls", self.max_model_calls, 0, PEER_MAX_MODEL_CALLS)
        _exact_int(
            "max_recursion_depth",
            self.max_recursion_depth,
            0,
            PEER_MAX_RECURSION_DEPTH,
        )

    @classmethod
    def from_wire(cls, value: Any) -> "PeerCallBudget":
        if not isinstance(value, Mapping):
            raise PeerCallAdmissionError("budget must be an object")
        unknown = set(value) - _BUDGET_FIELDS
        if unknown:
            raise PeerCallAdmissionError("budget contains unknown fields")
        missing = {"wall_time_ms", "max_output_bytes"} - set(value)
        if missing:
            raise PeerCallAdmissionError("budget is missing required fields")
        try:
            return cls(
                wall_time_ms=value["wall_time_ms"],
                max_output_bytes=value["max_output_bytes"],
                max_tokens=value.get("max_tokens", 0),
                max_cost_microunits=value.get("max_cost_microunits", 0),
                max_tool_calls=value.get("max_tool_calls", 0),
                max_model_calls=value.get("max_model_calls", 0),
                max_recursion_depth=value.get("max_recursion_depth", 0),
            )
        except ValueError as error:
            raise PeerCallAdmissionError(str(error)) from error

    def to_wire(self) -> dict[str, int]:
        return {
            "wall_time_ms": self.wall_time_ms,
            "max_output_bytes": self.max_output_bytes,
            "max_tokens": self.max_tokens,
            "max_cost_microunits": self.max_cost_microunits,
            "max_tool_calls": self.max_tool_calls,
            "max_model_calls": self.max_model_calls,
            "max_recursion_depth": self.max_recursion_depth,
        }


@dataclass(frozen=True)
class PeerAuthority:
    principal_id: str
    source_node_id: str
    session_id: str
    enrollment_generation: int
    capabilities: frozenset[Capability]

    def __post_init__(self) -> None:
        _token("principal_id", self.principal_id)
        _token("source_node_id", self.source_node_id)
        _token("session_id", self.session_id)
        _exact_int("enrollment_generation", self.enrollment_generation, 1, 2**63 - 1)
        for capability in self.capabilities:
            if not isinstance(capability, Capability):
                raise TypeError("capabilities must contain Capability values")


@dataclass(frozen=True)
class PeerCallRequest:
    operation: str
    request_id: str
    content: str
    context_refs: tuple[str, ...]
    media_refs: tuple[str, ...]
    requested_capabilities: frozenset[Capability]
    budget: PeerCallBudget
    deadline_ns: int
    hop_limit: int
    visited_nodes: tuple[str, ...]
    cycle_token: str
    trace_id: str
    correlation_id: str
    causation_id: str
    expected_enrollment_generation: int

    def __post_init__(self) -> None:
        if self.operation not in _PEER_OPERATIONS:
            raise ValueError("operation must be node.ask or node.delegate")
        _token("request_id", self.request_id)
        _bounded_text("content", self.content, max_bytes=PEER_MAX_CONTENT_BYTES)
        object.__setattr__(
            self,
            "context_refs",
            _refs("context_refs", self.context_refs, maximum=PEER_MAX_CONTEXT_REFS),
        )
        object.__setattr__(
            self,
            "media_refs",
            _refs("media_refs", self.media_refs, maximum=PEER_MAX_MEDIA_REFS),
        )
        if len(self.context_refs) + len(self.media_refs) > PEER_MAX_CONTEXT_REFS:
            raise ValueError("combined context/media refs exceed runtime context bound")
        object.__setattr__(self, "requested_capabilities", _capabilities(self.requested_capabilities))
        if not isinstance(self.budget, PeerCallBudget):
            raise TypeError("budget must be PeerCallBudget")
        _exact_int("deadline_ns", self.deadline_ns, 1, 2**63 - 1)
        _exact_int("hop_limit", self.hop_limit, 0, PEER_MAX_HOPS)
        object.__setattr__(
            self,
            "visited_nodes",
            _refs("visited_nodes", self.visited_nodes, maximum=PEER_MAX_HOPS + 1),
        )
        _token("cycle_token", self.cycle_token)
        _token("trace_id", self.trace_id)
        _token("correlation_id", self.correlation_id)
        _token("causation_id", self.causation_id)
        _exact_int(
            "expected_enrollment_generation",
            self.expected_enrollment_generation,
            1,
            2**63 - 1,
        )

    @classmethod
    def from_wire(
        cls,
        operation: str,
        request_id: str,
        body: Mapping[str, Any],
    ) -> "PeerCallRequest":
        if not isinstance(body, Mapping):
            raise PeerCallAdmissionError("peer call body must be an object")
        if set(body) & _AUTHORITY_FIELDS:
            raise PeerCallAdmissionError("peer authority is session-derived and cannot be payload-selected")
        unknown = set(body) - _REQUEST_FIELDS
        if unknown:
            raise PeerCallAdmissionError("peer call body contains unknown fields")
        required = {
            "content",
            "budget",
            "deadline_ns",
            "hop_limit",
            "cycle_token",
            "trace_id",
            "correlation_id",
            "causation_id",
            "expected_enrollment_generation",
        }
        if required - set(body):
            raise PeerCallAdmissionError("peer call body is missing required fields")
        try:
            return cls(
                operation=operation,
                request_id=request_id,
                content=body["content"],
                context_refs=_refs(
                    "context_refs", body.get("context_refs", ()), maximum=PEER_MAX_CONTEXT_REFS
                ),
                media_refs=_refs(
                    "media_refs", body.get("media_refs", ()), maximum=PEER_MAX_MEDIA_REFS
                ),
                requested_capabilities=_capabilities(body.get("requested_capabilities", ())),
                budget=PeerCallBudget.from_wire(body["budget"]),
                deadline_ns=body["deadline_ns"],
                hop_limit=body["hop_limit"],
                visited_nodes=_refs(
                    "visited_nodes", body.get("visited_nodes", ()), maximum=PEER_MAX_HOPS + 1
                ),
                cycle_token=body["cycle_token"],
                trace_id=body["trace_id"],
                correlation_id=body["correlation_id"],
                causation_id=body["causation_id"],
                expected_enrollment_generation=body["expected_enrollment_generation"],
            )
        except (TypeError, ValueError) as error:
            if isinstance(error, PeerCallAdmissionError):
                raise
            raise PeerCallAdmissionError(str(error)) from error

    def to_wire_body(self) -> dict[str, Any]:
        return {
            "content": self.content,
            "context_refs": list(self.context_refs),
            "media_refs": list(self.media_refs),
            "requested_capabilities": sorted(item.value for item in self.requested_capabilities),
            "budget": self.budget.to_wire(),
            "deadline_ns": self.deadline_ns,
            "hop_limit": self.hop_limit,
            "visited_nodes": list(self.visited_nodes),
            "cycle_token": self.cycle_token,
            "trace_id": self.trace_id,
            "correlation_id": self.correlation_id,
            "causation_id": self.causation_id,
            "expected_enrollment_generation": self.expected_enrollment_generation,
        }


@dataclass(frozen=True)
class AdmittedPeerCall:
    request: PeerCallRequest
    authority: PeerAuthority
    source_node_id: str
    target_node_id: str
    protocol: str = PEER_RUNTIME_PROTOCOL

    def to_runtime_command(self, *, conversation_id: str | None) -> SubmitTurn:
        # The opaque refs stay host-owned.  Mapping them to the canonical
        # SubmitTurn context_ids seam keeps the selected runtime responsible for
        # interpretation while avoiding any peer-specific runtime/task store.
        context_ids = (*self.request.context_refs, *self.request.media_refs)
        return SubmitTurn(
            request_id=self.request.request_id,
            text=self.request.content,
            conversation_id=conversation_id,
            context_ids=context_ids,
        )


def admit_peer_call(
    request: PeerCallRequest,
    *,
    authority: PeerAuthority,
    local_node_id: str,
    now_ns: int,
) -> AdmittedPeerCall:
    if not isinstance(request, PeerCallRequest):
        raise TypeError("request must be PeerCallRequest")
    if not isinstance(authority, PeerAuthority):
        raise TypeError("authority must be PeerAuthority")
    target = _token("local_node_id", local_node_id)
    _exact_int("now_ns", now_ns, 0, 2**63 - 1)
    if Capability.TURN_SUBMIT not in authority.capabilities:
        raise PeerCallAdmissionError("authenticated peer lacks turn submit capability")
    if request.deadline_ns <= now_ns:
        raise PeerCallAdmissionError("peer call deadline has expired")
    if request.expected_enrollment_generation != authority.enrollment_generation:
        raise PeerCallAdmissionError("peer call generation is stale")
    if not request.requested_capabilities.issubset(authority.capabilities):
        raise PeerCallAdmissionError("requested capability exceeds authenticated authority")
    if request.hop_limit <= 0:
        raise PeerCallAdmissionError("peer call hop budget is exhausted")
    if target in request.visited_nodes or authority.source_node_id in request.visited_nodes:
        raise PeerCallAdmissionError("peer call cycle detected")
    return AdmittedPeerCall(
        request=request,
        authority=authority,
        source_node_id=authority.source_node_id,
        target_node_id=target,
    )


@dataclass(frozen=True)
class PeerCancelRequest:
    request_id: str
    call_id: str
    turn_id: str
    expected_enrollment_generation: int
    reason: str = "cancelled"

    def __post_init__(self) -> None:
        _token("request_id", self.request_id)
        _token("call_id", self.call_id)
        _token("turn_id", self.turn_id)
        _exact_int(
            "expected_enrollment_generation",
            self.expected_enrollment_generation,
            1,
            2**63 - 1,
        )
        _bounded_text("reason", self.reason, max_bytes=PEER_MAX_ERROR_BYTES)

    @classmethod
    def from_wire(
        cls,
        request_id: str,
        body: Mapping[str, Any],
    ) -> "PeerCancelRequest":
        if not isinstance(body, Mapping):
            raise PeerCallAdmissionError("peer cancel body must be an object")
        if set(body) & _AUTHORITY_FIELDS:
            raise PeerCallAdmissionError(
                "peer authority is session-derived and cannot be payload-selected"
            )
        if set(body) - _CANCEL_FIELDS:
            raise PeerCallAdmissionError("peer cancel body contains unknown fields")
        missing = {"call_id", "turn_id", "expected_enrollment_generation"} - set(body)
        if missing:
            raise PeerCallAdmissionError("peer cancel body is missing required fields")
        try:
            return cls(
                request_id=request_id,
                call_id=body["call_id"],
                turn_id=body["turn_id"],
                expected_enrollment_generation=body["expected_enrollment_generation"],
                reason=body.get("reason", "cancelled"),
            )
        except (TypeError, ValueError) as error:
            raise PeerCallAdmissionError(str(error)) from error

    def to_wire_body(self) -> dict[str, Any]:
        return {
            "call_id": self.call_id,
            "turn_id": self.turn_id,
            "expected_enrollment_generation": self.expected_enrollment_generation,
            "reason": self.reason,
        }

    def to_runtime_command(self, *, authority: PeerAuthority) -> CancelTurn:
        if Capability.TURN_CANCEL not in authority.capabilities:
            raise PeerCallAdmissionError("authenticated peer lacks turn cancel capability")
        if self.expected_enrollment_generation != authority.enrollment_generation:
            raise PeerCallAdmissionError("peer cancel generation is stale")
        return CancelTurn(request_id=self.request_id, turn_id=self.turn_id)


@dataclass(frozen=True)
class PeerResult:
    request_id: str
    status: str
    source_node_id: str
    runtime_id: str
    runtime_generation: int
    text: str
    trace_id: str

    def __post_init__(self) -> None:
        _token("request_id", self.request_id)
        if self.status != "completed":
            raise ValueError("PeerResult status must be completed")
        _token("source_node_id", self.source_node_id)
        _token("runtime_id", self.runtime_id)
        _exact_int("runtime_generation", self.runtime_generation, 0, 2**63 - 1)
        _bounded_text("text", self.text, max_bytes=PEER_MAX_OUTPUT_BYTES, nonempty=False)
        _token("trace_id", self.trace_id)

    @classmethod
    def from_wire(cls, body: Mapping[str, Any]) -> "PeerResult":
        if not isinstance(body, Mapping) or set(body) != _RESULT_FIELDS:
            raise ValueError("peer result body has invalid fields")
        return cls(
            request_id=body["request_id"],
            status=body["status"],
            source_node_id=body["source_node_id"],
            runtime_id=body["runtime_id"],
            runtime_generation=body["runtime_generation"],
            text=body["text"],
            trace_id=body["trace_id"],
        )

    @classmethod
    def completed(
        cls,
        *,
        request_id: str,
        source_node_id: str,
        runtime_id: str,
        runtime_generation: int,
        text: str,
        trace_id: str,
    ) -> "PeerResult":
        return cls(
            request_id=request_id,
            status="completed",
            source_node_id=source_node_id,
            runtime_id=runtime_id,
            runtime_generation=runtime_generation,
            text=text,
            trace_id=trace_id,
        )

    def to_wire_body(self) -> dict[str, Any]:
        return {
            "request_id": self.request_id,
            "status": self.status,
            "source_node_id": self.source_node_id,
            "runtime_id": self.runtime_id,
            "runtime_generation": self.runtime_generation,
            "text": self.text,
            "trace_id": self.trace_id,
        }


@dataclass(frozen=True)
class PeerRemoteError:
    request_id: str
    code: str
    message: str
    retryable: bool
    source_node_id: str
    runtime_id: str
    runtime_generation: int
    trace_id: str

    def __post_init__(self) -> None:
        _token("request_id", self.request_id)
        if self.code not in _REMOTE_ERROR_CODES:
            raise ValueError("unknown peer remote error code")
        _bounded_text("message", self.message, max_bytes=PEER_MAX_ERROR_BYTES)
        _token("source_node_id", self.source_node_id)
        _token("runtime_id", self.runtime_id)
        _exact_int("runtime_generation", self.runtime_generation, 0, 2**63 - 1)
        _token("trace_id", self.trace_id)

    @classmethod
    def from_wire(cls, body: Mapping[str, Any]) -> "PeerRemoteError":
        if not isinstance(body, Mapping) or set(body) != _ERROR_FIELDS:
            raise ValueError("peer error body has invalid fields")
        if body.get("status") != "failed":
            raise ValueError("peer error status must be failed")
        return cls(
            request_id=body["request_id"],
            code=body["code"],
            message=body["message"],
            retryable=body["retryable"],
            source_node_id=body["source_node_id"],
            runtime_id=body["runtime_id"],
            runtime_generation=body["runtime_generation"],
            trace_id=body["trace_id"],
        )

    def to_wire_body(self) -> dict[str, Any]:
        return {
            "request_id": self.request_id,
            "status": "failed",
            "code": self.code,
            "message": self.message,
            "retryable": self.retryable,
            "source_node_id": self.source_node_id,
            "runtime_id": self.runtime_id,
            "runtime_generation": self.runtime_generation,
            "trace_id": self.trace_id,
        }


def require_live_runtime_budget(request: PeerCallRequest) -> None:
    """Fail closed for budget dimensions RuntimeHost cannot yet enforce per turn.

    Wall time/deadline and output bytes are enforced by the peer gateway.  The
    current canonical SubmitTurn seam has no request-scoped token/tool/model or
    cost meter, so accepting non-zero limits would falsely claim enforcement.
    """

    if not isinstance(request, PeerCallRequest):
        raise TypeError("request must be PeerCallRequest")
    budget = request.budget
    unsupported = (
        budget.max_tokens,
        budget.max_cost_microunits,
        budget.max_tool_calls,
        budget.max_model_calls,
        budget.max_recursion_depth,
    )
    if any(unsupported):
        raise PeerCallAdmissionError(
            "peer runtime budget requests an unsupported metered dimension"
        )


def project_remote_error(
    error: BaseException,
    *,
    request_id: str,
    source_node_id: str,
    runtime_id: str,
    runtime_generation: int,
    trace_id: str,
) -> PeerRemoteError:
    if isinstance(error, TimeoutError):
        code, message, retryable = "timeout", "peer runtime timed out", True
    elif isinstance(error, PermissionError):
        code, message, retryable = "unauthorized", "peer runtime request is not authorized", False
    elif isinstance(error, ValueError):
        code, message, retryable = "invalid_request", "peer runtime request is invalid", False
    elif isinstance(error, (ConnectionError, OSError)):
        code, message, retryable = "unavailable", "peer runtime is unavailable", True
    elif isinstance(error, KeyboardInterrupt):
        code, message, retryable = "cancelled", "peer runtime request was cancelled", False
    else:
        code, message, retryable = "runtime_error", "peer runtime failed", False
    return PeerRemoteError(
        request_id=request_id,
        code=code,
        message=message,
        retryable=retryable,
        source_node_id=source_node_id,
        runtime_id=runtime_id,
        runtime_generation=runtime_generation,
        trace_id=trace_id,
    )


__all__ = [
    "AdmittedPeerCall",
    "PEER_MAX_HOPS",
    "PEER_RUNTIME_PROTOCOL",
    "PeerAuthority",
    "PeerCallAdmissionError",
    "PeerCallRemoteError",
    "PeerCallBudget",
    "PeerCallRequest",
    "PeerCancelRequest",
    "PeerRemoteError",
    "PeerResult",
    "admit_peer_call",
    "project_remote_error",
    "require_live_runtime_budget",
]
