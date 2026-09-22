"""Inert wire contract for Core-owned registered-predicate execution authority.

This module is deliberately *not* an authority implementation.  It defines the
only data shapes that may cross from a same-process plugin into the future
Core-owned out-of-interpreter #663 execution owner.  The owner binds principal,
namespace and executable predicate identity out-of-band; callers never carry
those fields and never receive a mint, raw backend, registry or process pipe.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass, fields
from enum import Enum
from typing import Any, Mapping, Sequence


PREDICATE_AUTHORITY_PROTOCOL = "ZARA-PREDICATE/1"
MAX_REQUEST_ID_LENGTH = 128
MAX_OPERATION_LENGTH = 128
MAX_ARGUMENT_DEPTH = 8
MAX_ARGUMENT_NODES = 256
MAX_STRING_LENGTH = 4096

_IDENTIFIER = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:-]*$")
_OPERATION = re.compile(r"^[a-z][a-z0-9_.-]*$")
_FORBIDDEN_REQUEST_FIELDS = frozenset(
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


class PredicateAuthorityError(ValueError):
    """Invalid or unsafe predicate-authority wire data."""


class PredicateVerdict(str, Enum):
    SUCCEEDED = "succeeded"
    FAILED = "failed"
    UNKNOWN = "unknown"
    BLOCKED = "blocked"
    UNSUPPORTED = "unsupported"
    CANCELLED = "cancelled"
    ERROR = "error"


def _require_int(value: object, *, name: str, minimum: int = 0) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value < minimum:
        raise PredicateAuthorityError(f"{name} must be an integer >= {minimum}")
    return value


def _require_identifier(value: object, *, name: str, maximum: int) -> str:
    if not isinstance(value, str) or not value or len(value) > maximum:
        raise PredicateAuthorityError(f"{name} must contain 1 to {maximum} characters")
    if _IDENTIFIER.fullmatch(value) is None:
        raise PredicateAuthorityError(f"{name} contains unsupported characters")
    return value


def _require_operation(value: object) -> str:
    if not isinstance(value, str) or not value or len(value) > MAX_OPERATION_LENGTH:
        raise PredicateAuthorityError(
            f"operation must contain 1 to {MAX_OPERATION_LENGTH} characters"
        )
    if _OPERATION.fullmatch(value) is None:
        raise PredicateAuthorityError("operation must be a logical operation id")
    return value


def _validate_inert(value: Any, *, depth: int, budget: list[int]) -> None:
    budget[0] += 1
    if budget[0] > MAX_ARGUMENT_NODES:
        raise PredicateAuthorityError("argument payload exceeds node budget")
    if depth > MAX_ARGUMENT_DEPTH:
        raise PredicateAuthorityError("argument payload exceeds depth budget")

    if value is None or isinstance(value, bool) or isinstance(value, int):
        return
    if isinstance(value, float):
        if not math.isfinite(value):
            raise PredicateAuthorityError("argument payload contains non-finite float")
        return
    if isinstance(value, str):
        if len(value) > MAX_STRING_LENGTH:
            raise PredicateAuthorityError("argument string exceeds size budget")
        return
    if isinstance(value, Mapping):
        for key, item in value.items():
            if not isinstance(key, str) or not key or len(key) > MAX_STRING_LENGTH:
                raise PredicateAuthorityError("argument object keys must be bounded strings")
            _validate_inert(item, depth=depth + 1, budget=budget)
        return
    if isinstance(value, Sequence) and not isinstance(value, (str, bytes, bytearray)):
        for item in value:
            _validate_inert(item, depth=depth + 1, budget=budget)
        return
    raise PredicateAuthorityError(
        f"argument payload type {type(value).__name__!r} is not inert wire data"
    )


def validate_inert_arguments(arguments: Mapping[str, Any]) -> dict[str, Any]:
    """Validate and copy one caller-provided structured argument object."""

    if not isinstance(arguments, Mapping):
        raise PredicateAuthorityError("arguments must be a mapping")
    copied = dict(arguments)
    if _FORBIDDEN_REQUEST_FIELDS.intersection(copied):
        field = sorted(_FORBIDDEN_REQUEST_FIELDS.intersection(copied))[0]
        raise PredicateAuthorityError(
            f"caller argument field {field!r} is reserved authority metadata"
        )
    _validate_inert(copied, depth=0, budget=[0])
    return copied


@dataclass(frozen=True)
class PredicateInvocationRequest:
    """Plugin-facing request: logical selector + inert arguments only."""

    request_id: str
    operation: str
    expected_generation: int
    arguments: Mapping[str, Any]
    timeout_ms: int
    protocol: str = PREDICATE_AUTHORITY_PROTOCOL

    def __post_init__(self) -> None:
        if self.protocol != PREDICATE_AUTHORITY_PROTOCOL:
            raise PredicateAuthorityError("unsupported predicate-authority protocol")
        _require_identifier(
            self.request_id,
            name="request_id",
            maximum=MAX_REQUEST_ID_LENGTH,
        )
        _require_operation(self.operation)
        _require_int(self.expected_generation, name="expected_generation", minimum=1)
        _require_int(self.timeout_ms, name="timeout_ms", minimum=1)
        object.__setattr__(self, "arguments", validate_inert_arguments(self.arguments))

    @classmethod
    def authority_field_names(cls) -> frozenset[str]:
        return frozenset(field.name for field in fields(cls))


@dataclass(frozen=True)
class PredicateExecutionProvenance:
    """Observed identity emitted by the authority owner after execution."""

    namespace: str
    predicate: str
    arity: int
    generation: int
    request_id: str

    def __post_init__(self) -> None:
        _require_identifier(self.namespace, name="namespace", maximum=128)
        _require_identifier(self.predicate, name="predicate", maximum=128)
        _require_int(self.arity, name="arity", minimum=0)
        _require_int(self.generation, name="generation", minimum=1)
        _require_identifier(
            self.request_id,
            name="request_id",
            maximum=MAX_REQUEST_ID_LENGTH,
        )


@dataclass(frozen=True)
class PredicateInvocationResult:
    """Bounded result envelope; process completion is not a success verdict."""

    verdict: PredicateVerdict
    provenance: PredicateExecutionProvenance | None
    data: Mapping[str, Any]
    error_code: str = ""

    def __post_init__(self) -> None:
        if not isinstance(self.verdict, PredicateVerdict):
            raise PredicateAuthorityError("verdict must be a PredicateVerdict")
        if self.verdict is PredicateVerdict.SUCCEEDED and self.provenance is None:
            raise PredicateAuthorityError("successful result requires execution provenance")
        if self.error_code:
            _require_identifier(self.error_code, name="error_code", maximum=64)
        object.__setattr__(self, "data", validate_inert_arguments(self.data))
