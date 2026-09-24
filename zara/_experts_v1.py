"""ZARA-EXPERT/1 expert descriptor, activation and invocation contract.

This module owns the interoperable expert contract on Zara desktop: bounded
descriptors, the closed operation vocabulary, generation-fenced activation
handles, request/result envelopes and a trusted single-process registry.
Registration is a trusted host action; invocation data never becomes new
authority. V1 performs no model calls, no network access and no subprocesses,
and execution isolation remains out of scope per #663.
"""

from __future__ import annotations

import hashlib
import inspect
import json
import logging
import math
import re
import threading
import uuid
from collections.abc import Callable, Iterable, Mapping
from dataclasses import dataclass, field, replace
from enum import Enum
from typing import Any, Optional

logger = logging.getLogger(__name__)

ZARA_EXPERT_PROTOCOL = "ZARA-EXPERT/1"

EXPERT_OPERATIONS = frozenset(
    {
        "expert.list",
        "expert.describe",
        "expert.match",
        "expert.activate",
        "expert.status",
        "expert.invoke",
        "expert.explain",
        "expert.cancel",
        "expert.deactivate",
    }
)

_PROTOCOL = re.compile(r"^ZARA-EXPERT/([1-9][0-9]*)$")
_EXPERT_ID = re.compile(r"^[!-~]{1,128}$")
_NAMESPACE = re.compile(r"^[a-z][a-z0-9_-]{0,63}$")
_DIGEST = re.compile(r"^[a-z0-9][a-z0-9._-]{0,63}:[a-z0-9][a-z0-9._-]{0,127}$")
_OPERATION_ID = re.compile(r"^[a-z][a-z0-9_.-]{0,63}$")
_FIELD_NAME = re.compile(r"^[a-z][a-z0-9_]{0,63}$")
_KEYWORD = re.compile(r"^[a-z0-9][a-z0-9_-]{0,63}$")
_TOKEN = re.compile(r"^[a-z0-9][a-z0-9._-]{0,63}$")
_ENGINE = re.compile(r"^[a-z][a-z0-9_-]{0,31}$")
_PORTABLE = re.compile(r"^[!-~]{1,512}$")
_ACTIVATION_ID = re.compile(r"^act:[a-f0-9]{32}$")

_MAX_OPERATIONS = 64
_MAX_FIELDS = 64
_MAX_KEYWORDS = 32
_MAX_CAPABILITIES = 32
_MAX_EFFECTS = 32
_MAX_ENGINES = 16
_MAX_PLATFORMS = 16
_MAX_ENUM_VALUES = 32
_MAX_INPUT_BYTES = 8192
_MAX_INPUT_DEPTH = 8
_MAX_INPUT_KEYS = 32
_MAX_INPUT_LIST = 64
_MAX_INPUT_STRING = 4096
_MAX_DECISION_REFS = 16
_MAX_EVIDENCE_REFS = 32
_MAX_REF_LENGTH = 128
_MAX_RECEIPTS = 32
_MAX_ERROR_MESSAGE = 256
_SELECTABLE_AVAILABILITY = frozenset(
    {"installed", "available", "ready"}
)


class ExpertVerdict(str, Enum):
    SUCCEEDED = "succeeded"
    FAILED = "failed"
    UNKNOWN = "unknown"
    BLOCKED = "blocked"
    UNSUPPORTED = "unsupported"
    CANCELLED = "cancelled"
    ERROR = "error"


class ExpertErrorCode(str, Enum):
    INVALID_INPUT = "invalid_input"
    AMBIGUITY = "ambiguity"
    UNSUPPORTED_OPERATION = "unsupported_operation"
    UNSUPPORTED_BACKEND = "unsupported_backend"
    INCOMPATIBLE_PROTOCOL = "incompatible_protocol"
    DENIED = "denied"
    APPROVAL_REQUIRED = "approval_required"
    STALE_GENERATION = "stale_generation"
    UNAVAILABLE = "unavailable"
    DEADLINE_EXCEEDED = "deadline_exceeded"
    BUDGET_EXCEEDED = "budget_exceeded"
    CANCELLED = "cancelled"
    INTERRUPTED = "interrupted"
    UNKNOWN_EXTERNAL_OUTCOME = "unknown_external_outcome"


class ReasoningKind(str, Enum):
    SYMBOLIC = "symbolic"
    HYBRID = "hybrid"
    MODEL = "model"
    SERVICE = "service"


class EffectClass(str, Enum):
    NONE = "none"
    FILESYSTEM_READ = "filesystem_read"
    FILESYSTEM_WRITE = "filesystem_write"
    NETWORK_EGRESS = "network_egress"
    PROCESS_SPAWN = "process_spawn"
    MODEL_INFERENCE = "model_inference"
    AUDIO_CAPTURE = "audio_capture"


class ExpertAvailability(str, Enum):
    INSTALLED = "installed"
    AVAILABLE = "available"
    READY = "ready"
    UNAVAILABLE = "unavailable"
    ABSENT = "absent"


class FallbackPolicy(str, Enum):
    FAIL_CLOSED = "fail_closed"
    CONVERSATIONAL = "conversational"
    NONE = "none"


class DelegationPolicy(str, Enum):
    NEVER = "never"
    CHILDREN = "children"
    ANY = "any"


class FieldType(str, Enum):
    BOOLEAN = "boolean"
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"
    ENUM = "enum"
    LIST = "list"
    OBJECT = "object"
    REFERENCE = "reference"
    SECRET_REFERENCE = "secret_reference"


class LifecycleState(str, Enum):
    INACTIVE = "inactive"
    ACTIVATING = "activating"
    ACTIVE = "active"
    DRAINING = "draining"
    UNAVAILABLE = "unavailable"
    FAILED = "failed"


ACTIVATION_TRANSITIONS: dict[LifecycleState, frozenset[LifecycleState]] = {
    LifecycleState.INACTIVE: frozenset({LifecycleState.ACTIVATING}),
    LifecycleState.ACTIVATING: frozenset({LifecycleState.ACTIVE, LifecycleState.FAILED}),
    LifecycleState.ACTIVE: frozenset(
        {LifecycleState.DRAINING, LifecycleState.UNAVAILABLE, LifecycleState.FAILED}
    ),
    LifecycleState.DRAINING: frozenset({LifecycleState.INACTIVE}),
    LifecycleState.UNAVAILABLE: frozenset({LifecycleState.ACTIVE}),
    LifecycleState.FAILED: frozenset({LifecycleState.INACTIVE}),
}


class ExpertContractError(ValueError):
    """Base class for bounded ZARA-EXPERT/1 contract failures."""

    code = ExpertErrorCode.INVALID_INPUT


class ExpertInvalidInputError(ExpertContractError):
    code = ExpertErrorCode.INVALID_INPUT


class ExpertAmbiguityError(ExpertContractError):
    code = ExpertErrorCode.AMBIGUITY

    def __init__(self, message: str, candidates: Optional[Iterable[str]] = None) -> None:
        super().__init__(message)
        self.candidates: tuple[str, ...] = tuple(candidates or ())


class ExpertUnsupportedOperationError(ExpertContractError):
    code = ExpertErrorCode.UNSUPPORTED_OPERATION


class ExpertUnsupportedBackendError(ExpertContractError):
    code = ExpertErrorCode.UNSUPPORTED_BACKEND


class ExpertIncompatibleProtocolError(ExpertContractError):
    code = ExpertErrorCode.INCOMPATIBLE_PROTOCOL


class ExpertDeniedError(ExpertContractError):
    code = ExpertErrorCode.DENIED


class ExpertApprovalRequiredError(ExpertContractError):
    code = ExpertErrorCode.APPROVAL_REQUIRED


class ExpertStaleGenerationError(ExpertContractError):
    code = ExpertErrorCode.STALE_GENERATION


class ExpertUnavailableError(ExpertContractError):
    code = ExpertErrorCode.UNAVAILABLE


class ExpertDeadlineExceededError(ExpertContractError):
    code = ExpertErrorCode.DEADLINE_EXCEEDED


class ExpertBudgetExceededError(ExpertContractError):
    code = ExpertErrorCode.BUDGET_EXCEEDED


class ExpertCancelledError(ExpertContractError):
    code = ExpertErrorCode.CANCELLED


class ExpertInterruptedError(ExpertContractError):
    code = ExpertErrorCode.INTERRUPTED


class ExpertUnknownOutcomeError(ExpertContractError):
    code = ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME


def activation_transition(
    current: LifecycleState, target: LifecycleState
) -> LifecycleState:
    if not isinstance(current, LifecycleState):
        raise TypeError("current lifecycle state must be LifecycleState")
    if not isinstance(target, LifecycleState):
        raise TypeError("target lifecycle state must be LifecycleState")
    allowed = ACTIVATION_TRANSITIONS.get(current)
    if allowed is None or target not in allowed:
        raise ExpertInvalidInputError(
            f"invalid activation transition: {current.value} -> {target.value}"
        )
    return target


def _bounded_text(value: str, *, field_name: str, limit: int) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field_name} must be a string")
    if len(value) > limit:
        raise ValueError(f"{field_name} exceeds {limit} characters")
    if not value:
        raise ValueError(f"{field_name} must not be empty")
    if value != value.strip():
        raise ValueError(f"{field_name} must not contain surrounding whitespace")
    if any(ord(char) < 0x20 or ord(char) == 0x7F for char in value):
        raise ValueError(f"{field_name} contains control characters")
    return value


def _bounded_error_message(value: Any) -> str:
    if type(value) is not str:
        raise ExpertInvalidInputError("error_message must be a string")
    if not value:
        return value
    try:
        return _bounded_text(
            value,
            field_name="error_message",
            limit=_MAX_ERROR_MESSAGE,
        )
    except (TypeError, ValueError) as error:
        raise ExpertInvalidInputError(str(error)) from error


def _bounded_pattern(
    value: str, *, field_name: str, pattern: re.Pattern[str], limit: int
) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field_name} must be a string")
    if len(value) > limit or pattern.fullmatch(value) is None:
        raise ValueError(f"invalid {field_name}: {value!r}")
    return value


def _bounded_unique_tokens(
    values: Iterable[str],
    *,
    field_name: str,
    limit: int,
    pattern: re.Pattern[str],
    token_limit: int,
) -> tuple[str, ...]:
    items = tuple(values)
    if len(items) > limit:
        raise ValueError(f"{field_name} exceeds {limit} entries")
    if len(set(items)) != len(items):
        raise ValueError(f"{field_name} contains duplicate entries")
    for item in items:
        if not isinstance(item, str):
            raise TypeError(f"{field_name} entries must be strings")
        if len(item) > token_limit or pattern.fullmatch(item) is None:
            raise ValueError(f"invalid {field_name} entry: {item!r}")
    return items


def _wire_enum(enum_type: type, value: Any, *, field_name: str) -> Any:
    if not isinstance(value, str):
        raise ExpertInvalidInputError(f"{field_name} must be a wire enum string")
    try:
        return enum_type(value)
    except ValueError:
        raise ExpertInvalidInputError(
            f"unknown {field_name} value: {value!r}"
        ) from None


@dataclass(frozen=True)
class FieldSpec:
    name: str
    type: FieldType
    required: bool = True
    enum_values: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        _bounded_pattern(
            self.name, field_name="field name", pattern=_FIELD_NAME, limit=64
        )
        if not isinstance(self.type, FieldType):
            raise TypeError("field type must be FieldType")
        if type(self.required) is not bool:
            raise TypeError("field required must be a boolean")
        if self.enum_values:
            if len(self.enum_values) > _MAX_ENUM_VALUES:
                raise ValueError("enum_values exceeds 32 entries")
            if len(set(self.enum_values)) != len(self.enum_values):
                raise ValueError("enum_values contains duplicate entries")
            for value in self.enum_values:
                if not isinstance(value, str) or not value or len(value) > 128:
                    raise ValueError(f"invalid enum_values entry: {value!r}")
        if self.type is FieldType.ENUM and not self.enum_values:
            raise ValueError("enum fields must declare enum_values")

    def to_wire(self) -> dict[str, Any]:
        wire: dict[str, Any] = {
            "name": self.name,
            "type": self.type.value,
            "required": self.required,
        }
        if self.enum_values:
            wire["enum_values"] = list(self.enum_values)
        return wire

    @classmethod
    def from_wire(cls, payload: Mapping[str, Any]) -> FieldSpec:
        if not isinstance(payload, Mapping):
            raise ExpertInvalidInputError("field descriptor must be a mapping")
        unknown = set(payload) - {"name", "type", "required", "enum_values"}
        if unknown:
            raise ExpertInvalidInputError(f"unknown field descriptor key: {sorted(unknown)[0]!r}")
        missing = {"name", "type", "required"} - set(payload)
        if missing:
            raise ExpertInvalidInputError(f"field descriptor missing: {sorted(missing)[0]!r}")
        field_type = _wire_enum(FieldType, payload["type"], field_name="field type")
        enum_values = payload.get("enum_values", ())
        if not isinstance(enum_values, (list, tuple)):
            raise ExpertInvalidInputError("enum_values must be a list")
        return cls(
            name=payload["name"],
            type=field_type,
            required=payload["required"],
            enum_values=tuple(enum_values),
        )


@dataclass(frozen=True)
class OperationSpec:
    operation_id: str
    input_fields: tuple[FieldSpec, ...] = ()
    output_fields: tuple[FieldSpec, ...] = ()

    def __post_init__(self) -> None:
        _bounded_pattern(
            self.operation_id,
            field_name="operation_id",
            pattern=_OPERATION_ID,
            limit=64,
        )
        for label, fields in (("input", self.input_fields), ("output", self.output_fields)):
            if not isinstance(fields, tuple):
                raise TypeError(f"{label}_fields must be a tuple")
            if len(fields) > _MAX_FIELDS:
                raise ValueError(f"{label}_fields exceeds {_MAX_FIELDS} entries")
            names = [item.name for item in fields]
            if len(set(names)) != len(names):
                raise ValueError(f"{label}_fields contains duplicate names")
            for item in fields:
                if not isinstance(item, FieldSpec):
                    raise TypeError(f"{label}_fields entries must be FieldSpec")

    def to_wire(self) -> dict[str, Any]:
        return {
            "operation_id": self.operation_id,
            "input_schema": {"fields": [item.to_wire() for item in self.input_fields]},
            "output_schema": {"fields": [item.to_wire() for item in self.output_fields]},
        }

    @classmethod
    def from_wire(cls, payload: Mapping[str, Any]) -> OperationSpec:
        if not isinstance(payload, Mapping):
            raise ExpertInvalidInputError("operation descriptor must be a mapping")
        unknown = set(payload) - {"operation_id", "input_schema", "output_schema"}
        if unknown:
            raise ExpertInvalidInputError(f"unknown operation key: {sorted(unknown)[0]!r}")
        missing = {"operation_id", "input_schema", "output_schema"} - set(payload)
        if missing:
            raise ExpertInvalidInputError(f"operation descriptor missing: {sorted(missing)[0]!r}")
        fields: dict[str, tuple[FieldSpec, ...]] = {}
        for schema_key, target in (
            ("input_schema", "input_fields"),
            ("output_schema", "output_fields"),
        ):
            schema = payload[schema_key]
            if not isinstance(schema, Mapping) or set(schema) != {"fields"}:
                raise ExpertInvalidInputError(
                    f"{schema_key} must be an object with exactly a fields list"
                )
            raw_fields = schema["fields"]
            if not isinstance(raw_fields, (list, tuple)):
                raise ExpertInvalidInputError(f"{schema_key} fields must be a list")
            fields[target] = tuple(FieldSpec.from_wire(item) for item in raw_fields)
        return cls(
            operation_id=payload["operation_id"],
            input_fields=fields["input_fields"],
            output_fields=fields["output_fields"],
        )


HOST_CEILINGS = {
    "timeout_ms": 600_000,
    "max_results": 10_000,
    "max_output_bytes": 10_485_760,
    "max_model_calls": 64,
}


@dataclass(frozen=True)
class ExpertLimits:
    timeout_ms: int = 30_000
    max_results: int = 100
    max_output_bytes: int = 1_048_576
    max_model_calls: int = 0

    def __post_init__(self) -> None:
        for name in ("timeout_ms", "max_results", "max_output_bytes", "max_model_calls"):
            value = getattr(self, name)
            if type(value) is not int:
                raise ExpertInvalidInputError(f"limits {name} must be an integer")
        if not 1 <= self.timeout_ms <= HOST_CEILINGS["timeout_ms"]:
            raise ExpertInvalidInputError("limits timeout_ms is outside host ceilings")
        if not 1 <= self.max_results <= HOST_CEILINGS["max_results"]:
            raise ExpertInvalidInputError("limits max_results is outside host ceilings")
        if not 1 <= self.max_output_bytes <= HOST_CEILINGS["max_output_bytes"]:
            raise ExpertInvalidInputError(
                "limits max_output_bytes is outside host ceilings"
            )
        if not 0 <= self.max_model_calls <= HOST_CEILINGS["max_model_calls"]:
            raise ExpertInvalidInputError(
                "limits max_model_calls is outside host ceilings"
            )

    def to_wire(self) -> dict[str, int]:
        return {
            "timeout_ms": self.timeout_ms,
            "max_results": self.max_results,
            "max_output_bytes": self.max_output_bytes,
            "max_model_calls": self.max_model_calls,
        }

    @classmethod
    def from_wire(cls, payload: Mapping[str, int]) -> ExpertLimits:
        if not isinstance(payload, Mapping):
            raise ExpertInvalidInputError("resource_limits must be an object")
        unknown = set(payload) - set(HOST_CEILINGS)
        if unknown:
            raise ExpertInvalidInputError(
                f"unknown resource_limits key: {sorted(unknown)[0]!r}"
            )
        return cls(**dict(payload))


@dataclass(frozen=True)
class ExpertDescriptor:
    protocol: str
    expert_id: str
    expert_version: str
    package_namespace: str
    manifest_digest: str
    name: str
    description: str
    source_reference: str
    reasoning_kind: ReasoningKind
    operations: tuple[OperationSpec, ...]
    applicability_keywords: tuple[str, ...] = ()
    required_capabilities: tuple[str, ...] = ()
    possible_effects: tuple[EffectClass, ...] = (EffectClass.NONE,)
    supported_engines: tuple[str, ...] = ()
    supported_platforms: tuple[str, ...] = ()
    fallback_policy: FallbackPolicy = FallbackPolicy.FAIL_CLOSED
    delegation_policy: DelegationPolicy = DelegationPolicy.NEVER
    resource_limits: Optional[ExpertLimits] = None
    registry_generation: int = 0
    availability: ExpertAvailability = ExpertAvailability.READY
    unavailable_reason: str = ""

    def __post_init__(self) -> None:
        _bounded_text(self.protocol, field_name="protocol", limit=32)
        if _PROTOCOL.fullmatch(self.protocol) is None:
            raise ValueError(f"invalid expert protocol: {self.protocol!r}")
        _bounded_pattern(self.expert_id, field_name="expert_id", pattern=_EXPERT_ID, limit=128)
        _bounded_text(self.expert_version, field_name="expert_version", limit=64)
        _bounded_pattern(
            self.package_namespace,
            field_name="package_namespace",
            pattern=_NAMESPACE,
            limit=64,
        )
        _bounded_pattern(
            self.manifest_digest,
            field_name="manifest_digest",
            pattern=_DIGEST,
            limit=192,
        )
        _bounded_text(self.name, field_name="name", limit=128)
        _bounded_text(self.description, field_name="description", limit=2048)
        _bounded_pattern(
            self.source_reference,
            field_name="source_reference",
            pattern=_PORTABLE,
            limit=512,
        )
        if not isinstance(self.reasoning_kind, ReasoningKind):
            raise TypeError("reasoning_kind must be ReasoningKind")
        if not isinstance(self.operations, tuple) or not self.operations:
            raise ValueError("operations must be a non-empty tuple")
        if len(self.operations) > _MAX_OPERATIONS:
            raise ValueError(f"operations exceeds {_MAX_OPERATIONS} entries")
        operation_ids = [item.operation_id for item in self.operations]
        if len(set(operation_ids)) != len(operation_ids):
            raise ValueError("operations contains duplicate operation ids")
        for item in self.operations:
            if not isinstance(item, OperationSpec):
                raise TypeError("operations entries must be OperationSpec")
        object.__setattr__(
            self,
            "applicability_keywords",
            _bounded_unique_tokens(
                self.applicability_keywords,
                field_name="applicability_keywords",
                limit=_MAX_KEYWORDS,
                pattern=_KEYWORD,
                token_limit=64,
            ),
        )
        object.__setattr__(
            self,
            "required_capabilities",
            _bounded_unique_tokens(
                self.required_capabilities,
                field_name="required_capabilities",
                limit=_MAX_CAPABILITIES,
                pattern=_TOKEN,
                token_limit=64,
            ),
        )
        if not isinstance(self.possible_effects, tuple) or not self.possible_effects:
            raise ValueError("possible_effects must be a non-empty tuple")
        if len(self.possible_effects) > _MAX_EFFECTS:
            raise ValueError(f"possible_effects exceeds {_MAX_EFFECTS} entries")
        for item in self.possible_effects:
            if not isinstance(item, EffectClass):
                raise TypeError("possible_effects entries must be EffectClass")
        if len(set(self.possible_effects)) != len(self.possible_effects):
            raise ValueError("possible_effects contains duplicate entries")
        object.__setattr__(
            self,
            "supported_engines",
            _bounded_unique_tokens(
                self.supported_engines,
                field_name="supported_engines",
                limit=_MAX_ENGINES,
                pattern=_ENGINE,
                token_limit=32,
            ),
        )
        object.__setattr__(
            self,
            "supported_platforms",
            _bounded_unique_tokens(
                self.supported_platforms,
                field_name="supported_platforms",
                limit=_MAX_PLATFORMS,
                pattern=_ENGINE,
                token_limit=32,
            ),
        )
        if not isinstance(self.fallback_policy, FallbackPolicy):
            raise TypeError("fallback_policy must be FallbackPolicy")
        if not isinstance(self.delegation_policy, DelegationPolicy):
            raise TypeError("delegation_policy must be DelegationPolicy")
        if self.resource_limits is not None and not isinstance(
            self.resource_limits, ExpertLimits
        ):
            raise TypeError("resource_limits must be ExpertLimits")
        if type(self.registry_generation) is not int or not (
            0 <= self.registry_generation <= 2_147_483_647
        ):
            raise ValueError("registry_generation must be an integer between 0 and 2147483647")
        if not isinstance(self.availability, ExpertAvailability):
            raise TypeError("availability must be ExpertAvailability")
        if self.unavailable_reason:
            _bounded_pattern(
                self.unavailable_reason,
                field_name="unavailable_reason",
                pattern=_PORTABLE,
                limit=128,
            )
        if self.availability is ExpertAvailability.UNAVAILABLE and not self.unavailable_reason:
            raise ValueError("unavailable_reason is required when availability is unavailable")

    @property
    def protocol_compatible(self) -> bool:
        return self.protocol == ZARA_EXPERT_PROTOCOL

    @property
    def selectable(self) -> bool:
        return (
            self.protocol_compatible
            and self.availability.value in _SELECTABLE_AVAILABILITY
        )

    @property
    def alias(self) -> str:
        tail = self.expert_id.rsplit("/", 1)[-1]
        return tail.rsplit(":", 1)[-1]

    def to_wire(self) -> dict[str, Any]:
        wire: dict[str, Any] = {
            "protocol": self.protocol,
            "expert_id": self.expert_id,
            "expert_version": self.expert_version,
            "package_namespace": self.package_namespace,
            "manifest_digest": self.manifest_digest,
            "name": self.name,
            "description": self.description,
            "source_reference": self.source_reference,
            "reasoning_kind": self.reasoning_kind.value,
            "operations": [item.to_wire() for item in self.operations],
            "applicability": {"keywords": list(self.applicability_keywords)},
            "required_capabilities": list(self.required_capabilities),
            "possible_effects": [item.value for item in self.possible_effects],
            "supported_engines": list(self.supported_engines),
            "supported_platforms": list(self.supported_platforms),
            "fallback_policy": self.fallback_policy.value,
            "delegation_policy": self.delegation_policy.value,
            "registry_generation": self.registry_generation,
            "availability": self.availability.value,
        }
        if self.unavailable_reason:
            wire["unavailable_reason"] = self.unavailable_reason
        if self.resource_limits is not None:
            wire["resource_limits"] = self.resource_limits.to_wire()
        return wire

    def to_catalog_projection(self) -> dict[str, Any]:
        projection = {
            key: value
            for key, value in self.to_wire().items()
            if key != "registry_generation"
        }
        projection["applicability_keywords"] = projection.pop("applicability")["keywords"]
        projection["unavailable_reason"] = self.unavailable_reason
        return projection

    @classmethod
    def from_wire(cls, payload: Mapping[str, Any]) -> ExpertDescriptor:
        if not isinstance(payload, Mapping):
            raise ExpertInvalidInputError("descriptor payload must be a mapping")
        allowed = {
            "protocol",
            "expert_id",
            "expert_version",
            "package_namespace",
            "manifest_digest",
            "name",
            "description",
            "source_reference",
            "reasoning_kind",
            "operations",
            "applicability",
            "required_capabilities",
            "possible_effects",
            "supported_engines",
            "supported_platforms",
            "fallback_policy",
            "delegation_policy",
            "resource_limits",
            "registry_generation",
            "availability",
            "unavailable_reason",
        }
        unknown = set(payload) - allowed
        if unknown:
            raise ExpertInvalidInputError(f"unknown descriptor field: {sorted(unknown)[0]!r}")
        missing = allowed - set(payload) - {"resource_limits", "unavailable_reason", "registry_generation"}
        if missing:
            raise ExpertInvalidInputError(f"descriptor missing field: {sorted(missing)[0]!r}")
        protocol = payload["protocol"]
        if not isinstance(protocol, str):
            raise ExpertInvalidInputError("protocol must be a string")
        match = _PROTOCOL.fullmatch(protocol)
        if match is None:
            raise ExpertInvalidInputError(f"invalid expert protocol: {protocol!r}")
        if int(match.group(1)) != 1:
            raise ExpertIncompatibleProtocolError(
                f"expert {payload['expert_id']!r} uses incompatible protocol {protocol!r}"
            )
        applicability = payload["applicability"]
        if not isinstance(applicability, Mapping) or set(applicability) - {"keywords"}:
            raise ExpertInvalidInputError("applicability must be an object with keywords")
        if "keywords" not in applicability:
            raise ExpertInvalidInputError("applicability must declare keywords")
        raw_operations = payload["operations"]
        if not isinstance(raw_operations, (list, tuple)):
            raise ExpertInvalidInputError("operations must be a list")
        for list_field in (
            "required_capabilities",
            "possible_effects",
            "supported_engines",
            "supported_platforms",
        ):
            if not isinstance(payload[list_field], (list, tuple)):
                raise ExpertInvalidInputError(f"{list_field} must be a list")
        if not isinstance(applicability["keywords"], (list, tuple)):
            raise ExpertInvalidInputError("applicability keywords must be a list")
        resource_limits = payload.get("resource_limits")
        unavailable_reason = payload.get("unavailable_reason", "")
        try:
            return cls(
                protocol=protocol,
                expert_id=payload["expert_id"],
                expert_version=payload["expert_version"],
                package_namespace=payload["package_namespace"],
                manifest_digest=payload["manifest_digest"],
                name=payload["name"],
                description=payload["description"],
                source_reference=payload["source_reference"],
                reasoning_kind=_wire_enum(
                    ReasoningKind, payload["reasoning_kind"], field_name="reasoning_kind"
                ),
                operations=tuple(
                    OperationSpec.from_wire(item) for item in raw_operations
                ),
                applicability_keywords=tuple(applicability["keywords"]),
                required_capabilities=tuple(payload["required_capabilities"]),
                possible_effects=tuple(
                    _wire_enum(EffectClass, item, field_name="possible_effect")
                    for item in payload["possible_effects"]
                ),
                supported_engines=tuple(payload["supported_engines"]),
                supported_platforms=tuple(payload["supported_platforms"]),
                fallback_policy=_wire_enum(
                    FallbackPolicy, payload["fallback_policy"], field_name="fallback_policy"
                ),
                delegation_policy=_wire_enum(
                    DelegationPolicy,
                    payload["delegation_policy"],
                    field_name="delegation_policy",
                ),
                resource_limits=(
                    ExpertLimits.from_wire(resource_limits)
                    if resource_limits is not None
                    else None
                ),
                registry_generation=payload.get("registry_generation", 0),
                availability=_wire_enum(
                    ExpertAvailability, payload["availability"], field_name="availability"
                ),
                unavailable_reason=(
                    unavailable_reason if isinstance(unavailable_reason, str) else ""
                ),
            )
        except ExpertContractError:
            raise
        except (ValueError, TypeError) as error:
            raise ExpertInvalidInputError(str(error)) from error


@dataclass(frozen=True)
class ActivationHandle:
    activation_id: str
    principal: str
    workspace: str
    expert_id: str
    expert_version: str
    manifest_digest: str
    registry_generation: int
    runtime_generation: int

    def __post_init__(self) -> None:
        if not isinstance(self.activation_id, str) or _ACTIVATION_ID.fullmatch(
            self.activation_id
        ) is None:
            raise ValueError(f"invalid activation_id: {self.activation_id!r}")
        for field_name in ("principal", "workspace", "expert_id", "manifest_digest"):
            value = getattr(self, field_name)
            _bounded_pattern(
                value, field_name=field_name, pattern=_PORTABLE, limit=192
            )
        _bounded_text(self.expert_version, field_name="expert_version", limit=64)
        for field_name in ("registry_generation", "runtime_generation"):
            value = getattr(self, field_name)
            if type(value) is not int or value < 0:
                raise ValueError(f"{field_name} must be a non-negative integer")


@dataclass(frozen=True)
class ExpertRequest:
    request_id: Optional[str]
    operation: str
    activation_id: str
    expert_id: str
    expert_operation: str
    expected_registry_generation: Optional[int] = None
    expected_runtime_generation: Optional[int] = None
    input: dict[str, Any] = field(default_factory=dict)
    limits: Optional[ExpertLimits] = None
    idempotency_key: Optional[str] = None
    protocol: str = ZARA_EXPERT_PROTOCOL

    def __post_init__(self) -> None:
        if self.protocol != ZARA_EXPERT_PROTOCOL:
            raise ExpertIncompatibleProtocolError(
                f"request protocol {self.protocol!r} is not {ZARA_EXPERT_PROTOCOL}"
            )
        if self.operation not in EXPERT_OPERATIONS:
            raise ExpertInvalidInputError(f"unknown expert operation: {self.operation!r}")
        if self.request_id is not None:
            _bounded_pattern(
                self.request_id, field_name="request_id", pattern=_PORTABLE, limit=128
            )
        for field_name in ("activation_id", "expert_id", "expert_operation"):
            value = getattr(self, field_name)
            _bounded_pattern(
                value, field_name=field_name, pattern=_PORTABLE, limit=128
            )
        for field_name in (
            "expected_registry_generation",
            "expected_runtime_generation",
        ):
            value = getattr(self, field_name)
            if value is not None and (type(value) is not int or value < 0):
                raise ExpertInvalidInputError(f"{field_name} must be a non-negative integer")
        if self.limits is not None and not isinstance(self.limits, ExpertLimits):
            raise ExpertInvalidInputError("limits must be ExpertLimits")
        if self.idempotency_key is not None:
            _bounded_pattern(
                self.idempotency_key,
                field_name="idempotency_key",
                pattern=_PORTABLE,
                limit=128,
            )


@dataclass(frozen=True)
class ExpertResult:
    protocol: str
    request_id: str
    invocation_id: str
    activation_id: str
    expert_id: str
    expert_version: str
    manifest_digest: str
    expert_operation: str
    resolved_registry_generation: int
    resolved_runtime_generation: int
    verdict: ExpertVerdict
    data: dict[str, Any] = field(default_factory=dict)
    evidence_refs: tuple[str, ...] = ()
    usage: dict[str, Any] = field(default_factory=dict)
    effect_receipts: tuple[dict[str, Any], ...] = ()
    error_code: Optional[ExpertErrorCode] = None
    error_message: str = ""
    replayed: bool = False

    def __post_init__(self) -> None:
        _bounded_error_message(self.error_message)


@dataclass(frozen=True)
class ExpertRegistrySnapshot:
    generation: int
    runtime_generation: int
    expert_ids: tuple[str, ...]
    activation_ids: tuple[str, ...]
    invocation_ids: tuple[str, ...]


@dataclass
class _ActivationRecord:
    handle: ActivationHandle
    lifecycle: LifecycleState
    unavailable_reason: str = ""


@dataclass
class _InvocationRecord:
    invocation_id: str
    request_id: str
    expert_id: str
    expert_operation: str
    principal: str
    state: str
    verdict: Optional[ExpertVerdict]
    evidence_refs: tuple[str, ...]
    usage: dict[str, Any]
    effect_receipts: tuple[dict[str, Any], ...]
    decision_refs: tuple[str, ...]
    result: Optional[ExpertResult] = None


def _validate_input_value(value: Any, *, depth: int = 0) -> None:
    if depth > _MAX_INPUT_DEPTH:
        raise ExpertInvalidInputError("input payload exceeds bounded depth")
    if isinstance(value, bool):
        return
    if isinstance(value, int):
        return
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ExpertInvalidInputError("input payload contains non-finite number")
        return
    if isinstance(value, str):
        if len(value) > _MAX_INPUT_STRING:
            raise ExpertInvalidInputError("input payload contains oversized string value")
        return
    if isinstance(value, list):
        if len(value) > _MAX_INPUT_LIST:
            raise ExpertInvalidInputError("input payload contains oversized list value")
        for item in value:
            _validate_input_value(item, depth=depth + 1)
        return
    if isinstance(value, dict):
        if len(value) > _MAX_INPUT_KEYS:
            raise ExpertInvalidInputError("input payload contains oversized object value")
        for key, item in value.items():
            if not isinstance(key, str) or len(key) > 64:
                raise ExpertInvalidInputError("input payload contains invalid object key")
            _validate_input_value(item, depth=depth + 1)
        return
    raise ExpertInvalidInputError("input payload contains unsupported value type")


def _check_field_value(spec: FieldSpec, value: Any) -> None:
    field_type = spec.type
    if field_type is FieldType.BOOLEAN:
        if type(value) is not bool:
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be a boolean")
    elif field_type is FieldType.INTEGER:
        if type(value) is not int:
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be an integer")
    elif field_type is FieldType.NUMBER:
        if type(value) not in (int, float):
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be a number")
        if type(value) is float and not math.isfinite(value):
            raise ExpertInvalidInputError(
                f"input field {spec.name!r} must be a finite number"
            )
    elif field_type in (
        FieldType.STRING,
        FieldType.REFERENCE,
        FieldType.SECRET_REFERENCE,
    ):
        if not isinstance(value, str):
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be a string")
    elif field_type is FieldType.LIST:
        if not isinstance(value, list):
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be a list")
    elif field_type is FieldType.OBJECT:
        if not isinstance(value, dict):
            raise ExpertInvalidInputError(f"input field {spec.name!r} must be an object")
    elif field_type is FieldType.ENUM:
        if not isinstance(value, str) or value not in spec.enum_values:
            raise ExpertInvalidInputError(
                f"input field {spec.name!r} must be one of {list(spec.enum_values)}"
            )
    else:  # pragma: no cover - closed enum
        raise ExpertInvalidInputError(f"unsupported field type for {spec.name!r}")


class ExpertRegistry:
    """Trusted single-process registry for ZARA-EXPERT/1 expert contracts."""

    def __init__(self, *, engines: Iterable[str] = ("swipl",)) -> None:
        self._lock = threading.Lock()
        self._engines = tuple(engines)
        self._registry_generation = 0
        self._runtime_generation = 0
        self._descriptors: dict[str, ExpertDescriptor] = {}
        self._handlers: dict[str, Optional[Callable[..., Any]]] = {}
        self._aliases: dict[str, str] = {}
        self._activations: dict[str, _ActivationRecord] = {}
        self._invocations: dict[str, _InvocationRecord] = {}
        self._idempotency: dict[tuple[str, str, str, str], tuple[str, str]] = {}

    @property
    def generation(self) -> int:
        with self._lock:
            return self._registry_generation

    @property
    def runtime_generation(self) -> int:
        with self._lock:
            return self._runtime_generation

    def register(
        self,
        descriptor: ExpertDescriptor,
        handler: Optional[Callable[..., Any]] = None,
    ) -> dict[str, Any]:
        if not isinstance(descriptor, ExpertDescriptor):
            raise ExpertInvalidInputError("registration requires an ExpertDescriptor")
        self._validate_handler(descriptor, handler)
        with self._lock:
            self._ensure_registration_unlocked(descriptor, handler)
            self._bump_generations_unlocked()
            logger.info("[ExpertRegistry] registered expert %s", descriptor.expert_id)
            return {
                "expert_id": descriptor.expert_id,
                "registry_generation": self._registry_generation,
            }

    def reload(
        self,
        staged: Iterable[tuple[ExpertDescriptor, Optional[Callable[..., Any]]]],
    ) -> ExpertRegistrySnapshot:
        replacement_descriptors: dict[str, ExpertDescriptor] = {}
        replacement_handlers: dict[str, Optional[Callable[..., Any]]] = {}
        replacement_aliases: dict[str, str] = {}
        for descriptor, handler in staged:
            if not isinstance(descriptor, ExpertDescriptor):
                raise ExpertInvalidInputError(
                    "staged registration must yield ExpertDescriptor values"
                )
            self._validate_handler(descriptor, handler)
            if descriptor.expert_id in replacement_descriptors:
                raise ExpertInvalidInputError(
                    f"duplicate expert id: {descriptor.expert_id}"
                )
            existing = replacement_aliases.get(descriptor.alias)
            if existing is not None and existing != descriptor.expert_id:
                raise ExpertInvalidInputError(
                    f"expert alias collision: {descriptor.alias!r} already bound to {existing!r}"
                )
            replacement_descriptors[descriptor.expert_id] = descriptor
            replacement_handlers[descriptor.expert_id] = handler
            replacement_aliases[descriptor.alias] = descriptor.expert_id

        with self._lock:
            if (
                replacement_descriptors == self._descriptors
                and replacement_handlers == self._handlers
                and replacement_aliases == self._aliases
            ):
                return self._snapshot_unlocked()
            self._descriptors = replacement_descriptors
            self._handlers = replacement_handlers
            self._aliases = replacement_aliases
            self._bump_generations_unlocked()
            logger.info(
                "[ExpertRegistry] reloaded %d experts at generation %d",
                len(replacement_descriptors),
                self._registry_generation,
            )
            return self._snapshot_unlocked()

    def list_experts(
        self,
        principal: str,
        *,
        offset: int = 0,
        limit: int = 32,
    ) -> dict[str, Any]:
        _bounded_pattern(principal, field_name="principal", pattern=_PORTABLE, limit=128)
        if type(offset) is not int or offset < 0:
            raise ExpertInvalidInputError("offset must be a non-negative integer")
        if type(limit) is not int or not 1 <= limit <= 128:
            raise ExpertInvalidInputError("limit must be between 1 and 128")
        with self._lock:
            visible = [
                self._descriptors[key]
                for key in sorted(self._descriptors)
                if self._descriptors[key].availability is not ExpertAvailability.ABSENT
            ]
            projections = [item.to_catalog_projection() for item in visible]
            return {
                "experts": projections[offset : offset + limit],
                "total": len(projections),
                "offset": offset,
                "limit": limit,
                "registry_generation": self._registry_generation,
            }

    def describe(self, expert_id: str) -> dict[str, Any]:
        _bounded_pattern(expert_id, field_name="expert_id", pattern=_PORTABLE, limit=128)
        with self._lock:
            descriptor = self._descriptors.get(expert_id)
            if descriptor is None:
                raise ExpertUnavailableError(f"expert {expert_id!r} is not registered")
            return descriptor.to_wire()

    def match(self, goal_text: str) -> Optional[dict[str, Any]]:
        if not isinstance(goal_text, str):
            raise ExpertInvalidInputError("goal_text must be a string")
        if len(goal_text) > 512 or goal_text != goal_text.strip():
            raise ExpertInvalidInputError("goal_text must be bounded and trimmed")
        if any(ord(char) < 0x20 or ord(char) == 0x7F for char in goal_text):
            raise ExpertInvalidInputError("goal_text contains control characters")
        tokens = [token for token in re.split(r"[^a-z0-9]+", goal_text.lower()) if token]
        with self._lock:
            scored: list[tuple[int, ExpertDescriptor, tuple[str, ...]]] = []
            for key in sorted(self._descriptors):
                descriptor = self._descriptors[key]
                if not descriptor.selectable:
                    continue
                matched = tuple(
                    keyword
                    for keyword in descriptor.applicability_keywords
                    if any(
                        token == keyword or token.startswith(keyword)
                        for token in tokens
                    )
                )
                if matched:
                    scored.append((len(matched), descriptor, matched))
            if not scored:
                return None
            best_score = max(item[0] for item in scored)
            best = [item for item in scored if item[0] == best_score]
            if len(best) > 1:
                raise ExpertAmbiguityError(
                    "expert match is ambiguous: "
                    + ", ".join(sorted(item[1].expert_id for item in best)),
                    candidates=sorted(item[1].expert_id for item in best),
                )
            _, descriptor, matched = best[0]
            return {
                "expert_id": descriptor.expert_id,
                "score": best_score,
                "matched_keywords": sorted(matched),
            }

    def activate(
        self,
        principal: str,
        workspace: str,
        expert_id: str,
        *,
        expected_registry_generation: Optional[int] = None,
        expected_runtime_generation: Optional[int] = None,
    ) -> tuple[ActivationHandle, dict[str, Any]]:
        for field_name, value in (("principal", principal), ("workspace", workspace)):
            _bounded_pattern(
                value, field_name=field_name, pattern=_PORTABLE, limit=128
            )
        _bounded_pattern(expert_id, field_name="expert_id", pattern=_PORTABLE, limit=128)
        with self._lock:
            descriptor = self._descriptors.get(expert_id)
            if descriptor is None:
                raise ExpertUnavailableError(f"expert {expert_id!r} is not registered")
            if not descriptor.protocol_compatible:
                raise ExpertIncompatibleProtocolError(
                    f"expert {expert_id!r} uses incompatible protocol {descriptor.protocol!r}"
                )
            if descriptor.availability is ExpertAvailability.ABSENT:
                raise ExpertUnavailableError(
                    f"expert {expert_id!r} is absent"
                    + (
                        f": {descriptor.unavailable_reason}"
                        if descriptor.unavailable_reason
                        else ""
                    )
                )
            if descriptor.availability is ExpertAvailability.UNAVAILABLE:
                raise ExpertUnavailableError(
                    f"expert {expert_id!r} is unavailable: {descriptor.unavailable_reason}"
                )
            if descriptor.supported_engines and not (
                set(descriptor.supported_engines) & set(self._engines)
            ):
                raise ExpertUnsupportedBackendError(
                    f"expert {expert_id!r} supports engines "
                    f"{list(descriptor.supported_engines)} but the host provides "
                    f"{list(self._engines)}"
                )
            if expected_registry_generation is not None and (
                expected_registry_generation != self._registry_generation
            ):
                raise ExpertStaleGenerationError(
                    "expected registry generation "
                    f"{expected_registry_generation} != {self._registry_generation}"
                )
            if expected_runtime_generation is not None and (
                expected_runtime_generation != self._runtime_generation
            ):
                raise ExpertStaleGenerationError(
                    "expected runtime generation "
                    f"{expected_runtime_generation} != {self._runtime_generation}"
                )
            handle = ActivationHandle(
                activation_id=f"act:{uuid.uuid4().hex}",
                principal=principal,
                workspace=workspace,
                expert_id=descriptor.expert_id,
                expert_version=descriptor.expert_version,
                manifest_digest=descriptor.manifest_digest,
                registry_generation=self._registry_generation,
                runtime_generation=self._runtime_generation,
            )
            record = _ActivationRecord(handle=handle, lifecycle=LifecycleState.INACTIVE)
            self._transition_unlocked(record, LifecycleState.ACTIVATING)
            self._transition_unlocked(record, LifecycleState.ACTIVE)
            self._activations[handle.activation_id] = record
            logger.info(
                "[ExpertRegistry] activated expert %s for %s",
                descriptor.expert_id,
                principal,
            )
            receipt = {
                "activation_id": handle.activation_id,
                "expert_id": descriptor.expert_id,
                "state": record.lifecycle.value,
                "registry_generation": self._registry_generation,
                "runtime_generation": self._runtime_generation,
            }
            return handle, receipt

    def invoke(
        self,
        handle: ActivationHandle,
        expert_operation: str,
        input: Optional[Mapping[str, Any]] = None,
        *,
        limits: Optional[ExpertLimits] = None,
        idempotency_key: Optional[str] = None,
        request_id: Optional[str] = None,
    ) -> ExpertResult:
        if not isinstance(handle, ActivationHandle):
            raise ExpertInvalidInputError("invoke requires an ActivationHandle")
        with self._lock:
            return self._invoke_unlocked(
                handle,
                expert_operation,
                dict(input or {}),
                limits,
                idempotency_key,
                request_id,
            )

    def invoke_request(self, request: ExpertRequest) -> ExpertResult:
        if not isinstance(request, ExpertRequest):
            raise ExpertInvalidInputError("invoke_request requires an ExpertRequest")
        with self._lock:
            record = self._activations.get(request.activation_id)
            if record is None:
                raise ExpertDeniedError(
                    "activation handle denied: unknown or released activation"
                )
            if request.expected_registry_generation is not None and (
                request.expected_registry_generation != self._registry_generation
            ):
                raise ExpertStaleGenerationError(
                    "expected registry generation "
                    f"{request.expected_registry_generation} != {self._registry_generation}"
                )
            if request.expected_runtime_generation is not None and (
                request.expected_runtime_generation != self._runtime_generation
            ):
                raise ExpertStaleGenerationError(
                    "expected runtime generation "
                    f"{request.expected_runtime_generation} != {self._runtime_generation}"
                )
            return self._invoke_unlocked(
                record.handle,
                request.expert_operation,
                dict(request.input),
                request.limits,
                request.idempotency_key,
                request.request_id,
            )

    def cancel(self, invocation_id: str) -> dict[str, Any]:
        _bounded_pattern(
            invocation_id, field_name="invocation_id", pattern=_PORTABLE, limit=128
        )
        with self._lock:
            record = self._invocations.get(invocation_id)
            if record is None:
                raise ExpertInvalidInputError(
                    f"unknown invocation: {invocation_id!r}"
                )
            if record.state == "completed":
                logger.info(
                    "[ExpertRegistry] cancel after commit for %s (no reversal)",
                    invocation_id,
                )
                return {
                    "invocation_id": record.invocation_id,
                    "request_id": record.request_id,
                    "cancelled": False,
                    "committed": True,
                    "verdict": record.verdict.value if record.verdict else None,
                    "effect_receipts": [dict(item) for item in record.effect_receipts],
                }
            record.state = "cancelled"
            logger.info("[ExpertRegistry] cancelled invocation %s", invocation_id)
            return {
                "invocation_id": record.invocation_id,
                "request_id": record.request_id,
                "cancelled": True,
                "committed": False,
            }

    def explain(self, invocation_id: str) -> dict[str, Any]:
        _bounded_pattern(
            invocation_id, field_name="invocation_id", pattern=_PORTABLE, limit=128
        )
        with self._lock:
            record = self._invocations.get(invocation_id)
            if record is None:
                raise ExpertInvalidInputError(
                    f"unknown invocation: {invocation_id!r}"
                )
            return {
                "invocation_id": record.invocation_id,
                "request_id": record.request_id,
                "expert_id": record.expert_id,
                "expert_operation": record.expert_operation,
                "verdict": record.verdict.value if record.verdict else None,
                "decision_refs": list(record.decision_refs[:_MAX_DECISION_REFS]),
                "evidence_refs": list(record.evidence_refs[:_MAX_EVIDENCE_REFS]),
                "usage": dict(record.usage),
            }

    def deactivate(self, handle: ActivationHandle) -> dict[str, Any]:
        if not isinstance(handle, ActivationHandle):
            raise ExpertInvalidInputError("deactivate requires an ActivationHandle")
        with self._lock:
            record = self._activations.get(handle.activation_id)
            if record is None:
                raise ExpertDeniedError(
                    "activation handle denied: unknown or released activation"
                )
            if record.handle.principal != handle.principal:
                raise ExpertDeniedError(
                    "activation handle denied: principal mismatch"
                )
            self._transition_unlocked(record, LifecycleState.DRAINING)
            self._transition_unlocked(record, LifecycleState.INACTIVE)
            del self._activations[handle.activation_id]
            logger.info(
                "[ExpertRegistry] deactivated expert %s for %s",
                handle.expert_id,
                handle.principal,
            )
            return {
                "activation_id": handle.activation_id,
                "expert_id": handle.expert_id,
                "deactivated": True,
                "state": LifecycleState.INACTIVE.value,
            }

    def mark_backend_unavailable(self, expert_id: str, reason: str) -> None:
        _bounded_pattern(expert_id, field_name="expert_id", pattern=_PORTABLE, limit=128)
        _bounded_pattern(reason, field_name="reason", pattern=_PORTABLE, limit=128)
        with self._lock:
            descriptor = self._descriptors.get(expert_id)
            if descriptor is None:
                raise ExpertUnavailableError(f"expert {expert_id!r} is not registered")
            self._descriptors[expert_id] = replace(
                descriptor,
                availability=ExpertAvailability.UNAVAILABLE,
                unavailable_reason=reason,
            )
            for record in self._activations.values():
                if (
                    record.handle.expert_id == expert_id
                    and record.lifecycle is LifecycleState.ACTIVE
                ):
                    self._transition_unlocked(record, LifecycleState.UNAVAILABLE)
                    record.unavailable_reason = reason
            logger.warning(
                "[ExpertRegistry] expert %s backend unavailable: %s", expert_id, reason
            )

    def recover_backend(self, expert_id: str) -> None:
        _bounded_pattern(expert_id, field_name="expert_id", pattern=_PORTABLE, limit=128)
        with self._lock:
            descriptor = self._descriptors.get(expert_id)
            if descriptor is None:
                raise ExpertUnavailableError(f"expert {expert_id!r} is not registered")
            if descriptor.availability is not ExpertAvailability.UNAVAILABLE:
                raise ExpertInvalidInputError(
                    f"expert {expert_id!r} is not marked unavailable"
                )
            self._descriptors[expert_id] = replace(
                descriptor,
                availability=ExpertAvailability.READY,
                unavailable_reason="",
            )
            for record in self._activations.values():
                if (
                    record.handle.expert_id == expert_id
                    and record.lifecycle is LifecycleState.UNAVAILABLE
                ):
                    self._transition_unlocked(record, LifecycleState.ACTIVE)
                    record.unavailable_reason = ""
            logger.info("[ExpertRegistry] expert %s backend recovered", expert_id)

    def snapshot(self) -> ExpertRegistrySnapshot:
        with self._lock:
            return self._snapshot_unlocked()

    def _snapshot_unlocked(self) -> ExpertRegistrySnapshot:
        return ExpertRegistrySnapshot(
            generation=self._registry_generation,
            runtime_generation=self._runtime_generation,
            expert_ids=tuple(sorted(self._descriptors)),
            activation_ids=tuple(sorted(self._activations)),
            invocation_ids=tuple(sorted(self._invocations)),
        )

    def _bump_generations_unlocked(self) -> None:
        self._registry_generation += 1
        self._runtime_generation += 1

    def _ensure_registration_unlocked(
        self,
        descriptor: ExpertDescriptor,
        handler: Optional[Callable[..., Any]],
    ) -> None:
        if descriptor.expert_id in self._descriptors:
            raise ExpertInvalidInputError(
                f"duplicate expert id: {descriptor.expert_id}"
            )
        existing = self._aliases.get(descriptor.alias)
        if existing is not None and existing != descriptor.expert_id:
            raise ExpertInvalidInputError(
                f"expert alias collision: {descriptor.alias!r} already bound to {existing!r}"
            )
        self._descriptors[descriptor.expert_id] = descriptor
        self._handlers[descriptor.expert_id] = handler
        self._aliases[descriptor.alias] = descriptor.expert_id

    def _validate_handler(
        self,
        descriptor: ExpertDescriptor,
        handler: Optional[Callable[..., Any]],
    ) -> None:
        if handler is None:
            return
        if not callable(handler):
            raise ExpertInvalidInputError(
                "expert handler must be callable"
            )
        try:
            signature = inspect.signature(handler)
        except (TypeError, ValueError) as error:
            raise ExpertInvalidInputError(
                f"expert handler signature is not inspectable: {error}"
            ) from error
        for operation in descriptor.operations:
            try:
                signature.bind(
                    **{spec.name: None for spec in operation.input_fields}
                )
            except TypeError as error:
                raise ExpertInvalidInputError(
                    f"handler for expert {descriptor.expert_id!r} does not accept "
                    f"the declared input fields of {operation.operation_id!r}: {error}"
                ) from error

    def _transition_unlocked(
        self, record: _ActivationRecord, target: LifecycleState
    ) -> None:
        record.lifecycle = activation_transition(record.lifecycle, target)

    def _resolve_activation_unlocked(
        self, handle: ActivationHandle
    ) -> tuple[_ActivationRecord, ExpertDescriptor]:
        record = self._resolve_record_unlocked(handle)
        descriptor = self._resolve_active_state_unlocked(record)
        return record, descriptor

    def _resolve_record_unlocked(
        self, handle: ActivationHandle
    ) -> _ActivationRecord:
        record = self._activations.get(handle.activation_id)
        if record is None:
            raise ExpertDeniedError(
                "activation handle denied: unknown or released activation"
            )
        bound = record.handle
        if (
            bound.principal != handle.principal
            or bound.workspace != handle.workspace
            or bound.expert_id != handle.expert_id
            or bound.expert_version != handle.expert_version
            or bound.manifest_digest != handle.manifest_digest
        ):
            raise ExpertDeniedError("activation handle denied: bound identity mismatch")
        return record

    def _resolve_active_state_unlocked(
        self, record: _ActivationRecord
    ) -> ExpertDescriptor:
        descriptor = self._descriptors.get(record.handle.expert_id)
        if descriptor is None:
            raise ExpertDeniedError(
                "activation handle denied: expert is no longer registered"
            )
        if record.lifecycle is LifecycleState.UNAVAILABLE:
            raise ExpertUnavailableError(
                "expert backend unavailable"
                + (
                    f": {record.unavailable_reason}"
                    if record.unavailable_reason
                    else ""
                )
            )
        if record.lifecycle is not LifecycleState.ACTIVE:
            raise ExpertDeniedError(
                f"activation handle denied: lifecycle state is {record.lifecycle.value}"
            )
        return descriptor

    def _invoke_unlocked(
        self,
        handle: ActivationHandle,
        expert_operation: str,
        input: dict[str, Any],
        limits: Optional[ExpertLimits],
        idempotency_key: Optional[str],
        request_id: Optional[str],
    ) -> ExpertResult:
        if not isinstance(expert_operation, str) or not expert_operation:
            raise ExpertInvalidInputError("expert_operation must be a non-empty string")
        if idempotency_key is not None:
            _bounded_pattern(
                idempotency_key,
                field_name="idempotency_key",
                pattern=_PORTABLE,
                limit=128,
            )
        record = self._resolve_record_unlocked(handle)
        decision_refs = ["handle:validated"]

        if (
            handle.registry_generation != self._registry_generation
            or handle.runtime_generation != self._runtime_generation
        ):
            raise ExpertStaleGenerationError(
                f"activation handle generation "
                f"({handle.registry_generation}/{handle.runtime_generation}) is stale "
                f"against registry ({self._registry_generation}/{self._runtime_generation})"
            )
        decision_refs.append("generation:fenced")
        descriptor = self._resolve_active_state_unlocked(record)
        resolved_input = dict(input)
        try:
            _validate_input_value(resolved_input)
            serialized = json.dumps(
                resolved_input,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            )
        except (TypeError, ValueError) as error:
            raise ExpertInvalidInputError(
                f"input payload is not bounded JSON data: {error}"
            ) from error
        if len(serialized) > _MAX_INPUT_BYTES:
            raise ExpertInvalidInputError(
                "input payload exceeds bounded size"
            )

        idempotency_key_tuple = None
        if idempotency_key is not None:
            idempotency_key_tuple = (
                handle.principal,
                descriptor.expert_id,
                expert_operation,
                idempotency_key,
            )
            prior = self._idempotency.get(idempotency_key_tuple)
            if prior is not None:
                prior_digest, prior_invocation_id = prior
                if prior_digest != _input_digest(serialized):
                    raise ExpertInvalidInputError(
                        "idempotency conflict: idempotency key "
                        f"{idempotency_key!r} was reused with changed input"
                    )
                prior_record = self._invocations.get(prior_invocation_id)
                if prior_record is not None and prior_record.result is not None:
                    logger.info(
                        "[ExpertRegistry] replaying durable result for key %r",
                        idempotency_key,
                    )
                    return replace(prior_record.result, replayed=True)
        decision_refs.append("idempotency:resolved")

        if limits is None:
            limits = ExpertLimits()
        elif not isinstance(limits, ExpertLimits):
            raise ExpertInvalidInputError(
                "limits must be an ExpertLimits instance"
            )
        decision_refs.append("limits:admitted")

        operation = next(
            (item for item in descriptor.operations if item.operation_id == expert_operation),
            None,
        )
        if operation is None:
            raise ExpertUnsupportedOperationError(
                f"expert {descriptor.expert_id!r} does not declare operation "
                f"{expert_operation!r}"
            )
        handler = self._handlers.get(descriptor.expert_id)
        if handler is None:
            raise ExpertUnsupportedOperationError(
                f"expert {descriptor.expert_id!r} has no registered handler"
            )
        self._validate_input_against_operation(operation, resolved_input)
        decision_refs.append("input:schema-validated")

        needs_model_budget = (
            EffectClass.MODEL_INFERENCE in descriptor.possible_effects
            or descriptor.reasoning_kind is ReasoningKind.MODEL
        )
        if needs_model_budget and limits.max_model_calls == 0:
            raise ExpertBudgetExceededError(
                f"expert {descriptor.expert_id!r} requires model inference but the "
                "admitted budget is max_model_calls=0"
            )
        decision_refs.append("effects:admitted")

        invocation_id = f"inv:{uuid.uuid4().hex}"
        resolved_request_id = request_id or f"req:{uuid.uuid4().hex}"
        invocation = _InvocationRecord(
            invocation_id=invocation_id,
            request_id=resolved_request_id,
            expert_id=descriptor.expert_id,
            expert_operation=expert_operation,
            principal=handle.principal,
            state="dispatching",
            verdict=None,
            evidence_refs=(),
            usage={},
            effect_receipts=(),
            decision_refs=tuple(decision_refs),
        )
        self._invocations[invocation_id] = invocation

        outcome: Any = None
        handler_error: Optional[BaseException] = None
        try:
            outcome = handler(**resolved_input)
        except Exception as error:  # noqa: BLE001
            handler_error = error
            logger.warning(
                "[ExpertRegistry] expert handler failed for %s/%s: %r",
                descriptor.expert_id,
                expert_operation,
                error,
            )

        verdict = ExpertVerdict.UNKNOWN
        error_code: Optional[ExpertErrorCode] = None
        error_message = ""
        data: dict[str, Any] = {}
        evidence_refs: tuple[str, ...] = ()
        usage: dict[str, Any] = {"model_calls": 0}
        effect_receipts: tuple[dict[str, Any], ...] = ()
        if handler_error is None and isinstance(outcome, Mapping):
            raw_verdict = outcome.get("verdict")
            if isinstance(raw_verdict, str):
                try:
                    verdict = ExpertVerdict(raw_verdict)
                except ValueError:
                    verdict = ExpertVerdict.UNKNOWN
                    error_code = ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
                    error_message = "handler returned an unknown verdict"
            else:
                error_code = ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
                error_message = "handler returned no verdict"
            if verdict is not ExpertVerdict.UNKNOWN:
                data = _bounded_mapping(outcome.get("data", {}), "data")
                evidence_refs = _bounded_refs(outcome.get("evidence_refs", ()))
                usage = _bounded_mapping(outcome.get("usage", {"model_calls": 0}), "usage")
                effect_receipts = _bounded_receipts(
                    outcome.get("effect_receipts", ())
                )
        elif handler_error is not None:
            error_code = ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
            error_message = f"handler raised: {handler_error!r}"[:_MAX_ERROR_MESSAGE]
        else:
            error_code = ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
            error_message = "handler returned an unparseable outcome"

        if verdict is ExpertVerdict.SUCCEEDED:
            try:
                output_contract = replace(
                    operation,
                    input_fields=operation.output_fields,
                )
                self._validate_input_against_operation(output_contract, data)
            except ExpertInvalidInputError as error:
                verdict = ExpertVerdict.UNKNOWN
                data = {}
                evidence_refs = ()
                error_code = ExpertErrorCode.INVALID_INPUT
                error_message = f"output schema violation: {error}"

        error_message = _bounded_error_message(error_message)
        result = ExpertResult(
            protocol=ZARA_EXPERT_PROTOCOL,
            request_id=resolved_request_id,
            invocation_id=invocation_id,
            activation_id=handle.activation_id,
            expert_id=descriptor.expert_id,
            expert_version=descriptor.expert_version,
            manifest_digest=descriptor.manifest_digest,
            expert_operation=expert_operation,
            resolved_registry_generation=self._registry_generation,
            resolved_runtime_generation=self._runtime_generation,
            verdict=verdict,
            data=data,
            evidence_refs=evidence_refs,
            usage=usage,
            effect_receipts=effect_receipts,
            error_code=error_code,
            error_message=error_message,
        )
        invocation.verdict = verdict
        invocation.evidence_refs = evidence_refs
        invocation.usage = usage
        invocation.effect_receipts = effect_receipts
        invocation.decision_refs = tuple([*decision_refs, "dispatch:completed"])
        invocation.result = result
        invocation.state = "completed"
        if idempotency_key_tuple is not None:
            self._idempotency[idempotency_key_tuple] = (
                _input_digest(serialized),
                invocation_id,
            )
        return result

    def _validate_input_against_operation(
        self,
        operation: OperationSpec,
        input: Mapping[str, Any],
    ) -> None:
        declared = {spec.name: spec for spec in operation.input_fields}
        for key in input:
            if key not in declared:
                raise ExpertInvalidInputError(
                    f"unknown input field {key!r} for operation "
                    f"{operation.operation_id!r}"
                )
        for spec in operation.input_fields:
            if spec.required and spec.name not in input:
                raise ExpertInvalidInputError(
                    f"missing required input field {spec.name!r} for operation "
                    f"{operation.operation_id!r}"
                )
            if spec.name in input:
                _check_field_value(spec, input[spec.name])


def _input_digest(serialized: str) -> str:
    return hashlib.sha256(serialized.encode("utf-8")).hexdigest()


def _result_output_size_bytes(result: ExpertResult) -> int:
    payload = {
        "verdict": result.verdict.value,
        "data": result.data,
        "evidence_refs": list(result.evidence_refs),
        "usage": result.usage,
        "effect_receipts": list(result.effect_receipts),
        "error_code": result.error_code.value if result.error_code else None,
        "error_message": result.error_message,
    }
    serialized = json.dumps(
        payload,
        sort_keys=True,
        separators=(",", ":"),
        default=repr,
    )
    return len(serialized.encode("utf-8"))


def _bounded_mapping(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ExpertInvalidInputError(f"handler {label} must be a mapping")
    if len(value) > 16:
        raise ExpertInvalidInputError(f"handler {label} exceeds 16 entries")
    return dict(value)


def _bounded_refs(value: Any) -> tuple[str, ...]:
    if not isinstance(value, (list, tuple)):
        raise ExpertInvalidInputError("handler evidence_refs must be a list")
    refs = tuple(value)
    if len(refs) > _MAX_EVIDENCE_REFS:
        raise ExpertInvalidInputError("handler evidence_refs exceeds 32 entries")
    for ref in refs:
        if not isinstance(ref, str) or not ref or len(ref) > _MAX_REF_LENGTH:
            raise ExpertInvalidInputError("handler evidence_refs entries must be bounded")
    return refs


def _bounded_receipts(value: Any) -> tuple[dict[str, Any], ...]:
    if not isinstance(value, (list, tuple)):
        raise ExpertInvalidInputError("handler effect_receipts must be a list")
    receipts = tuple(value)
    if len(receipts) > _MAX_RECEIPTS:
        raise ExpertInvalidInputError("handler effect_receipts exceeds 32 entries")
    for receipt in receipts:
        if not isinstance(receipt, Mapping):
            raise ExpertInvalidInputError("handler effect_receipts must be mappings")
        if len(json.dumps(dict(receipt), default=repr)) > 2048:
            raise ExpertInvalidInputError("handler effect_receipt entry exceeds bounds")
    return tuple(dict(item) for item in receipts)
