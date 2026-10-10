"""Runtime-neutral WRAITH/1 contracts.

Wraith models agent identity, tasks, messages, budgets, and runtime bindings.
It deliberately does not execute agents or grant capabilities. Prolog policy and
Zara's canonical authorization/runtime layers remain the authorities that admit
spawn, delegation, tools, and side effects.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, replace
from enum import Enum
from typing import Iterable, Mapping

WRAITH_PROTOCOL = "WRAITH/1"
_TOKEN = re.compile(r"^[a-z][a-z0-9._-]{0,63}$")
_OPAQUE_REF = re.compile(r"^[a-z][a-z0-9._-]{0,31}:[a-z0-9][a-z0-9._-]{0,127}$")


class TaskState(str, Enum):
    CREATED = "created"
    READY = "ready"
    RUNNING = "running"
    PAUSED = "paused"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class MessageType(str, Enum):
    TASK = "task"
    RESULT = "result"
    OBSERVATION = "observation"
    REQUEST = "request"
    VOTE = "vote"
    CRITIQUE = "critique"
    STATUS = "status"
    FAILURE = "failure"
    CANCEL = "cancel"
    BUDGET_WARNING = "budget_warning"


TERMINAL_TASK_STATES = frozenset(
    {TaskState.COMPLETED, TaskState.FAILED, TaskState.CANCELLED}
)

_TASK_TRANSITIONS = {
    TaskState.CREATED: frozenset({TaskState.READY, TaskState.FAILED, TaskState.CANCELLED}),
    TaskState.READY: frozenset({TaskState.RUNNING, TaskState.FAILED, TaskState.CANCELLED}),
    TaskState.RUNNING: frozenset(
        {TaskState.PAUSED, TaskState.COMPLETED, TaskState.FAILED, TaskState.CANCELLED}
    ),
    TaskState.PAUSED: frozenset({TaskState.RUNNING, TaskState.FAILED, TaskState.CANCELLED}),
    TaskState.COMPLETED: frozenset(),
    TaskState.FAILED: frozenset(),
    TaskState.CANCELLED: frozenset(),
}


def _token(value: str, *, field: str) -> str:
    if not isinstance(value, str) or _TOKEN.fullmatch(value) is None:
        raise ValueError(f"invalid {field}: {value!r}")
    return value


def _optional_token(value: str | None, *, field: str) -> str | None:
    if value is None:
        return None
    return _token(value, field=field)


def _opaque_ref(value: str, *, field: str, prefix: str | None = None) -> str:
    if not isinstance(value, str) or _OPAQUE_REF.fullmatch(value) is None:
        raise ValueError(f"invalid {field}: {value!r}")
    if prefix is not None and not value.startswith(prefix):
        raise ValueError(f"invalid {field}: expected {prefix!r} reference")
    return value


def _tokens(values: Iterable[str], *, field: str, limit: int) -> tuple[str, ...]:
    items = tuple(values)
    if len(items) > limit:
        raise ValueError(f"{field} exceeds {limit} entries")
    if len(set(items)) != len(items):
        raise ValueError(f"{field} contains duplicate entries")
    return tuple(_token(item, field=field) for item in items)


def _wire_mapping(value: object, *, field: str) -> Mapping[str, object]:
    if not isinstance(value, Mapping):
        raise TypeError(f"{field} must be a mapping")
    return value


@dataclass(frozen=True, order=True)
class Budget:
    kind: str
    limit: int

    def __post_init__(self) -> None:
        _token(self.kind, field="budget kind")
        if not isinstance(self.limit, int) or isinstance(self.limit, bool) or self.limit < 0:
            raise ValueError("budget limit must be a non-negative integer")

    def to_wire(self) -> dict[str, object]:
        return {"kind": self.kind, "limit": self.limit}

    @classmethod
    def from_wire(cls, value: object) -> "Budget":
        wire = _wire_mapping(value, field="budget")
        return cls(kind=str(wire["kind"]), limit=wire["limit"])


@dataclass(frozen=True)
class RuntimeBinding:
    runtime_id: str
    generation: int

    def __post_init__(self) -> None:
        _token(self.runtime_id, field="runtime_id")
        if (
            not isinstance(self.generation, int)
            or isinstance(self.generation, bool)
            or self.generation <= 0
        ):
            raise ValueError("runtime generation must be a positive integer")

    def to_wire(self) -> dict[str, object]:
        return {"runtime_id": self.runtime_id, "generation": self.generation}

    @classmethod
    def from_wire(cls, value: object) -> "RuntimeBinding":
        wire = _wire_mapping(value, field="runtime_binding")
        return cls(runtime_id=str(wire["runtime_id"]), generation=wire["generation"])


@dataclass(frozen=True)
class AgentSpec:
    agent_id: str
    profile: str
    runtime: RuntimeBinding
    parent_id: str | None = None
    capabilities: tuple[str, ...] = ()
    budgets: tuple[Budget, ...] = ()

    def __post_init__(self) -> None:
        _token(self.agent_id, field="agent_id")
        _token(self.profile, field="profile")
        _optional_token(self.parent_id, field="parent_id")
        if self.parent_id == self.agent_id:
            raise ValueError("agent cannot be its own parent")
        object.__setattr__(
            self,
            "capabilities",
            _tokens(self.capabilities, field="capabilities", limit=64),
        )
        budgets = tuple(self.budgets)
        if len(budgets) > 32:
            raise ValueError("budgets exceeds 32 entries")
        if any(not isinstance(item, Budget) for item in budgets):
            raise TypeError("budgets must contain Budget values")
        kinds = tuple(item.kind for item in budgets)
        if len(set(kinds)) != len(kinds):
            raise ValueError("budgets contains duplicate kinds")
        object.__setattr__(self, "budgets", tuple(sorted(budgets)))

    def to_wire(self) -> dict[str, object]:
        return {
            "agent_id": self.agent_id,
            "profile": self.profile,
            "runtime": self.runtime.to_wire(),
            "parent_id": self.parent_id,
            "capabilities": list(self.capabilities),
            "budgets": [item.to_wire() for item in self.budgets],
        }

    @classmethod
    def from_wire(cls, value: object) -> "AgentSpec":
        wire = _wire_mapping(value, field="agent_spec")
        capabilities = wire.get("capabilities", ())
        budgets = wire.get("budgets", ())
        if not isinstance(capabilities, (list, tuple)):
            raise TypeError("capabilities must be a sequence")
        if not isinstance(budgets, (list, tuple)):
            raise TypeError("budgets must be a sequence")
        return cls(
            agent_id=str(wire["agent_id"]),
            profile=str(wire["profile"]),
            runtime=RuntimeBinding.from_wire(wire["runtime"]),
            parent_id=(None if wire.get("parent_id") is None else str(wire["parent_id"])),
            capabilities=tuple(str(item) for item in capabilities),
            budgets=tuple(Budget.from_wire(item) for item in budgets),
        )


@dataclass(frozen=True)
class TaskRecord:
    task_id: str
    goal_ref: str
    owner_id: str
    state: TaskState = TaskState.CREATED

    def __post_init__(self) -> None:
        _token(self.task_id, field="task_id")
        _opaque_ref(self.goal_ref, field="goal_ref", prefix="goal:")
        _token(self.owner_id, field="owner_id")
        if not isinstance(self.state, TaskState):
            raise TypeError("state must be a TaskState")

    @property
    def terminal(self) -> bool:
        return self.state in TERMINAL_TASK_STATES

    def transition(self, next_state: TaskState) -> "TaskRecord":
        if not isinstance(next_state, TaskState):
            raise TypeError("next_state must be a TaskState")
        if next_state not in _TASK_TRANSITIONS[self.state]:
            raise ValueError(f"invalid task transition: {self.state.value} -> {next_state.value}")
        return replace(self, state=next_state)

    def to_wire(self) -> dict[str, object]:
        return {
            "task_id": self.task_id,
            "goal_ref": self.goal_ref,
            "owner_id": self.owner_id,
            "state": self.state.value,
        }

    @classmethod
    def from_wire(cls, value: object) -> "TaskRecord":
        wire = _wire_mapping(value, field="task")
        return cls(
            task_id=str(wire["task_id"]),
            goal_ref=str(wire["goal_ref"]),
            owner_id=str(wire["owner_id"]),
            state=TaskState(str(wire["state"])),
        )


@dataclass(frozen=True)
class WraithMessage:
    message_id: str
    sender_id: str
    recipient_id: str
    message_type: MessageType
    payload_ref: str
    trace_ref: str | None = None

    def __post_init__(self) -> None:
        _token(self.message_id, field="message_id")
        _token(self.sender_id, field="sender_id")
        _token(self.recipient_id, field="recipient_id")
        if not isinstance(self.message_type, MessageType):
            raise TypeError("message_type must be a MessageType")
        _opaque_ref(self.payload_ref, field="payload_ref", prefix="payload:")
        if self.trace_ref is not None:
            _opaque_ref(self.trace_ref, field="trace_ref", prefix="trace:")

    def to_wire(self) -> dict[str, object]:
        return {
            "message_id": self.message_id,
            "sender_id": self.sender_id,
            "recipient_id": self.recipient_id,
            "message_type": self.message_type.value,
            "payload_ref": self.payload_ref,
            "trace_ref": self.trace_ref,
        }

    @classmethod
    def from_wire(cls, value: object) -> "WraithMessage":
        wire = _wire_mapping(value, field="message")
        return cls(
            message_id=str(wire["message_id"]),
            sender_id=str(wire["sender_id"]),
            recipient_id=str(wire["recipient_id"]),
            message_type=MessageType(str(wire["message_type"])),
            payload_ref=str(wire["payload_ref"]),
            trace_ref=(None if wire.get("trace_ref") is None else str(wire["trace_ref"])),
        )


def canonical_json(value: AgentSpec | TaskRecord | WraithMessage) -> str:
    """Serialize a Wraith value deterministically for traces and fixtures."""

    if not isinstance(value, (AgentSpec, TaskRecord, WraithMessage)):
        raise TypeError("unsupported Wraith value")
    envelope = {"protocol": WRAITH_PROTOCOL, "value": value.to_wire()}
    return json.dumps(envelope, sort_keys=True, separators=(",", ":"), ensure_ascii=False)
