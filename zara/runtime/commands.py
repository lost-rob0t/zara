"""Desktop-neutral commands accepted by :class:`RuntimeHost`.

Commands describe user/application intent. They do not expose Qt widgets,
LangChain objects, PySWIP queries, or concrete tool implementations.
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from typing import Iterable, Optional


MAX_CONTEXT_IDS = 32
MAX_CONTEXT_ID_CHARS = 128


def _request_id() -> str:
    return uuid.uuid4().hex


def _normalize_context_ids(values: Iterable[str]) -> tuple[str, ...]:
    """Return bounded opaque context ids or fail closed before dispatch."""
    context_ids = tuple(values)
    if len(context_ids) > MAX_CONTEXT_IDS:
        raise ValueError(f"context_ids exceeds maximum count {MAX_CONTEXT_IDS}")

    normalized: list[str] = []
    for index, value in enumerate(context_ids):
        label = f"context_ids[{index}]"
        if not isinstance(value, str):
            raise ValueError(f"{label} must be a string")
        context_id = value.strip()
        if not context_id:
            raise ValueError(f"{label} must not be empty")
        if "\x00" in context_id:
            raise ValueError(f"{label} must not contain NUL")
        if len(context_id) > MAX_CONTEXT_ID_CHARS:
            raise ValueError(
                f"{label} exceeds maximum length {MAX_CONTEXT_ID_CHARS}"
            )
        normalized.append(context_id)
    return tuple(normalized)


@dataclass(frozen=True, kw_only=True)
class RuntimeCommand:
    """Base command carrying a stable request correlation id."""

    request_id: str = field(default_factory=_request_id)


@dataclass(frozen=True, kw_only=True)
class SubmitTurn(RuntimeCommand):
    text: str
    conversation_id: Optional[str] = None
    context_ids: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        if not self.text.strip():
            raise ValueError("turn text must not be empty")
        object.__setattr__(self, "context_ids", _normalize_context_ids(self.context_ids))


@dataclass(frozen=True, kw_only=True)
class CancelTurn(RuntimeCommand):
    turn_id: str

    def __post_init__(self) -> None:
        if not self.turn_id:
            raise ValueError("turn_id must not be empty")


@dataclass(frozen=True, kw_only=True)
class StartVoice(RuntimeCommand):
    pass


@dataclass(frozen=True, kw_only=True)
class StopVoice(RuntimeCommand):
    pass


@dataclass(frozen=True, kw_only=True)
class MuteSpeech(RuntimeCommand):
    enabled: bool


@dataclass(frozen=True, kw_only=True)
class ApproveTool(RuntimeCommand):
    tool_run_id: str

    def __post_init__(self) -> None:
        if not self.tool_run_id:
            raise ValueError("tool_run_id must not be empty")


@dataclass(frozen=True, kw_only=True)
class RejectTool(RuntimeCommand):
    tool_run_id: str
    reason: str = ""

    def __post_init__(self) -> None:
        if not self.tool_run_id:
            raise ValueError("tool_run_id must not be empty")


@dataclass(frozen=True, kw_only=True)
class RestartRuntime(RuntimeCommand):
    reason: str = "user-requested restart"


@dataclass(frozen=True, kw_only=True)
class ShutdownRuntime(RuntimeCommand):
    reason: str = "user-requested shutdown"


@dataclass(frozen=True)
class CommandReceipt:
    """Immediate acknowledgement that a runtime command was accepted."""

    request_id: str
    turn_id: Optional[str] = None
    detail: str = ""
