"""Provider-neutral application events emitted by the Zara runtime.

These events describe user-visible/runtime-significant state without exposing
LangChain, PySWIP, Qt, or pet-specific objects.  Turn-scoped producers should
reuse the canonical turn id owned by :mod:`zara.actors`.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional


@dataclass(frozen=True, kw_only=True)
class RuntimeEvent:
    """Base event correlation fields shared by every runtime event."""

    turn_id: Optional[str] = None
    conversation_id: Optional[str] = None
    label: Optional[str] = None


# Runtime lifecycle ---------------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class RuntimeStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class RuntimeStopped(RuntimeEvent):
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class RuntimeError(RuntimeEvent):
    reason: str = ""
    fatal: bool = False


@dataclass(frozen=True, kw_only=True)
class RuntimeIdle(RuntimeEvent):
    pass


# Turn / agent lifecycle ----------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class TurnStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class TurnCancelled(RuntimeEvent):
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class AgentStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class AgentCompleted(RuntimeEvent):
    success: bool = True


@dataclass(frozen=True, kw_only=True)
class AgentFailed(RuntimeEvent):
    reason: str = ""


# Assistant/model generation ----------------------------------------------

@dataclass(frozen=True, kw_only=True)
class AssistantStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class AssistantDelta(RuntimeEvent):
    text: str = ""


@dataclass(frozen=True, kw_only=True)
class AssistantComplete(RuntimeEvent):
    text: str = ""
    success: bool = True


@dataclass(frozen=True, kw_only=True)
class AssistantFailed(RuntimeEvent):
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class ResponseText(RuntimeEvent):
    """User-facing response text for secondary surfaces such as Pets."""

    text: str = ""
    truncated: bool = False


@dataclass(frozen=True, kw_only=True)
class OutputReady(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class OutputSeen(RuntimeEvent):
    pass


# Voice / transcription ----------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class VoiceStateChanged(RuntimeEvent):
    state: str = "idle"
    detail: str = ""

    def __post_init__(self) -> None:
        allowed = {"idle", "listening", "transcribing", "thinking", "speaking", "error"}
        if self.state not in allowed:
            raise ValueError(f"unsupported voice state: {self.state!r}")


@dataclass(frozen=True, kw_only=True)
class TranscriptReady(RuntimeEvent):
    text: str = ""


# Intent / Prolog ----------------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class IntentResolved(RuntimeEvent):
    intent: str = ""
    resolver: str = ""
    arguments: tuple[Any, ...] = ()


@dataclass(frozen=True, kw_only=True)
class PrologQueryCompleted(RuntimeEvent):
    success: bool = True
    summary: str = ""


# Tool execution -----------------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class ToolQueued(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None


@dataclass(frozen=True, kw_only=True)
class ToolStarted(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None


@dataclass(frozen=True, kw_only=True)
class ToolProgress(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None
    message: str = ""
    progress: Optional[float] = None

    def __post_init__(self) -> None:
        if self.progress is not None and not 0.0 <= self.progress <= 1.0:
            raise ValueError("tool progress must be between 0.0 and 1.0")


@dataclass(frozen=True, kw_only=True)
class ToolWaitingForUser(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None
    kind: str = "approval"
    prompt: str = ""


@dataclass(frozen=True, kw_only=True)
class ToolCompleted(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None
    success: bool = True


@dataclass(frozen=True, kw_only=True)
class ToolFailed(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class ToolCancelled(RuntimeEvent):
    tool_run_id: Optional[str] = None
    tool_name: Optional[str] = None
    reason: str = ""


# Generic user-input / background activity --------------------------------

@dataclass(frozen=True, kw_only=True)
class UserInputRequired(RuntimeEvent):
    kind: str = "approval"
    prompt: str = ""


@dataclass(frozen=True, kw_only=True)
class UserResponded(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class IndexingStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class IndexingCompleted(RuntimeEvent):
    success: bool = True


@dataclass(frozen=True, kw_only=True)
class SearchStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class SearchCompleted(RuntimeEvent):
    success: bool = True


@dataclass(frozen=True, kw_only=True)
class BackgroundStarted(RuntimeEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class BackgroundCompleted(RuntimeEvent):
    success: bool = True


# Timer lifecycle ----------------------------------------------------------

def _validate_timer_identity(
    timer_id: str,
    name: str,
    created_at_ns: int,
    due_at_ns: int,
) -> None:
    if not isinstance(timer_id, str) or not timer_id.strip():
        raise ValueError("timer_id must be a non-empty string")
    if timer_id != timer_id.strip():
        raise ValueError("timer_id must not contain surrounding whitespace")
    if not isinstance(name, str):
        raise TypeError("timer name must be a string")
    if type(created_at_ns) is not int or type(due_at_ns) is not int:
        raise TypeError("timer timestamps must be integers")
    if created_at_ns < 0 or due_at_ns < created_at_ns:
        raise ValueError("timer timestamps are not ordered")


@dataclass(frozen=True, kw_only=True)
class TimerScheduled(RuntimeEvent):
    timer_id: str
    name: str = ""
    created_at_ns: int
    due_at_ns: int
    revision: int = 1

    def __post_init__(self) -> None:
        _validate_timer_identity(
            self.timer_id,
            self.name,
            self.created_at_ns,
            self.due_at_ns,
        )
        if type(self.revision) is not int:
            raise TypeError("timer revision must be an integer")
        if self.revision != 1:
            raise ValueError("scheduled timer revision must be 1")


@dataclass(frozen=True, kw_only=True)
class TimerFired(RuntimeEvent):
    timer_id: str
    name: str = ""
    created_at_ns: int
    due_at_ns: int
    fired_at_ns: int
    revision: int = 2
    message: str = ""

    def __post_init__(self) -> None:
        _validate_timer_identity(
            self.timer_id,
            self.name,
            self.created_at_ns,
            self.due_at_ns,
        )
        if type(self.fired_at_ns) is not int:
            raise TypeError("timer fired timestamp must be an integer")
        if self.fired_at_ns < self.due_at_ns:
            raise ValueError("timer fired timestamp precedes due timestamp")
        if type(self.revision) is not int:
            raise TypeError("timer revision must be an integer")
        if self.revision != 2:
            raise ValueError("fired timer revision must be 2")
        if not isinstance(self.message, str):
            raise TypeError("timer message must be a string")


# Provider / notification --------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class ProviderChanged(RuntimeEvent):
    provider: str = ""
    model: str = ""


@dataclass(frozen=True, kw_only=True)
class ProviderUnavailable(RuntimeEvent):
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class NotificationRequested(RuntimeEvent):
    title: str = ""
    message: str = ""
    urgency: str = "normal"

    def __post_init__(self) -> None:
        if self.urgency not in {"low", "normal", "critical"}:
            raise ValueError(f"unsupported notification urgency: {self.urgency!r}")
