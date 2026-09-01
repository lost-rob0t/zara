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


# Audio output ---------------------------------------------------------------

@dataclass(frozen=True, kw_only=True)
class AudioOutputStarted(RuntimeEvent):
    """A daemon-synthesized audio stream began for a turn."""

    stream_id: str = ""
    sample_rate: int = 24000
    channels: int = 1


@dataclass(frozen=True, kw_only=True)
class AudioOutputChunk(RuntimeEvent):
    """One block of raw s16le mono PCM for a turn's audio stream."""

    stream_id: str = ""
    pcm: bytes = b""


@dataclass(frozen=True, kw_only=True)
class AudioOutputFinished(RuntimeEvent):
    """A turn's audio stream completed normally."""

    stream_id: str = ""


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


@dataclass(frozen=True, kw_only=True)
class VoiceSpeechStarted(RuntimeEvent):
    stream_id: str = ""
    trace_id: Optional[str] = None
    pre_speech_samples: int = 0


@dataclass(frozen=True, kw_only=True)
class VoiceTranscriptPartial(RuntimeEvent):
    stream_id: str = ""
    trace_id: Optional[str] = None
    text: str = ""


@dataclass(frozen=True, kw_only=True)
class VoiceSpeechEnded(RuntimeEvent):
    stream_id: str = ""
    trace_id: Optional[str] = None
    reason: str = ""


@dataclass(frozen=True, kw_only=True)
class VoiceTranscriptFinal(RuntimeEvent):
    stream_id: str = ""
    trace_id: Optional[str] = None
    text: str = ""
    provider: str = ""


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
