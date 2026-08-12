"""Stable domain events the runtime emits for pet consumption.

These are intentionally provider-agnostic so the pet renderer never learns
about LLM providers, tool implementations, or transport details. The
``PetStateActor`` translates these into ``PetState`` transitions.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class PetEvent:
    """Base domain event for the pet system."""

    label: Optional[str] = None


# Model generation ---------------------------------------------------------

@dataclass(frozen=True)
class ModelStarted(PetEvent):
    pass


@dataclass(frozen=True)
class ModelStreaming(PetEvent):
    pass


@dataclass(frozen=True)
class ModelCompleted(PetEvent):
    success: bool = True


@dataclass(frozen=True)
class ModelFailed(PetEvent):
    reason: str = ""


# Tool execution -----------------------------------------------------------

@dataclass(frozen=True)
class ToolStarted(PetEvent):
    pass


@dataclass(frozen=True)
class ToolCompleted(PetEvent):
    success: bool = True


@dataclass(frozen=True)
class ToolFailed(PetEvent):
    reason: str = ""


# Agent execution ----------------------------------------------------------

@dataclass(frozen=True)
class AgentStarted(PetEvent):
    pass


@dataclass(frozen=True)
class AgentCompleted(PetEvent):
    success: bool = True


@dataclass(frozen=True)
class AgentFailed(PetEvent):
    reason: str = ""


# User input ---------------------------------------------------------------

@dataclass(frozen=True)
class UserInputRequired(PetEvent):
    kind: str = "approval"  # approval|confirmation|permission|clarification


@dataclass(frozen=True)
class UserResponded(PetEvent):
    pass


# Background work ----------------------------------------------------------

@dataclass(frozen=True)
class IndexingStarted(PetEvent):
    pass


@dataclass(frozen=True)
class IndexingCompleted(PetEvent):
    success: bool = True


@dataclass(frozen=True)
class SearchStarted(PetEvent):
    pass


@dataclass(frozen=True)
class SearchCompleted(PetEvent):
    success: bool = True


@dataclass(frozen=True)
class BackgroundStarted(PetEvent):
    pass


@dataclass(frozen=True)
class BackgroundCompleted(PetEvent):
    success: bool = True


# Lifecycle ----------------------------------------------------------------

@dataclass(frozen=True)
class ResponseText(PetEvent):
    """Zara's spoken/text response — shown in the pet's speech bubble."""

    text: str = ""
    truncated: bool = False


@dataclass(frozen=True)
class OutputReady(PetEvent):
    """Output is available but not yet seen by the user."""

    pass


@dataclass(frozen=True)
class OutputSeen(PetEvent):
    """User has viewed the result; ready state should clear."""

    pass


@dataclass(frozen=True)
class TaskCancelled(PetEvent):
    """A task was cancelled; its activity is removed from the aggregate."""

    pass


@dataclass(frozen=True)
class ProviderUnavailable(PetEvent):
    reason: str = ""


@dataclass(frozen=True)
class RuntimeIdle(PetEvent):
    """Explicit return-to-idle signal (e.g. after OutputSeen)."""

    pass