"""Pure adapter from generic Zara runtime events to Pet domain events."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from zara.runtime import events as runtime_events

from . import events as pet_events


@dataclass(frozen=True)
class PetDispatch:
    event: pet_events.PetEvent
    event_name: str
    payload: dict[str, object]


def _dispatch(event: pet_events.PetEvent, event_name: str, **payload: object) -> PetDispatch:
    return PetDispatch(event=event, event_name=event_name, payload=payload)


def adapt_runtime_event(event: runtime_events.RuntimeEvent) -> Optional[PetDispatch]:
    """Translate one generic runtime event into the existing Pet vocabulary."""

    label = event.label

    if isinstance(event, runtime_events.AssistantStarted):
        return _dispatch(pet_events.ModelStarted(label=label), "ModelStarted", label=label)
    if isinstance(event, runtime_events.AssistantDelta):
        return _dispatch(pet_events.ModelStreaming(label=label), "ModelStreaming", label=label)
    if isinstance(event, runtime_events.AssistantComplete):
        return _dispatch(
            pet_events.ModelCompleted(success=event.success, label=label),
            "ModelCompleted",
            success=event.success,
            label=label,
        )
    if isinstance(event, runtime_events.AssistantFailed):
        return _dispatch(
            pet_events.ModelFailed(reason=event.reason, label=label),
            "ModelFailed",
            reason=event.reason,
            label=label,
        )

    if isinstance(event, (runtime_events.ToolQueued, runtime_events.ToolStarted)):
        return _dispatch(pet_events.ToolStarted(label=label), "ToolStarted", label=label)
    if isinstance(event, runtime_events.ToolCompleted):
        return _dispatch(
            pet_events.ToolCompleted(success=event.success, label=label),
            "ToolCompleted",
            success=event.success,
            label=label,
        )
    if isinstance(event, runtime_events.ToolFailed):
        return _dispatch(
            pet_events.ToolFailed(reason=event.reason, label=label),
            "ToolFailed",
            reason=event.reason,
            label=label,
        )
    if isinstance(event, runtime_events.ToolWaitingForUser):
        return _dispatch(
            pet_events.UserInputRequired(kind=event.kind, label=label),
            "UserInputRequired",
            kind=event.kind,
            label=label,
        )
    if isinstance(event, runtime_events.ToolCancelled):
        return _dispatch(pet_events.TaskCancelled(label=label), "TaskCancelled", label=label)

    if isinstance(event, runtime_events.AgentStarted):
        return _dispatch(pet_events.AgentStarted(label=label), "AgentStarted", label=label)
    if isinstance(event, runtime_events.AgentCompleted):
        return _dispatch(
            pet_events.AgentCompleted(success=event.success, label=label),
            "AgentCompleted",
            success=event.success,
            label=label,
        )
    if isinstance(event, runtime_events.AgentFailed):
        return _dispatch(
            pet_events.AgentFailed(reason=event.reason, label=label),
            "AgentFailed",
            reason=event.reason,
            label=label,
        )

    if isinstance(event, runtime_events.UserInputRequired):
        return _dispatch(
            pet_events.UserInputRequired(kind=event.kind, label=label),
            "UserInputRequired",
            kind=event.kind,
            label=label,
        )
    if isinstance(event, runtime_events.UserResponded):
        return _dispatch(pet_events.UserResponded(label=label), "UserResponded", label=label)

    if isinstance(event, runtime_events.IndexingStarted):
        return _dispatch(pet_events.IndexingStarted(label=label), "IndexingStarted", label=label)
    if isinstance(event, runtime_events.IndexingCompleted):
        return _dispatch(
            pet_events.IndexingCompleted(success=event.success, label=label),
            "IndexingCompleted",
            success=event.success,
            label=label,
        )
    if isinstance(event, runtime_events.SearchStarted):
        return _dispatch(pet_events.SearchStarted(label=label), "SearchStarted", label=label)
    if isinstance(event, runtime_events.SearchCompleted):
        return _dispatch(
            pet_events.SearchCompleted(success=event.success, label=label),
            "SearchCompleted",
            success=event.success,
            label=label,
        )
    if isinstance(event, runtime_events.BackgroundStarted):
        return _dispatch(pet_events.BackgroundStarted(label=label), "BackgroundStarted", label=label)
    if isinstance(event, runtime_events.BackgroundCompleted):
        return _dispatch(
            pet_events.BackgroundCompleted(success=event.success, label=label),
            "BackgroundCompleted",
            success=event.success,
            label=label,
        )

    if isinstance(event, runtime_events.ResponseText):
        return _dispatch(
            pet_events.ResponseText(text=event.text, truncated=event.truncated, label=label),
            "ResponseText",
            text=event.text,
            truncated=event.truncated,
            label=label,
        )
    if isinstance(event, runtime_events.OutputReady):
        return _dispatch(pet_events.OutputReady(label=label), "OutputReady", label=label)
    if isinstance(event, runtime_events.OutputSeen):
        return _dispatch(pet_events.OutputSeen(label=label), "OutputSeen", label=label)
    if isinstance(event, runtime_events.TurnCancelled):
        return _dispatch(pet_events.TaskCancelled(label=label), "TaskCancelled", label=label)
    if isinstance(event, runtime_events.ProviderUnavailable):
        return _dispatch(
            pet_events.ProviderUnavailable(reason=event.reason, label=label),
            "ProviderUnavailable",
            reason=event.reason,
            label=label,
        )
    if isinstance(event, runtime_events.RuntimeIdle):
        return _dispatch(pet_events.RuntimeIdle(label=label), "RuntimeIdle", label=label)
    if isinstance(event, runtime_events.RuntimeError):
        return _dispatch(
            pet_events.AgentFailed(reason=event.reason, label=label),
            "AgentFailed",
            reason=event.reason,
            label=label,
        )

    return None
