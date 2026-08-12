"""Runtime event bridge — emits domain events to the pet.

Two paths:
1. In-process: if a PetStateActor is registered (the overlay and the
   runtime share a process), events are told directly to the actor.
2. Cross-process: events are also published via ZMQ PUB/SUB so a
   separately-running ``zara --pets`` overlay receives them.

When neither is active, calls are no-ops with negligible overhead.
"""

from __future__ import annotations

import logging
import weakref
from typing import Optional

from . import events
from .ipc import PetPublisher

logger = logging.getLogger(__name__)

_actor_ref: Optional[weakref.ReferenceType] = None
_publisher: Optional[PetPublisher] = None


def register_actor(actor) -> None:
    global _actor_ref
    _actor_ref = weakref.ref(actor)


def unregister_actor() -> None:
    global _actor_ref
    _actor_ref = None


def _ensure_publisher() -> Optional[PetPublisher]:
    global _publisher
    if _publisher is None:
        pub = PetPublisher()
        pub.start()
        if pub._socket is not None:
            _publisher = pub
        return pub if _publisher is not None else None
    return _publisher


def _publish(event_name: str, **kwargs) -> None:
    pub = _ensure_publisher()
    if pub is not None:
        pub.publish(event_name, **kwargs)


def _tell_and_publish(event: events.PetEvent, event_name: str, **kwargs) -> None:
    # In-process actor (shared process case).
    ref = _actor_ref() if _actor_ref is not None else None
    if ref is not None:
        try:
            ref.tell(event)
        except Exception:
            logger.debug("[PetBridge] tell failed for %r", event, exc_info=True)
    # Cross-process (ZMQ) — always also publish so a separate overlay picks up.
    logger.debug("[PetBridge] publishing %s %s", event_name, kwargs)
    _publish(event_name, **kwargs)


def model_started(label: Optional[str] = None) -> None:
    _tell_and_publish(events.ModelStarted(label=label), "ModelStarted", label=label)


def model_streaming(label: Optional[str] = None) -> None:
    _tell_and_publish(events.ModelStreaming(label=label), "ModelStreaming", label=label)


def model_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.ModelCompleted(success=success, label=label),
        "ModelCompleted", success=success, label=label,
    )


def model_failed(reason: str = "", label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.ModelFailed(reason=reason, label=label),
        "ModelFailed", reason=reason, label=label,
    )


def tool_started(label: Optional[str] = None) -> None:
    _tell_and_publish(events.ToolStarted(label=label), "ToolStarted", label=label)


def tool_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.ToolCompleted(success=success, label=label),
        "ToolCompleted", success=success, label=label,
    )


def agent_started(label: Optional[str] = None) -> None:
    _tell_and_publish(events.AgentStarted(label=label), "AgentStarted", label=label)


def agent_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.AgentCompleted(success=success, label=label),
        "AgentCompleted", success=success, label=label,
    )


def user_input_required(kind: str = "approval", label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.UserInputRequired(kind=kind, label=label),
        "UserInputRequired", kind=kind, label=label,
    )


def user_responded(label: Optional[str] = None) -> None:
    _tell_and_publish(events.UserResponded(label=label), "UserResponded", label=label)


def output_ready(label: Optional[str] = None) -> None:
    _tell_and_publish(events.OutputReady(label=label), "OutputReady", label=label)


def output_seen(label: Optional[str] = None) -> None:
    _tell_and_publish(events.OutputSeen(label=label), "OutputSeen", label=label)


def task_cancelled(label: Optional[str] = None) -> None:
    _tell_and_publish(events.TaskCancelled(label=label), "TaskCancelled", label=label)


def runtime_idle(label: Optional[str] = None) -> None:
    _tell_and_publish(events.RuntimeIdle(label=label), "RuntimeIdle", label=label)


def provider_unavailable(reason: str = "", label: Optional[str] = None) -> None:
    _tell_and_publish(
        events.ProviderUnavailable(reason=reason, label=label),
        "ProviderUnavailable", reason=reason, label=label,
    )