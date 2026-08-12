"""Runtime event bridge — emits domain events to the PetStateActor.

The bridge keeps a weak reference to the pet actor so the voice/agent
runtime can call ``pet_event(...)`` without importing Qt or caring
whether pets are enabled. When no actor is registered, calls are no-ops
with negligible overhead.
"""

from __future__ import annotations

import logging
import weakref
from typing import Optional

from . import events
from .actor import PetStateActor

logger = logging.getLogger(__name__)

_actor_ref: Optional[weakref.ReferenceType] = None


def register_actor(actor: PetStateActor) -> None:
    global _actor_ref
    _actor_ref = weakref.ref(actor)


def unregister_actor() -> None:
    global _actor_ref
    _actor_ref = None


def _tell(event: events.PetEvent) -> None:
    ref = _actor_ref() if _actor_ref is not None else None
    if ref is None:
        return
    try:
        ref.tell(event)
    except Exception:
        logger.debug("[PetBridge] tell failed for %r", event, exc_info=True)


# Convenience emitters keyed to runtime concerns. Callers in wake.py /
# agent/graph.py use these so they don't import the event classes
# directly.

def model_started(label: Optional[str] = None) -> None:
    _tell(events.ModelStarted(label=label))


def model_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell(events.ModelCompleted(success=success, label=label))


def model_failed(reason: str = "", label: Optional[str] = None) -> None:
    _tell(events.ModelFailed(reason=reason, label=label))


def tool_started(label: Optional[str] = None) -> None:
    _tell(events.ToolStarted(label=label))


def tool_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell(events.ToolCompleted(success=success, label=label))


def agent_started(label: Optional[str] = None) -> None:
    _tell(events.AgentStarted(label=label))


def agent_completed(success: bool = True, label: Optional[str] = None) -> None:
    _tell(events.AgentCompleted(success=success, label=label))


def user_input_required(kind: str = "approval", label: Optional[str] = None) -> None:
    _tell(events.UserInputRequired(kind=kind, label=label))


def user_responded(label: Optional[str] = None) -> None:
    _tell(events.UserResponded(label=label))


def output_ready(label: Optional[str] = None) -> None:
    _tell(events.OutputReady(label=label))


def output_seen(label: Optional[str] = None) -> None:
    _tell(events.OutputSeen(label=label))


def task_cancelled(label: Optional[str] = None) -> None:
    _tell(events.TaskCancelled(label=label))


def runtime_idle(label: Optional[str] = None) -> None:
    _tell(events.RuntimeIdle(label=label))


def provider_unavailable(reason: str = "", label: Optional[str] = None) -> None:
    _tell(events.ProviderUnavailable(reason=reason, label=label))