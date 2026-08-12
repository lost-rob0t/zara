"""PetStateActor — Pykka actor owning the live pet state.

The runtime emits domain events (see ``zara.pets.events``); this actor
applies them to a ``PetStatusDeriver`` and publishes the resulting
``PetState`` to subscribers. The actor is the sole mutator of the deriver
in production, which keeps the priority policy centralized and tested.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any, Callable, ClassVar, Optional

import pykka

from . import events
from .state import ActivityKind, PetState, PetStatusDeriver

logger = logging.getLogger(__name__)


class PetStateActor(pykka.ThreadingActor):
    """Consumes pet domain events and derives the aggregate ``PetState``."""

    mailbox_size: ClassVar[int] = 128
    mailbox_overflow: ClassVar[str] = "drop_oldest"

    def __init__(
        self,
        deriver: Optional[PetStatusDeriver] = None,
        subscriber: Optional[Callable[[PetState, list[str]], None]] = None,
    ) -> None:
        super().__init__()
        self._deriver = deriver or PetStatusDeriver()
        self._subscriber = subscriber
        self._current = PetState.IDLE

    @property
    def current(self) -> PetState:
        return self._current

    def on_start(self) -> None:
        logger.info("[PetStateActor] started")
        self._publish()

    def on_stop(self) -> None:
        logger.info("[PetStateActor] stopped")

    def on_receive(self, message: Any) -> Any:
        if isinstance(message, events.PetEvent):
            return self._handle_event(message)
        if isinstance(message, _GetState):
            return self._current
        if isinstance(message, _GetDeriver):
            return self._deriver
        return super().on_receive(message)

    def _handle_event(self, event: events.PetEvent) -> None:
        if isinstance(event, events.TaskCancelled):
            # TaskCancelled removes the most recent agent activity so the
            # aggregate re-derives from whatever remains.
            kind, _ = _map_event(event)
            if kind is not None:
                self._deriver.cancel(kind)
            self._publish()
            return
        if isinstance(event, events.OutputSeen):
            self._deriver.mark_all_seen()
            self._publish()
            return
        if isinstance(event, events.RuntimeIdle):
            self._deriver.clear()
            self._publish()
            return
        kind, state = _map_event(event)
        if kind is None or state is None:
            return
        self._deriver.transition(kind, state, label=event.label)
        self._publish()

    def _publish(self) -> None:
        new_state = self._deriver.aggregate()
        labels = self._deriver.pending_labels()
        if new_state is not self._current:
            logger.info(
                "[PetStateActor] %s -> %s (labels=%s)",
                self._current.value, new_state.value, labels,
            )
            self._current = new_state
        if self._subscriber is not None:
            try:
                self._subscriber(new_state, labels)
            except Exception:
                logger.exception("[PetStateActor] subscriber raised")


# Control messages ---------------------------------------------------------

@dataclass
class _GetState:
    pass


class _GetDeriver:
    pass


# Event → (ActivityKind, PetState) mapping ---------------------------------

def _map_event(event: events.PetEvent) -> tuple[Optional[ActivityKind], Optional[PetState]]:
    """Translate a domain event into an (activity kind, target state) pair.

    Returns (None, None) for events the actor does not own. The mapping is
    exhaustive and intentionally explicit so it can be audited and tested.
    """
    label = event.label
    if isinstance(event, events.ModelStarted):
        return ActivityKind.MODEL, PetState.RUNNING
    if isinstance(event, events.ModelStreaming):
        return ActivityKind.MODEL, PetState.RUNNING
    if isinstance(event, events.ModelCompleted):
        if event.success:
            return ActivityKind.MODEL, PetState.READY
        return ActivityKind.MODEL, PetState.BLOCKED
    if isinstance(event, events.ModelFailed):
        return ActivityKind.MODEL, PetState.BLOCKED
    if isinstance(event, events.ToolStarted):
        return ActivityKind.TOOL, PetState.RUNNING
    if isinstance(event, events.ToolCompleted):
        return ActivityKind.TOOL, PetState.READY if event.success else PetState.BLOCKED
    if isinstance(event, events.ToolFailed):
        return ActivityKind.TOOL, PetState.BLOCKED
    if isinstance(event, events.AgentStarted):
        return ActivityKind.AGENT, PetState.RUNNING
    if isinstance(event, events.AgentCompleted):
        return ActivityKind.AGENT, PetState.READY if event.success else PetState.BLOCKED
    if isinstance(event, events.AgentFailed):
        return ActivityKind.AGENT, PetState.BLOCKED
    if isinstance(event, events.UserInputRequired):
        return ActivityKind.AGENT, PetState.NEEDS_INPUT
    if isinstance(event, events.UserResponded):
        return ActivityKind.AGENT, PetState.RUNNING
    if isinstance(event, events.IndexingStarted):
        return ActivityKind.INDEXING, PetState.RUNNING
    if isinstance(event, events.IndexingCompleted):
        return ActivityKind.INDEXING, PetState.READY if event.success else PetState.BLOCKED
    if isinstance(event, events.SearchStarted):
        return ActivityKind.SEARCH, PetState.RUNNING
    if isinstance(event, events.SearchCompleted):
        return ActivityKind.SEARCH, PetState.READY if event.success else PetState.BLOCKED
    if isinstance(event, events.BackgroundStarted):
        return ActivityKind.BACKGROUND, PetState.RUNNING
    if isinstance(event, events.BackgroundCompleted):
        return ActivityKind.BACKGROUND, PetState.READY if event.success else PetState.BLOCKED
    if isinstance(event, events.OutputReady):
        # OutputReady is handled by the kind owner's transition to READY;
        # mark the most recent activity unread. The caller typically emits
        # this alongside a *Completed event, so we no-op here to avoid a
        # second transition.
        return None, None
    if isinstance(event, events.OutputSeen):
        return None, None  # handled in _handle_event
    if isinstance(event, events.RuntimeIdle):
        return None, None  # handled in _handle_event
    if isinstance(event, events.TaskCancelled):
        return ActivityKind.AGENT, None
    if isinstance(event, events.ProviderUnavailable):
        return ActivityKind.MODEL, PetState.BLOCKED
    return None, None