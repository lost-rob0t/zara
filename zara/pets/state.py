"""Canonical pet state derivation with a centralized priority policy.

The pet UI must not contain scattered knowledge about models, providers,
tools, or agent implementations. This module is the single source of truth
for mapping runtime activity into one of five aggregate states:

    idle         no active operation
    running      model/streaming/tool/agent/indexing/search/background task
    needs-input  waiting for approval/confirmation/permission/clarification
    ready        operation finished with unseen/unread output
    blocked      fatal failure, provider unavailable, tool failure needing
                 intervention, or runtime unable to progress

Priority (highest first): needs-input > blocked > ready > running > idle.

Multiple simultaneous operations are aggregated: the highest-priority state
among all live activities wins. Cancellation removes an activity and
re-derives the aggregate from whatever remains — it never blindly forces
idle. Repeated identical state events do not restart animations because
the deriver is idempotent per activity kind.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, Iterable, Optional

logger = logging.getLogger(__name__)


class PetState(str, Enum):
    """Five aggregate pet states, ordered by display priority."""

    IDLE = "idle"
    RUNNING = "running"
    NEEDS_INPUT = "needs-input"
    READY = "ready"
    BLOCKED = "blocked"

    @classmethod
    def from_str(cls, value: str) -> "PetState":
        try:
            return cls(value)
        except ValueError as exc:
            raise ValueError(f"Unknown pet state: {value!r}") from exc


# Display priority: higher number wins. The order is intentional and tested:
# needs-input is most urgent, then blocked, then ready, then running, then
# idle. This list is the canonical policy — do not duplicate it elsewhere.
PRIORITY: dict[PetState, int] = {
    PetState.NEEDS_INPUT: 4,
    PetState.BLOCKED: 3,
    PetState.READY: 2,
    PetState.RUNNING: 1,
    PetState.IDLE: 0,
}


class ActivityKind(str, Enum):
    """Coarse classification of a runtime activity.

    Activities of the same kind share a slot: a later start replaces the
    earlier one for that kind, which keeps the activity map bounded and
    prevents unbounded growth from chatty event sources.
    """

    MODEL = "model"
    TOOL = "tool"
    AGENT = "agent"
    INDEXING = "indexing"
    SEARCH = "search"
    BACKGROUND = "background"
    OTHER = "other"


@dataclass
class Activity:
    """One live runtime operation that maps to a pet state."""

    kind: ActivityKind
    state: PetState
    label: Optional[str] = None
    # True when the activity has produced output the user has not seen.
    unread: bool = False

    def effective_state(self) -> PetState:
        """The state this activity contributes to aggregation."""
        if self.state is PetState.READY and not self.unread:
            return PetState.IDLE
        return self.state


class PetStatusDeriver:
    """Single owner of the pet priority policy.

    Thread-safety is the caller's responsibility (the ``PetStateActor`` is
    the sole mutator in production). This class is pure logic so it can be
    unit-tested without actors or Qt.
    """

    def __init__(self) -> None:
        self._activities: dict[ActivityKind, Activity] = {}

    @property
    def activities(self) -> Dict[ActivityKind, Activity]:
        return dict(self._activities)

    def start(self, kind: ActivityKind, state: PetState = PetState.RUNNING,
              label: Optional[str] = None) -> Activity:
        # READY means "operation finished with unseen output", so starting
        # in READY implies unread=True. Other states default to unread=False
        # until mark_unread is called.
        unread = state is PetState.READY
        activity = Activity(kind=kind, state=state, label=label, unread=unread)
        self._activities[kind] = activity
        logger.debug("[PetState] start kind=%s state=%s", kind.value, state.value)
        return activity

    def transition(self, kind: ActivityKind, state: PetState,
                   label: Optional[str] = None) -> Optional[Activity]:
        activity = self._activities.get(kind)
        if activity is None:
            return self.start(kind, state, label)
        activity.state = state
        if label is not None:
            activity.label = label
        if state is PetState.READY:
            activity.unread = True
        elif state is PetState.IDLE:
            activity.unread = False
        logger.debug("[PetState] transition kind=%s -> %s", kind.value, state.value)
        return activity

    def mark_unread(self, kind: ActivityKind, unread: bool = True) -> None:
        activity = self._activities.get(kind)
        if activity is not None:
            activity.unread = unread

    def mark_all_seen(self) -> None:
        """Mark every activity as seen so READY falls back to IDLE."""
        for activity in self._activities.values():
            activity.unread = False

    def cancel(self, kind: ActivityKind) -> Optional[Activity]:
        """Cancel one activity. Returns the removed activity or None.

        Cancellation removes the activity and lets the aggregate fall back
        to the next-highest remaining state rather than forcing idle.
        """
        removed = self._activities.pop(kind, None)
        if removed is not None:
            logger.debug("[PetState] cancel kind=%s", kind.value)
        return removed

    def clear(self) -> None:
        self._activities.clear()

    def aggregate(self) -> PetState:
        """Derive the single highest-priority state across all activities."""
        if not self._activities:
            return PetState.IDLE
        best = PetState.IDLE
        best_priority = PRIORITY[PetState.IDLE]
        for activity in self._activities.values():
            candidate = activity.effective_state()
            priority = PRIORITY[candidate]
            if priority > best_priority:
                best = candidate
                best_priority = priority
        return best

    def pending_labels(self) -> list[str]:
        """Human-readable labels for activities informing the current state."""
        target = self.aggregate()
        labels: list[str] = []
        for activity in self._activities.values():
            if activity.effective_state() is target and activity.label:
                labels.append(activity.label)
        return sorted(set(labels))