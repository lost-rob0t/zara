"""Actor-style real-time turn coordinator with bounded mailboxes (ZARA-023).

Defines typed event schemas, a bounded mailbox with explicit overflow
policy, and a TurnCoordinator actor that is the sole owner of turn
state, turn IDs, deadlines, and cancellation.

The implementation uses Pykka's ThreadingActor. Bounded mailboxes are
implemented by overriding ``_create_actor_inbox()`` to return a
``BoundedMailbox`` that applies a drop-oldest (or configurable) overflow
policy when the queue is full, so slow actors cannot grow memory without
bound.

Cancellation is idempotent and scoped by turn ID: once a turn is
cancelled, any event carrying that turn ID is rejected before it reaches
downstream actors, so stale results cannot speak in a newer turn.
"""

from __future__ import annotations

import logging
import queue
import uuid
from dataclasses import dataclass, field
from typing import Any, ClassVar, Optional

import pykka

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Typed event schemas
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Event:
    """Base message for all actor events.

    ``turn_id`` scopes every event to a specific turn. Events without a
    turn ID are control messages (start, stop, drain, cancel).
    """

    turn_id: Optional[str] = None


@dataclass(frozen=True)
class AudioFrameReceived(Event):
    frames: int = 0
    sequence: int = 0


@dataclass(frozen=True)
class WakeDetected(Event):
    pass


@dataclass(frozen=True)
class AckRequested(Event):
    channel: str = "notification"


@dataclass(frozen=True)
class SpeechStart(Event):
    pass


@dataclass(frozen=True)
class SpeechEnd(Event):
    reason: str = "silence"


@dataclass(frozen=True)
class FinalTranscript(Event):
    text_length: int = 0
    provider: str = ""


@dataclass(frozen=True)
class RouteSelected(Event):
    route: str = ""


@dataclass(frozen=True)
class PrologResult(Event):
    status: str = ""


@dataclass(frozen=True)
class LLMRequest(Event):
    provider: str = ""
    request_index: int = 0


@dataclass(frozen=True)
class LLMToken(Event):
    kind: str = "first"
    provider: str = ""
    request_index: int = 0


@dataclass(frozen=True)
class TTSRequest(Event):
    provider: str = ""
    text_length: int = 0


@dataclass(frozen=True)
class TTSChunk(Event):
    provider: str = ""


@dataclass(frozen=True)
class TTSPlayback(Event):
    kind: str = "first"
    success: bool = True
    cancelled: bool = False


@dataclass(frozen=True)
class InterruptionDetected(Event):
    pass


@dataclass(frozen=True)
class CancellationCompleted(Event):
    pass


# Control messages (no turn_id)


@dataclass(frozen=True)
class StartTurn:
    """Ask the coordinator to begin a new turn and return its ID."""


@dataclass(frozen=True)
class CancelTurn:
    """Cancel a turn by ID. Idempotent — cancelling twice is a no-op."""

    turn_id: str = ""


@dataclass(frozen=True)
class Drain:
    """Drain remaining messages and prepare for shutdown."""


@dataclass(frozen=True)
class TurnStartedReply:
    """Reply to StartTurn containing the new turn ID."""
    turn_id: str


@dataclass(frozen=True)
class TurnCancelledReply:
    """Reply to CancelTurn confirming the turn was already or is now cancelled."""
    turn_id: str
    was_already_cancelled: bool


# ---------------------------------------------------------------------------
# Bounded mailbox
# ---------------------------------------------------------------------------


class BoundedMailbox(queue.Queue):
    """A bounded queue with an explicit overflow policy.

    Policies:
    - ``drop_oldest`` (default): remove the oldest message and insert the new one.
    - ``drop_newest``: discard the incoming message.
    - ``block``: block until space is available (standard queue.Queue behavior).
    """

    def __init__(self, maxsize: int = 1, overflow: str = "drop_oldest"):
        super().__init__(maxsize=max(1, maxsize))
        self._overflow = overflow
        self._dropped_count = 0

    @property
    def dropped_count(self) -> int:
        return self._dropped_count

    def put(self, item, block=True, timeout=None):
        if self._overflow == "block":
            return queue.Queue.put(self, item, block=block, timeout=timeout)
        try:
            queue.Queue.put(self, item, block=False)
        except queue.Full:
            if self._overflow == "drop_oldest":
                try:
                    queue.Queue.get(self, block=False)
                    self._dropped_count += 1
                except queue.Empty:
                    pass
                queue.Queue.put(self, item, block=False)
            elif self._overflow == "drop_newest":
                self._dropped_count += 1
            else:
                raise ValueError(f"Unknown overflow policy: {self._overflow}")


# ---------------------------------------------------------------------------
# Bounded actor base
# ---------------------------------------------------------------------------


class BoundedActor(pykka.ThreadingActor):
    """ThreadingActor with a bounded mailbox and explicit overflow policy.

    Subclass and set ``mailbox_size`` and ``mailbox_overflow`` class
    attributes before calling ``start()``.
    """

    mailbox_size: ClassVar[int] = 32
    mailbox_overflow: ClassVar[str] = "drop_oldest"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    @classmethod
    def _create_actor_inbox(cls) -> BoundedMailbox:
        return BoundedMailbox(
            maxsize=cls.mailbox_size,
            overflow=cls.mailbox_overflow,
        )

    def on_receive(self, message: Any) -> Any:
        logger.debug("[Actor] %s received %r", self.__class__.__name__, message)


# ---------------------------------------------------------------------------
# Turn state
# ---------------------------------------------------------------------------


@dataclass
class TurnState:
    """Mutable per-turn state owned only by TurnCoordinator."""

    turn_id: str
    started: bool = False
    cancelled: bool = False
    events: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# TurnCoordinator actor
# ---------------------------------------------------------------------------


class TurnCoordinator(BoundedActor):
    """Sole owner of conversation state, turn IDs, and cancellation.

    Responsibilities:
    - Generate unique turn IDs.
    - Track which turns are active vs. cancelled.
    - Reject events from cancelled turns so stale results cannot speak.
    - Forward valid events to the telemetry actor (if any).

    The coordinator does **not** own audio, TTS, or LLM state — those
    belong to their respective actors. The coordinator only owns the
    turn lifecycle.
    """

    mailbox_size: ClassVar[int] = 64
    mailbox_overflow: ClassVar[str] = "drop_oldest"

    def __init__(self, telemetry_ref: Optional[pykka.ActorRef] = None):
        super().__init__()
        self._turns: dict[str, TurnState] = {}
        self._telemetry_ref = telemetry_ref
        self._turn_counter = 0

    def on_start(self) -> None:
        logger.info("[TurnCoordinator] started")

    def on_stop(self) -> None:
        logger.info("[TurnCoordinator] stopped")

    def on_receive(self, message: Any) -> Any:
        if isinstance(message, StartTurn):
            return self._handle_start_turn()
        if isinstance(message, CancelTurn):
            return self._handle_cancel_turn(message)
        if isinstance(message, Drain):
            self._turns.clear()
            return None
        if isinstance(message, Event):
            return self._handle_event(message)
        return super().on_receive(message)

    def _handle_start_turn(self) -> TurnStartedReply:
        self._turn_counter += 1
        turn_id = f"turn-{self._turn_counter:04d}"
        state = TurnState(turn_id=turn_id, started=True)
        self._turns[turn_id] = state
        logger.info("[TurnCoordinator] started turn %s", turn_id)
        return TurnStartedReply(turn_id=turn_id)

    def _handle_cancel_turn(self, message: CancelTurn) -> TurnCancelledReply:
        state = self._turns.get(message.turn_id)
        if state is None:
            return TurnCancelledReply(
                turn_id=message.turn_id, was_already_cancelled=False
            )
        already = state.cancelled
        state.cancelled = True
        logger.info(
            "[TurnCoordinator] cancelled turn %s (already=%s)",
            message.turn_id,
            already,
        )
        return TurnCancelledReply(
            turn_id=message.turn_id, was_already_cancelled=already
        )

    def _handle_event(self, event: Event) -> bool:
        if event.turn_id is None:
            return True
        state = self._turns.get(event.turn_id)
        if state is None:
            logger.warning(
                "[TurnCoordinator] rejecting event %s: unknown turn %s",
                type(event).__name__,
                event.turn_id,
            )
            return False
        if state.cancelled:
            logger.info(
                "[TurnCoordinator] rejecting stale event %s for cancelled turn %s",
                type(event).__name__,
                event.turn_id,
            )
            return False
        state.events.append(type(event).__name__)
        if self._telemetry_ref is not None:
            self._telemetry_ref.tell(event)
        return True
