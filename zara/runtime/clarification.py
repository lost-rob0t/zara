"""Clarification dialogue state machine (issue #155, contract section 4).

Owns one open clarification session per (principal, conversation_id). Prompts
derive deterministically from the frame's missing list; follow-ups fill only
the active requested slot; corrections rotate the frame id and record
supersedes in the session bookkeeping (never inside the frame). Raw-utterance
parsing is deterministic dialogue-layer parsing for the active slot; #156
replaces it with the pure ``resolve_frames/4`` resolver.
"""

from __future__ import annotations

import enum
import itertools
import re
import threading
import time
from dataclasses import dataclass
from typing import Callable, Mapping, Optional, Sequence

from .frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    NumberValue,
    RefValue,
    SlotOrigin,
    SlotValue,
    TextValue,
    validate_value,
)

CANCEL_PHRASES = frozenset(
    {
        "never mind",
        "nevermind",
        "forget it",
        "cancel",
        "cancel that",
    }
)

CORRECTION_MARKERS: tuple[str, ...] = (
    "change that to ",
    "change it to ",
    "set it to ",
    "make that ",
    "make it ",
    "actually ",
    "no, ",
    "no ",
)

MAX_DURATION_SECONDS = 7 * 86400

STALE_MESSAGE = "That question isn't active anymore."
CAPACITY_MESSAGE = "I can't start a new question right now."
CANCELLED_MESSAGE = "Cancelled."

_UNIT_FACTORS = {
    "seconds": 1,
    "second": 1,
    "secs": 1,
    "sec": 1,
    "minutes": 60,
    "minute": 60,
    "mins": 60,
    "min": 60,
    "hours": 3600,
    "hour": 3600,
    "hrs": 3600,
    "hr": 3600,
}

_NUMBER_WORDS = {
    "zero": 0,
    "a": 1,
    "an": 1,
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
    "ten": 10,
    "eleven": 11,
    "twelve": 12,
    "thirteen": 13,
    "fourteen": 14,
    "fifteen": 15,
    "sixteen": 16,
    "seventeen": 17,
    "eighteen": 18,
    "nineteen": 19,
    "twenty": 20,
    "thirty": 30,
    "forty": 40,
    "fifty": 50,
    "sixty": 60,
    "seventy": 70,
    "eighty": 80,
    "ninety": 90,
    "hundred": 100,
}

_TOKEN_SPLIT = re.compile(r"[^a-z0-9]+")
_DIGIT_NUMBER = re.compile(r"\d+(?:\.\d+)?")
_ISO_DATETIME = re.compile(
    r"^(\d{4})-(\d{2})-(\d{2})[ t]+(\d{1,2}):(\d{2})(?::(\d{2}))?$"
)


class SlotType(str, enum.Enum):
    TEXT = "text"
    NUMBER = "number"
    DURATION = "duration"
    DATETIME = "datetime"
    REF = "ref"
    BOOLEAN = "boolean"


@dataclass(frozen=True)
class SlotSpec:
    name: str
    slot_type: SlotType
    required: bool = True
    prompt: str = ""
    ref_kind: str = "ref"


@dataclass(frozen=True)
class DialogueTemplate:
    intent_ns: str
    intent_name: str
    specs: tuple[SlotSpec, ...]
    arg_order: tuple[str, ...] = ()

    def spec(self, name: str) -> SlotSpec:
        for candidate in self.specs:
            if candidate.name == name:
                return candidate
        raise KeyError(name)

    def arg_order_names(self) -> tuple[str, ...]:
        if self.arg_order:
            return self.arg_order
        return tuple(spec.name for spec in self.specs)


TIMER_SET_TEMPLATE = DialogueTemplate(
    intent_ns="device",
    intent_name="timer.set",
    specs=(
        SlotSpec("duration", SlotType.DURATION, prompt="For how long?"),
        SlotSpec("label", SlotType.TEXT, required=False, prompt="What should I call it?"),
    ),
    arg_order=("duration", "label"),
)

OPEN_APP_TEMPLATE = DialogueTemplate(
    intent_ns="app",
    intent_name="open",
    specs=(
        SlotSpec("target", SlotType.REF, prompt="Which app?", ref_kind="app_alias"),
    ),
)

TEXT_MESSAGE_TEMPLATE = DialogueTemplate(
    intent_ns="message",
    intent_name="text",
    specs=(
        SlotSpec(
            "contact",
            SlotType.REF,
            prompt="Who should I message?",
            ref_kind="contact",
        ),
        SlotSpec("message", SlotType.TEXT, prompt="What should the message say?"),
    ),
)

SCHEDULE_TODO_TEMPLATE = DialogueTemplate(
    intent_ns="skill",
    intent_name="schedule_todo",
    specs=(SlotSpec("task", SlotType.TEXT, prompt="What is the task?"),),
)


def _tokens(text: str) -> list[str]:
    return [token for token in _TOKEN_SPLIT.split(text.casefold()) if token]


def _parse_number_words(tokens: Sequence[str]) -> Optional[int]:
    if not tokens:
        return None
    total = 0
    saw_tens = False
    saw_units = False
    for token in tokens:
        if token.isdigit():
            if saw_units:
                return None
            total += int(token)
            saw_units = True
            continue
        if token not in _NUMBER_WORDS:
            return None
        value = _NUMBER_WORDS[token]
        if token == "hundred":
            if total == 0:
                total = 100
            else:
                total *= 100
            saw_tens = False
            saw_units = False
        elif value >= 20:
            if saw_tens or saw_units:
                return None
            total += value
            saw_tens = True
        else:
            if saw_units:
                return None
            total += value
            saw_units = True
    return total


def parse_duration(text: str) -> Optional[int]:
    tokens = _tokens(text)
    if len(tokens) < 2:
        return None
    amount = _parse_number_words(tokens[:-1])
    if amount is None:
        return None
    factor = _UNIT_FACTORS.get(tokens[-1])
    if factor is None:
        return None
    seconds = amount * factor
    if seconds < 0 or seconds > MAX_DURATION_SECONDS:
        return None
    return seconds


def parse_number(text: str) -> Optional[NumberValue]:
    tokens = _tokens(text)
    if len(tokens) == 1 and _DIGIT_NUMBER.fullmatch(tokens[0]):
        raw = tokens[0]
        value: Union[int, float] = float(raw) if "." in raw else int(raw)
        return NumberValue(value=value)
    word_number = _parse_number_words(tokens)
    if word_number is not None:
        return NumberValue(value=word_number)
    return None


def parse_boolean(text: str) -> Optional[BoolValue]:
    tokens = _tokens(text)
    if len(tokens) != 1:
        return None
    if tokens[0] in {"yes", "yeah", "yep", "true", "on"}:
        return BoolValue(value=True)
    if tokens[0] in {"no", "nope", "false", "off"}:
        return BoolValue(value=False)
    return None


def parse_datetime(text: str) -> Optional[DateTimeValue]:
    match = _ISO_DATETIME.match(text.strip())
    if match is None:
        return None
    year, month, day, hour, minute, second = match.groups()
    return DateTimeValue(
        year=int(year),
        month=int(month),
        day=int(day),
        hour=int(hour),
        minute=int(minute),
        second=int(second or 0),
    )


def parse_slot_value(
    spec: SlotSpec, text: str, *, max_chars: int
) -> Optional[SlotValue]:
    stripped = text.strip()
    if not stripped or len(stripped) > max_chars:
        return None
    if spec.slot_type is SlotType.DURATION:
        seconds = parse_duration(stripped)
        return DurationValue(seconds=seconds) if seconds is not None else None
    if spec.slot_type is SlotType.NUMBER:
        return parse_number(stripped)
    if spec.slot_type is SlotType.BOOLEAN:
        return parse_boolean(stripped)
    if spec.slot_type is SlotType.DATETIME:
        return parse_datetime(stripped)
    if spec.slot_type is SlotType.REF:
        tokens = _tokens(stripped)
        if 1 <= len(tokens) <= 2:
            return RefValue(kind=spec.ref_kind, id=" ".join(tokens))
        return None
    return TextValue(text=stripped)


def strip_correction_marker(text: str) -> tuple[str, bool]:
    lowered = text.casefold()
    for marker in sorted(CORRECTION_MARKERS, key=len, reverse=True):
        if lowered.startswith(marker) and len(lowered) > len(marker):
            return text[len(marker) :].strip(), True
    return text, False


class SessionState(str, enum.Enum):
    ELICITING = "eliciting"
    READY = "ready"
    CLOSED = "closed"


class SessionCloseReason(str, enum.Enum):
    CANCELLED = "cancelled"
    EXPIRED = "expired"
    EXECUTED = "executed"
    ATTEMPTS_EXHAUSTED = "attempts_exhausted"
    SUPERSEDED_BY_NEW_DIALOGUE = "superseded_by_new_dialogue"
    SUPERSEDED_BY_NEW_COMMAND = "superseded_by_new_command"
    RESTART = "restart"
    SHUTDOWN = "shutdown"


_SUPERSEDED_REASONS = {
    SessionCloseReason.SUPERSEDED_BY_NEW_DIALOGUE,
    SessionCloseReason.SUPERSEDED_BY_NEW_COMMAND,
    SessionCloseReason.RESTART,
    SessionCloseReason.SHUTDOWN,
}
_CANCELLED_REASONS = {
    SessionCloseReason.CANCELLED,
    SessionCloseReason.EXPIRED,
    SessionCloseReason.ATTEMPTS_EXHAUSTED,
}


@dataclass(frozen=True)
class FollowUpOutcome:
    kind: str
    frame: Optional[IntentFrame] = None
    question: Optional[str] = None
    message: str = ""
    frame_id: Optional[str] = None
    supersedes: Optional[str] = None
    session: Optional["ClarificationSession"] = None
    reason: Optional[str] = None


@dataclass(frozen=True)
class OpenOutcome:
    kind: str
    session: Optional["ClarificationSession"]
    question: str = ""
    frame_id: Optional[str] = None
    superseded_frame_id: Optional[str] = None
    message: str = ""


def _stale_outcome(reason: str) -> FollowUpOutcome:
    return FollowUpOutcome(
        kind="stale", message=STALE_MESSAGE, reason=reason
    )


class ClarificationSession:
    """Single-conversation dialogue state. Transitions are pure; the
    coordinator serializes access with its lock."""

    def __init__(
        self,
        *,
        template: DialogueTemplate,
        principal: str,
        conversation_id: str,
        frame_id: str,
        frame: IntentFrame,
        deadline: float,
        max_attempts: int,
        id_factory: Callable[[], str],
    ) -> None:
        self.template = template
        self.principal = principal
        self.conversation_id = conversation_id
        self.frame_id = frame_id
        self.frame = frame
        self.deadline = deadline
        self.max_attempts = max_attempts
        self._id_factory = id_factory
        self.state = SessionState.ELICITING
        self.close_reason: Optional[str] = None
        self.closed_at: Optional[float] = None
        self.attempts = 0
        self.supersedes: Optional[str] = None

    def active_spec(self) -> Optional[SlotSpec]:
        filled = {slot.name for slot in self.frame.slots}
        for spec in self.template.specs:
            if spec.required and spec.name not in filled:
                return spec
        return None

    def question(self) -> Optional[str]:
        if self.state is SessionState.CLOSED:
            return None
        if self.frame.status is FrameStatus.AMBIGUOUS and self.frame.alternatives:
            alternatives = list(self.frame.alternatives)
            joined = ", ".join(alternatives[:-1])
            if len(alternatives) > 1:
                joined = f"{joined} or {alternatives[-1]}"
            else:
                joined = alternatives[0]
            return f"Did you mean: {joined}?"
        spec = self.active_spec()
        if spec is not None:
            return spec.prompt
        if self.frame.status is FrameStatus.INVALID:
            invalid_spec = self.template.spec(self.frame.invalid_slot)
            return invalid_spec.prompt
        return None

    def expired(self, now: float) -> bool:
        return self.state is not SessionState.CLOSED and now > self.deadline

    def close(self, reason: SessionCloseReason | str, *, now: float) -> FollowUpOutcome:
        if isinstance(reason, str):
            reason = SessionCloseReason(reason)
        if self.state is SessionState.CLOSED:
            return _stale_outcome("closed")
        if reason in _CANCELLED_REASONS:
            status = FrameStatus.CANCELLED
        elif reason in _SUPERSEDED_REASONS:
            status = FrameStatus.SUPERSEDED
        else:
            status = self.frame.status
        self.frame = IntentFrame(
            intent_ns=self.frame.intent_ns,
            intent_name=self.frame.intent_name,
            slots=self.frame.slots,
            status=status,
            missing=self.frame.missing,
            alternatives=self.frame.alternatives,
            invalid_slot=self.frame.invalid_slot,
            invalid_reason=self.frame.invalid_reason,
        )
        self.state = SessionState.CLOSED
        self.close_reason = reason.value
        self.closed_at = now
        if reason is SessionCloseReason.CANCELLED:
            return FollowUpOutcome(
                kind="cancelled",
                frame=self.frame,
                message=CANCELLED_MESSAGE,
                frame_id=self.frame_id,
                supersedes=self.supersedes,
                session=self,
            )
        return FollowUpOutcome(
            kind="stale",
            message=STALE_MESSAGE,
            reason=reason.value,
            session=self,
            frame=self.frame,
            frame_id=self.frame_id,
        )

    def register_invalid(self, *, now: float) -> FollowUpOutcome:
        self.attempts += 1
        if self.attempts >= self.max_attempts:
            return self.close(SessionCloseReason.ATTEMPTS_EXHAUSTED, now=now)
        return FollowUpOutcome(
            kind="retry",
            frame=self.frame,
            question=self.question(),
            message=self.question() or "",
            frame_id=self.frame_id,
            supersedes=self.supersedes,
            session=self,
        )

    def submit_value(self, value: SlotValue, *, now: float) -> FollowUpOutcome:
        if self.state is SessionState.CLOSED:
            return _stale_outcome("closed")
        if self.expired(now):
            return self.close(SessionCloseReason.EXPIRED, now=now)
        spec = self.active_spec()
        if spec is None:
            return _stale_outcome("already_complete")
        return self._fill(spec, value, origin=SlotOrigin.FOLLOW_UP, rotate=False, now=now)

    def correct_value(
        self, slot_name: str, value: SlotValue, *, now: float
    ) -> FollowUpOutcome:
        if self.state is SessionState.CLOSED:
            return _stale_outcome("closed")
        if self.expired(now):
            return self.close(SessionCloseReason.EXPIRED, now=now)
        spec = self.template.spec(slot_name)
        reason = validate_value(value)
        if reason is not None:
            return self.register_invalid(now=now)
        return self._fill(spec, value, origin=SlotOrigin.CORRECTION, rotate=True, now=now)

    def _fill(
        self,
        spec: SlotSpec,
        value: SlotValue,
        *,
        origin: SlotOrigin,
        rotate: bool,
        now: float,
    ) -> FollowUpOutcome:
        reason = validate_value(value)
        if reason is not None:
            return self.register_invalid(now=now)

        prior_frame_id = self.frame_id
        if rotate:
            self.frame_id = self._id_factory()
            self.supersedes = prior_frame_id

        replaced = False
        slots: list[FilledSlot] = []
        for slot in self.frame.slots:
            if slot.name == spec.name:
                slots.append(FilledSlot(spec.name, value, origin))
                replaced = True
            else:
                slots.append(slot)
        if not replaced:
            slots.append(FilledSlot(spec.name, value, origin))

        remaining = tuple(
            candidate.name
            for candidate in self.template.specs
            if candidate.required
            and candidate.name not in {slot.name for slot in slots}
        )
        if self.frame.status is FrameStatus.AMBIGUOUS and remaining:
            status = FrameStatus.MISSING
        elif remaining:
            status = FrameStatus.MISSING
        else:
            status = FrameStatus.COMPLETE
        self.frame = IntentFrame(
            intent_ns=self.template.intent_ns,
            intent_name=self.template.intent_name,
            slots=tuple(slots),
            status=status,
            missing=remaining,
        )
        if status is FrameStatus.COMPLETE:
            self.state = SessionState.READY
        kind = "complete" if status is FrameStatus.COMPLETE else "filled"
        question = self.question() if kind == "filled" else None
        return FollowUpOutcome(
            kind=kind,
            frame=self.frame,
            question=question,
            message=question or "",
            frame_id=self.frame_id,
            supersedes=self.supersedes if rotate else None,
            session=self,
        )


def _default_id_factory() -> Callable[[], str]:
    counter = itertools.count()

    def factory() -> str:
        return f"frame-{next(counter):06d}"

    return factory


class ClarificationCoordinator:
    """Principal/conversation-scoped registry of clarification sessions."""

    STALE_MESSAGE = STALE_MESSAGE
    CAPACITY_MESSAGE = CAPACITY_MESSAGE

    def __init__(
        self,
        *,
        clock: Callable[[], float] = time.monotonic,
        id_factory: Optional[Callable[[], str]] = None,
        max_sessions: int = 8,
        session_ttl: float = 120.0,
        max_value_chars: int = 512,
        max_attempts: int = 5,
    ) -> None:
        self._clock = clock
        self._id_factory = id_factory or _default_id_factory()
        self._max_sessions = max(1, int(max_sessions))
        self._session_ttl = max(0.1, float(session_ttl))
        self._max_value_chars = max(1, int(max_value_chars))
        self._max_attempts = max(1, int(max_attempts))
        self._lock = threading.RLock()
        self._sessions: dict[tuple[str, str], ClarificationSession] = {}

    def open(
        self,
        template: DialogueTemplate,
        *,
        principal: str,
        conversation_id: str,
        prefilled: Optional[Mapping[str, SlotValue]] = None,
        alternatives: Optional[Sequence[str]] = None,
        invalid_slot: Optional[str] = None,
        invalid_reason: Optional[str] = None,
        now: Optional[float] = None,
    ) -> OpenOutcome:
        with self._lock:
            now = self._clock() if now is None else now
            key = (principal, conversation_id)
            self._prune_locked(now)

            superseded_frame_id = None
            existing = self._sessions.get(key)
            if existing is not None and existing.state is not SessionState.CLOSED:
                existing.close(
                    SessionCloseReason.SUPERSEDED_BY_NEW_DIALOGUE, now=now
                )
                superseded_frame_id = existing.frame_id

            open_count = sum(
                1
                for session in self._sessions.values()
                if session.state is not SessionState.CLOSED
            )
            if open_count >= self._max_sessions:
                return OpenOutcome(
                    kind="capacity",
                    session=None,
                    message=self.CAPACITY_MESSAGE,
                )

            prefills = dict(prefilled or {})
            slots: list[FilledSlot] = []
            for spec in template.specs:
                if spec.name in prefills:
                    value = prefills.pop(spec.name)
                    reason = validate_value(value)
                    if reason is not None:
                        raise ValueError(
                            f"invalid prefilled slot {spec.name!r}: {reason}"
                        )
                    slots.append(FilledSlot(spec.name, value, SlotOrigin.UTTERANCE))
            if prefills:
                unknown = sorted(prefills)
                raise ValueError(f"unknown prefilled slots: {unknown}")

            if alternatives:
                status = FrameStatus.AMBIGUOUS
                remaining = tuple(
                    spec.name for spec in template.specs if spec.required
                )
                missing: tuple[str, ...] = tuple(
                    name for name in remaining if name not in {s.name for s in slots}
                )
            elif invalid_slot is not None:
                status = FrameStatus.INVALID
                missing = ()
            else:
                status = FrameStatus.MISSING
                missing = tuple(
                    spec.name
                    for spec in template.specs
                    if spec.required and spec.name not in {s.name for s in slots}
                )

            frame = IntentFrame(
                intent_ns=template.intent_ns,
                intent_name=template.intent_name,
                slots=tuple(slots),
                status=status,
                missing=missing,
                alternatives=tuple(alternatives or ()),
                invalid_slot=invalid_slot,
                invalid_reason=invalid_reason,
            )
            session = ClarificationSession(
                template=template,
                principal=principal,
                conversation_id=conversation_id,
                frame_id=self._id_factory(),
                frame=frame,
                deadline=now + self._session_ttl,
                max_attempts=self._max_attempts,
                id_factory=self._id_factory,
            )
            self._sessions[key] = session
            return OpenOutcome(
                kind="opened",
                session=session,
                question=session.question() or "",
                frame_id=session.frame_id,
                superseded_frame_id=superseded_frame_id,
            )

    def submit_follow_up(
        self,
        text: str,
        *,
        principal: str,
        conversation_id: str,
        now: Optional[float] = None,
    ) -> FollowUpOutcome:
        with self._lock:
            now = self._clock() if now is None else now
            session = self._sessions.get((principal, conversation_id))
            if session is None or session.state is SessionState.CLOSED:
                return _stale_outcome("no_active_dialogue")
            if session.expired(now):
                return session.close(SessionCloseReason.EXPIRED, now=now)

            normalized = " ".join(text.split()).casefold()
            if normalized in CANCEL_PHRASES:
                return session.close(SessionCloseReason.CANCELLED, now=now)

            stripped, marker = strip_correction_marker(text.strip())

            if session.state is SessionState.READY:
                if not marker:
                    return _stale_outcome("already_complete")
                return self._correct_filled_slots(session, stripped, now=now)

            active = session.active_spec()
            if marker and active is not None:
                value = parse_slot_value(active, stripped, max_chars=self._max_value_chars)
                if value is not None:
                    return session._fill(
                        active,
                        value,
                        origin=SlotOrigin.CORRECTION,
                        rotate=True,
                        now=now,
                    )
                corrected = self._correct_filled_slots(session, stripped, now=now)
                if corrected.kind != "stale":
                    return corrected

            if active is None:
                return _stale_outcome("already_complete")
            value = parse_slot_value(active, stripped, max_chars=self._max_value_chars)
            if value is None:
                return session.register_invalid(now=now)
            return session._fill(
                active, value, origin=SlotOrigin.FOLLOW_UP, rotate=False, now=now
            )

    def _correct_filled_slots(
        self, session: ClarificationSession, stripped: str, *, now: float
    ) -> FollowUpOutcome:
        filled_names = {slot.name for slot in session.frame.slots}
        for spec in session.template.specs:
            if spec.name not in filled_names or spec.slot_type is SlotType.TEXT:
                continue
            value = parse_slot_value(spec, stripped, max_chars=self._max_value_chars)
            if value is not None:
                return session.correct_value(spec.name, value, now=now)
        return _stale_outcome("already_complete")

    def submit_correction(
        self,
        slot_name: str,
        value: SlotValue,
        *,
        principal: str,
        conversation_id: str,
        now: Optional[float] = None,
    ) -> FollowUpOutcome:
        with self._lock:
            now = self._clock() if now is None else now
            session = self._sessions.get((principal, conversation_id))
            if session is None or session.state is SessionState.CLOSED:
                return _stale_outcome("no_active_dialogue")
            if slot_name not in {spec.name for spec in session.template.specs}:
                raise ValueError(f"unknown slot {slot_name!r}")
            return session.correct_value(slot_name, value, now=now)

    def cancel(
        self,
        *,
        principal: str,
        conversation_id: str,
        reason: SessionCloseReason = SessionCloseReason.CANCELLED,
        now: Optional[float] = None,
    ) -> FollowUpOutcome:
        with self._lock:
            now = self._clock() if now is None else now
            session = self._sessions.get((principal, conversation_id))
            if session is None or session.state is SessionState.CLOSED:
                return _stale_outcome("no_active_dialogue")
            if session.expired(now):
                return session.close(SessionCloseReason.EXPIRED, now=now)
            return session.close(reason, now=now)

    def finish(
        self, *, principal: str, conversation_id: str, now: Optional[float] = None
    ) -> bool:
        with self._lock:
            now = self._clock() if now is None else now
            session = self._sessions.get((principal, conversation_id))
            if session is None or session.state is not SessionState.READY:
                return False
            session.close(SessionCloseReason.EXECUTED, now=now)
            return True

    def expire(self, *, now: Optional[float] = None) -> tuple[str, ...]:
        with self._lock:
            now = self._clock() if now is None else now
            closed: list[str] = []
            for session in self._sessions.values():
                if session.expired(now):
                    session.close(SessionCloseReason.EXPIRED, now=now)
                    closed.append(session.frame_id)
            self._prune_locked(now)
            return tuple(closed)

    def drop_all(
        self,
        *,
        reason: SessionCloseReason = SessionCloseReason.RESTART,
        now: Optional[float] = None,
    ) -> int:
        with self._lock:
            now = self._clock() if now is None else now
            dropped = 0
            for session in self._sessions.values():
                if session.state is not SessionState.CLOSED:
                    session.close(reason, now=now)
                    dropped += 1
            return dropped

    def session_for(
        self, principal: str, conversation_id: str
    ) -> Optional[ClarificationSession]:
        with self._lock:
            return self._sessions.get((principal, conversation_id))

    def active_question(
        self, principal: str, conversation_id: str
    ) -> Optional[str]:
        with self._lock:
            session = self._sessions.get((principal, conversation_id))
            if session is None or session.state is SessionState.CLOSED:
                return None
            return session.question()

    def _prune_locked(self, now: float) -> None:
        expired_keys = [
            key
            for key, session in self._sessions.items()
            if session.state is SessionState.CLOSED
            and session.closed_at is not None
            and now - session.closed_at > self._session_ttl
        ]
        for key in expired_keys:
            del self._sessions[key]


__all__ = [
    "CANCEL_PHRASES",
    "ClarificationCoordinator",
    "ClarificationSession",
    "DialogueTemplate",
    "FollowUpOutcome",
    "OpenOutcome",
    "OPEN_APP_TEMPLATE",
    "SCHEDULE_TODO_TEMPLATE",
    "SessionCloseReason",
    "SessionState",
    "SlotSpec",
    "SlotType",
    "TEXT_MESSAGE_TEMPLATE",
    "TIMER_SET_TEMPLATE",
    "parse_slot_value",
    "strip_correction_marker",
]
