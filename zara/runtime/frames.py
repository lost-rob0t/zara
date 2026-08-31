"""IntentFrame v1 Python mirror of docs/intentframe-contract.md.

Frames are pure values. Envelope metadata (frame ids, supersedes, principal,
turn and conversation ids) never lives inside a frame; clarification sessions
own the envelope bookkeeping (rage/154-intentframe-design.org, D2).
"""

from __future__ import annotations

import enum
import math
from dataclasses import dataclass
from typing import Optional, Union


class SlotOrigin(str, enum.Enum):
    UTTERANCE = "utterance"
    FOLLOW_UP = "follow_up"
    CONTEXT = "context"
    DEFAULT = "default"
    CORRECTION = "correction"


class FrameStatus(str, enum.Enum):
    COMPLETE = "complete"
    MISSING = "missing"
    AMBIGUOUS = "ambiguous"
    INVALID = "invalid"
    CANCELLED = "cancelled"
    SUPERSEDED = "superseded"


@dataclass(frozen=True)
class TextValue:
    text: str


@dataclass(frozen=True)
class NumberValue:
    value: Union[int, float]


@dataclass(frozen=True)
class DurationValue:
    seconds: int


@dataclass(frozen=True)
class DateTimeValue:
    year: int
    month: int
    day: int
    hour: int
    minute: int
    second: int


@dataclass(frozen=True)
class RefValue:
    kind: str
    id: str


@dataclass(frozen=True)
class BoolValue:
    value: bool


SlotValue = Union[
    TextValue,
    NumberValue,
    DurationValue,
    DateTimeValue,
    RefValue,
    BoolValue,
]


def validate_value(value: SlotValue) -> Optional[str]:
    """Return a typed rejection reason for malformed slot values, else None."""
    if isinstance(value, TextValue):
        if not isinstance(value.text, str) or not value.text.strip():
            return "empty"
        return None
    if isinstance(value, NumberValue):
        if isinstance(value.value, float) and not math.isfinite(value.value):
            return "non_finite"
        if not isinstance(value.value, (int, float)) or isinstance(value.value, bool):
            return "not_a_number"
        return None
    if isinstance(value, DurationValue):
        if not isinstance(value.seconds, int) or isinstance(value.seconds, bool):
            return "not_an_integer"
        if value.seconds < 0:
            return "negative"
        return None
    if isinstance(value, DateTimeValue):
        if not 1 <= value.month <= 12:
            return "month_range"
        if not 1 <= value.day <= 31:
            return "day_range"
        if not 0 <= value.hour <= 23:
            return "hour_range"
        if not 0 <= value.minute <= 59:
            return "minute_range"
        if not 0 <= value.second <= 59:
            return "second_range"
        return None
    if isinstance(value, RefValue):
        if not isinstance(value.kind, str) or not value.kind:
            return "empty_kind"
        if not isinstance(value.id, str) or not value.id.strip():
            return "empty_id"
        return None
    if isinstance(value, BoolValue):
        if not isinstance(value.value, bool):
            return "not_a_boolean"
        return None
    return "unknown_type"


@dataclass(frozen=True)
class FilledSlot:
    name: str
    value: SlotValue
    origin: SlotOrigin


@dataclass(frozen=True)
class IntentFrame:
    intent_ns: str
    intent_name: str
    slots: tuple[FilledSlot, ...] = ()
    status: FrameStatus = FrameStatus.MISSING
    missing: tuple[str, ...] = ()
    alternatives: tuple[str, ...] = ()
    invalid_slot: Optional[str] = None
    invalid_reason: Optional[str] = None

    def slot_value(self, name: str) -> Optional[SlotValue]:
        for slot in self.slots:
            if slot.name == name:
                return slot.value
        return None

    def origin_of(self, name: str) -> Optional[SlotOrigin]:
        for slot in self.slots:
            if slot.name == name:
                return slot.origin
        return None


__all__ = [
    "BoolValue",
    "DateTimeValue",
    "DurationValue",
    "FilledSlot",
    "FrameStatus",
    "IntentFrame",
    "NumberValue",
    "RefValue",
    "SlotOrigin",
    "SlotValue",
    "TextValue",
    "validate_value",
]
