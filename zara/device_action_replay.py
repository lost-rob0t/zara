"""Protocol-owned replay semantics for ZARA/1 device actions."""

from __future__ import annotations

from enum import Enum


DEVICE_ACTION_REPLAY_WINDOW = 256


class ReplayDisposition(str, Enum):
    FRESH = "fresh"
    DUPLICATE = "duplicate"
    EXPIRED = "expired"


def _positive_int(name: str, value: object) -> int:
    if type(value) is not int or value <= 0:
        raise ValueError(f"{name} must be a positive integer")
    return value


def _nonnegative_int(name: str, value: object) -> int:
    if type(value) is not int or value < 0:
        raise ValueError(f"{name} must be a non-negative integer")
    return value


def classify_action_seq(
    action_seq: object,
    high_water: object,
    *,
    window: object = DEVICE_ACTION_REPLAY_WINDOW,
) -> ReplayDisposition:
    """Classify one session-scoped action sequence without retaining IDs.

    Fresh values are strictly greater than the receiver's high-water mark.
    Older values inside the retained horizon are duplicates; older values at
    or beyond the horizon are expired. Callers advance their high-water mark
    only after accepting a FRESH value.
    """

    seq = _positive_int("action_seq", action_seq)
    high = _nonnegative_int("high_water", high_water)
    horizon = _positive_int("window", window)

    if seq > high:
        return ReplayDisposition.FRESH
    if seq <= high - horizon:
        return ReplayDisposition.EXPIRED
    return ReplayDisposition.DUPLICATE
