"""Deterministic derived-state reducer for Org automation run receipts.

Ordinary Org remains the canonical automation definition.  This module consumes
bounded events emitted by Zara's canonical execution/runtime owner and derives an
inspectable run projection.  It does not execute commands, schedule work, grant
capabilities, approve tools, persist events, or own retry policy.

The reducer is intentionally strict: event identity must match the correlated
:class:`AutomationRunPlan`, sequence numbers are contiguous, retry backoff is
explicit, and exactly one terminal event is permitted.  Runtime/provider payloads
never cross this boundary; terminal data is represented only by bounded host-owned
references and typed error identifiers.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Optional

from .org_automation_run import AutomationRunPlan


_MAX_EVENTS = 512
_MAX_ATTEMPT = 64
_MAX_REF_LENGTH = 256
_MAX_ERROR_KIND_LENGTH = 96
_TOKEN_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.:/@-]*$")
_RESULT_REF_RE = re.compile(r"^result:[A-Za-z0-9][A-Za-z0-9_.:/@-]*$")
_ALLOWED_KINDS = frozenset(
    {
        "accepted",
        "started",
        "retry_scheduled",
        "completed",
        "cancelled",
        "failed",
    }
)


class AutomationReceiptError(ValueError):
    """A run-event stream violates the bounded automation receipt contract."""


@dataclass(frozen=True)
class AutomationRunEvent:
    """One inert event emitted by the canonical automation execution owner.

    ``result_ref`` is a host-owned bounded reference in the ``result:``
    namespace, never a raw provider/tool result. ``error_kind`` is a typed
    identifier, never a stack trace/message.
    """

    sequence: int
    kind: str
    run_id: str
    replay_key: str
    definition_hash: str
    timestamp_unix_ms: int
    attempt: int = 0
    retry_at_unix_ms: Optional[int] = None
    result_ref: str = ""
    error_kind: str = ""


@dataclass(frozen=True)
class AutomationRunProjection:
    """Rebuildable derived state for one correlated automation run."""

    automation_id: str
    run_id: str
    replay_key: str
    definition_hash: str
    status: str
    attempts: int
    last_sequence: Optional[int]
    terminal_sequence: Optional[int]
    retry_at_unix_ms: Optional[int]
    result_ref: str
    error_kind: str


def reduce_automation_run(
    plan: AutomationRunPlan,
    events: tuple[AutomationRunEvent, ...],
) -> AutomationRunProjection:
    """Reduce bounded run events into deterministic inspectable derived state.

    Persistence and exactly-once compare-and-set remain responsibilities of the
    canonical run/event owner.  This reducer defines the replay invariant those
    stores must preserve: a single identity, contiguous sequence, and no event
    after the first terminal receipt.
    """

    if not isinstance(plan, AutomationRunPlan):
        raise AutomationReceiptError("plan must be an AutomationRunPlan")
    if not isinstance(events, tuple):
        raise AutomationReceiptError("events must be an immutable tuple")
    if len(events) > _MAX_EVENTS:
        raise AutomationReceiptError(f"event stream exceeds {_MAX_EVENTS} entries")

    status = "planned"
    attempts = 0
    last_sequence: Optional[int] = None
    terminal_sequence: Optional[int] = None
    retry_at_unix_ms: Optional[int] = None
    result_ref = ""
    error_kind = ""
    last_timestamp = 0

    for expected_sequence, raw_event in enumerate(events):
        event = _validate_event(raw_event)
        if event.sequence != expected_sequence:
            raise AutomationReceiptError(
                f"event sequence mismatch: expected {expected_sequence}, got {event.sequence}"
            )
        if (
            event.run_id != plan.run_id
            or event.replay_key != plan.replay_key
            or event.definition_hash != plan.definition_hash
        ):
            raise AutomationReceiptError("event identity does not match run plan")
        if event.timestamp_unix_ms < last_timestamp:
            raise AutomationReceiptError("event timestamps must be monotonic")
        if terminal_sequence is not None:
            raise AutomationReceiptError("terminal receipt already recorded; later event rejected")

        if event.kind == "accepted":
            if status != "planned" or event.attempt != 0:
                raise AutomationReceiptError("accepted event is only valid before execution")
            _require_empty_payload(event, "accepted")
            status = "accepted"

        elif event.kind == "started":
            if status not in {"planned", "accepted", "retry_wait"}:
                raise AutomationReceiptError(f"started event is invalid from state {status}")
            expected_attempt = 1 if attempts == 0 else attempts + 1
            if event.attempt != expected_attempt:
                raise AutomationReceiptError(
                    f"started attempt mismatch: expected {expected_attempt}, got {event.attempt}"
                )
            if status == "retry_wait":
                assert retry_at_unix_ms is not None
                if event.timestamp_unix_ms < retry_at_unix_ms:
                    raise AutomationReceiptError("retry attempt started before explicit backoff elapsed")
            _require_empty_payload(event, "started")
            attempts = event.attempt
            retry_at_unix_ms = None
            error_kind = ""
            status = "running"

        elif event.kind == "retry_scheduled":
            if status != "running" or attempts == 0 or event.attempt != attempts:
                raise AutomationReceiptError("retry event does not match the active attempt")
            if event.retry_at_unix_ms is None or event.retry_at_unix_ms <= event.timestamp_unix_ms:
                raise AutomationReceiptError("retry event requires future explicit backoff")
            if not event.error_kind:
                raise AutomationReceiptError("retry event requires a typed error kind")
            if event.result_ref:
                raise AutomationReceiptError("retry event cannot carry a result reference")
            retry_at_unix_ms = event.retry_at_unix_ms
            error_kind = event.error_kind
            status = "retry_wait"

        elif event.kind == "completed":
            if status != "running" or attempts == 0:
                raise AutomationReceiptError("completed event requires a started active attempt")
            _validate_terminal_attempt(event, attempts)
            if not event.result_ref:
                raise AutomationReceiptError("completed event requires a bounded result reference")
            _bounded_result_ref(event.result_ref)
            if event.error_kind:
                raise AutomationReceiptError("completed event cannot carry an error kind")
            if event.retry_at_unix_ms is not None:
                raise AutomationReceiptError("completed event cannot carry retry backoff")
            status = "completed"
            terminal_sequence = event.sequence
            retry_at_unix_ms = None
            result_ref = event.result_ref
            error_kind = ""

        elif event.kind == "cancelled":
            if status not in {"planned", "accepted", "running", "retry_wait"}:
                raise AutomationReceiptError(f"cancelled event is invalid from state {status}")
            _validate_terminal_attempt(event, attempts)
            _require_empty_payload(event, "cancelled")
            status = "cancelled"
            terminal_sequence = event.sequence
            retry_at_unix_ms = None
            result_ref = ""
            error_kind = ""

        elif event.kind == "failed":
            if status not in {"planned", "accepted", "running", "retry_wait"}:
                raise AutomationReceiptError(f"failed event is invalid from state {status}")
            _validate_terminal_attempt(event, attempts)
            if not event.error_kind:
                raise AutomationReceiptError("failed event requires a typed error kind")
            if event.result_ref:
                raise AutomationReceiptError("failed event cannot carry a result reference")
            if event.retry_at_unix_ms is not None:
                raise AutomationReceiptError("failed event cannot carry retry backoff")
            status = "failed"
            terminal_sequence = event.sequence
            retry_at_unix_ms = None
            result_ref = ""
            error_kind = event.error_kind

        else:  # pragma: no cover - _validate_event owns the closed vocabulary.
            raise AutomationReceiptError(f"unsupported event kind: {event.kind}")

        last_sequence = event.sequence
        last_timestamp = event.timestamp_unix_ms

    return AutomationRunProjection(
        automation_id=plan.automation_id,
        run_id=plan.run_id,
        replay_key=plan.replay_key,
        definition_hash=plan.definition_hash,
        status=status,
        attempts=attempts,
        last_sequence=last_sequence,
        terminal_sequence=terminal_sequence,
        retry_at_unix_ms=retry_at_unix_ms,
        result_ref=result_ref,
        error_kind=error_kind,
    )


def _validate_event(event: object) -> AutomationRunEvent:
    if not isinstance(event, AutomationRunEvent):
        raise AutomationReceiptError("event must be an AutomationRunEvent")
    if isinstance(event.sequence, bool) or not isinstance(event.sequence, int) or event.sequence < 0:
        raise AutomationReceiptError("event sequence must be a non-negative integer")
    if event.kind not in _ALLOWED_KINDS:
        raise AutomationReceiptError(f"unknown event kind: {event.kind!r}")
    _bounded_token(event.run_id, "run_id", _MAX_REF_LENGTH)
    _bounded_token(event.replay_key, "replay_key", _MAX_REF_LENGTH)
    _bounded_token(event.definition_hash, "definition_hash", _MAX_REF_LENGTH)
    if (
        isinstance(event.timestamp_unix_ms, bool)
        or not isinstance(event.timestamp_unix_ms, int)
        or event.timestamp_unix_ms <= 0
    ):
        raise AutomationReceiptError("event timestamp must be a positive integer")
    if (
        isinstance(event.attempt, bool)
        or not isinstance(event.attempt, int)
        or event.attempt < 0
        or event.attempt > _MAX_ATTEMPT
    ):
        raise AutomationReceiptError(f"attempt must be an integer in 0..{_MAX_ATTEMPT}")
    if event.retry_at_unix_ms is not None and (
        isinstance(event.retry_at_unix_ms, bool)
        or not isinstance(event.retry_at_unix_ms, int)
        or event.retry_at_unix_ms <= 0
    ):
        raise AutomationReceiptError("retry timestamp must be a positive integer")
    if event.result_ref:
        _bounded_token(event.result_ref, "result_ref", _MAX_REF_LENGTH)
    if event.error_kind:
        _bounded_token(event.error_kind, "error_kind", _MAX_ERROR_KIND_LENGTH)
    return event


def _validate_terminal_attempt(event: AutomationRunEvent, attempts: int) -> None:
    expected = attempts if attempts else 0
    if event.attempt != expected:
        raise AutomationReceiptError(
            f"terminal attempt mismatch: expected {expected}, got {event.attempt}"
        )


def _require_empty_payload(event: AutomationRunEvent, kind: str) -> None:
    if event.retry_at_unix_ms is not None or event.result_ref or event.error_kind:
        raise AutomationReceiptError(f"{kind} event carries an unexpected payload")


def _bounded_result_ref(value: str) -> str:
    if len(value) > _MAX_REF_LENGTH or not _RESULT_REF_RE.fullmatch(value):
        raise AutomationReceiptError("result_ref must be a bounded host-owned result: reference")
    return value


def _bounded_token(value: object, name: str, limit: int) -> str:
    if not isinstance(value, str) or not value or len(value) > limit:
        raise AutomationReceiptError(f"{name} must be a non-empty bounded string")
    if not _TOKEN_RE.fullmatch(value):
        raise AutomationReceiptError(f"{name} must be a bounded opaque identifier")
    return value


__all__ = [
    "AutomationReceiptError",
    "AutomationRunEvent",
    "AutomationRunProjection",
    "reduce_automation_run",
]
