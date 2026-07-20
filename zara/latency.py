"""Structured monotonic latency traces for the voice critical path."""

from __future__ import annotations

import json
import logging
import math
import os
import threading
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, Optional, Sequence


SCHEMA_VERSION = 1

VOICE_EVENTS = (
    "audio_frame_received",
    "wake_detected",
    "ack_requested",
    "ack_first_audio",
    "speech_start",
    "speech_end",
    "final_transcript",
    "route_selected",
    "prolog_result",
    "llm_request",
    "llm_first_token",
    "llm_final_token",
    "tts_request",
    "tts_first_chunk",
    "tts_first_playback",
    "tts_final_playback",
    "interruption_detected",
    "cancellation_completed",
)

STAGE_EVENTS = {
    "wake_to_ack_first_audio": ("wake_detected", "ack_first_audio"),
    "speech_end_to_final_transcript": ("speech_end", "final_transcript"),
    "llm_request_to_first_token": ("llm_request", "llm_first_token"),
    "first_text_chunk_to_first_tts_audio": ("llm_first_token", "tts_first_chunk"),
    "barge_in_to_playback_stop": ("interruption_detected", "cancellation_completed"),
    "tts_request_to_first_playback": ("tts_request", "tts_first_playback"),
    "route_to_tts_request": ("route_selected", "tts_request"),
}

BUDGET_STAGE_KEYS = {
    "wake_to_ack_first_audio_p95_ms": "wake_to_ack_first_audio",
    "speech_end_to_final_transcript_p95_ms": "speech_end_to_final_transcript",
    "first_text_chunk_to_first_tts_audio_p95_ms": "first_text_chunk_to_first_tts_audio",
    "barge_in_to_playback_stop_p95_ms": "barge_in_to_playback_stop",
}

_SAFE_METADATA_TYPES = (str, int, float, bool, type(None))
logger = logging.getLogger(__name__)

EVENT_METADATA_KEYS = {
    "audio_frame_received": {"frames"},
    "wake_detected": set(),
    "ack_requested": {"channel"},
    "ack_first_audio": {"provider", "observable_proxy"},
    "speech_start": set(),
    "speech_end": {"reason", "observable_proxy"},
    "final_transcript": {"text_length", "provider"},
    "route_selected": {"route"},
    "prolog_result": {"status"},
    "llm_request": {"provider", "request_index"},
    "llm_first_token": {"provider", "request_index", "buffered_proxy"},
    "llm_final_token": {"provider", "request_index"},
    "tts_request": {"provider", "text_length"},
    "tts_first_chunk": {"provider", "buffered_proxy"},
    "tts_first_playback": {"audio_format", "observable_proxy"},
    "tts_final_playback": {"success", "cancelled"},
    "interruption_detected": set(),
    "cancellation_completed": set(),
}


class LatencyValidationError(ValueError):
    """Raised when a trace has missing or out-of-order events."""


class LatencyBudgetError(AssertionError):
    """Raised when deterministic fixture metrics exceed a configured budget."""


@dataclass(frozen=True)
class LatencyEvent:
    trace_id: str
    event: str
    monotonic_ns: int
    run_kind: str
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        record = {
            "schema_version": SCHEMA_VERSION,
            "trace_id": self.trace_id,
            "event": self.event,
            "monotonic_ns": self.monotonic_ns,
            "run_kind": self.run_kind,
        }
        record.update(self.metadata)
        return record


class JSONLMetricsSink:
    """Append structured trace events to one JSONL file."""

    def __init__(self, path: Path):
        self.path = path
        self._lock = threading.Lock()

    def write(self, events: Iterable[LatencyEvent]) -> None:
        records = [json.dumps(event.to_dict(), sort_keys=True) for event in events]
        if not records:
            return
        with self._lock:
            self.path.parent.mkdir(parents=True, exist_ok=True)
            with self.path.open("a", encoding="utf-8") as output:
                output.write("\n".join(records))
                output.write("\n")


class LatencyTrace:
    """Thread-safe in-memory event trace with explicit safe-point flushing."""

    def __init__(
        self,
        trace_id: Optional[str] = None,
        run_kind: str = "warm",
        clock_ns: Callable[[], int] = time.monotonic_ns,
        sink: Optional[JSONLMetricsSink] = None,
    ):
        if run_kind not in {"cold", "warm"}:
            raise ValueError("run_kind must be 'cold' or 'warm'")
        self.trace_id = trace_id or uuid.uuid4().hex
        self.run_kind = run_kind
        self._clock_ns = clock_ns
        self._sink = sink
        self._events: list[LatencyEvent] = []
        self._flushed = 0
        self._lock = threading.Lock()
        self._flush_lock = threading.Lock()

    @property
    def events(self) -> list[LatencyEvent]:
        with self._lock:
            return list(self._events)

    def record(self, event: str, **metadata: Any) -> LatencyEvent:
        if event not in VOICE_EVENTS:
            raise ValueError(f"Unknown latency event: {event}")
        safe_metadata = _sanitize_metadata(event, metadata)
        with self._lock:
            timestamp = int(self._clock_ns())
            if self._events and timestamp < self._events[-1].monotonic_ns:
                raise LatencyValidationError(
                    f"Event {event} timestamp moved backwards for trace {self.trace_id}"
                )
            record = LatencyEvent(
                trace_id=self.trace_id,
                event=event,
                monotonic_ns=timestamp,
                run_kind=self.run_kind,
                metadata=safe_metadata,
            )
            self._events.append(record)
        return record

    def has_event(self, event: str) -> bool:
        with self._lock:
            return any(record.event == event for record in self._events)

    def flush(self) -> None:
        if self._sink is None:
            return
        with self._flush_lock:
            with self._lock:
                pending = self._events[self._flushed:]
                target = len(self._events)
            try:
                self._sink.write(pending)
            except OSError as error:
                logger.warning(
                    "[Latency] Disabling metrics sink after write failure: %s",
                    error,
                )
                self._sink = None
                return
            with self._lock:
                self._flushed = max(self._flushed, target)

    def validate(self, required_events: Sequence[str]) -> None:
        events = self.events
        names = [event.event for event in events]
        cursor = -1
        for required in required_events:
            try:
                index = names.index(required, cursor + 1)
            except ValueError as error:
                if required in names:
                    raise LatencyValidationError(
                        f"Event {required} is out of order in trace {self.trace_id}"
                    ) from error
                raise LatencyValidationError(
                    f"Missing event {required} in trace {self.trace_id}"
                ) from error
            cursor = index

    def duration_ms(self, start_event: str, end_event: str) -> float:
        events = self.events
        start = next((event for event in events if event.event == start_event), None)
        if start is None:
            raise LatencyValidationError(f"Missing event {start_event}")
        end = next(
            (
                event
                for event in events
                if event.event == end_event and event.monotonic_ns >= start.monotonic_ns
            ),
            None,
        )
        if end is None:
            if any(event.event == end_event for event in events):
                raise LatencyValidationError(
                    f"Event {end_event} occurs before {start_event}"
                )
            raise LatencyValidationError(f"Missing event {end_event}")
        return (end.monotonic_ns - start.monotonic_ns) / 1_000_000.0

    def stage_durations_ms(self) -> dict[str, float]:
        durations: dict[str, float] = {}
        for stage, (start, end) in STAGE_EVENTS.items():
            if self.has_event(start) and self.has_event(end):
                durations[stage] = self.duration_ms(start, end)
        return durations

    def dominant_stage(self) -> Optional[dict[str, Any]]:
        durations = self.stage_durations_ms()
        if not durations:
            return None
        stage = max(durations, key=durations.get)
        return {"stage": stage, "duration_ms": durations[stage]}


def percentile(values: Sequence[float], percentile_value: float) -> float:
    if not values:
        raise ValueError("Cannot calculate a percentile with no values")
    if not 0 <= percentile_value <= 100:
        raise ValueError("Percentile must be between 0 and 100")
    ordered = sorted(float(value) for value in values)
    if len(ordered) == 1:
        return ordered[0]
    rank = (len(ordered) - 1) * percentile_value / 100.0
    lower = math.floor(rank)
    upper = math.ceil(rank)
    if lower == upper:
        return ordered[lower]
    weight = rank - lower
    return ordered[lower] * (1.0 - weight) + ordered[upper] * weight


def build_report(
    traces: Sequence[LatencyTrace],
    required_events: Optional[Sequence[str]] = None,
) -> dict[str, Any]:
    report: dict[str, Any] = {"schema_version": SCHEMA_VERSION, "cohorts": {}}
    if required_events is not None:
        for trace in traces:
            trace.validate(required_events)
    for run_kind in ("cold", "warm"):
        cohort = [trace for trace in traces if trace.run_kind == run_kind]
        if not cohort:
            continue
        stages: dict[str, list[float]] = {}
        turns = []
        for trace in cohort:
            durations = trace.stage_durations_ms()
            for stage, value in durations.items():
                stages.setdefault(stage, []).append(value)
            turns.append(
                {
                    "trace_id": trace.trace_id,
                    "dominant_stage": trace.dominant_stage(),
                    "stages_ms": durations,
                }
            )
        report["cohorts"][run_kind] = {
            "turn_count": len(cohort),
            "stages": {
                stage: {
                    "count": len(values),
                    "p50_ms": percentile(values, 50),
                    "p95_ms": percentile(values, 95),
                    "p99_ms": percentile(values, 99),
                }
                for stage, values in sorted(stages.items())
            },
            "turns": turns,
        }
    return report


def enforce_budgets(
    report: Mapping[str, Any],
    budgets: Mapping[str, float],
    run_kind: str = "warm",
) -> None:
    cohort = report.get("cohorts", {}).get(run_kind, {})
    stages = cohort.get("stages", {})
    failures = []
    for budget_key, stage in BUDGET_STAGE_KEYS.items():
        if budget_key not in budgets:
            continue
        stage_report = stages.get(stage)
        if stage_report is None:
            failures.append(f"{stage}: no samples")
            continue
        observed = float(stage_report["p95_ms"])
        allowed = float(budgets[budget_key])
        if observed > allowed:
            failures.append(f"{stage}: p95 {observed:.2f}ms > {allowed:.2f}ms")
    if failures:
        raise LatencyBudgetError("Latency budgets failed: " + "; ".join(failures))


def metrics_path(config: Mapping[str, Any]) -> Path:
    configured = config.get("metrics_path")
    if configured:
        return Path(os.path.expandvars(os.path.expanduser(str(configured))))
    state_home = os.getenv("XDG_STATE_HOME")
    if state_home:
        return Path(state_home) / "zarathushtra" / "latency.jsonl"
    return Path.home() / ".local" / "state" / "zarathushtra" / "latency.jsonl"


def _sanitize_metadata(event: str, metadata: Mapping[str, Any]) -> dict[str, Any]:
    safe: dict[str, Any] = {}
    allowed = EVENT_METADATA_KEYS[event]
    for key, value in metadata.items():
        if not isinstance(key, str) or not key:
            raise ValueError("Latency metadata keys must be non-empty strings")
        if key not in allowed:
            raise ValueError(f"Latency metadata key is not allowed for {event}: {key}")
        if not isinstance(value, _SAFE_METADATA_TYPES):
            raise ValueError(f"Latency metadata value for {key} must be scalar")
        safe[key] = value
    return safe
