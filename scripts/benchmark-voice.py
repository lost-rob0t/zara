#!/usr/bin/env python3
"""Deterministic voice latency benchmark for ZARA-022.

This benchmark exercises the latency trace/reporting contract with fixture
providers and a fake monotonic clock. It never opens audio hardware, contacts a
provider, downloads a model, or requires credentials.
"""

from __future__ import annotations

import argparse
import json
import sys
import tomllib
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT))

from zara.config import DEFAULT_CONFIG_TOML
from zara.latency import (
    JSONLMetricsSink,
    LatencyBudgetError,
    LatencyTrace,
    build_report,
    enforce_budgets,
)

FIXTURE_REQUIRED_EVENTS = (
    "audio_frame_received",
    "wake_detected",
    "ack_requested",
    "ack_first_audio",
    "speech_start",
    "speech_end",
    "final_transcript",
    "prolog_result",
    "route_selected",
    "llm_request",
    "llm_first_token",
    "llm_final_token",
    "tts_request",
    "tts_first_chunk",
    "tts_first_playback",
    "interruption_detected",
    "cancellation_completed",
    "tts_final_playback",
)


class FixtureClock:
    def __init__(self, start_ns: int = 0):
        self.now_ns = start_ns

    def __call__(self) -> int:
        return self.now_ns

    def advance_ms(self, milliseconds: float) -> None:
        self.now_ns += int(milliseconds * 1_000_000)


def build_fixture_trace(
    run_kind: str,
    index: int,
    sink: JSONLMetricsSink,
) -> LatencyTrace:
    cold_extra = 100 if run_kind == "cold" else 0
    jitter = index % 5
    clock = FixtureClock(start_ns=index * 10_000_000_000)
    trace = LatencyTrace(
        trace_id=f"fixture-{run_kind}-{index:03d}",
        run_kind=run_kind,
        clock_ns=clock,
        sink=sink,
    )

    trace.record("audio_frame_received", frames=160)
    clock.advance_ms(5)
    trace.record("wake_detected")
    trace.record("ack_requested", channel="fixture")
    clock.advance_ms(120 + cold_extra + jitter)
    trace.record(
        "ack_first_audio",
        provider="fixture-ack",
        observable_proxy=False,
    )

    clock.advance_ms(25)
    trace.record("speech_start")
    clock.advance_ms(500)
    trace.record("speech_end", reason="fixture")
    clock.advance_ms(300 + cold_extra + jitter * 2)
    trace.record("final_transcript", text_length=12, provider="fixture-stt")

    clock.advance_ms(15)
    trace.record("prolog_result", status="ask")
    trace.record("route_selected", route="agent")
    clock.advance_ms(5)
    trace.record("llm_request", provider="fixture-llm", request_index=0)
    clock.advance_ms(180 + cold_extra + jitter)
    trace.record(
        "llm_first_token",
        provider="fixture-llm",
        request_index=0,
        buffered_proxy=False,
    )
    clock.advance_ms(50)
    trace.record("llm_final_token", provider="fixture-llm", request_index=0)

    clock.advance_ms(10)
    trace.record("tts_request", provider="fixture-tts", text_length=24)
    clock.advance_ms(220 + cold_extra + jitter)
    trace.record("tts_first_chunk", provider="fixture-tts", buffered_proxy=False)
    clock.advance_ms(10)
    trace.record("tts_first_playback", audio_format="wav")

    clock.advance_ms(100)
    trace.record("interruption_detected")
    clock.advance_ms(70 + jitter)
    trace.record("cancellation_completed")
    trace.record("tts_final_playback", success=False, cancelled=True)
    trace.flush()
    return trace


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Deterministic voice latency benchmark")
    parser.add_argument("--jsonl", type=Path, required=True, help="Event JSONL output")
    parser.add_argument("--report", type=Path, required=True, help="Percentile JSON report")
    parser.add_argument("--cold-runs", type=int, default=3)
    parser.add_argument("--warm-runs", type=int, default=20)
    parser.add_argument("--no-enforce", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.cold_runs < 1 or args.warm_runs < 1:
        raise SystemExit("cold-runs and warm-runs must both be positive")

    args.jsonl.parent.mkdir(parents=True, exist_ok=True)
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.jsonl.unlink(missing_ok=True)
    sink = JSONLMetricsSink(args.jsonl)
    traces = [
        build_fixture_trace("cold", index, sink)
        for index in range(args.cold_runs)
    ]
    traces.extend(
        build_fixture_trace("warm", index, sink)
        for index in range(args.warm_runs)
    )

    report = build_report(traces, required_events=FIXTURE_REQUIRED_EVENTS)
    budgets = tomllib.loads(DEFAULT_CONFIG_TOML)["latency"]["budgets"]
    report["budgets_ms"] = budgets
    budget_error = None
    if args.no_enforce:
        report["budget_status"] = {"status": "not_enforced"}
    else:
        try:
            enforce_budgets(report, budgets, run_kind="warm")
        except LatencyBudgetError as error:
            budget_error = error
            report["budget_status"] = {"status": "failed", "detail": str(error)}
        else:
            report["budget_status"] = {"status": "passed"}
    args.report.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")

    for run_kind, cohort in report["cohorts"].items():
        print(f"{run_kind}: {cohort['turn_count']} turns")
        for stage, metrics in cohort["stages"].items():
            print(
                f"  {stage}: p50={metrics['p50_ms']:.2f}ms "
                f"p95={metrics['p95_ms']:.2f}ms p99={metrics['p99_ms']:.2f}ms"
            )
    print(f"JSONL: {args.jsonl}")
    print(f"Report: {args.report}")
    if budget_error is not None:
        print(str(budget_error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
