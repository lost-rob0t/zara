from __future__ import annotations

import asyncio
import json
import pathlib
import subprocess
import sys

import pytest
from langchain_core.messages import AIMessage
from unittest.mock import AsyncMock, MagicMock

from zara.config import ConfigError, ZaraConfig
from zara.agent.graph import create_agent_node
from zara.latency import (
    JSONLMetricsSink,
    LatencyBudgetError,
    LatencyTrace,
    LatencyValidationError,
    build_report,
    enforce_budgets,
    metrics_path,
    percentile,
)
from zara.prolog_engine import IntentResult
from zara.tts import PlaybackResult, SynthesisResult
from zara.wake import WakeWordListener


ROOT = pathlib.Path(__file__).resolve().parent.parent


class FakeClock:
    def __init__(self):
        self.value = 0

    def __call__(self):
        return self.value

    def advance_ms(self, milliseconds):
        self.value += int(milliseconds * 1_000_000)


def build_trace(run_kind="warm", trace_id="trace-1"):
    clock = FakeClock()
    trace = LatencyTrace(trace_id=trace_id, run_kind=run_kind, clock_ns=clock)
    return trace, clock


def test_fake_clock_duration_calculation():
    trace, clock = build_trace()
    trace.record("speech_end")
    clock.advance_ms(321.5)
    trace.record("final_transcript", text_length=8)

    assert trace.duration_ms("speech_end", "final_transcript") == 321.5


def test_missing_event_fails_validation():
    trace, _ = build_trace()
    trace.record("speech_end")

    with pytest.raises(LatencyValidationError, match="Missing event final_transcript"):
        trace.validate(["speech_end", "final_transcript"])


def test_out_of_order_event_fails_validation():
    trace, clock = build_trace()
    trace.record("final_transcript")
    clock.advance_ms(1)
    trace.record("speech_end")

    with pytest.raises(LatencyValidationError, match="out of order"):
        trace.validate(["speech_end", "final_transcript"])


def test_backwards_monotonic_clock_is_rejected():
    trace, clock = build_trace()
    clock.value = 10
    trace.record("speech_start")
    clock.value = 9

    with pytest.raises(LatencyValidationError, match="moved backwards"):
        trace.record("speech_end")


def test_jsonl_sink_writes_structured_safe_records(tmp_path):
    path = tmp_path / "latency.jsonl"
    sink = JSONLMetricsSink(path)
    trace, clock = build_trace()
    trace._sink = sink
    trace.record("final_transcript", text_length=42, provider="fixture")
    trace.flush()

    record = json.loads(path.read_text().strip())
    assert record == {
        "event": "final_transcript",
        "monotonic_ns": 0,
        "provider": "fixture",
        "run_kind": "warm",
        "schema_version": 1,
        "text_length": 42,
        "trace_id": "trace-1",
    }
    assert "text" not in record


def test_metrics_sink_failure_does_not_break_trace(tmp_path, caplog):
    sink = JSONLMetricsSink(tmp_path)  # A directory cannot be opened for append.
    trace = LatencyTrace(trace_id="trace", sink=sink)
    trace.record("speech_start")

    trace.flush()
    trace.record("speech_end", reason="fixture", observable_proxy=False)
    trace.flush()

    assert [event.event for event in trace.events] == ["speech_start", "speech_end"]
    assert "Disabling metrics sink" in caplog.text


@pytest.mark.parametrize("key", ["text", "prompt", "audio", "api_key", "secret", "token"])
def test_sensitive_metadata_keys_are_rejected(key):
    trace, _ = build_trace()
    with pytest.raises(ValueError, match="not allowed"):
        trace.record("final_transcript", **{key: "sensitive"})


def test_flush_only_appends_new_events(tmp_path):
    path = tmp_path / "latency.jsonl"
    sink = JSONLMetricsSink(path)
    clock = FakeClock()
    trace = LatencyTrace(trace_id="trace", clock_ns=clock, sink=sink)
    trace.record("speech_start")
    trace.flush()
    clock.advance_ms(1)
    trace.record("speech_end")
    trace.flush()
    trace.flush()

    records = [json.loads(line) for line in path.read_text().splitlines()]
    assert [record["event"] for record in records] == ["speech_start", "speech_end"]


def test_percentile_reporting():
    assert percentile([1, 2, 3, 4], 50) == 2.5
    assert percentile([1, 2, 3, 4], 95) == pytest.approx(3.85)
    assert percentile([5], 99) == 5


def test_report_separates_cold_and_warm_and_identifies_dominant_stage():
    traces = []
    for run_kind, value in (("cold", 500), ("warm", 100)):
        trace, clock = build_trace(run_kind, f"{run_kind}-trace")
        trace.record("wake_detected")
        clock.advance_ms(value)
        trace.record("ack_first_audio")
        trace.record("speech_end")
        clock.advance_ms(10)
        trace.record("final_transcript")
        traces.append(trace)

    report = build_report(traces)

    assert report["cohorts"]["cold"]["stages"]["wake_to_ack_first_audio"]["p95_ms"] == 500
    assert report["cohorts"]["warm"]["stages"]["wake_to_ack_first_audio"]["p95_ms"] == 100
    assert report["cohorts"]["warm"]["turns"][0]["dominant_stage"] == {
        "stage": "wake_to_ack_first_audio",
        "duration_ms": 100,
    }


def test_budget_enforcement_passes_and_fails():
    trace, clock = build_trace()
    trace.record("wake_detected")
    clock.advance_ms(300)
    trace.record("ack_first_audio")
    report = build_report([trace])

    enforce_budgets(report, {"wake_to_ack_first_audio_p95_ms": 350})
    with pytest.raises(LatencyBudgetError, match="300.00ms > 250.00ms"):
        enforce_budgets(report, {"wake_to_ack_first_audio_p95_ms": 250})


def test_metrics_path_prefers_xdg_state_home(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    assert metrics_path({}) == tmp_path / "zarathushtra" / "latency.jsonl"


def test_prolog_route_records_same_trace_id():
    trace, _ = build_trace(trace_id="voice-turn")
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.current_latency_trace = trace
    listener.prolog = MagicMock()
    listener.prolog.resolve_intent.return_value = IntentResult("prolog", "open", ["browser"])
    listener.prolog.execute_intent.return_value = True
    listener.executor = None
    listener.log = MagicMock()
    listener.session_id = None
    listener.memory = MagicMock()
    listener.memory.start_session.return_value = "session"
    listener.agent_manager = None
    listener.in_conversation_mode = MagicMock(return_value=False)

    used_agent, response = asyncio.run(listener.query_with_fallback_async("open browser"))

    assert (used_agent, response) == (False, "Executed: open ['browser']")
    assert [event.event for event in trace.events] == ["prolog_result", "route_selected"]
    assert {event.trace_id for event in trace.events} == {"voice-turn"}


def test_agent_node_records_buffered_llm_boundaries():
    trace, _ = build_trace(trace_id="agent-turn")

    class FakeLLM:
        async def ainvoke(self, _messages):
            return AIMessage(content="fixture response")

    registry = MagicMock()
    registry.to_langchain_tools.return_value = []
    node = create_agent_node(FakeLLM(), registry)
    result = asyncio.run(
        node(
            {
                "messages": [AIMessage(content="prompt")],
                "step_count": 0,
                "latency_trace": trace,
            }
        )
    )

    assert result["step_count"] == 1
    assert [event.event for event in trace.events] == [
        "llm_request",
        "llm_first_token",
        "llm_final_token",
    ]
    first_token = trace.events[1]
    assert first_token.metadata["buffered_proxy"] is True


def test_tts_records_request_chunk_and_final_boundaries():
    trace, _ = build_trace(trace_id="tts-turn")
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.current_latency_trace = trace
    listener.tts_config = {"provider": "fixture"}
    listener.tts_client = MagicMock()
    listener.tts_client.synthesize_async = AsyncMock(
        return_value=SynthesisResult(
            provider="fixture",
            success=True,
            audio=b"RIFF",
            audio_format="wav",
        )
    )
    listener._play_audio_task = AsyncMock(
        return_value=PlaybackResult(provider="fixture", success=True)
    )
    listener.last_tts_status = None
    listener.log = MagicMock()
    stop_event = asyncio.Event()
    stop_event.latency_trace = trace

    status = asyncio.run(listener._synthesize_and_play_task("hello", stop_event))

    assert status.success is True
    assert [event.event for event in trace.events] == [
        "tts_request",
        "tts_first_chunk",
        "tts_final_playback",
    ]
    assert trace.events[1].metadata["buffered_proxy"] is True


def test_interruption_completion_waits_for_player_task():
    trace, _ = build_trace(trace_id="interrupt-turn")
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.tts_stop_event = asyncio.Event()
    listener.tts_player_proc = None
    listener.tts_playback_active = True
    completed = []

    async def player():
        await listener.tts_stop_event.wait()
        await asyncio.sleep(0)
        listener.tts_playback_active = False
        completed.append(True)

    async def run():
        listener.tts_task = asyncio.create_task(player())
        await listener._interrupt_tts(trace)

    asyncio.run(run())

    assert completed == [True]
    assert [event.event for event in trace.events] == [
        "interruption_detected",
        "cancellation_completed",
    ]


@pytest.mark.parametrize(
    "toml_value, expected",
    [
        ("enabled = 1", "latency.enabled must be true or false"),
        ("metrics_path = 1", "latency.metrics_path must be a string"),
        (
            "enabled = true\n[latency.budgets]\nwake_to_ack_first_audio_p95_ms = 0",
            "must be a positive number",
        ),
        (
            "enabled = true\n[latency.budgets]\nunknown_budget = 1",
            "Unknown latency budget",
        ),
        (
            "enabled = true\n[latency.budgets]\nwake_to_ack_first_audio_p95_ms = nan",
            "must be a positive number",
        ),
    ],
)
def test_latency_config_validation(tmp_path, toml_value, expected):
    config_path = tmp_path / "config.toml"
    config_path.write_text(f"[latency]\n{toml_value}\n")
    with pytest.raises(ConfigError, match=expected):
        ZaraConfig(str(config_path))


def test_benchmark_outputs_jsonl_and_cold_warm_report(tmp_path):
    jsonl = tmp_path / "metrics.jsonl"
    report = tmp_path / "report.json"
    result = subprocess.run(
        [
            sys.executable,
            str(ROOT / "scripts" / "benchmark-voice.py"),
            "--jsonl",
            str(jsonl),
            "--report",
            str(report),
            "--cold-runs",
            "1",
            "--warm-runs",
            "3",
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    assert jsonl.exists()
    parsed = json.loads(report.read_text())
    assert parsed["cohorts"]["cold"]["turn_count"] == 1
    assert parsed["cohorts"]["warm"]["turn_count"] == 3
    assert "dominant_stage" in parsed["cohorts"]["warm"]["turns"][0]
