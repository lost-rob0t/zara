#!/usr/bin/env python3
"""Deterministic local end-to-end benchmark for the ZARA/1 ZMQ transport."""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import statistics
import time
from dataclasses import asdict, dataclass

import zmq

from zara.protocol import AUDIO_INPUT_FRAME_BYTES
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.server_core import ServerState
from zara.principals import PrincipalContext
from zara.zmq_hardening import HardenedZaraZmqGateway, HardenedZmqZaraClient
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, ZmqZaraClient


@dataclass(frozen=True)
class Sample:
    count: int
    ops_per_second: float
    p50_us: float
    p95_us: float
    p99_us: float
    max_us: float


class _Supervisor:
    def __init__(self) -> None:
        self.state = ServerState.READY
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, _principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, _principal, command):
        future = concurrent.futures.Future()
        if isinstance(command, SubmitTurn):
            future.set_result(CommandReceipt(request_id=command.request_id, turn_id="bench-turn"))
        else:
            future.set_result(CommandReceipt(request_id=command.request_id))
        return future


def _percentile(values: list[float], ratio: float) -> float:
    ordered = sorted(values)
    index = min(len(ordered) - 1, max(0, int((len(ordered) - 1) * ratio)))
    return ordered[index]


def _sample(latencies_ns: list[int], elapsed_ns: int) -> Sample:
    latencies_us = [value / 1_000.0 for value in latencies_ns]
    return Sample(
        count=len(latencies_ns),
        ops_per_second=len(latencies_ns) / (elapsed_ns / 1_000_000_000.0),
        p50_us=statistics.median(latencies_us),
        p95_us=_percentile(latencies_us, 0.95),
        p99_us=_percentile(latencies_us, 0.99),
        max_us=max(latencies_us),
    )


def _measure(call, count: int) -> Sample:
    latencies = []
    started = time.perf_counter_ns()
    for _ in range(count):
        before = time.perf_counter_ns()
        call().result(timeout=5.0)
        latencies.append(time.perf_counter_ns() - before)
    elapsed = time.perf_counter_ns() - started
    return _sample(latencies, elapsed)


def _run_stack(name, gateway_type, client_type, config, iterations, *, audio=False):
    context = zmq.Context()
    endpoint = f"inproc://zara-zmq-bench-{name}-{time.time_ns()}"
    supervisor = _Supervisor()
    principal = PrincipalContext("benchmark")
    gateway = gateway_type(
        endpoint,
        supervisor=supervisor,
        principal=principal,
        context=context,
        config=config,
    )
    client = client_type(endpoint, context=context, config=config)
    try:
        gateway.start().result(timeout=5.0)
        client.start().result(timeout=5.0)
        for _ in range(50):
            client.ping().result(timeout=5.0)
        ping = _measure(client.ping, iterations)

        result = {"ping": asdict(ping)}
        if audio:
            stream_id = "benchmark-audio"
            client.start_audio_input(stream_id).result(timeout=5.0)
            pcm = bytes(AUDIO_INPUT_FRAME_BYTES)
            seq = 0

            def chunk():
                nonlocal seq
                future = client.send_audio_input(stream_id, seq=seq, pcm=pcm)
                seq += 1
                return future

            audio_sample = _measure(chunk, iterations)
            client.commit_audio_input(stream_id).result(timeout=5.0)
            result["audio_1k"] = asdict(audio_sample)
        return result
    finally:
        client.close(timeout=5.0)
        gateway.close(timeout=5.0)
        context.term()


def _gate(report: dict) -> None:
    baseline = report["baseline"]["ping"]
    hardened = report["hardened"]["ping"]
    audio = report["hardened"]["audio_1k"]
    failures = []

    if hardened["p50_us"] >= baseline["p50_us"]:
        failures.append(
            f"hardened ping p50 regressed: {hardened['p50_us']:.1f} >= {baseline['p50_us']:.1f} us"
        )
    if hardened["p95_us"] > 10_000:
        failures.append(f"hardened ping p95 exceeds 10 ms: {hardened['p95_us']:.1f} us")
    if audio["p95_us"] > 10_000:
        failures.append(f"1 KiB audio p95 exceeds 10 ms: {audio['p95_us']:.1f} us")
    if hardened["ops_per_second"] < 100:
        failures.append(
            f"hardened ping throughput below 100 ops/s: {hardened['ops_per_second']:.1f}"
        )
    if failures:
        raise SystemExit("ZMQ E2E benchmark gate failed:\n- " + "\n- ".join(failures))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--iterations", type=int, default=300)
    parser.add_argument("--output")
    parser.add_argument("--gate", action="store_true")
    args = parser.parse_args()
    if args.iterations < 50:
        parser.error("--iterations must be at least 50")

    baseline_config = TransportConfig()
    hardened_config = TransportConfig(
        sndhwm=256,
        rcvhwm=256,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=5_000,
        heartbeat_timeout_ms=15_000,
        linger_ms=0,
        request_timeout=5.0,
        poll_interval_ms=1,
        event_queue_size=256,
        pending_request_limit=256,
        idempotency_cache_size=512,
    )
    report = {
        "schema": 1,
        "transport": "ZARA/1 over pyzmq",
        "audio_frame_bytes": AUDIO_INPUT_FRAME_BYTES,
        "baseline": _run_stack(
            "baseline",
            ZaraZmqGateway,
            ZmqZaraClient,
            baseline_config,
            args.iterations,
        ),
        "hardened": _run_stack(
            "hardened",
            HardenedZaraZmqGateway,
            HardenedZmqZaraClient,
            hardened_config,
            args.iterations,
            audio=True,
        ),
    }
    baseline_p50 = report["baseline"]["ping"]["p50_us"]
    hardened_p50 = report["hardened"]["ping"]["p50_us"]
    report["ping_p50_speedup"] = baseline_p50 / hardened_p50

    if args.gate:
        _gate(report)

    rendered = json.dumps(report, indent=2, sort_keys=True)
    print(rendered)
    if args.output:
        with open(args.output, "w", encoding="utf-8") as handle:
            handle.write(rendered + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
