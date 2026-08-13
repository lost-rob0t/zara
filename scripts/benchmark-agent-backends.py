#!/usr/bin/env python3
"""Small fixed-fixture baseline for Zara conversational backends."""

from __future__ import annotations

import argparse
import asyncio
import json
import time
import uuid

from zara.config import get_config
from zara.runtime.backend import create_runtime_backend


FIXTURES = (
    ("alpha", "Reply with exactly ZARA_BENCH_ALPHA.", "ZARA_BENCH_ALPHA"),
    ("beta", "Reply with exactly ZARA_BENCH_BETA.", "ZARA_BENCH_BETA"),
    ("omega", "Reply with exactly ZARA_BENCH_OMEGA.", "ZARA_BENCH_OMEGA"),
)


class BackendConfig:
    def __init__(self, base, backend: str):
        self.base = base
        self.backend = backend

    def get_section(self, section):
        values = dict(self.base.get_section(section))
        if section == "agent":
            values["backend"] = self.backend
        return values

    def get(self, section, key, default=None):
        return self.get_section(section).get(key, default)

    def __getattr__(self, name):
        return getattr(self.base, name)


def usage_fields(metadata):
    usage = metadata.get("usage", {}) if isinstance(metadata, dict) else {}
    return {
        "model_calls": usage.get("model_calls"),
        "prompt_tokens": usage.get("prompt_tokens") if usage.get("tokens_known") is not False else None,
        "completion_tokens": usage.get("completion_tokens") if usage.get("tokens_known") is not False else None,
        "total_tokens": usage.get("total_tokens") if usage.get("tokens_known") is not False else None,
        "cost_usd": usage.get("cost_usd") if usage.get("cost_known") is not False else None,
    }


async def benchmark_backend(name: str):
    backend = create_runtime_backend(BackendConfig(get_config(), name))
    records = []
    await backend.start()
    try:
        for fixture_name, prompt, expected in FIXTURES:
            turn_id = f"bench-{name}-{uuid.uuid4().hex}"
            started = time.perf_counter()
            try:
                result = await backend.submit_turn(prompt, turn_id=turn_id)
                elapsed = time.perf_counter() - started
                record = {
                    "fixture": fixture_name,
                    "backend": name,
                    "success": True,
                    "correct": expected in result.response,
                    "wall_time_seconds": elapsed,
                    "tool_calls": len(result.tool_results),
                    **usage_fields(result.metadata),
                }
            except Exception as error:
                elapsed = time.perf_counter() - started
                record = {
                    "fixture": fixture_name,
                    "backend": name,
                    "success": False,
                    "correct": False,
                    "wall_time_seconds": elapsed,
                    "tool_calls": None,
                    "model_calls": None,
                    "prompt_tokens": None,
                    "completion_tokens": None,
                    "total_tokens": None,
                    "cost_usd": None,
                    "error": f"{type(error).__name__}: {error}",
                }
            records.append(record)
    finally:
        await backend.stop()
    return records


async def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--backend",
        choices=("langgraph", "prolog_rlm", "both"),
        default="both",
    )
    args = parser.parse_args()
    names = ("langgraph", "prolog_rlm") if args.backend == "both" else (args.backend,)
    records = []
    for name in names:
        records.extend(await benchmark_backend(name))
    print(json.dumps({"fixtures": records}, indent=2, sort_keys=True))


if __name__ == "__main__":
    asyncio.run(main())
