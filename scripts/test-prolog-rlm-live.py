#!/usr/bin/env python3
"""Opt-in real OpenRouter acceptance test for Zara's Prolog-RLM backend."""

from __future__ import annotations

import asyncio
import os
import sys

from zara.config import get_config
from zara.runtime.prolog_rlm import PrologRLMBackend


class ConfigOverlay:
    def __init__(self, base, *, agent=None, prolog_rlm=None):
        self.base = base
        self.agent = agent or {}
        self.prolog_rlm = prolog_rlm or {}

    def get_section(self, section):
        values = dict(self.base.get_section(section))
        if section == "agent":
            values.update(self.agent)
        elif section == "prolog_rlm":
            values.update(self.prolog_rlm)
        return values

    def get(self, section, key, default=None):
        return self.get_section(section).get(key, default)

    def __getattr__(self, name):
        return getattr(self.base, name)


DIRECT_PLAN = r'''Return ONLY this JSON object, changing nothing except JSON whitespace:
{"steps":[
{"op":"model","provider":"openrouter","prompt":{"ref":"var","name":"query"},"options":{"max_tokens":64},"bind":"answer"},
{"op":"final","value":{"ref":"var","name":"answer"}}
]}'''

DEPTH_ONE_PLAN = r'''Return ONLY this JSON object, changing nothing except JSON whitespace:
{"steps":[
{"op":"tool","name":"calculator","args":{"expression":"2+2"},"bind":"calc"},
{"op":"rlm","plan":{"steps":[
{"op":"model","provider":"openrouter","prompt":"Reply with exactly RLM_CHILD_OK.","options":{"max_tokens":64},"bind":"child_response"},
{"op":"final","value":{"ref":"var","name":"child_response"}}
]},"bind":"child"},
{"op":"final","value":{"ref":"var","name":"child"}}
]}'''


async def run_case(*, instruction: str, depth: int, query: str):
    config = ConfigOverlay(
        get_config(),
        agent={"backend": "prolog_rlm"},
        prolog_rlm={
            "mode": "rlm",
            "model": os.getenv("OPENROUTER_TEST_MODEL", "openrouter/free"),
            "planner_instruction": instruction,
            "planner_max_tokens": 1000,
            "max_recursion_depth": depth,
            "max_model_calls": 4,
            "max_total_tokens": 6000,
            "max_cost_usd": 0.25,
            "request_timeout": 120.0,
        },
    )
    backend = PrologRLMBackend(config)
    await backend.start()
    try:
        return await backend.submit_turn(query, turn_id=f"live-depth-{depth}")
    finally:
        await backend.stop()


async def main() -> int:
    if os.getenv("ZARA_RLM_LIVE") != "1":
        print("SKIP: set ZARA_RLM_LIVE=1 to run real Prolog-RLM integration")
        return 0
    if not os.getenv("OPENROUTER_API_KEY"):
        print("ERROR: OPENROUTER_API_KEY is required for live integration", file=sys.stderr)
        return 2

    direct = await run_case(
        instruction=DIRECT_PLAN,
        depth=0,
        query="Reply with exactly ZARA_PROLOG_RLM_OK.",
    )
    if "ZARA_PROLOG_RLM_OK" not in direct.response:
        raise AssertionError("direct RLM fixture token missing")
    print("prolog_rlm_direct: ok")
    print(f"prolog_rlm_direct_model_calls: {direct.metadata.get('usage', {}).get('model_calls')}")

    recursive = await run_case(
        instruction=DEPTH_ONE_PLAN,
        depth=1,
        query="Execute the required depth-one integration fixture.",
    )
    recursion = recursive.metadata.get("recursion", {})
    if recursion.get("max_depth") != 1 or recursion.get("recursive_calls") != 1:
        raise AssertionError(f"unexpected recursion metadata: {recursion!r}")
    if "RLM_CHILD_OK" not in recursive.response:
        raise AssertionError("recursive child fixture token missing")
    if not any(item.get("tool_name") == "calculator" for item in recursive.tool_results):
        raise AssertionError("calculator bridge was not exercised")

    print("prolog_rlm_depth_one: ok")
    print(f"prolog_rlm_recursive_depth: {recursion.get('max_depth')}")
    print(f"prolog_rlm_recursive_calls: {recursion.get('recursive_calls')}")
    print(f"prolog_rlm_tool_calls: {len(recursive.tool_results)}")
    print(
        "prolog_rlm_total_model_calls:",
        recursive.metadata.get("usage", {}).get("model_calls"),
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
