#!/usr/bin/env python3
"""Manual LLM bench matrix for Zara's OpenAI-compatible backends.

Invoked by hand during reviews or performance work; never wired to push or
cron triggers. Every probe is tiny so a full matrix run costs well under one
US cent. Writes machine-readable JSON plus a markdown report into
``artifacts/llm-bench/<timestamp>/``.

Usage:
    python3 scripts/llm_bench.py [--base-url URL] [--api-key-file PATH]
                                 [--models id,id,...] [--out DIR]
"""

from __future__ import annotations

import argparse
import json
import sys
import time
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path

DEFAULT_BASE_URL = "https://api.nano-gpt.com/api/v1"
CATALOG_URL = "https://nano-gpt.com/api/models"
DEFAULT_KEY_FILE = Path.home() / ".config" / "nanogpt" / "api-key"

DEFAULT_MODELS = [
    "kitani/evolve-beta",
    "jaredpalmer/kev-9b",
    "baselabs/groundhog-1",
    "subq/subq-snap-1.1",
    "liquid/d1",
    "upstage/solar-mini-4-5",
    "inclusionai/ling-3.0-tiny",
    "meta-llama/llama-3.1-8b-instruct",
    "z-ai/glm-5.3",
]
BASELINE_MODELS = {"z-ai/glm-5.3"}
BASELINE_PROBES = ["ready", "tool_call"]
MAX_REQUESTS = 45
REQUEST_TIMEOUT = 45.0
RETRY_DELAY = 5.0
RETRYABLE_STATUS = {429, 500, 502, 503, 529}

TOOL_PROBE_TOOLS = [
    {
        "type": "function",
        "function": {
            "name": "timer",
            "description": "Set a timer",
            "parameters": {
                "type": "object",
                "properties": {
                    "seconds": {"type": "integer"},
                    "name": {"type": "string"},
                },
                "required": ["seconds"],
            },
        },
    }
]


@dataclass
class CallResult:
    latency_s: float
    status: int
    body: dict = field(default_factory=dict)
    error: str = ""


@dataclass
class ProbeResult:
    name: str
    passed: bool
    latency_s: float
    detail: str = ""
    prompt_tokens: int = 0
    completion_tokens: int = 0
    cost_usd: float | None = None
    error: str = ""


@dataclass
class ModelReport:
    model: str
    input_price: float | None = None
    output_price: float | None = None
    probes: list[ProbeResult] = field(default_factory=list)

    @property
    def total_cost(self) -> float | None:
        costs = [p.cost_usd for p in self.probes]
        if any(c is None for c in costs):
            return None
        return sum(costs)

    @property
    def avg_latency(self) -> float | None:
        latencies = [p.latency_s for p in self.probes if not p.error]
        return sum(latencies) / len(latencies) if latencies else None


def load_api_key(path: Path) -> str:
    key = path.expanduser().read_text().strip()
    if not key:
        raise SystemExit(f"api key file is empty: {path}")
    return key


def call_chat(api_key: str, base_url: str, payload: dict) -> CallResult:
    request = urllib.request.Request(
        f"{base_url}/chat/completions",
        method="POST",
        data=json.dumps(payload).encode(),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
    )
    started = time.monotonic()
    for attempt in (1, 2):
        try:
            with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
                body = json.loads(response.read().decode())
                return CallResult(time.monotonic() - started, response.status, body)
        except urllib.error.HTTPError as error:
            detail = error.read().decode(errors="replace")[:120]
            if attempt == 1 and error.code in RETRYABLE_STATUS:
                time.sleep(RETRY_DELAY)
                continue
            return CallResult(time.monotonic() - started, error.code, error=detail)
        except (urllib.error.URLError, json.JSONDecodeError, OSError) as error:
            if attempt == 1:
                time.sleep(RETRY_DELAY)
                continue
            return CallResult(time.monotonic() - started, 0, error=str(error)[:120])
    return CallResult(time.monotonic() - started, 0, error="unreachable")


def fetch_prices(api_key: str) -> dict[str, tuple[float, float]]:
    request = urllib.request.Request(
        CATALOG_URL, headers={"Authorization": f"Bearer {api_key}"}
    )
    try:
        with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
            catalog = json.loads(response.read().decode())
    except (urllib.error.URLError, json.JSONDecodeError, OSError):
        return {}
    prices = {}
    for model in catalog.get("models", {}).get("text", {}).values():
        in_price = model.get("input_price_per_million")
        out_price = model.get("output_price_per_million")
        if isinstance(in_price, (int, float)) and isinstance(out_price, (int, float)):
            prices[model["model"]] = (in_price, out_price)
    return prices


def request_cost(
    usage: dict, prices: dict[str, tuple[float, float]], model: str
) -> float | None:
    if model not in prices:
        return None
    in_price, out_price = prices[model]
    prompt = usage.get("prompt_tokens", 0) or 0
    completion = usage.get("completion_tokens", 0) or 0
    return prompt / 1e6 * in_price + completion / 1e6 * out_price


def probe_result(
    name: str,
    call: CallResult,
    prices: dict[str, tuple[float, float]],
    model: str,
    assertion,
) -> ProbeResult:
    if call.error or call.status != 200:
        return ProbeResult(name, False, call.latency_s, error=f"HTTP {call.status}: {call.error}")
    choice = (call.body.get("choices") or [{}])[0]
    message = choice.get("message") or {}
    usage = call.body.get("usage") or {}
    passed, detail = assertion(choice, message)
    return ProbeResult(
        name,
        passed,
        call.latency_s,
        detail=detail,
        prompt_tokens=usage.get("prompt_tokens", 0) or 0,
        completion_tokens=usage.get("completion_tokens", 0) or 0,
        cost_usd=request_cost(usage, prices, model),
    )


def strip_fences(text: str) -> str:
    text = text.strip()
    if text.startswith("```"):
        text = text.strip("`")
        text = text[4:] if text.startswith("json") else text
    return text.strip()


def probe_ready(api_key, base_url, model, prices) -> ProbeResult:
    call = call_chat(
        api_key,
        base_url,
        {"model": model, "messages": [{"role": "user", "content": "Reply with exactly: READY"}], "max_tokens": 10},
    )

    def assert_ready(choice, message) -> tuple[bool, str]:
        content = str(message.get("content", ""))
        return content.strip() == "READY", content[:60]

    return probe_result("ready", call, prices, model, assert_ready)


def probe_tool_call(api_key, base_url, model, prices) -> ProbeResult:
    call = call_chat(
        api_key,
        base_url,
        {
            "model": model,
            "messages": [{"role": "user", "content": "Set a timer for 10 minutes using the tool."}],
            "tools": TOOL_PROBE_TOOLS,
            "max_tokens": 200,
        },
    )

    def assert_timer(choice, message) -> tuple[bool, str]:
        tool_calls = message.get("tool_calls") or []
        if not tool_calls:
            return False, "no tool_calls"
        function = tool_calls[0]["function"]
        try:
            arguments = json.loads(function.get("arguments", "{}"))
        except json.JSONDecodeError:
            return False, "unparseable arguments"
        detail = f"{function.get('name')}({function.get('arguments', '')[:60]})"
        return function.get("name") == "timer" and arguments.get("seconds") == 600, detail

    return probe_result("tool_call", call, prices, model, assert_timer)


def probe_json(api_key, base_url, model, prices) -> ProbeResult:
    call = call_chat(
        api_key,
        base_url,
        {
            "model": model,
            "messages": [
                {
                    "role": "user",
                    "content": 'Return exactly this JSON object and nothing else: {"status": "ok", "count": 3}',
                }
            ],
            "max_tokens": 30,
        },
    )

    def assert_json(choice, message) -> tuple[bool, str]:
        text = strip_fences(str(message.get("content", "")))
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            return False, f"unparseable: {text[:60]}"
        return parsed.get("status") == "ok" and parsed.get("count") == 3, text[:60]

    return probe_result("json", call, prices, model, assert_json)


def probe_context(api_key, base_url, model, prices) -> ProbeResult:
    messages = [
        {"role": "user", "content": "My secret code word is pineapple. Just acknowledge it."},
    ]
    turn_one = call_chat(api_key, base_url, {"model": model, "messages": messages, "max_tokens": 30})
    if turn_one.error or turn_one.status != 200:
        return ProbeResult("context", False, turn_one.latency_s, error=f"turn 1 HTTP {turn_one.status}")
    messages.append(turn_one.body["choices"][0]["message"])
    messages.append({"role": "user", "content": "Reply with just the code word."})
    call = call_chat(api_key, base_url, {"model": model, "messages": messages, "max_tokens": 10})

    def assert_code(choice, message) -> tuple[bool, str]:
        content = str(message.get("content", ""))
        return "pineapple" in content.lower(), content[:60]

    return probe_result("context", call, prices, model, assert_code)


PROBES = {
    "ready": probe_ready,
    "tool_call": probe_tool_call,
    "json": probe_json,
    "context": probe_context,
}


def bench_model(api_key, base_url, model, prices, probe_names) -> ModelReport:
    report = ModelReport(model, *prices.get(model, (None, None)))
    for probe_name in probe_names:
        report.probes.append(PROBES[probe_name](api_key, base_url, model, prices))
    return report


def markdown_report(reports: list[ModelReport]) -> str:
    lines = [
        "# LLM bench matrix",
        "",
        f"Generated {datetime.now(timezone.utc).isoformat(timespec='seconds')}",
        "",
        "| model | $in/M | $out/M | ready | tool_call | json | context | avg s | run cost |",
        "|---|---|---|---|---|---|---|---|---|",
    ]
    for report in reports:
        cells = [
            report.model,
            "?" if report.input_price is None else f"{report.input_price:g}",
            "?" if report.output_price is None else f"{report.output_price:g}",
        ]
        for probe in report.probes:
            cells.append("pass" if probe.passed else ("ERR" if probe.error else "FAIL"))
        latency = report.avg_latency
        cells.append("?" if latency is None else f"{latency:.1f}")
        cost = report.total_cost
        cells.append("?" if cost is None else f"${cost:.4f}")
        lines.append("| " + " | ".join(cells) + " |")
    known = [r.total_cost for r in reports if r.total_cost is not None]
    if known:
        lines.append("")
        lines.append(f"Total known run cost: ${sum(known):.4f}")
    return "\n".join(lines) + "\n"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--base-url", default=DEFAULT_BASE_URL)
    parser.add_argument("--api-key-file", type=Path, default=DEFAULT_KEY_FILE)
    parser.add_argument("--models", default=",".join(DEFAULT_MODELS))
    parser.add_argument("--probes", default=",".join(PROBES))
    parser.add_argument("--out", type=Path, default=Path("artifacts/llm-bench"))
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    probe_names = [p for p in args.probes.split(",") if p]
    unknown = [p for p in probe_names if p not in PROBES]
    if unknown:
        raise SystemExit(f"unknown probes: {unknown}")
    models = [m for m in args.models.split(",") if m]
    planned = sum(len(BASELINE_PROBES if m in BASELINE_MODELS else probe_names) + 1 for m in models)
    if planned > MAX_REQUESTS:
        raise SystemExit(f"planned {planned} requests exceed the {MAX_REQUESTS} request budget")
    api_key = load_api_key(args.api_key_file)
    prices = fetch_prices(api_key)
    print(f"models={len(models)} probes={probe_names} planned_requests={planned}")
    print(f"prices fetched for {len(prices)} models")
    reports = []
    for model in models:
        selected = BASELINE_PROBES if model in BASELINE_MODELS else probe_names
        print(f"bench {model} ({len(selected)} probes)...")
        reports.append(bench_model(api_key, args.base_url, model, prices, selected))
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    out_dir = args.out / stamp
    out_dir.mkdir(parents=True, exist_ok=True)
    payload = [
        {
            "model": r.model,
            "input_price_per_million": r.input_price,
            "output_price_per_million": r.output_price,
            "probes": [
                {
                    "name": p.name,
                    "passed": p.passed,
                    "latency_s": round(p.latency_s, 2),
                    "detail": p.detail,
                    "error": p.error,
                    "prompt_tokens": p.prompt_tokens,
                    "completion_tokens": p.completion_tokens,
                    "cost_usd": p.cost_usd,
                }
                for p in r.probes
            ],
            "total_cost_usd": r.total_cost,
        }
        for r in reports
    ]
    (out_dir / "results.json").write_text(json.dumps(payload, indent=2) + "\n")
    (out_dir / "REPORT.md").write_text(markdown_report(reports))
    print(markdown_report(reports))
    print(f"artifacts: {out_dir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
