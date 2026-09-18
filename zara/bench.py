"""Zara LLM benchmark and bounded evolutionary tuning harness.

The harness deliberately talks to the configured LLM endpoint directly. It does
not require zara-server, which makes it useful for provider/runtime isolation
and for evaluating fallback behavior when the server is unavailable.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import os
import re
import statistics
import tempfile
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable, Optional


from zara.actors import BoundedActor
from zara.config import ZaraConfig
from zara.llm import LLMClient, LLMResult

DEFAULT_GLM_MODEL = "z-ai/glm-5.3-flash"
DEFAULT_WORKERS = 10
SUPPORTED_RESOURCE_KINDS = {"prompt", "prolog-skill", "kb"}


@dataclass(frozen=True)
class BenchmarkCase:
    case_id: str
    prompt: str
    split: str = "train"
    must_include: tuple[str, ...] = ()
    must_not_include: tuple[str, ...] = ()
    exact: Optional[str] = None


@dataclass(frozen=True)
class RolloutResult:
    case_id: str
    split: str
    success: bool
    score: float
    latency_ms: float
    text: str = ""
    error_type: str = ""
    attempts: int = 1


@dataclass(frozen=True)
class CandidateSummary:
    candidate_id: str
    generation: int
    parent_id: Optional[str]
    train_score: float
    heldout_score: float
    policy_failures: int
    median_latency_ms: float
    accepted: bool


def _tuple_strings(value: Any) -> tuple[str, ...]:
    if value is None:
        return ()
    if isinstance(value, str):
        return (value,)
    if isinstance(value, list) and all(isinstance(item, str) for item in value):
        return tuple(value)
    raise ValueError("benchmark include/exclude fields must be a string or string list")


def load_cases(path: Path) -> list[BenchmarkCase]:
    cases: list[BenchmarkCase] = []
    for line_number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        raw = raw.strip()
        if not raw or raw.startswith("#"):
            continue
        data = json.loads(raw)
        case_id = str(data.get("id") or f"line-{line_number}")
        prompt = data.get("prompt")
        if not isinstance(prompt, str) or not prompt.strip():
            raise ValueError(f"{path}:{line_number}: prompt must be non-empty text")
        split = str(data.get("split", "train"))
        if split not in {"train", "heldout"}:
            raise ValueError(f"{path}:{line_number}: split must be train or heldout")
        exact = data.get("exact")
        if exact is not None and not isinstance(exact, str):
            raise ValueError(f"{path}:{line_number}: exact must be text")
        cases.append(
            BenchmarkCase(
                case_id=case_id,
                prompt=prompt,
                split=split,
                must_include=_tuple_strings(data.get("must_include")),
                must_not_include=_tuple_strings(data.get("must_not_include")),
                exact=exact,
            )
        )
    if not cases:
        raise ValueError(f"benchmark corpus is empty: {path}")
    if not any(case.split == "heldout" for case in cases):
        raise ValueError("benchmark corpus must contain at least one heldout case")
    return cases


def score_case(case: BenchmarkCase, text: str) -> tuple[float, bool]:
    normalized = text.strip()
    checks: list[bool] = []
    if case.exact is not None:
        checks.append(normalized == case.exact.strip())
    lower = normalized.lower()
    checks.extend(token.lower() in lower for token in case.must_include)
    forbidden = [token.lower() in lower for token in case.must_not_include]
    policy_failure = any(forbidden)
    checks.extend(not value for value in forbidden)
    if not checks:
        return (1.0 if normalized else 0.0), policy_failure
    return sum(1.0 for ok in checks if ok) / len(checks), policy_failure


def normalize_openai_endpoint(endpoint: Optional[str]) -> Optional[str]:
    if not endpoint:
        return None
    value = endpoint.rstrip("/")
    if value.endswith("/chat/completions"):
        return value
    if value.endswith("/v1"):
        return value + "/chat/completions"
    return value


def resolve_api_key(provider: str, config: dict[str, Any]) -> Optional[str]:
    generic = config.get("api_key") or os.getenv("ZARA_LLM_API_KEY")
    if provider == "openrouter":
        return config.get("openrouter_api_key") or os.getenv("OPENROUTER_API_KEY") or generic
    if provider == "openai":
        return config.get("openai_api_key") or os.getenv("OPENAI_API_KEY") or generic
    if provider == "anthropic":
        return config.get("anthropic_api_key") or os.getenv("ANTHROPIC_API_KEY") or generic
    return None


class RolloutWorker(BoundedActor):
    """One isolated bounded rollout actor. No shared mutable score state."""

    mailbox_size = 64
    mailbox_overflow = "block"

    def __init__(
        self,
        *,
        provider: str,
        model: Optional[str],
        endpoint: Optional[str],
        api_key: Optional[str],
        timeout: float,
    ) -> None:
        super().__init__()
        self.provider = provider
        self.model = model
        self.endpoint = endpoint
        self.api_key = api_key
        self.timeout = timeout

    def on_receive(self, message: dict[str, Any]) -> dict[str, Any]:
        if message.get("op") != "run":
            raise ValueError("unsupported rollout operation")
        case = BenchmarkCase(**message["case"])
        system_prompt = str(message["system_prompt"])
        started = time.perf_counter()
        result = asyncio.run(self._query(case.prompt, system_prompt))
        latency_ms = (time.perf_counter() - started) * 1000.0
        score, policy_failure = score_case(case, result.text if result.success else "")
        return asdict(
            RolloutResult(
                case_id=case.case_id,
                split=case.split,
                success=result.success and not policy_failure,
                score=score,
                latency_ms=latency_ms,
                text=result.text,
                error_type=result.error_type,
                attempts=result.attempts,
            )
        ) | {"policy_failure": policy_failure}

    async def _query(self, prompt: str, system_prompt: str) -> LLMResult:
        client = LLMClient(
            provider=self.provider,
            model=self.model,
            endpoint=self.endpoint,
            api_key=self.api_key,
            connect_timeout=min(10.0, self.timeout),
            read_timeout=self.timeout,
            total_timeout=self.timeout,
            max_retries=1,
        )
        try:
            return await client.query_async(
                prompt,
                system_prompt=system_prompt,
                max_tokens=1024,
            )
        finally:
            await client.close()


def run_rollouts(
    cases: list[BenchmarkCase],
    *,
    system_prompt: str,
    provider: str,
    model: Optional[str],
    endpoint: Optional[str],
    api_key: Optional[str],
    workers: int = DEFAULT_WORKERS,
    timeout: float = 90.0,
) -> list[dict[str, Any]]:
    if workers < 1:
        raise ValueError("workers must be positive")
    refs = [
        RolloutWorker.start(
            provider=provider,
            model=model,
            endpoint=endpoint,
            api_key=api_key,
            timeout=timeout,
        )
        for _ in range(workers)
    ]
    try:
        futures = []
        for index, case in enumerate(cases):
            futures.append(
                refs[index % workers].ask(
                    {"op": "run", "case": asdict(case), "system_prompt": system_prompt},
                    block=False,
                )
            )
        return [future.get(timeout=timeout + 5.0) for future in futures]
    finally:
        for ref in refs:
            ref.stop(block=True)


def summarize_results(
    candidate_id: str,
    generation: int,
    parent_id: Optional[str],
    results: Iterable[dict[str, Any]],
    *,
    accepted: bool,
) -> CandidateSummary:
    rows = list(results)
    train = [float(row["score"]) for row in rows if row["split"] == "train"]
    heldout = [float(row["score"]) for row in rows if row["split"] == "heldout"]
    latencies = [float(row["latency_ms"]) for row in rows]
    return CandidateSummary(
        candidate_id=candidate_id,
        generation=generation,
        parent_id=parent_id,
        train_score=statistics.fmean(train) if train else 0.0,
        heldout_score=statistics.fmean(heldout) if heldout else 0.0,
        policy_failures=sum(bool(row.get("policy_failure")) for row in rows),
        median_latency_ms=statistics.median(latencies) if latencies else 0.0,
        accepted=accepted,
    )


async def propose_mutation(
    *,
    resource_kind: str,
    resource_text: str,
    failures: list[dict[str, Any]],
    provider: str,
    model: Optional[str],
    endpoint: Optional[str],
    api_key: Optional[str],
    timeout: float,
) -> str:
    failure_excerpt = json.dumps(
        [
            {
                "case_id": row["case_id"],
                "split": row["split"],
                "score": row["score"],
                "error_type": row.get("error_type", ""),
            }
            for row in failures[:30]
        ],
        sort_keys=True,
    )
    mutation_prompt = (
        "Improve the mutable Zara resource below using the benchmark failure evidence. "
        "Preserve its format and intent. Do not add secrets, weaken permissions, alter evaluators, "
        "or include benchmark answers verbatim. Return only the complete candidate between "
        "<candidate> and </candidate>.\n\n"
        f"resource_kind={resource_kind}\nfailures={failure_excerpt}\n"
        f"<resource>\n{resource_text}\n</resource>"
    )
    client = LLMClient(
        provider=provider,
        model=model,
        endpoint=endpoint,
        api_key=api_key,
        connect_timeout=min(10.0, timeout),
        read_timeout=timeout,
        total_timeout=timeout,
        max_retries=1,
    )
    try:
        result = await client.query_async(
            mutation_prompt,
            system_prompt=(
                "You are Zara's bounded resource optimizer. Make the smallest high-value edit. "
                "Never modify permission policy, secrets, evaluator definitions, or benchmark fixtures."
            ),
            max_tokens=4096,
        )
    finally:
        await client.close()
    if not result.success:
        raise RuntimeError(f"mutation failed: {result.error_type}: {result.error}")
    match = re.search(r"<candidate>\s*(.*?)\s*</candidate>", result.text, re.S)
    if not match:
        raise RuntimeError("mutation response did not contain <candidate> markers")
    candidate = match.group(1)
    if not candidate.strip():
        raise RuntimeError("mutation candidate is empty")
    return candidate


def validate_resource(resource_kind: str, text: str) -> None:
    if resource_kind not in SUPPORTED_RESOURCE_KINDS:
        raise ValueError(f"unsupported resource kind: {resource_kind}")
    if not text.strip():
        raise ValueError("candidate resource is empty")
    if resource_kind not in {"prolog-skill", "kb"}:
        return

    import shutil
    import subprocess

    swipl = shutil.which("swipl")
    if swipl is None:
        raise RuntimeError("SWI-Prolog is required to validate Prolog/KB candidates")
    with tempfile.NamedTemporaryFile("w", suffix=".pl", encoding="utf-8") as handle:
        handle.write(text)
        handle.flush()
        goal = f"read_file_to_terms('{handle.name}',_,[syntax_errors(error)]),halt"
        process = subprocess.run(
            [swipl, "-q", "-g", goal, "-t", "halt(1)"],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    if process.returncode != 0:
        detail = process.stderr.strip() or process.stdout.strip()
        raise RuntimeError(f"Prolog candidate failed syntax validation: {detail}")


def build_system_prompt(resource_kind: str, resource_text: str) -> str:
    return (
        "You are Zara under benchmark. Follow the mutable resource exactly while remaining safe and precise.\n"
        f"RESOURCE_KIND={resource_kind}\n"
        "<mutable-resource>\n"
        f"{resource_text}\n"
        "</mutable-resource>"
    )


def evolve(
    *,
    cases: list[BenchmarkCase],
    resource_path: Path,
    resource_kind: str,
    generations: int,
    provider: str,
    model: Optional[str],
    endpoint: Optional[str],
    api_key: Optional[str],
    workers: int,
    timeout: float,
    output_dir: Path,
) -> list[CandidateSummary]:
    resource_text = resource_path.read_text(encoding="utf-8")
    validate_resource(resource_kind, resource_text)
    output_dir.mkdir(parents=True, exist_ok=True)
    lineage: list[CandidateSummary] = []
    parent_id: Optional[str] = None
    best_heldout = -1.0
    best_resource_text = resource_text
    best_results: list[dict[str, Any]] = []
    best_candidate_id: Optional[str] = None

    for generation in range(generations + 1):
        candidate_id = f"gen-{generation:03d}"
        results = run_rollouts(
            cases,
            system_prompt=build_system_prompt(resource_kind, resource_text),
            provider=provider,
            model=model,
            endpoint=endpoint,
            api_key=api_key,
            workers=workers,
            timeout=timeout,
        )
        heldout = [float(row["score"]) for row in results if row["split"] == "heldout"]
        heldout_score = statistics.fmean(heldout) if heldout else 0.0
        policy_failures = sum(bool(row.get("policy_failure")) for row in results)
        accepted = policy_failures == 0 and heldout_score > best_heldout
        summary = summarize_results(
            candidate_id,
            generation,
            parent_id,
            results,
            accepted=accepted,
        )
        lineage.append(summary)
        (output_dir / f"{candidate_id}.json").write_text(
            json.dumps(
                {"summary": asdict(summary), "results": results},
                indent=2,
                sort_keys=True,
            ),
            encoding="utf-8",
        )
        (output_dir / f"{candidate_id}.candidate").write_text(
            resource_text,
            encoding="utf-8",
        )
        if accepted:
            best_heldout = heldout_score
            best_resource_text = resource_text
            best_results = results
            best_candidate_id = candidate_id
        if generation == generations:
            break
        source_results = best_results or results
        failures = [
            row
            for row in source_results
            if float(row["score"]) < 1.0 or row.get("policy_failure")
        ]
        next_text = asyncio.run(
            propose_mutation(
                resource_kind=resource_kind,
                resource_text=best_resource_text,
                failures=failures,
                provider=provider,
                model=model,
                endpoint=endpoint,
                api_key=api_key,
                timeout=timeout,
            )
        )
        validate_resource(resource_kind, next_text)
        parent_id = best_candidate_id
        resource_text = next_text
    return lineage


def _provider_settings(
    args: argparse.Namespace,
) -> tuple[str, Optional[str], Optional[str], Optional[str]]:
    config: dict[str, Any] = {}
    if args.config:
        config = ZaraConfig(args.config).get_llm_config()
    provider = args.provider or str(config.get("provider") or "openrouter")
    model = args.model or config.get("model")
    if not model and provider == "openrouter":
        model = DEFAULT_GLM_MODEL
    endpoint = args.endpoint or config.get("endpoint")
    if provider in {"openrouter", "openai"}:
        endpoint = normalize_openai_endpoint(endpoint)
    api_key = resolve_api_key(provider, config)
    return provider, model, endpoint, api_key


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="zara-bench")
    parser.add_argument("--config", help="existing Zara config.toml")
    parser.add_argument(
        "--provider",
        choices=("openrouter", "openai", "anthropic", "ollama"),
    )
    parser.add_argument(
        "--model",
        help=f"model id (default: {DEFAULT_GLM_MODEL} for OpenRouter)",
    )
    parser.add_argument(
        "--endpoint",
        help="direct provider endpoint; /v1 is normalized for OpenAI-compatible providers",
    )
    parser.add_argument("--workers", type=int, default=DEFAULT_WORKERS)
    parser.add_argument("--timeout", type=float, default=90.0)

    sub = parser.add_subparsers(dest="command", required=True)
    bench = sub.add_parser("bench")
    bench.add_argument("corpus", type=Path)
    bench.add_argument(
        "--system-prompt",
        default="You are Zara. Be precise, capable, and concise.",
    )
    bench.add_argument("--json", action="store_true")

    evolution = sub.add_parser("evolve")
    evolution.add_argument("corpus", type=Path)
    evolution.add_argument("--resource", type=Path, required=True)
    evolution.add_argument(
        "--kind",
        choices=sorted(SUPPORTED_RESOURCE_KINDS),
        required=True,
    )
    evolution.add_argument("--generations", type=int, default=1)
    evolution.add_argument(
        "--output-dir",
        type=Path,
        default=Path(".zara/bench/evolution"),
    )
    evolution.add_argument(
        "--promote",
        type=Path,
        help="atomically install the best held-out-approved candidate at this path",
    )
    return parser


def main(argv: Optional[list[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    provider, model, endpoint, api_key = _provider_settings(args)
    cases = load_cases(args.corpus)

    if args.command == "bench":
        results = run_rollouts(
            cases,
            system_prompt=args.system_prompt,
            provider=provider,
            model=model,
            endpoint=endpoint,
            api_key=api_key,
            workers=args.workers,
            timeout=args.timeout,
        )
        summary = summarize_results("baseline", 0, None, results, accepted=False)
        if args.json:
            print(
                json.dumps(
                    {"summary": asdict(summary), "results": results},
                    indent=2,
                    sort_keys=True,
                )
            )
        else:
            print(
                f"provider={provider} model={model} workers={args.workers} "
                f"train={summary.train_score:.3f} heldout={summary.heldout_score:.3f} "
                f"policy_failures={summary.policy_failures} "
                f"p50_ms={summary.median_latency_ms:.1f}"
            )
        return 0 if summary.policy_failures == 0 else 2

    lineage = evolve(
        cases=cases,
        resource_path=args.resource,
        resource_kind=args.kind,
        generations=max(0, args.generations),
        provider=provider,
        model=model,
        endpoint=endpoint,
        api_key=api_key,
        workers=args.workers,
        timeout=args.timeout,
        output_dir=args.output_dir,
    )
    if args.promote:
        accepted = [item for item in lineage if item.accepted and item.generation > 0]
        if not accepted:
            raise RuntimeError(
                "no candidate passed held-out/policy gates; refusing promotion"
            )
        winner = accepted[-1]
        source = args.output_dir / f"{winner.candidate_id}.candidate"
        args.promote.parent.mkdir(parents=True, exist_ok=True)
        temporary = args.promote.with_suffix(
            args.promote.suffix + ".zara-bench-tmp"
        )
        temporary.write_text(
            source.read_text(encoding="utf-8"),
            encoding="utf-8",
        )
        temporary.replace(args.promote)
    print(json.dumps([asdict(item) for item in lineage], indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
