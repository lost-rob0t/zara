#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import subprocess
from pathlib import Path
from typing import Any

SHA_RE = re.compile(r"^[0-9a-f]{40}$")
GOAL_RE = re.compile(r"^[a-z][a-zA-Z0-9_]*$")


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected JSON object")
    return value


def is_ancestor(repo: Path, ancestor: str, candidate: str) -> bool:
    result = subprocess.run(
        ["git", "merge-base", "--is-ancestor", ancestor, candidate],
        cwd=repo,
        check=False,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def verifier_path(repo: Path, value: str) -> Path:
    path = Path(value)
    if path.is_absolute() or ".." in path.parts:
        raise ValueError("verifier path must stay inside repository")
    if path.suffix != ".pl" or not path.parts or path.parts[0] != "verification":
        raise ValueError("verifier must be a .pl file under verification/")
    full = (repo / path).resolve()
    if not full.is_file():
        raise ValueError(f"verifier does not exist: {value}")
    return full


def run_slice_verifier(repo: Path, receipt: dict[str, Any]) -> bool:
    goal = str(receipt.get("goal", "verify"))
    if not GOAL_RE.fullmatch(goal):
        raise ValueError("invalid Prolog verifier goal")
    path = verifier_path(repo, str(receipt["verifier"]))
    result = subprocess.run(
        ["swipl", "-q", "-s", str(path), "-g", goal, "-t", "halt(1)"],
        cwd=repo,
        check=False,
    )
    return result.returncode == 0


def parse_gate_status(values: list[str]) -> dict[str, str]:
    out: dict[str, str] = {}
    for raw in values:
        name, sep, state = raw.partition("=")
        if not sep or not name or state not in {"passed", "failed", "missing"}:
            raise ValueError(f"invalid --gate-status: {raw}")
        out[name] = state
    return out


def slice_state(repo: Path, row: dict[str, Any], candidate: str) -> str:
    slice_id = str(row["id"])
    issue = int(row["issue"])
    path = repo / "release" / "rage-receipts" / f"{slice_id}.json"
    if not path.exists():
        return "missing"
    receipt = load_json(path)
    if receipt.get("schema") != 1:
        return "failed"
    if receipt.get("slice_id") != slice_id or int(receipt.get("issue", -1)) != issue:
        return "failed"
    if receipt.get("result") != "passed":
        return "failed"
    verified_sha = str(receipt.get("verified_sha", ""))
    if not SHA_RE.fullmatch(verified_sha):
        return "failed"
    if not is_ancestor(repo, verified_sha, candidate):
        return "failed"
    return "passed" if run_slice_verifier(repo, receipt) else "failed"


def gate_state(
    repo: Path,
    gate: str,
    candidate: str,
    overrides: dict[str, str],
) -> str:
    if gate in overrides:
        return overrides[gate]
    path = repo / "release" / "gate-receipts" / f"{gate}.json"
    if not path.exists():
        return "missing"
    receipt = load_json(path)
    if receipt.get("schema") != 1 or receipt.get("gate") != gate:
        return "failed"
    if receipt.get("result") != "passed":
        return "failed"
    verified_sha = str(receipt.get("verified_sha", ""))
    if not SHA_RE.fullmatch(verified_sha):
        return "failed"
    return "passed" if is_ancestor(repo, verified_sha, candidate) else "failed"


def build(
    repo: Path,
    plan: dict[str, Any],
    candidate: str,
    gate_overrides: dict[str, str],
    apk_source_sha: str | None,
    unresolved_review_threads: int,
) -> dict[str, Any]:
    if plan.get("schema") != 1:
        raise ValueError("plan schema must be 1")
    if not SHA_RE.fullmatch(candidate):
        raise ValueError("candidate SHA must be 40 lowercase hex characters")
    if apk_source_sha is not None and not SHA_RE.fullmatch(apk_source_sha):
        raise ValueError("APK source SHA must be 40 lowercase hex characters")

    slices = {
        str(row["id"]): slice_state(repo, row, candidate)
        for row in plan["slices"]
    }
    gates = {
        str(gate): gate_state(repo, str(gate), candidate, gate_overrides)
        for gate in plan["required_gates"]
    }
    return {
        "schema": 1,
        "candidate_sha": candidate,
        "slices": slices,
        "gates": gates,
        "apk_source_sha": apk_source_sha,
        "model_calls": 0,
        "unresolved_review_threads": unresolved_review_threads,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=Path, default=Path("."))
    parser.add_argument("--plan", type=Path, required=True)
    parser.add_argument("--candidate-sha", required=True)
    parser.add_argument("--gate-status", action="append", default=[])
    parser.add_argument("--apk-source-sha")
    parser.add_argument("--unresolved-review-threads", type=int, default=0)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    repo = args.repo.resolve()
    plan = load_json(args.plan)
    evidence = build(
        repo,
        plan,
        args.candidate_sha,
        parse_gate_status(args.gate_status),
        args.apk_source_sha,
        args.unresolved_review_threads,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
