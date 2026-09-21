#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Any

SHA_RE = re.compile(r"^[0-9a-f]{40}$")
ID_RE = re.compile(r"^[a-z0-9][a-z0-9_-]*$")
STATUS = {"passed", "failed", "missing", "skipped", "cancelled", "pending"}


def atom(value: str) -> str:
    return "'" + value.replace("\\", "\\\\").replace("'", "\\'") + "'"


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected JSON object")
    return value


def require_sha(value: str, name: str) -> str:
    if not SHA_RE.fullmatch(value):
        raise ValueError(f"{name}: expected 40 lowercase hex chars")
    return value


def require_id(value: str, name: str) -> str:
    if not ID_RE.fullmatch(value):
        raise ValueError(f"{name}: invalid identifier")
    return value


def render(plan: dict[str, Any], candidate_sha: str, evidence: dict[str, Any] | None) -> str:
    if plan.get("schema") != 1:
        raise ValueError("plan schema must be 1")

    program_issue = int(plan["program_issue"])
    target_version = str(plan["target_version"])
    target_code = int(plan["target_android_version_code"])
    slices = plan.get("slices")
    gates = plan.get("required_gates")
    if not isinstance(slices, list) or not slices:
        raise ValueError("plan slices must be a non-empty list")
    if not isinstance(gates, list) or not gates:
        raise ValueError("required_gates must be a non-empty list")

    seen_slices: set[str] = set()
    seen_gates: set[str] = set()
    lines = [
        "% Generated. Do not hand-edit.",
        f"program_issue({program_issue}).",
        f"release_candidate_sha({atom(candidate_sha)}).",
        f"release_target({atom(target_version)}, {target_code}).",
    ]

    for row in slices:
        if not isinstance(row, dict):
            raise ValueError("slice entries must be objects")
        slice_id = require_id(str(row["id"]), "slice id")
        if slice_id in seen_slices:
            raise ValueError(f"duplicate slice id: {slice_id}")
        seen_slices.add(slice_id)
        issue = int(row["issue"])
        required = bool(row.get("required", True))
        lines.append(
            f"planned_slice({atom(slice_id)}, {issue}, {'true' if required else 'false'})."
        )

    for raw_gate in gates:
        gate = require_id(str(raw_gate), "gate")
        if gate in seen_gates:
            raise ValueError(f"duplicate gate: {gate}")
        seen_gates.add(gate)
        lines.append(f"required_gate({gate}).")

    if evidence is not None:
        if evidence.get("schema") != 1:
            raise ValueError("evidence schema must be 1")
        evidence_sha = require_sha(str(evidence["candidate_sha"]), "evidence candidate_sha")
        if evidence_sha != candidate_sha:
            raise ValueError("evidence candidate_sha does not match candidate")
        for slice_id, state in sorted((evidence.get("slices") or {}).items()):
            require_id(str(slice_id), "evidence slice id")
            state = str(state)
            if state not in STATUS:
                raise ValueError(f"invalid slice state: {state}")
            lines.append(
                f"slice_evidence({atom(str(slice_id))}, {atom(candidate_sha)}, {state})."
            )
        for gate, state in sorted((evidence.get("gates") or {}).items()):
            require_id(str(gate), "evidence gate")
            state = str(state)
            if state not in STATUS:
                raise ValueError(f"invalid gate state: {state}")
            lines.append(f"gate_evidence({gate}, {atom(candidate_sha)}, {state}).")

        apk_sha = evidence.get("apk_source_sha")
        if apk_sha is not None:
            lines.append(f"apk_source_sha({atom(require_sha(str(apk_sha), 'apk_source_sha'))}).")
        lines.append(f"model_calls({int(evidence.get('model_calls', -1))}).")
        lines.append(
            f"unresolved_review_threads({int(evidence.get('unresolved_review_threads', -1))})."
        )

    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--plan", type=Path, required=True)
    parser.add_argument("--candidate-sha", required=True)
    parser.add_argument("--evidence", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    candidate_sha = require_sha(args.candidate_sha, "candidate_sha")
    plan = load_json(args.plan)
    evidence = load_json(args.evidence) if args.evidence else None
    text = render(plan, candidate_sha, evidence)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(text, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
