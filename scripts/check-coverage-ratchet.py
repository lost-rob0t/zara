#!/usr/bin/env python3
"""Enforce Zara's monotonic Python coverage ratchet."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any


EPSILON = 1e-6


def load_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def load_policy_from_ref(ref: str, path: str) -> dict[str, Any] | None:
    result = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return json.loads(result.stdout)


def changed_paths(base_ref: str) -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", f"{base_ref}...HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def percent(numerator: int | float, denominator: int | float) -> float:
    if denominator == 0:
        return 100.0
    return (float(numerator) / float(denominator)) * 100.0


def coverage_metrics(coverage: dict[str, Any]) -> dict[str, float]:
    totals = coverage["totals"]
    return {
        "line": percent(totals["covered_lines"], totals["num_statements"]),
        "branch": percent(totals["covered_branches"], totals["num_branches"]),
        "total": float(totals["percent_covered"]),
    }


def policy_floors(policy: dict[str, Any]) -> dict[str, float]:
    python = policy["python"]
    return {
        "line": float(python["line_floor_percent"]),
        "branch": float(python["branch_floor_percent"]),
        "total": float(python["total_floor_percent"]),
    }


def fail(message: str) -> None:
    print(f"COVERAGE GATE FAIL: {message}", file=sys.stderr)
    raise SystemExit(1)


def check_actual_against_floor(
    metrics: dict[str, float],
    floors: dict[str, float],
) -> None:
    for name in ("line", "branch", "total"):
        if metrics[name] + EPSILON < floors[name]:
            fail(
                f"{name} coverage {metrics[name]:.2f}% is below "
                f"the committed floor {floors[name]:.2f}%"
            )


def check_policy_shape(policy: dict[str, Any]) -> None:
    ratchet = policy["ratchet"]
    minimum = float(ratchet["minimum_increment_points"])
    target = float(ratchet["target_increment_points"])
    ultimate = float(ratchet["ultimate_target_percent"])
    if minimum != 2.0:
        fail(f"minimum ratchet must remain 2.0 points, got {minimum:.2f}")
    if target != 10.0:
        fail(f"target ratchet must remain 10.0 points, got {target:.2f}")
    if ultimate != 100.0:
        fail(f"ultimate coverage target must remain 100.0%, got {ultimate:.2f}")
    for name, value in policy_floors(policy).items():
        if value < 0.0 or value > 100.0:
            fail(f"{name} floor must be within 0..100, got {value:.2f}")


def check_ratchet(
    policy: dict[str, Any],
    base_policy: dict[str, Any] | None,
    base_ref: str | None,
) -> tuple[bool, dict[str, float] | None]:
    if base_policy is None:
        return False, None

    current = policy_floors(policy)
    base = policy_floors(base_policy)
    ratchet = policy["ratchet"]
    minimum = float(ratchet["minimum_increment_points"])
    target = float(ratchet["target_increment_points"])
    ultimate = float(ratchet["ultimate_target_percent"])

    for name in ("line", "branch", "total"):
        if current[name] + EPSILON < base[name]:
            fail(
                f"{name} floor regressed from {base[name]:.2f}% "
                f"to {current[name]:.2f}%"
            )

    paths = changed_paths(base_ref) if base_ref else []
    product_python_changed = any(
        path.startswith("zara/") and path.endswith(".py") for path in paths
    )
    targets = {
        name: min(ultimate, base[name] + target)
        for name in ("line", "branch", "total")
    }

    if product_python_changed:
        for name in ("line", "branch", "total"):
            required = min(ultimate, base[name] + minimum)
            if current[name] + EPSILON < required:
                fail(
                    f"{name} floor must increase by at least {minimum:.2f} "
                    f"points for reachable Python changes: "
                    f"base={base[name]:.2f}% required={required:.2f}% "
                    f"head={current[name]:.2f}%"
                )

    return product_python_changed, targets


def write_summary(
    metrics: dict[str, float],
    floors: dict[str, float],
    product_python_changed: bool,
    targets: dict[str, float] | None,
) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return

    lines = [
        "## Coverage ratchet",
        "",
        "| Metric | Actual | Committed floor |",
        "| --- | ---: | ---: |",
        f"| Line | {metrics['line']:.2f}% | {floors['line']:.2f}% |",
        f"| Branch | {metrics['branch']:.2f}% | {floors['branch']:.2f}% |",
        f"| Combined total | {metrics['total']:.2f}% | {floors['total']:.2f}% |",
        "",
        f"Reachable Python changed: **{'yes' if product_python_changed else 'no'}**",
    ]
    if targets:
        lines.extend(
            [
                "",
                "Target for this slice: **+10 percentage points**, capped at 100%.",
                "",
                "| Metric | +10 target floor |",
                "| --- | ---: |",
                f"| Line | {targets['line']:.2f}% |",
                f"| Branch | {targets['branch']:.2f}% |",
                f"| Combined total | {targets['total']:.2f}% |",
            ]
        )

    with Path(summary_path).open("a", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--coverage", type=Path, required=True)
    parser.add_argument("--policy", type=Path, default=Path("coverage-baseline.json"))
    parser.add_argument("--base-ref")
    args = parser.parse_args()

    coverage = load_json(args.coverage)
    policy = load_json(args.policy)
    check_policy_shape(policy)

    metrics = coverage_metrics(coverage)
    floors = policy_floors(policy)
    check_actual_against_floor(metrics, floors)

    base_policy = None
    if args.base_ref:
        policy_ref_path = args.policy
        if policy_ref_path.is_absolute():
            try:
                policy_ref_path = policy_ref_path.relative_to(Path.cwd())
            except ValueError:
                fail(
                    "coverage policy must live inside the repository when "
                    "comparing against a base ref"
                )
        base_policy = load_policy_from_ref(
            args.base_ref,
            policy_ref_path.as_posix(),
        )

    product_python_changed, targets = check_ratchet(
        policy,
        base_policy,
        args.base_ref,
    )

    print(
        "coverage: "
        f"line={metrics['line']:.2f}% "
        f"branch={metrics['branch']:.2f}% "
        f"total={metrics['total']:.2f}%"
    )
    print(
        "floors: "
        f"line={floors['line']:.2f}% "
        f"branch={floors['branch']:.2f}% "
        f"total={floors['total']:.2f}%"
    )
    if product_python_changed:
        print("ratchet: reachable Python changed; >=2 point floor increase enforced")
        print("target: +10 points this slice, capped at 100%")
    else:
        print("ratchet: no reachable Python production change; floors may stay flat")

    write_summary(metrics, floors, product_python_changed, targets)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
