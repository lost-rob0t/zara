from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "check-coverage-ratchet.py"
SPEC = importlib.util.spec_from_file_location("coverage_ratchet", SCRIPT)
assert SPEC is not None
assert SPEC.loader is not None
coverage_ratchet = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(coverage_ratchet)


def policy(line: float, branch: float, total: float) -> dict:
    return {
        "schema": 1,
        "python": {
            "line_floor_percent": line,
            "branch_floor_percent": branch,
            "total_floor_percent": total,
        },
        "ratchet": {
            "minimum_increment_points": 2.0,
            "target_increment_points": 10.0,
            "ultimate_target_percent": 100.0,
        },
    }


def test_coverage_metrics_reports_line_branch_and_combined_total():
    coverage = {
        "totals": {
            "covered_lines": 80,
            "num_statements": 100,
            "covered_branches": 30,
            "num_branches": 50,
            "percent_covered": 73.333333,
        }
    }

    assert coverage_ratchet.coverage_metrics(coverage) == {
        "line": 80.0,
        "branch": 60.0,
        "total": pytest.approx(73.333333),
    }


def test_reachable_python_change_requires_two_point_floor_increase(monkeypatch):
    monkeypatch.setattr(
        coverage_ratchet,
        "changed_paths",
        lambda _base: ["zara/runtime/commands.py", "t/test_runtime_commands.py"],
    )

    with pytest.raises(SystemExit):
        coverage_ratchet.check_ratchet(
            policy(11.9, 12.0, 12.0),
            policy(10.0, 10.0, 10.0),
            "origin/master",
        )

    changed, targets = coverage_ratchet.check_ratchet(
        policy(12.0, 12.0, 12.0),
        policy(10.0, 10.0, 10.0),
        "origin/master",
    )

    assert changed is True
    assert targets == {"line": 20.0, "branch": 20.0, "total": 20.0}


def test_target_and_minimum_cap_at_one_hundred(monkeypatch):
    monkeypatch.setattr(
        coverage_ratchet,
        "changed_paths",
        lambda _base: ["zara/server.py"],
    )

    changed, targets = coverage_ratchet.check_ratchet(
        policy(100.0, 100.0, 100.0),
        policy(99.0, 99.0, 99.0),
        "origin/master",
    )

    assert changed is True
    assert targets == {"line": 100.0, "branch": 100.0, "total": 100.0}


def test_non_product_change_can_hold_floor_but_never_lower_it(monkeypatch):
    monkeypatch.setattr(
        coverage_ratchet,
        "changed_paths",
        lambda _base: ["README.org", "docs/development.org"],
    )

    changed, targets = coverage_ratchet.check_ratchet(
        policy(25.0, 20.0, 23.0),
        policy(25.0, 20.0, 23.0),
        "origin/master",
    )

    assert changed is False
    assert targets == {"line": 35.0, "branch": 30.0, "total": 33.0}

    with pytest.raises(SystemExit):
        coverage_ratchet.check_ratchet(
            policy(24.9, 20.0, 23.0),
            policy(25.0, 20.0, 23.0),
            "origin/master",
        )


def test_actual_coverage_must_meet_committed_floor():
    coverage_ratchet.check_actual_against_floor(
        {"line": 50.0, "branch": 40.0, "total": 45.0},
        {"line": 50.0, "branch": 40.0, "total": 45.0},
    )

    with pytest.raises(SystemExit):
        coverage_ratchet.check_actual_against_floor(
            {"line": 49.99, "branch": 40.0, "total": 45.0},
            {"line": 50.0, "branch": 40.0, "total": 45.0},
        )


def test_full_repository_gate_reuses_canonical_coverage_authority():
    source = (ROOT / "scripts" / "test-all.sh").read_text(encoding="utf-8")

    assert "--cov=zara" in source
    assert "--cov-branch" in source
    assert 'coverage.json' in source
    assert source.count("scripts/check-coverage-ratchet.py") == 1
    assert "coverage-baseline.json" in source
