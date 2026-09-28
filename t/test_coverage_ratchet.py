from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

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


def test_missing_base_policy_cannot_bypass_python_change_ratchet(monkeypatch):
    calls = []
    monkeypatch.setattr(
        coverage_ratchet,
        "changed_paths",
        lambda base: calls.append(base) or ["zara/server.py"],
    )

    with pytest.raises(SystemExit):
        coverage_ratchet.check_ratchet(
            policy(80.0, 66.8, 76.8),
            None,
            "origin/release/0.3.x",
        )

    assert calls == ["origin/release/0.3.x"]


def test_missing_base_policy_bootstraps_only_nonproduct_changes(monkeypatch):
    calls = []
    monkeypatch.setattr(
        coverage_ratchet,
        "changed_paths",
        lambda base: calls.append(base) or ["coverage-baseline.json"],
    )

    changed, targets = coverage_ratchet.check_ratchet(
        policy(80.0, 66.8, 76.8),
        None,
        "origin/release/0.3.x",
    )

    assert calls == ["origin/release/0.3.x"]
    assert changed is False
    assert targets is None


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
    assert "coverage.json" in source
    assert source.count("scripts/check-coverage-ratchet.py") == 1
    assert "coverage-baseline.json" in source


def test_nix_pytest_check_declares_bash_prerequisite():
    source = (ROOT / "flake.nix").read_text(encoding="utf-8")
    pytest_check = source.split(
        'pytest = pkgs.runCommand "zara-check-pytest"', 1
    )[1].split(
        'syntax = pkgs.runCommand "zara-check-syntax"', 1
    )[0]

    assert "pkgs.bash" in pytest_check


def test_coverage_entrypoint_rejects_unknown_arguments_before_pytest(tmp_path):
    fake_bin = tmp_path / "bin"
    fake_bin.mkdir()
    marker = tmp_path / "python-ran"
    fake_python = fake_bin / "python"
    fake_python.write_text(
        "#!/bin/sh\nprintf ran > \"$COVERAGE_PYTHON_MARKER\"\nexit 0\n",
        encoding="utf-8",
    )
    fake_python.chmod(0o755)
    env = os.environ.copy()
    env["PATH"] = f"{fake_bin}:{env.get('PATH', '')}"
    env["COVERAGE_PYTHON_MARKER"] = str(marker)
    env["ARTIFACT_DIR"] = str(tmp_path / "artifacts")

    result = subprocess.run(
        ["bash", str(ROOT / "scripts" / "test-coverage.sh"), "--bogus"],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )

    assert result.returncode == 2
    assert not marker.exists()


def _git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=repo,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def _write_coverage(path: Path) -> None:
    path.write_text(
        json.dumps(
            {
                "totals": {
                    "covered_lines": 100,
                    "num_statements": 100,
                    "covered_branches": 100,
                    "num_branches": 100,
                    "percent_covered": 100.0,
                }
            }
        ),
        encoding="utf-8",
    )


def _init_push_policy_repo(
    tmp_path: Path,
    *,
    base_policy: dict,
    head_policy: dict,
    product_change: bool,
) -> tuple[Path, str]:
    repo = tmp_path / "repo"
    repo.mkdir()
    _git(repo, "init", "-q")
    _git(repo, "config", "user.name", "coverage-test")
    _git(repo, "config", "user.email", "coverage-test@example.invalid")

    (repo / "coverage-baseline.json").write_text(
        json.dumps(base_policy), encoding="utf-8"
    )
    _git(repo, "add", "coverage-baseline.json")
    _git(repo, "commit", "-qm", "base policy")
    base_sha = _git(repo, "rev-parse", "HEAD")

    (repo / "coverage-baseline.json").write_text(
        json.dumps(head_policy), encoding="utf-8"
    )
    if product_change:
        production = repo / "zara" / "runtime" / "push_probe.py"
        production.parent.mkdir(parents=True)
        production.write_text("VALUE = 1\n", encoding="utf-8")
    _git(repo, "add", ".")
    _git(repo, "commit", "-qm", "head policy")
    _write_coverage(repo / "coverage.json")
    return repo, base_sha


def _run_checker_for_push(repo: Path, base_sha: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env["CI"] = "true"
    env["ZARA_COVERAGE_BASE_REF"] = base_sha
    return subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--coverage",
            "coverage.json",
            "--policy",
            "coverage-baseline.json",
        ],
        cwd=repo,
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )


def test_push_trusted_base_rejects_floor_regression(tmp_path):
    repo, base_sha = _init_push_policy_repo(
        tmp_path,
        base_policy=policy(80.0, 66.8, 76.8),
        head_policy=policy(79.9, 66.8, 76.8),
        product_change=False,
    )

    result = _run_checker_for_push(repo, base_sha)

    assert result.returncode == 1
    assert "line floor regressed" in result.stderr


def test_push_trusted_base_cannot_bypass_reachable_python_ratchet(tmp_path):
    repo, base_sha = _init_push_policy_repo(
        tmp_path,
        base_policy=policy(80.0, 66.8, 76.8),
        head_policy=policy(80.0, 66.8, 76.8),
        product_change=True,
    )

    result = _run_checker_for_push(repo, base_sha)

    assert result.returncode == 1
    assert "must increase by at least 2.00 points" in result.stderr


def test_ci_without_trusted_coverage_base_fails_closed(tmp_path):
    coverage = tmp_path / "coverage.json"
    policy_path = tmp_path / "coverage-baseline.json"
    _write_coverage(coverage)
    policy_path.write_text(json.dumps(policy(80.0, 66.8, 76.8)), encoding="utf-8")
    env = os.environ.copy()
    env["CI"] = "true"
    env.pop("ZARA_COVERAGE_BASE_REF", None)

    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--coverage",
            str(coverage),
            "--policy",
            str(policy_path),
        ],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )

    assert result.returncode == 1
    assert "trusted coverage base ref" in result.stderr


def test_coverage_workflow_resolves_push_before_and_rejects_branch_create():
    source = (ROOT / ".github" / "workflows" / "coverage.yml").read_text(
        encoding="utf-8"
    )

    assert "${{ github.event.before }}" in source
    assert "0000000000000000000000000000000000000000" in source
    assert "git cat-file -e" in source
    assert "ZARA_COVERAGE_BASE_REF" in source
    assert 'base_ref="origin/$PR_BASE_REF"' in source


def test_full_ci_propagates_the_same_trusted_coverage_base():
    source = (ROOT / ".github" / "workflows" / "ci.yml").read_text(
        encoding="utf-8"
    )

    assert "${{ github.event.before }}" in source
    assert "0000000000000000000000000000000000000000" in source
    assert "git cat-file -e" in source
    assert "ZARA_COVERAGE_BASE_REF" in source
    assert 'base_ref="origin/$PR_BASE_REF"' in source
