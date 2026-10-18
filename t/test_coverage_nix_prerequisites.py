from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def _pytest_check_source() -> str:
    source = (ROOT / "flake.nix").read_text(encoding="utf-8")
    return source.split(
        'pytest = pkgs.runCommand "zara-check-pytest"', 1
    )[1].split(
        'syntax = pkgs.runCommand "zara-check-syntax"', 1
    )[0]


def test_nix_pytest_check_declares_git_for_push_policy_regressions() -> None:
    pytest_check = _pytest_check_source()

    assert "pkgs.git" in pytest_check
