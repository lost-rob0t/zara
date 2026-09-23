from __future__ import annotations

from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]


def test_full_regression_gate_runs_native_emacs_ert() -> None:
    gate = (REPO_ROOT / "scripts" / "test-all.sh").read_text(encoding="utf-8")

    assert "emacs/zara-test.el" in gate
    assert "ert-run-tests-batch-and-exit" in gate


def test_full_regression_gate_pins_emacs_to_repository_nixpkgs() -> None:
    gate = (REPO_ROOT / "scripts" / "test-all.sh").read_text(encoding="utf-8")

    assert 'nix shell --inputs-from "$repo_root" nixpkgs#emacs-nox' in gate
    assert '-l "$repo_root/emacs/zara-test.el"' in gate


def test_flake_check_executes_native_emacs_ert() -> None:
    flake = (REPO_ROOT / "flake.nix").read_text(encoding="utf-8")

    assert 'emacs-client = pkgs.runCommand "zara-check-emacs-client"' in flake
    assert "nativeBuildInputs = [ pkgs.emacs-nox ];" in flake
    assert "emacs -Q --batch" in flake
    assert '-l "$src/emacs/zara-test.el"' in flake
    assert "ert-run-tests-batch-and-exit" in flake
