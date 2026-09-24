import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github" / "workflows" / "common-lisp.yml"
RUNNER = ROOT / "scripts" / "test-common-lisp.sh"
DEPS = ROOT / "lisp" / "deps.nix"
LOCK = ROOT / "flake.lock"


def test_common_lisp_ci_uses_repo_pinned_nix_closure() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    runner = RUNNER.read_text(encoding="utf-8")
    deps = DEPS.read_text(encoding="utf-8")
    lock = json.loads(LOCK.read_text(encoding="utf-8"))

    # Exact Zara SHAs must not resolve a mutable live Quicklisp distribution.
    for source in (workflow, runner):
        assert "beta.quicklisp.org" not in source
        assert "quicklisp-quickstart:install" not in source
        assert "ql:quickload" not in source

    # Both FiveAM passes, including packaged-daemon integration, share one runner.
    assert workflow.count("bash scripts/test-common-lisp.sh") == 2
    for watched_path in (
        '"lisp/**"',
        '"scripts/test-common-lisp.sh"',
        '"flake.nix"',
        '"flake.lock"',
    ):
        assert watched_path in workflow

    # The runner resolves nixpkgs from this repository's flake lock, not a registry.
    assert "builtins.getFlake (toString ./.)" in runner
    assert "flake.inputs.nixpkgs" in runner
    assert "nix shell --impure --expr" in runner
    assert "pkgs.sbcl.withPackages" in deps
    for dependency in (
        "pzmq",
        "com_dot_inuoe_dot_jzon",
        "babel",
        "bordeaux-threads",
        "fiveam",
    ):
        assert dependency in deps

    locked = lock["nodes"]["nixpkgs"]["locked"]
    assert len(locked["rev"]) == 40
    assert locked["narHash"].startswith("sha256-")
