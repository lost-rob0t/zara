from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github" / "workflows" / "common-lisp.yml"
FLAKE = ROOT / "flake.nix"


def test_common_lisp_ci_uses_repo_pinned_nix_closure() -> None:
    workflow = WORKFLOW.read_text(encoding="utf-8")
    flake = FLAKE.read_text(encoding="utf-8")

    # Exact Zara SHAs must not resolve a mutable live Quicklisp distribution.
    assert "beta.quicklisp.org" not in workflow
    assert "quicklisp-quickstart:install" not in workflow
    assert "ql:quickload" not in workflow

    # Both FiveAM passes must consume the same repository-pinned Nix closure.
    assert workflow.count("nix develop .#lisp -c sbcl") == 2
    assert '"flake.nix"' in workflow
    assert '"flake.lock"' in workflow

    assert "devShells.lisp = pkgs.mkShell" in flake
    assert "pkgs.sbcl.withPackages" in flake
    for dependency in (
        "pzmq",
        "com_dot_inuoe_dot_jzon",
        "babel",
        "bordeaux-threads",
        "fiveam",
    ):
        assert dependency in flake
