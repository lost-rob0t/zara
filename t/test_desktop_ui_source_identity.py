from __future__ import annotations

import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DESKTOP_EVIDENCE = ROOT / "scripts" / "desktop_ui_evidence.py"
SOURCE_SHA = "a" * 40
CLAIMED_SHA = "b" * 40


def _load():
    spec = importlib.util.spec_from_file_location(
        "zara_w10_desktop_source_identity",
        DESKTOP_EVIDENCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_desktop_cli_verifies_claimed_source_before_render(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load()
    verified_claims: list[str] = []
    rendered_sources: list[str] = []

    def verify(claimed: str) -> str:
        verified_claims.append(claimed)
        return SOURCE_SHA

    def render(_output: Path, *, source_commit: str):
        rendered_sources.append(source_commit)
        return {}

    monkeypatch.setattr(module, "verified_source_sha", verify, raising=False)
    monkeypatch.setattr(module, "render_desktop_ui_evidence", render)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "desktop_ui_evidence.py",
            "--output",
            str(tmp_path),
            "--source-sha",
            CLAIMED_SHA,
        ],
    )

    assert module.main() == 0
    assert verified_claims == [CLAIMED_SHA]
    assert rendered_sources == [SOURCE_SHA]


def _git(repo: Path, *arguments: str) -> str:
    return subprocess.check_output(
        ["git", *arguments],
        cwd=repo,
        text=True,
    ).strip()


def _init_git_repo(repo: Path) -> str:
    _git(repo, "init", "-q")
    (repo / "tracked.txt").write_text("base\n", encoding="utf-8")
    _git(repo, "add", "tracked.txt")
    subprocess.check_call(
        [
            "git",
            "-c",
            "user.name=W10",
            "-c",
            "user.email=w10@example.invalid",
            "commit",
            "-qm",
            "base",
        ],
        cwd=repo,
    )
    return _git(repo, "rev-parse", "HEAD")


@pytest.mark.parametrize(
    "staged",
    (False, True),
    ids=("unstaged", "staged"),
)
def test_desktop_source_identity_rejects_tracked_or_index_changes(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    staged: bool,
) -> None:
    module = _load()
    source_sha = _init_git_repo(tmp_path)
    (tmp_path / "tracked.txt").write_text("changed\n", encoding="utf-8")
    if staged:
        _git(tmp_path, "add", "tracked.txt")
    monkeypatch.setattr(module, "REPO_ROOT", tmp_path)

    with pytest.raises(RuntimeError, match="tracked or index changes"):
        module.verified_source_sha(source_sha)


def test_desktop_source_identity_ignores_untracked_evidence_output(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load()
    source_sha = _init_git_repo(tmp_path)
    (tmp_path / "evidence-output.png").write_bytes(b"synthetic output")
    monkeypatch.setattr(module, "REPO_ROOT", tmp_path)

    assert module.verified_source_sha(source_sha) == source_sha
