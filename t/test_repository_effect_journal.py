from __future__ import annotations

import os
import subprocess
from concurrent.futures import ThreadPoolExecutor

import pytest

from zara.tasks.effect_journal import RepositoryEffectJournal


@pytest.fixture
def repository(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    return tmp_path


def claim(journal, *, principal="user:alice", attempt="attempt-1", fingerprint="effect-1", args=None):
    return journal.claim(principal, attempt, fingerprint, "write_file", args or {"path": "a.txt"})


def test_claim_complete_and_replay_across_restart(repository):
    journal = RepositoryEffectJournal(repository)
    first = claim(journal)
    assert first.status == "claimed"
    journal.complete(first, "wrote a.txt")
    second = claim(RepositoryEffectJournal(repository))
    assert second.status == "replay"
    assert second.result == "wrote a.txt"
    assert second.claim_token is None


def test_in_progress_or_crashed_attempt_is_indeterminate(repository):
    first = claim(RepositoryEffectJournal(repository))
    assert first.status == "claimed"
    duplicate = claim(RepositoryEffectJournal(repository))
    assert duplicate.status == "indeterminate"
    assert duplicate.claim_token is None


@pytest.mark.parametrize("change", [
    {"principal": "user:bob"},
    {"fingerprint": "different-effect"},
    {"args": {"path": "b.txt"}},
])
def test_mismatched_attempt_identity_is_denied(repository, change):
    journal = RepositoryEffectJournal(repository)
    assert claim(journal).status == "claimed"
    assert claim(journal, **change).status == "denied"


def test_only_one_concurrent_caller_can_claim(repository):
    journals = [RepositoryEffectJournal(repository) for _ in range(8)]
    with ThreadPoolExecutor(max_workers=8) as pool:
        outcomes = list(pool.map(claim, journals))
    assert [outcome.status for outcome in outcomes].count("claimed") == 1
    assert [outcome.status for outcome in outcomes].count("indeterminate") == 7


def test_completion_requires_original_claim_and_is_single_use(repository):
    journal = RepositoryEffectJournal(repository)
    first = claim(journal)
    other = claim(journal)
    with pytest.raises(PermissionError):
        journal.complete(other, "forged")
    journal.complete(first, "done")
    with pytest.raises(PermissionError):
        journal.complete(first, "again")


def test_repository_selection_is_prolog_bounded(repository):
    class Engine:
        def query_all(self, goal):
            assert goal == "kb_config:file_repository(Alias, Path)"
            return [{"Alias": "code", "Path": str(repository)}]

    journal = RepositoryEffectJournal.from_alias(Engine(), "code")
    assert claim(journal).status == "claimed"


def test_private_state_is_ignored_and_symlinked_state_is_rejected(repository, tmp_path):
    journal = RepositoryEffectJournal(repository)
    assert claim(journal).status == "claimed"
    effects = repository / ".zara" / "effects"
    assert effects.stat().st_mode & 0o777 == 0o700
    assert all(path.stat().st_mode & 0o777 == 0o600 for path in effects.iterdir())
    status = subprocess.run(
        ["git", "-C", str(repository), "status", "--short", "--untracked-files=all"],
        capture_output=True, text=True, check=True,
    )
    assert status.stdout == ""
    outside = tmp_path.parent / "outside-effects"
    outside.mkdir(exist_ok=True)
    os.rename(effects, repository / ".zara" / "old-effects")
    effects.symlink_to(outside, target_is_directory=True)
    with pytest.raises(ValueError, match="symlink"):
        RepositoryEffectJournal(repository)


def test_symlinked_attempt_file_cannot_be_claimed(repository, tmp_path):
    journal = RepositoryEffectJournal(repository)
    attempt = repository / ".zara" / "effects" / journal._attempt_name("attempt-1")
    outside = tmp_path.parent / "outside-attempt.json"
    outside.write_text("private")
    attempt.symlink_to(outside)
    with pytest.raises(OSError):
        claim(journal)
    assert outside.read_text() == "private"


def test_result_is_bounded_before_completion(repository):
    journal = RepositoryEffectJournal(repository)
    first = claim(journal)
    with pytest.raises(ValueError, match="size limit"):
        journal.complete(first, "x" * 8193)
    assert claim(RepositoryEffectJournal(repository)).status == "indeterminate"
