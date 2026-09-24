"""Mutation-epoch regressions for ZARA-VERIFY/1 host authority."""
from __future__ import annotations

from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
import verification.zara_verify_runner as runner


TRANSIENT_MUTATION = r'''
from pathlib import Path
import sys

path = Path(sys.argv[1])
replacement = sys.argv[2].encode()
original = path.read_bytes()
try:
    path.write_bytes(replacement)
    if path.read_bytes() != replacement:
        raise SystemExit(3)
    print('transient-observed', flush=True)
finally:
    path.write_bytes(original)
'''


TRANSIENT_NEW_PATH = r'''
from pathlib import Path
import sys

path = Path(sys.argv[1])
content = sys.argv[2]
if path.exists():
    raise SystemExit(4)
try:
    path.write_text(content)
    if path.read_text() != content:
        raise SystemExit(3)
    print('transient-new-path-observed', flush=True)
finally:
    path.unlink(missing_ok=True)
'''


def git(root: Path, *args: str) -> str:
    return subprocess.check_output(['git', '-C', str(root), *args], text=True).strip()


@pytest.fixture
def repository(tmp_path: Path) -> Path:
    root = tmp_path / 'repo'
    root.mkdir()
    git(root, 'init', '-q')
    git(root, 'config', 'user.email', 'fixture@example.invalid')
    git(root, 'config', 'user.name', 'Fixture')
    (root / 'rules.pl').write_text('fact(alpha).\n')
    (root / 'pkg').mkdir()
    (root / 'pkg' / 'anchor.py').write_text('ANCHOR = True\n')
    git(root, 'add', '.')
    git(root, 'commit', '-qm', 'initial')
    return root


@pytest.fixture
def policy_root(tmp_path: Path) -> Path:
    root = tmp_path / 'policy'
    for name in runner.PROTECTED:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text('stable verifier authority\n')
    return root


def verified_policy(request, _policy_root):
    if request['operation'] == 'plan':
        return {'protocol': runner.PROTOCOL, 'required': ['fixture']}
    return {
        'protocol': runner.PROTOCOL,
        'required': ['fixture'],
        'verdict': 'verified',
        'reasons': [],
    }


def run_fixture(repository: Path, policy_root: Path, monkeypatch, argv: list[str]):
    gate = {
        'collector': 'local',
        'argv': argv,
        'requires': [],
        'timeout_seconds': 3,
    }
    monkeypatch.setattr(runner, 'invoke_policy', verified_policy)
    monkeypatch.setattr(runner, 'load_spec', lambda _root: {'gates': {'fixture': gate}})
    return runner.run_verification(repository, 'HEAD', policy_root, 'session')


def test_transient_tracked_source_mutation_invalidates_verified_receipt(
        repository, policy_root, monkeypatch):
    target = repository / 'rules.pl'
    original = target.read_bytes()

    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', TRANSIENT_MUTATION, str(target), 'fact(transient).\n'],
    )

    assert target.read_bytes() == original
    assert result['verdict'] == 'blocked'
    assert 'source_mutated_during_verification' in result['reasons']


def test_transient_protected_policy_mutation_invalidates_verified_receipt(
        repository, policy_root, monkeypatch):
    target = policy_root / 'verification/zara_verify.pl'
    original = target.read_bytes()

    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', TRANSIENT_MUTATION, str(target), 'transient verifier authority\n'],
    )

    assert target.read_bytes() == original
    assert result['verdict'] == 'blocked'
    assert 'source_mutated_during_verification' in result['reasons']


def test_transient_new_source_path_invalidates_verified_receipt(
        repository, policy_root, monkeypatch):
    target = repository / 'pkg' / 'transient.py'

    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', TRANSIENT_NEW_PATH, str(target), 'TRANSIENT = True\\n'],
    )

    assert not target.exists()
    assert result['verdict'] == 'blocked'
    assert 'source_mutated_during_verification' in result['reasons']


def test_transient_new_policy_path_invalidates_verified_receipt(
        repository, policy_root, monkeypatch):
    target = policy_root / 'verification' / 'transient_helper.py'

    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', TRANSIENT_NEW_PATH, str(target), 'TRANSIENT = True\\n'],
    )

    assert not target.exists()
    assert result['verdict'] == 'blocked'
    assert 'source_mutated_during_verification' in result['reasons']


def test_ignored_generated_namespace_is_not_source_mutation(
        repository, policy_root, monkeypatch):
    (repository / '.gitignore').write_text('pkg/*.tmp\\n')
    git(repository, 'add', '.gitignore')
    git(repository, 'commit', '-qm', 'ignore generated fixture')
    target = repository / 'pkg' / 'generated.tmp'
    assert runner.git_ignored(repository, 'pkg/generated.tmp')

    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', TRANSIENT_NEW_PATH, str(target), 'generated\\n'],
    )

    assert not target.exists()
    assert result['verdict'] == 'verified', result['reasons']
    assert result['reasons'] == []


def test_untouched_gate_preserves_verified_receipt(repository, policy_root, monkeypatch):
    result = run_fixture(
        repository,
        policy_root,
        monkeypatch,
        [sys.executable, '-c', 'print("ok")'],
    )

    assert result['verdict'] == 'verified'
    assert result['reasons'] == []


def test_policy_evaluation_mutation_invalidates_verified_receipt(repository, policy_root, monkeypatch):
    gate = {
        'collector': 'local',
        'argv': [sys.executable, '-c', 'print("ok")'],
        'requires': [],
        'timeout_seconds': 3,
    }

    def policy(request, _policy_root):
        if request['operation'] == 'plan':
            return {'protocol': runner.PROTOCOL, 'required': ['fixture']}
        (repository / 'rules.pl').write_text('fact(mutated_during_evaluate).\n')
        return {
            'protocol': runner.PROTOCOL,
            'required': ['fixture'],
            'verdict': 'verified',
            'reasons': [],
        }

    monkeypatch.setattr(runner, 'invoke_policy', policy)
    monkeypatch.setattr(runner, 'load_spec', lambda _root: {'gates': {'fixture': gate}})

    result = runner.run_verification(repository, 'HEAD', policy_root, 'session')

    assert result['verdict'] == 'blocked'
    assert 'source_changed_during_verification' in result['reasons']
