"""Mutation-epoch regressions for ZARA-VERIFY/1 host authority."""
from __future__ import annotations

from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
import verification.zara_verify_runner as runner


def git(root: Path, *args: str) -> str:
    return subprocess.check_output(['git', '-C', str(root), *args], text=True).strip()


@pytest.fixture
def repository(tmp_path: Path) -> Path:
    git(tmp_path, 'init', '-q')
    git(tmp_path, 'config', 'user.email', 'fixture@example.invalid')
    git(tmp_path, 'config', 'user.name', 'Fixture')
    (tmp_path / 'rules.pl').write_text('fact(alpha).\n')
    git(tmp_path, 'add', '.')
    git(tmp_path, 'commit', '-qm', 'initial')
    return tmp_path


def test_policy_evaluation_mutation_invalidates_verified_receipt(repository, monkeypatch):
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

    result = runner.run_verification(repository, 'HEAD', ROOT, 'session')

    assert result['verdict'] == 'blocked'
    assert 'source_changed_during_verification' in result['reasons']
