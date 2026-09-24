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
    git(tmp_path, 'commit', '-qm', 'base')
    return tmp_path


def host_fixture(monkeypatch, argv: list[str]) -> list[list[str]]:
    captured: list[list[str]] = []

    def policy(request, _root):
        if request['operation'] == 'plan':
            return {'protocol': runner.PROTOCOL, 'required': ['coverage']}
        bad = [row['gate'] for row in request['evidence'] if row['state'] != 'passed']
        return {
            'protocol': runner.PROTOCOL,
            'required': ['coverage'],
            'verdict': 'blocked' if bad else 'verified',
            'reasons': bad,
        }

    def execute(gate_id, command, _root, directory, _timeout):
        captured.append(command)
        directory.mkdir(parents=True, exist_ok=True)
        artifact = directory / 'output.log'
        artifact.write_text('ok\n')
        return {
            'gate': gate_id,
            'state': 'passed',
            'reason': 'exit_zero',
            'exit_code': 0,
            'duration_ms': 1,
            'artifact': str(artifact),
            'artifact_sha256': 'a' * 64,
            'bytes': 3,
        }

    monkeypatch.setattr(runner, 'invoke_policy', policy)
    monkeypatch.setattr(runner, 'load_spec', lambda _root: {'gates': {
        'coverage': {
            'collector': 'local',
            'argv': argv,
            'requires': [],
            'timeout_seconds': 3,
        },
    }})
    monkeypatch.setattr(runner, 'execute_gate', execute)
    return captured


def test_coverage_catalog_uses_only_resolved_source_base_sentinel():
    spec = runner.load_spec(ROOT)
    assert spec['gates']['coverage']['argv'] == [
        'bash', 'scripts/test-coverage.sh', '--base-ref', '@SOURCE_BASE@']


def test_run_materializes_coverage_base_from_resolved_snapshot(repository, monkeypatch):
    expected_base = git(repository, 'rev-parse', 'HEAD')
    (repository / 'rules.pl').write_text('fact(beta).\n')
    git(repository, 'add', 'rules.pl')
    git(repository, 'commit', '-qm', 'candidate')
    assert git(repository, 'rev-parse', 'HEAD') != expected_base

    captured = host_fixture(monkeypatch, [
        'bash', 'scripts/test-coverage.sh', '--base-ref', '@SOURCE_BASE@'])
    result = runner.run_verification(repository, expected_base, ROOT, 'base-identity')

    assert result['verdict'] == 'verified'
    assert result['source']['base'] == expected_base
    assert captured == [[
        'bash', 'scripts/test-coverage.sh', '--base-ref', expected_base]]


def test_partial_source_base_interpolation_fails_closed(repository, monkeypatch):
    captured = host_fixture(monkeypatch, ['echo', 'prefix-@SOURCE_BASE@'])
    result = runner.run_verification(repository, 'HEAD', ROOT, 'base-identity')

    assert result['verdict'] == 'blocked'
    assert result['evidence'] == []
    assert 'invalid_gate_base_placeholder' in result['reasons']
    assert captured == []
