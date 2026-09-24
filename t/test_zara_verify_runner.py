"""Host-side tests. Prolog policy has a separate, required plunit gate."""
from __future__ import annotations

import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import time

import pytest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from verification.zara_verify_runner import (
    VerificationError, canonical_digest, collect_snapshot, current_report,
    execute_gate, gate_environment, load_spec, parse_junit, run_verification,
)


def git(root, *args):
    return subprocess.check_output(['git', '-C', str(root), *args], text=True).strip()


@pytest.fixture
def repository(tmp_path):
    git(tmp_path, 'init', '-q')
    git(tmp_path, 'config', 'user.email', 'fixture@example.invalid')
    git(tmp_path, 'config', 'user.name', 'Fixture')
    (tmp_path / 'rules.pl').write_text('fact(alpha).\n')
    git(tmp_path, 'add', '.')
    git(tmp_path, 'commit', '-qm', 'initial')
    return tmp_path


def snapshot(root):
    return collect_snapshot(root, 'HEAD', ROOT)


def report(source):
    return {'protocol': 'ZARA-VERIFY/1', 'scope': 'local', 'verdict': 'verified',
            'source': source, 'run_id': 'a' * 32, 'created_ms': int(time.time() * 1000),
            'ttl_ms': 600000, 'merge_authorized': False, 'model_calls': 0,
            'provider_calls': 0, 'reasons': [], 'evidence': []}


def test_canonical_digest_is_order_independent():
    assert canonical_digest({'a': 1, 'b': 2}) == canonical_digest({'b': 2, 'a': 1})
    assert canonical_digest({'a': 1}) != canonical_digest({'a': True})


def test_snapshot_sees_untracked_files_and_same_length_edits(repository):
    first = snapshot(repository)
    (repository / 'rules.pl').write_text('fact(bravo).\n')
    second = snapshot(repository)
    assert first['worktree'] != second['worktree']
    assert second['changed_paths'] == ['rules.pl']
    (repository / 'new file.pl').write_text('fact(charlie).\n')
    third = snapshot(repository)
    assert third['worktree'] != second['worktree']
    assert 'new file.pl' in third['changed_paths']


def test_snapshot_observes_deletion_and_unborn_repository(repository):
    (repository / 'rules.pl').unlink()
    assert snapshot(repository)['changed_paths'] == ['rules.pl']
    with pytest.raises(VerificationError):
        collect_snapshot(repository, '--help', ROOT)


def test_snapshot_binds_root_and_permissions(repository):
    first = snapshot(repository)
    (repository / 'rules.pl').chmod(0o755)
    assert snapshot(repository)['worktree'] != first['worktree']
    assert first['workspace'] == str(repository.resolve())


def test_snapshot_rejects_gitlinks_and_oversize_files(repository):
    git(repository, 'update-index', '--add', '--cacheinfo',
        '160000,' + git(repository, 'rev-parse', 'HEAD') + ',submodule')
    with pytest.raises(VerificationError, match='submodule'):
        snapshot(repository)


def test_snapshot_does_not_follow_symlinks(repository, tmp_path):
    target = repository.parent / 'outside.txt'
    target.write_text('outside')
    (repository / 'link').symlink_to(target)
    first = snapshot(repository)
    target.write_text('changed outside')
    assert snapshot(repository)['worktree'] == first['worktree']


def test_deleted_and_renamed_paths_remain_in_impact(repository):
    base = git(repository, 'rev-parse', 'HEAD')
    git(repository, 'mv', 'rules.pl', 'rules.txt')
    git(repository, 'commit', '-qm', 'rename')
    result = collect_snapshot(repository, base, ROOT)
    assert result['changed_paths'] == ['rules.pl', 'rules.txt']


def test_report_is_bound_to_snapshot_and_freshness(repository):
    source = snapshot(repository)
    good = report(source)
    assert current_report(good, source)
    for key in ['head', 'base', 'worktree', 'policy', 'workspace']:
        changed = copy.deepcopy(source)
        changed[key] = 'different'
        assert not current_report(good, changed)
    old = {**good, 'created_ms': 1}
    assert not current_report(old, source)
    future = {**good, 'created_ms': int(time.time() * 1000) + 5000}
    assert not current_report(future, source)


@pytest.mark.parametrize('patch', [
    {'verdict': 'passed'}, {'scope': 'merge'}, {'model_calls': False},
    {'model_calls': 1}, {'provider_calls': False}, {'provider_calls': 1},
    {'merge_authorized': True}, {'ttl_ms': True}, {'protocol': 'ZARA-VERIFY/2'},
    {'reasons': ['failed']}, {'created_ms': True},
])
def test_report_rejects_false_or_wrong_scope_success(repository, patch):
    source = snapshot(repository)
    assert not current_report({**report(source), **patch}, source)


def test_environment_does_not_forward_provider_credentials(monkeypatch, tmp_path):
    monkeypatch.setenv('OPENAI_API_KEY', 'never-copy')
    monkeypatch.setenv('GH_TOKEN', 'never-copy')
    monkeypatch.setenv('PYTHONPATH', '/attacker')
    env = gate_environment(tmp_path)
    assert 'OPENAI_API_KEY' not in env
    assert 'GH_TOKEN' not in env
    assert 'PYTHONPATH' not in env
    assert env['HOME'].startswith(str(tmp_path))


def test_executor_collects_real_exit_and_log_digest(tmp_path):
    result = execute_gate('fixture', [sys.executable, '-c', 'print("ok")'],
                          tmp_path, tmp_path / 'evidence', 3, 1024)
    assert result['state'] == 'passed'
    assert result['exit_code'] == 0
    assert Path(result['artifact']).read_bytes() == b'ok\n'
    assert result['bytes'] == 3
    assert len(result['artifact_sha256']) == 64


def test_executor_does_not_turn_failure_into_success(tmp_path):
    result = execute_gate('fixture', [sys.executable, '-c', 'raise SystemExit(7)'],
                          tmp_path, tmp_path / 'evidence', 3, 1024)
    assert result['state'] == 'failed' and result['exit_code'] == 7


def test_executor_bounds_output_and_runtime(tmp_path):
    result = execute_gate('flood', [sys.executable, '-c', 'print("x"*100000)'],
                          tmp_path, tmp_path / 'flood', 3, 1024)
    assert result['state'] == 'error' and result['reason'] == 'output_limit'
    assert Path(result['artifact']).stat().st_size <= 1024
    result = execute_gate('sleep', [sys.executable, '-c', 'import time;time.sleep(5)'],
                          tmp_path, tmp_path / 'sleep', 0.05, 1024)
    assert result['state'] == 'error' and result['reason'] == 'timeout'


def test_executor_missing_executable_is_blocked(tmp_path):
    result = execute_gate('missing', ['/no/such/zara/executable'],
                          tmp_path, tmp_path / 'missing', 1, 1024)
    assert result['state'] == 'missing'


@pytest.mark.parametrize('xml', [
    '<testsuites/>', '<testsuite><testcase><skipped/></testcase></testsuite>',
    '<testsuite><testcase><failure/></testcase></testsuite>',
    '<!DOCTYPE x [<!ENTITY x "hi">]><testsuite/>', '<broken',
])
def test_junit_cannot_pass_empty_skipped_failed_or_hostile_xml(tmp_path, xml):
    path = tmp_path / 'junit.xml'
    path.write_text(xml)
    assert parse_junit(path)['state'] != 'passed'


def test_junit_requires_real_test_cases_not_summary_attributes(tmp_path):
    path = tmp_path / 'junit.xml'
    path.write_text('<testsuite tests="999"><testcase name="real"/></testsuite>')
    result = parse_junit(path)
    assert result == {'state': 'passed', 'tests': 1, 'failures': 0, 'skipped': 0}


def test_spec_has_closed_commands_and_bounded_limits():
    spec = load_spec(ROOT)
    assert spec['protocol'] == 'ZARA-VERIFY/1'
    assert spec['gates']['repository']['argv'] == ['bash', 'scripts/test-all.sh']
    assert spec['gates']['coverage']['argv'] == [
        'bash', 'scripts/test-coverage.sh', '--base-ref', '@SOURCE_BASE@']
    assert all(gate['timeout_seconds'] <= 3600 for gate in spec['gates'].values())


def test_runner_missing_prolog_is_explicitly_blocked(repository, monkeypatch):
    import verification.zara_verify_runner as runner
    monkeypatch.setattr(runner.shutil, 'which', lambda name: None if name == 'swipl' else '/usr/bin/' + name)
    result = run_verification(repository, 'HEAD', ROOT, 'fixture-session')
    assert result['verdict'] == 'blocked'
    assert result['model_calls'] == 0
    assert result['provider_calls'] == 0
    assert result['evidence'] == []
    assert 'prolog_unavailable' in result['reasons']


def fixture_spec(argv=None, collector='local', requires=None, junit=None):
    gate = {'collector': collector, 'argv': argv or [], 'requires': requires or [],
            'timeout_seconds': 3}
    if junit:
        gate['junit'] = junit
    return gate


def host_fixture(monkeypatch, required, spec):
    import verification.zara_verify_runner as runner
    def policy(request, _root):
        if request['operation'] == 'plan':
            return {'protocol': runner.PROTOCOL, 'required': required}
        bad = [row['gate'] for row in request['evidence'] if row['state'] != 'passed']
        return {'protocol': runner.PROTOCOL, 'required': required,
                'verdict': 'blocked' if bad else 'verified', 'reasons': bad}
    monkeypatch.setattr(runner, 'invoke_policy', policy)
    monkeypatch.setattr(runner, 'load_spec', lambda _root: {'gates': spec})


def test_host_runs_observed_commands_and_binds_evidence(repository, monkeypatch):
    host_fixture(monkeypatch, ['fixture'], {
        'fixture': fixture_spec([sys.executable, '-c', 'print("observed")'])})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert result['verdict'] == 'verified'
    assert result['merge_authorized'] is False
    assert result['model_calls'] == 0 and result['provider_calls'] == 0
    row = result['evidence'][0]
    assert row['source_digest'] == canonical_digest(result['source'])
    assert row['run_id'] == result['run_id']
    assert Path(row['artifact']).read_text() == 'observed\n'
    assert (Path(row['artifact']).parents[1] / 'observations.json').is_file()


def test_dependency_failure_prevents_child_execution(repository, monkeypatch):
    marker = repository / 'must-not-exist'
    host_fixture(monkeypatch, ['first', 'second'], {
        'first': fixture_spec([sys.executable, '-c', 'raise SystemExit(7)']),
        'second': fixture_spec([sys.executable, '-c', f'open({str(marker)!r}, "w")'],
                               requires=['first'])})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert [item['state'] for item in result['evidence']] == ['failed', 'blocked']
    assert not marker.exists()
    assert result['verdict'] == 'blocked'


@pytest.mark.parametrize('requires', [['other'], ['fixture']])
def test_missing_dependency_and_cycle_fail_closed(repository, monkeypatch, requires):
    host_fixture(monkeypatch, ['fixture'], {'fixture': fixture_spec(
        [sys.executable, '-c', 'print("never")'], requires=requires)})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert result['verdict'] == 'blocked'
    assert result['evidence'] == []
    assert any(code in result['reasons'] for code in ['dependency_cycle', 'incomplete_dependency_plan'])


def test_external_evidence_is_not_manufactured(repository, monkeypatch):
    host_fixture(monkeypatch, ['fixture'], {'fixture': fixture_spec(collector='external')})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert result['verdict'] == 'blocked'
    assert result['evidence'][0]['state'] == 'missing'
    assert result['evidence'][0]['artifact_sha256'] == ''


def test_source_mutation_during_gate_invalidates_success(repository, monkeypatch):
    command = 'from pathlib import Path;Path("rules.pl").write_text("changed")'
    host_fixture(monkeypatch, ['fixture'], {'fixture': fixture_spec([sys.executable, '-c', command])})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert result['verdict'] == 'blocked'
    assert 'source_changed_during_verification' in result['reasons']


def test_empty_junit_blocks_successful_command(repository, monkeypatch):
    host_fixture(monkeypatch, ['fixture'], {'fixture': fixture_spec(
        [sys.executable, '-c', 'print("not a test")'], junit='junit.xml')})
    result = run_verification(repository, 'HEAD', ROOT, 'session')
    assert result['verdict'] == 'blocked'
    assert result['evidence'][0]['reason'] == 'junit_not_passed'


@pytest.mark.parametrize('field,value', [
    ('requires', 'fixture'), ('requires', [True]), ('requires', ['fixture', 'fixture']),
    ('junit', '../escape.xml'), ('junit', '/absolute.xml'), ('argv', ['x' * 5000]),
])
def test_spec_rejects_unbounded_or_escaping_fields(tmp_path, field, value):
    gate = fixture_spec(['true'])
    gate[field] = value
    path = tmp_path / 'contracts/zara-verify-v1'
    path.mkdir(parents=True)
    (path / 'spec.json').write_text(json.dumps({'protocol': 'ZARA-VERIFY/1', 'gates': {'fixture': gate}}))
    with pytest.raises(VerificationError):
        load_spec(tmp_path)
