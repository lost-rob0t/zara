"""RED-first verifier authority regressions for W09 integration."""
from __future__ import annotations

import copy
import json
from pathlib import Path
import sys

from jsonschema import Draft202012Validator

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
import verification.zara_verify_runner as runner


def _source() -> dict[str, object]:
    return {
        'workspace': '/repo', 'head': 'a' * 40, 'base': 'b' * 40,
        'merge_base': 'c' * 40, 'worktree': 'd' * 64, 'policy': 'e' * 64,
        'changed_paths': [], 'clean': True,
    }


def _report() -> dict[str, object]:
    return {
        'protocol': 'ZARA-VERIFY/1', 'scope': 'local', 'verdict': 'verified',
        'source': _source(), 'run_id': 'a' * 32, 'session_id': 'fixture',
        'created_ms': 1, 'ttl_ms': 600000, 'merge_authorized': False,
        'model_calls': 0, 'provider_calls': 0, 'reasons': [], 'evidence': [],
        'required': ['repository'],
        'expert': {
            'expert_id': 'zara:verifier', 'operation': 'verify.assert',
            'verdict': 'succeeded', 'data': {'verified': True},
            'usage': {'model_calls': 0, 'provider_calls': 0},
            'invocation_id': 'inv:fixture',
        },
    }


def test_schema_requires_authoritative_top_level_provider_calls_zero():
    schema = json.loads((ROOT / 'contracts/zara-verify-v1/report.schema.json').read_text())
    validator = Draft202012Validator(schema)
    good = _report()
    assert not list(validator.iter_errors(good))
    for mutation in ('missing', 'nonzero', 'boolean'):
        candidate = copy.deepcopy(good)
        if mutation == 'missing':
            del candidate['provider_calls']
        elif mutation == 'nonzero':
            candidate['provider_calls'] = 1
        else:
            candidate['provider_calls'] = False
        assert list(validator.iter_errors(candidate)), mutation


def test_current_report_fails_closed_on_provider_accounting():
    source = _source()
    good = _report()
    assert runner.current_report(good, source, now_ms=good['created_ms'])
    for value in (None, 1, False):
        candidate = copy.deepcopy(good)
        if value is None:
            del candidate['provider_calls']
        else:
            candidate['provider_calls'] = value
        assert not runner.current_report(candidate, source, now_ms=good['created_ms'])


def test_runner_receipt_owns_provider_counter_when_policy_is_unavailable(tmp_path, monkeypatch):
    monkeypatch.setattr(runner, 'collect_snapshot', lambda *_args, **_kwargs: _source())
    monkeypatch.setattr(runner, 'load_spec', lambda *_args, **_kwargs: {'gates': {}})
    monkeypatch.setattr(runner, 'invoke_policy', lambda *_args, **_kwargs: (_ for _ in ()).throw(runner.VerificationError('policy_unavailable')))
    result = runner.run_verification(tmp_path, 'HEAD', ROOT, 'fixture')
    assert result['provider_calls'] == 0
    assert result['model_calls'] == 0
    assert result['verdict'] == 'blocked'


def test_cli_default_base_is_release_lane(monkeypatch, tmp_path, capsys):
    seen: list[str] = []
    monkeypatch.setattr(runner, 'collect_snapshot', lambda _root, base, _policy: seen.append(base) or _source())
    monkeypatch.setattr(sys, 'argv', ['zara_verify_runner.py', 'snapshot', '--root', str(tmp_path)])
    assert runner.main() == 0
    assert seen == ['origin/release/0.3.x']
    payload = json.loads(capsys.readouterr().out)
    assert payload['protocol'] == 'ZARA-VERIFY/1'


def test_contract_docs_do_not_revive_master_or_absent_coverage_authority():
    spec = (ROOT / 'contracts/zara-verify-v1/SPEC.md').read_text()
    assert 'origin/master' not in spec
    assert 'coverage work is integrated' in spec
    assert 'scripts/test-coverage.sh' in spec
