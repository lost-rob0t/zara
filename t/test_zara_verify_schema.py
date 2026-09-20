"""The wire schema cannot turn a caller assertion into verifier provenance."""
import copy
import json
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator

ROOT = Path(__file__).resolve().parents[1]


def valid_report():
    return {'protocol': 'ZARA-VERIFY/1', 'scope': 'local', 'verdict': 'verified',
            'run_id': 'a' * 32, 'session_id': 'fixture', 'created_ms': 1, 'ttl_ms': 100,
            'merge_authorized': False, 'model_calls': 0, 'reasons': [], 'evidence': [],
            'required': ['repository'], 'source': {
                'workspace': '/repo', 'head': 'a' * 40, 'base': 'a' * 40,
                'merge_base': 'a' * 40, 'worktree': 'b' * 64, 'policy': 'c' * 64,
                'changed_paths': [], 'clean': True},
            'expert': {'expert_id': 'zara:verifier', 'operation': 'verify.assert',
                       'verdict': 'succeeded', 'data': {'verified': True},
                       'usage': {'model_calls': 0, 'provider_calls': 0}, 'invocation_id': 'inv:fixture'}}


def validator():
    schema = json.loads((ROOT / 'contracts/zara-verify-v1/report.schema.json').read_text())
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def test_wire_schema_is_valid_but_not_an_authentication_mechanism():
    assert not list(validator().iter_errors(valid_report()))


@pytest.mark.parametrize('mutation', ['expert', 'plan', 'boolean', 'merge', 'unknown', 'reason'])
def test_invalid_verification_claims_are_rejected(mutation):
    report = copy.deepcopy(valid_report())
    if mutation == 'expert': del report['expert']
    if mutation == 'plan': report['expert']['operation'] = 'verify.plan'
    if mutation == 'boolean': report['model_calls'] = False
    if mutation == 'merge': report['merge_authorized'] = True
    if mutation == 'unknown': report['self_approved'] = True
    if mutation == 'reason': report['reasons'] = ['failed']
    assert list(validator().iter_errors(report))
