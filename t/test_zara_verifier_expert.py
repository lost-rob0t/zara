from __future__ import annotations
import copy
from pathlib import Path
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from verification.zara_verifier_expert import ZaraVerifierExpert, descriptor_wire


class Observations:
    def __init__(self):
        self.source = {'worktree': 'observed'}
        self.receipt = None
        self.calls = 0
        self.mutate = False
    def snapshot(self):
        self.calls += 1
        return {**self.source, 'generation': self.calls if self.mutate else 1}
    def report(self):
        return self.receipt
    def plan(self):
        return {'required':['repository']}
    def verified_receipt(self):
        self.receipt = {'protocol':'ZARA-VERIFY/1','scope':'local','verdict':'verified',
            'source':self.snapshot(),'model_calls':0,'provider_calls':0,'merge_authorized':False,
            'created_ms':int(time.time()*1000),'ttl_ms':600000,'reasons':[]}


def test_no_host_report_is_blocked_with_zero_models():
    expert=ZaraVerifierExpert(Observations())
    result=expert(expert_operation='verify.assert')
    assert result['verdict']=='blocked'
    assert result['data']['verified'] is False
    assert result['usage']=={'model_calls':0,'provider_calls':0}


def test_plan_success_is_not_verification_success():
    result=ZaraVerifierExpert(Observations())(expert_operation='verify.plan')
    assert result['verdict']=='succeeded' and result['data']['verified'] is False


def test_host_observed_receipt_is_required():
    host=Observations(); host.verified_receipt()
    result=ZaraVerifierExpert(host)(expert_operation='verify.assert')
    assert result['verdict']=='succeeded' and result['data']['verified'] is True
    assert result['data']['merge_authorized'] is False


def test_provider_accounting_tamper_blocks_host_receipt():
    for value in (None, 1, False):
        host=Observations(); host.verified_receipt()
        if value is None:
            del host.receipt['provider_calls']
        else:
            host.receipt['provider_calls']=value
        assert ZaraVerifierExpert(host)(expert_operation='verify.assert')['verdict']=='blocked'


def test_scope_change_during_observation_fences_result():
    host=Observations(); host.verified_receipt(); host.mutate=True
    assert ZaraVerifierExpert(host)(expert_operation='verify.assert')['verdict']=='blocked'


def test_expired_and_unknown_reports_fail_closed():
    host=Observations(); host.verified_receipt(); host.receipt['created_ms']=0
    assert ZaraVerifierExpert(host)(expert_operation='verify.assert')['verdict']=='blocked'
    assert ZaraVerifierExpert(host)(expert_operation='bad.operation')['verdict']=='unsupported'


def test_descriptor_disallows_caller_evidence_and_has_zero_budget():
    descriptor=descriptor_wire('a'*64)
    assert descriptor['protocol']=='ZARA-EXPERT/1'
    assert descriptor['resource_limits']['max_model_calls']==0
    assert descriptor['fallback_policy']=='fail_closed'
    assert all(op['input_schema']['fields']==[] for op in descriptor['operations'])


def test_plan_result_does_not_alias_mutable_host_observations():
    import json
    host = Observations()
    plan = {'required': ['repository']}
    host.plan = lambda: plan
    result = ZaraVerifierExpert(host)(expert_operation='verify.plan')
    plan['expert'] = result
    # CLI attaches this result to the original plan: aliasing would form a cycle.
    assert json.loads(json.dumps(result))['data']['plan'] == {'required': ['repository']}
