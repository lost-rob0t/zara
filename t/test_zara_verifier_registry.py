"""Required in the real repository; no stub registry and no importorskip."""
from pathlib import Path
from zara.experts import ExpertRegistry, ExpertLimits, ExpertInvalidInputError
import pytest
from verification.zara_verifier_expert import register_verifier_expert

ROOT=Path(__file__).resolve().parents[1]


class Observations:
    def plan(self): return {'required':['repository']}
    def snapshot(self): return {'worktree':'observed'}
    def report(self): return None


def test_canonical_registry_dispatches_verifier_and_rejects_caller_evidence():
    registry=ExpertRegistry(engines=('swipl',))
    register_verifier_expert(registry,Observations(),ROOT)
    handle,_=registry.activate('test-principal','test-workspace','zara:verifier')
    result=registry.invoke(handle,'verify.plan',{},limits=ExpertLimits(max_model_calls=0))
    assert result.verdict.value=='succeeded'
    assert result.data['verified'] is False
    assert result.usage['model_calls']==0
    result=registry.invoke(handle,'verify.assert',{},limits=ExpertLimits(max_model_calls=0))
    assert result.verdict.value=='blocked'
    with pytest.raises(ExpertInvalidInputError):
        registry.invoke(handle,'verify.assert',{'verified':True})
