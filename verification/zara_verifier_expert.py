"""ZARA-EXPERT/1 adapter; the existing Core registry retains all authority.

The host injects observation access. Invocation inputs cannot supply source IDs,
command strings, evidence rows, paths, verdicts, or provider selection.
"""
from __future__ import annotations

from copy import deepcopy
from pathlib import Path
from typing import Any, Protocol

from verification.zara_verify_runner import current_report, policy_digest


class VerificationObservations(Protocol):
    def plan(self) -> dict[str, Any]: ...
    def snapshot(self) -> dict[str, Any]: ...
    def report(self) -> dict[str, Any] | None: ...


class ZaraVerifierExpert:
    def __init__(self, observations: VerificationObservations):
        self._observations = observations

    def __call__(self, *, expert_operation: str) -> dict[str, Any]:
        usage = {'model_calls': 0, 'provider_calls': 0}
        if expert_operation == 'verify.plan':
            return {'verdict': 'succeeded', 'data': {
                'verified': False, 'plan': deepcopy(self._observations.plan()),
                'merge_authorized': False}, 'usage': usage}
        if expert_operation not in {'verify.assert', 'verify.explain'}:
            return {'verdict': 'unsupported', 'data': {'verified': False}, 'usage': usage}
        before = self._observations.snapshot()
        report = self._observations.report()
        after = self._observations.snapshot()
        verified = before == after and current_report(report, after)
        data = {'verified': verified, 'merge_authorized': False,
                'verification_verdict': 'verified' if verified else 'blocked',
                'reasons': [] if verified else ['no_current_host_verification'],
                'scope': 'local'}
        if expert_operation == 'verify.explain':
            data['required'] = report.get('required', []) if isinstance(report, dict) else []
        return {'verdict': 'succeeded' if verified or expert_operation == 'verify.explain' else 'blocked',
                'data': data, 'usage': usage}


def descriptor_wire(manifest_digest: str) -> dict[str, Any]:
    operations = [
        {'operation_id': operation, 'input_schema': {'fields': []},
         'output_schema': {'fields': [{'name': 'verified', 'type': 'boolean', 'required': True}]}}
        for operation in ['verify.plan', 'verify.explain', 'verify.assert']
    ]
    return {
        'protocol': 'ZARA-EXPERT/1', 'expert_id': 'zara:verifier',
        'expert_version': '1.0.0', 'package_namespace': 'zara',
        'manifest_digest': 'sha256:' + manifest_digest,
        'name': 'Zara Verifier', 'description': 'Exact-source, fail-closed verification policy expert',
        'source_reference': 'verification/zara_verifier_expert.py', 'reasoning_kind': 'symbolic',
        'operations': operations, 'applicability': {'keywords': ['zara', 'verify', 'verification']},
        'required_capabilities': ['verification.observe'],
        'possible_effects': ['filesystem_read', 'process_spawn'],
        'supported_engines': ['swipl'], 'supported_platforms': ['linux'],
        'fallback_policy': 'fail_closed', 'delegation_policy': 'never',
        'resource_limits': {'timeout_ms': 10000, 'max_results': 64,
                            'max_output_bytes': 65536, 'max_model_calls': 0},
        'registry_generation': 0, 'availability': 'installed',
    }


def register_verifier_expert(registry: Any, observations: VerificationObservations,
                             policy_root: Path) -> dict[str, Any]:
    """Trusted host bootstrap only; never a tool-call registration operation."""
    from zara.experts import ExpertDescriptor

    descriptor = ExpertDescriptor.from_wire(descriptor_wire(policy_digest(policy_root)))
    return registry.register(descriptor, ZaraVerifierExpert(observations))


def invoke_verifier_expert(observations: VerificationObservations, policy_root: Path,
                           session_id: str, operation: str) -> dict[str, Any]:
    """Standalone CLI host. Embedding apps instead pass their existing registry.

    No new expert implementation/state machine is introduced: this uses the
    public Core class, dispatch fences, activation handles and zero-model budget.
    """
    from zara.experts import ExpertRegistry, ExpertLimits
    from verification.zara_verify_runner import canonical_digest

    registry = ExpertRegistry(engines=('swipl',))
    register_verifier_expert(registry, observations, policy_root)
    principal = 'opencode:' + canonical_digest(session_id)
    workspace = 'workspace:' + canonical_digest(str(policy_root.resolve()))
    handle, _ = registry.activate(principal, workspace, 'zara:verifier')
    try:
        result = registry.invoke(handle, operation, {}, limits=ExpertLimits(max_model_calls=0))
        return {'expert_id': result.expert_id, 'operation': result.expert_operation,
                'verdict': result.verdict.value, 'data': result.data,
                'usage': result.usage, 'invocation_id': result.invocation_id}
    finally:
        registry.deactivate(handle)
