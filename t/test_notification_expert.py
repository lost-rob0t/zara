from __future__ import annotations

from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import ExpertLimits, ExpertRegistry, ExpertRequest, ExpertVerdict
from zara.notification_expert import NOTIFICATION_SPAM_EXPERT_ID, notification_spam_expert
from zara.notification_routing import TypedHookAction


class Policy:
    def source_decision(self, app: str) -> str:
        return "allow"

    def content_mode(self, app: str) -> str:
        return "metadata_only"

    def route_policy(self, app: str) -> str:
        return "most_recently_active"

    def filter_decision(self, app: str, category: str, importance: str) -> tuple[None, None]:
        return None, None

    def spam_decision(self, app: str, count: int, duplicate: bool, feedback: str) -> tuple[str, str]:
        if feedback == "always_allow":
            return "allow", "explicit_always_allow"
        if duplicate:
            return "group", "duplicate"
        if count >= 12:
            return "digest", "burst_digest"
        return "allow", "default_policy"

    def hooks(self, app: str, category: str, importance: str) -> tuple[TypedHookAction, ...]:
        return ()


def _invoke(port: CanonicalExpertInvocationPort, handle, *, count: int, duplicate: bool, feedback: str):
    return port.invoke(
        ExpertRequest(
            request_id=f"req:spam:{count}:{int(duplicate)}:{feedback}",
            operation="expert.invoke",
            activation_id=handle.activation_id,
            expert_id=handle.expert_id,
            expert_operation="classify",
            expected_registry_generation=handle.registry_generation,
            expected_runtime_generation=handle.runtime_generation,
            input={
                "app": "com.example.chat",
                "count": count,
                "duplicate": duplicate,
                "feedback": feedback,
            },
            limits=ExpertLimits(
                timeout_ms=1000,
                max_results=1,
                max_output_bytes=4096,
                max_model_calls=0,
            ),
            idempotency_key=f"idem:spam:{count}:{int(duplicate)}:{feedback}",
        )
    )


def test_notification_spam_expert_uses_existing_registry_with_zero_model_budget() -> None:
    registry = ExpertRegistry()
    descriptor, handler = notification_spam_expert(Policy())
    registry.reload([(descriptor, handler)])
    handle, _ = registry.activate("user:alice", "ws:main", NOTIFICATION_SPAM_EXPERT_ID)
    port = CanonicalExpertInvocationPort(registry)

    result = _invoke(port, handle, count=12, duplicate=False, feedback="none")

    assert result.verdict is ExpertVerdict.SUCCEEDED
    assert result.data == {"decision": "digest", "reason": "burst_digest"}
    assert result.usage == {"model_calls": 0}
    assert result.effect_receipts == ()
    assert "policy:notification-spam:burst_digest" in result.evidence_refs


def test_explicit_allow_feedback_is_structured_expert_evidence_not_a_score() -> None:
    registry = ExpertRegistry()
    descriptor, handler = notification_spam_expert(Policy())
    registry.reload([(descriptor, handler)])
    handle, _ = registry.activate("user:alice", "ws:main", NOTIFICATION_SPAM_EXPERT_ID)
    port = CanonicalExpertInvocationPort(registry)

    result = _invoke(port, handle, count=99, duplicate=True, feedback="always_allow")

    assert result.data == {"decision": "allow", "reason": "explicit_always_allow"}
    assert result.usage["model_calls"] == 0
    assert "score" not in result.data
