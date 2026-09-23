"""ZARA-EXPERT/1 adapter for deterministic notification anti-spam policy.

This module intentionally does not construct an ExpertRegistry or mint activation
handles.  The canonical registry owner may register the returned descriptor and
handler, then consumers invoke it through ``CanonicalExpertInvocationPort``.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from .experts import ZARA_EXPERT_PROTOCOL, ExpertDescriptor
from .notification_routing import NotificationPolicy

NOTIFICATION_SPAM_EXPERT_ID = "zara:expert/notification-spam"


def notification_spam_descriptor() -> ExpertDescriptor:
    """Return the built-in zero-model anti-spam expert descriptor."""

    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": NOTIFICATION_SPAM_EXPERT_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:notification-spam-v1",
            "name": "Notification Anti-Spam",
            "description": (
                "Deterministic notification dedupe/rate/feedback classification "
                "with structured policy evidence."
            ),
            "source_reference": "zara/notification_expert.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "classify",
                    "input_schema": {
                        "fields": [
                            {"name": "app", "type": "string", "required": True},
                            {"name": "count", "type": "integer", "required": True},
                            {"name": "duplicate", "type": "boolean", "required": True},
                            {"name": "feedback", "type": "string", "required": True},
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {"name": "decision", "type": "string", "required": True},
                            {"name": "reason", "type": "string", "required": True},
                        ]
                    },
                }
            ],
            "applicability": {
                "keywords": ["notification", "spam", "dedupe", "digest", "burst"]
            },
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": ["swipl"],
            "supported_platforms": ["linux", "android", "wear"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {
                "timeout_ms": 1000,
                "max_results": 1,
                "max_output_bytes": 4096,
                "max_model_calls": 0,
            },
            "availability": "ready",
        }
    )


@dataclass(frozen=True)
class NotificationSpamExpertHandler:
    """Thin handler backed by the same Prolog policy used by the router."""

    policy: NotificationPolicy

    def __call__(
        self,
        *,
        app: str,
        count: int,
        duplicate: bool,
        feedback: str,
    ) -> dict[str, Any]:
        decision, reason = self.policy.spam_decision(app, count, duplicate, feedback)
        return {
            "verdict": "succeeded",
            "data": {"decision": decision, "reason": reason},
            "evidence_refs": [
                f"policy:notification-spam:{reason}",
                f"policy:notification-app:{app}",
            ],
            "usage": {"model_calls": 0},
            "effect_receipts": [],
        }


def notification_spam_expert(policy: NotificationPolicy) -> tuple[ExpertDescriptor, NotificationSpamExpertHandler]:
    """Return registration material for the existing canonical expert registry."""

    return notification_spam_descriptor(), NotificationSpamExpertHandler(policy)


__all__ = [
    "NOTIFICATION_SPAM_EXPERT_ID",
    "NotificationSpamExpertHandler",
    "notification_spam_descriptor",
    "notification_spam_expert",
]
