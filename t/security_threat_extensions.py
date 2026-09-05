"""Research-derived threats that cross more than one STRIDE boundary cell.

The primary catalog guarantees the 8x6 STRIDE matrix. These cases capture
compound/business-logic threats discovered by adversarial review and external
references, where forcing the scenario into one matrix cell would hide the
actual multi-boundary invariant.
"""

from __future__ import annotations

EXTRA_THREATS = (
    {
        "id": "X-DOS-JSON-DEPTH",
        "scenario": "Deeply nested authenticated/admin/persisted JSON must be rejected by an explicit application bound before decoder recursion becomes the safety mechanism.",
        "sources": ("CWE-674", "CWE-400"),
        "tests": (
            "t/test_json_limits.py::test_untrusted_json_nesting_accepts_exact_boundary_and_rejects_next_level",
            "t/test_security_threat_json_depth.py::test_admin_recursive_json_exhaustion_is_a_closed_admin_error",
            "t/test_security_threat_json_depth.py::test_persisted_recursive_json_exhaustion_is_a_closed_state_error",
            "t/test_security_threat_protocol_depth.py::test_recursive_protocol_body_is_rejected_and_gateway_remains_healthy",
        ),
    },
    {
        "id": "X-RECONNECT-INFLIGHT",
        "scenario": "An authenticated reconnect must remain possible while an abandoned route still owns an in-flight runtime concurrency slot, without creating a free request-flood lane.",
        "sources": ("OWASP-BUSINESS-LOGIC", "CWE-400", "ZEROMQ-ROUTER"),
        "tests": (
            "t/test_security_threat_reconnect.py::test_runtime_completion_releases_quota_even_when_original_route_vanished",
            "t/test_security_quota_control_plane.py::test_rate_only_control_request_is_not_blocked_by_runtime_concurrency",
            "t/test_security_quota_control_plane.py::test_rate_only_control_request_still_enforces_request_window",
        ),
    },
    {
        "id": "X-LIFECYCLE-ORDER",
        "scenario": "Out-of-order, repeated, and replacement enrollment operations must not create or silently transfer key authority.",
        "sources": ("OWASP-BUSINESS-LOGIC", "CWE-287", "CWE-362"),
        "tests": (
            "t/test_security_threat_lifecycle_sequences.py::test_revoke_before_enrollment_is_closed_and_does_not_create_identity",
            "t/test_security_adversarial.py::test_admin_protocol_replay_cannot_duplicate_or_resurrect_identity",
            "t/test_security_threat_lifecycle_sequences.py::test_duplicate_active_device_with_different_key_cannot_replace_authority",
        ),
    },
    {
        "id": "X-LIFECYCLE-REENROLL",
        "scenario": "Re-enrolling a revoked device with a new key must preserve the old key as revoked across restart while activating only the new key.",
        "sources": ("OWASP-BUSINESS-LOGIC", "CWE-672", "CWE-287"),
        "tests": (
            "t/test_security_threat_lifecycle_sequences.py::test_revoked_device_can_reenroll_new_key_without_resurrecting_old_key",
            "t/test_security_process_chaos.py::test_process_death_after_revocation_durable_commit_recovers_revoked_authority",
        ),
    },
)


__all__ = ["EXTRA_THREATS"]
