from __future__ import annotations

import os
from pathlib import Path

import pytest
import zmq

from zara.security import (
    AuthorizationPolicy,
    BoundedAuditSink,
    Capability,
    KeyRecord,
    KeyRegistry,
    PrincipalQuotaPolicy,
    QuotaExceeded,
    QuotaTracker,
    SecurityAuditRecord,
    SecurityConfigurationError,
    validate_curve_public_key,
    validate_secret_file,
)
from zara.server import PrincipalContext


def _keys():
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _record(*, principal="alice", capabilities=(Capability.CONVERSATION,)):
    public, _secret = _keys()
    return KeyRecord(
        public_key=public,
        principal=PrincipalContext(principal, kind="authenticated"),
        device_id=f"device-{principal}",
        capabilities=frozenset(capabilities),
    )


def test_registry_enrolls_and_looks_up_canonical_public_key():
    registry = KeyRegistry()
    record = _record()

    registry.enroll(record)

    assert registry.lookup(record.public_key) == record
    assert registry.require_enabled(record.public_key) == record


def test_registry_rejects_conflicting_duplicate_key():
    registry = KeyRegistry()
    record = _record(principal="alice")
    registry.enroll(record)
    conflicting = KeyRecord(
        public_key=record.public_key,
        principal=PrincipalContext("bob", kind="authenticated"),
        device_id="other-device",
        capabilities=frozenset({Capability.STATUS}),
    )

    with pytest.raises(SecurityConfigurationError):
        registry.enroll(conflicting)


def test_registry_identical_enrollment_is_idempotent():
    registry = KeyRegistry()
    record = _record()
    registry.enroll(record)

    assert registry.enroll(record) == record
    assert registry.lookup(record.public_key) == record


def test_revocation_is_immediate_for_registry_lookups():
    registry = KeyRegistry()
    record = _record()
    registry.enroll(record)

    registry.revoke(record.public_key)

    assert registry.lookup(record.public_key).enabled is False
    assert registry.require_enabled(record.public_key) is None


def test_reenable_restores_same_principal_and_capabilities():
    registry = KeyRegistry()
    record = _record(capabilities=(Capability.CONVERSATION, Capability.STATUS))
    registry.enroll(record)
    registry.revoke(record.public_key)

    restored = registry.enable(record.public_key)

    assert restored.enabled is True
    assert restored.principal == record.principal
    assert restored.capabilities == record.capabilities


def test_unknown_key_cannot_be_enabled_or_revoked():
    registry = KeyRegistry()
    public, _ = _keys()

    with pytest.raises(KeyError):
        registry.revoke(public)
    with pytest.raises(KeyError):
        registry.enable(public)


def test_key_record_rejects_invalid_public_key_and_empty_device():
    with pytest.raises(SecurityConfigurationError):
        KeyRecord(
            public_key="not-a-curve-key",
            principal=PrincipalContext("alice", kind="authenticated"),
            device_id="device",
            capabilities=frozenset(),
        )

    public, _ = _keys()
    with pytest.raises(SecurityConfigurationError):
        KeyRecord(
            public_key=public,
            principal=PrincipalContext("alice", kind="authenticated"),
            device_id=" ",
            capabilities=frozenset(),
        )


def test_validate_curve_public_key_accepts_z85_and_canonicalizes_bytes():
    public, _ = _keys()

    assert validate_curve_public_key(public) == public
    assert validate_curve_public_key(public.encode("ascii")) == public


@pytest.mark.parametrize("value", ["", " ", "short", "x" * 40, b"bad"])
def test_validate_curve_public_key_rejects_invalid_values(value):
    with pytest.raises(SecurityConfigurationError):
        validate_curve_public_key(value)


def test_authorization_policy_is_closed_and_capability_based():
    policy = AuthorizationPolicy()
    conversation = _record(capabilities=(Capability.CONVERSATION,))
    status = _record(capabilities=(Capability.STATUS,))

    assert policy.authorize(conversation, "hello")
    assert policy.authorize(conversation, "conversation.open")
    assert policy.authorize(conversation, "turn.submit")
    assert not policy.authorize(conversation, "runtime.status")
    assert policy.authorize(status, "runtime.status")
    assert policy.authorize(status, "ping")
    assert not policy.authorize(status, "turn.submit")
    assert not policy.authorize(conversation, "totally.unknown")


def test_authorization_denies_disabled_record_even_with_capability():
    record = _record(capabilities=(Capability.CONVERSATION,))
    disabled = KeyRecord(
        public_key=record.public_key,
        principal=record.principal,
        device_id=record.device_id,
        capabilities=record.capabilities,
        enabled=False,
    )

    assert not AuthorizationPolicy().authorize(disabled, "turn.submit")


def test_audit_sink_is_bounded_and_preserves_order():
    sink = BoundedAuditSink(max_records=2)
    for index in range(3):
        sink.append(
            SecurityAuditRecord(
                timestamp_ns=index + 1,
                action="turn.submit",
                decision="allow",
                principal_id="alice",
                key_id="device-a",
                request_id=f"req-{index}",
            )
        )

    records = sink.snapshot()
    assert [record.request_id for record in records] == ["req-1", "req-2"]


def test_audit_record_has_no_content_or_secret_fields():
    fields = set(SecurityAuditRecord.__dataclass_fields__)

    forbidden = {
        "body",
        "text",
        "transcript",
        "prompt",
        "audio",
        "payload",
        "secret",
        "secret_key",
        "authorization",
        "environment",
    }
    assert not fields.intersection(forbidden)


def test_quota_enforces_active_route_limit_per_principal():
    tracker = QuotaTracker(PrincipalQuotaPolicy(max_routes=1))

    tracker.open_route("alice")
    with pytest.raises(QuotaExceeded, match="route"):
        tracker.open_route("alice")

    tracker.open_route("bob")
    tracker.close_route("alice")
    tracker.open_route("alice")


def test_quota_enforces_request_window_without_cross_principal_leakage(monkeypatch):
    now = [100.0]
    monkeypatch.setattr("zara.security.time.monotonic", lambda: now[0])
    tracker = QuotaTracker(
        PrincipalQuotaPolicy(max_requests=2, request_window_seconds=10.0)
    )

    tracker.check_request("alice")
    tracker.check_request("alice")
    with pytest.raises(QuotaExceeded, match="request"):
        tracker.check_request("alice")

    tracker.check_request("bob")
    now[0] = 111.0
    tracker.check_request("alice")


def test_quota_enforces_concurrent_runtime_commands_and_releases():
    tracker = QuotaTracker(PrincipalQuotaPolicy(max_concurrent_commands=1))

    tracker.begin_command("alice")
    with pytest.raises(QuotaExceeded, match="concurrent"):
        tracker.begin_command("alice")

    tracker.end_command("alice")
    tracker.begin_command("alice")


def test_quota_end_and_close_are_idempotent_at_zero():
    tracker = QuotaTracker(PrincipalQuotaPolicy())

    tracker.close_route("alice")
    tracker.end_command("alice")

    assert tracker.snapshot("alice").routes == 0
    assert tracker.snapshot("alice").concurrent_commands == 0


def test_secret_file_requires_owner_only_mode(tmp_path: Path):
    secret_path = tmp_path / "client.key_secret"
    secret_path.write_text("secret", encoding="utf-8")
    os.chmod(secret_path, 0o600)

    assert validate_secret_file(secret_path) == secret_path

    os.chmod(secret_path, 0o644)
    with pytest.raises(SecurityConfigurationError, match="0600"):
        validate_secret_file(secret_path)


def test_secret_file_rejects_directory_and_missing_path(tmp_path: Path):
    with pytest.raises(SecurityConfigurationError):
        validate_secret_file(tmp_path / "missing")
    with pytest.raises(SecurityConfigurationError):
        validate_secret_file(tmp_path)
