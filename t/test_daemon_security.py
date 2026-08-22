import os
from pathlib import Path

import pytest
import zmq

from zara.security import (
    AuthorizationDenied,
    Capability,
    KeyAlreadyEnrolled,
    KeyNotActive,
    QuotaExceeded,
    SecurityAuditLog,
    SecurityAuditRecord,
    SecurityLimits,
    SecurityRegistry,
    authorize,
    validate_listener_security,
    validate_secret_key_file,
)
from zara.server import PrincipalContext


def keypair():
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def test_registry_enroll_resolves_key_and_zap_identity():
    public, _ = keypair()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry = SecurityRegistry()

    enrolled = registry.enroll(
        public,
        principal=principal,
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )

    assert registry.resolve_public_key(public) == enrolled
    assert registry.resolve_user_id(enrolled.user_id) == enrolled
    assert enrolled.principal == principal
    assert enrolled.device_id == "alice-phone"


def test_registry_rejects_duplicate_active_key_ownership():
    public, _ = keypair()
    registry = SecurityRegistry()
    registry.enroll(public, principal=PrincipalContext("user:a"), device_id="a")

    with pytest.raises(KeyAlreadyEnrolled):
        registry.enroll(public, principal=PrincipalContext("user:b"), device_id="b")


def test_registry_revocation_is_immediate_for_both_lookup_paths():
    public, _ = keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(public, principal=PrincipalContext("user:a"), device_id="a")

    registry.revoke(enrolled.device_id)

    with pytest.raises(KeyNotActive):
        registry.resolve_public_key(public)
    with pytest.raises(KeyNotActive):
        registry.resolve_user_id(enrolled.user_id)


def test_registry_rotation_revokes_old_key_and_preserves_principal():
    old_public, _ = keypair()
    new_public, _ = keypair()
    principal = PrincipalContext("user:a")
    registry = SecurityRegistry()
    old = registry.enroll(old_public, principal=principal, device_id="a-phone")

    new = registry.rotate("a-phone", new_public)

    assert new.principal == principal
    assert new.device_id == old.device_id
    assert new.user_id != old.user_id
    assert registry.resolve_public_key(new_public) == new
    with pytest.raises(KeyNotActive):
        registry.resolve_public_key(old_public)


def test_authorization_is_capability_based_and_fail_deny():
    public, _ = keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public,
        principal=PrincipalContext("user:a"),
        device_id="a",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )

    authorize(enrolled, Capability.TURN_SUBMIT)
    with pytest.raises(AuthorizationDenied):
        authorize(enrolled, Capability.DAEMON_ADMIN)


def test_quota_is_per_principal_and_release_restores_capacity():
    limits = SecurityLimits(max_connections=1, max_concurrent_requests=1, requests_per_window=2)
    quotas = limits.new_quota_manager()

    quotas.acquire_connection("user:a")
    with pytest.raises(QuotaExceeded):
        quotas.acquire_connection("user:a")
    quotas.acquire_connection("user:b")

    quotas.acquire_request("user:a", now=10.0)
    with pytest.raises(QuotaExceeded):
        quotas.acquire_request("user:a", now=10.1)
    quotas.release_request("user:a")
    quotas.acquire_request("user:a", now=10.2)

    quotas.release_connection("user:a")
    quotas.acquire_connection("user:a")


def test_request_rate_window_is_bounded_per_principal():
    limits = SecurityLimits(
        max_connections=2,
        max_concurrent_requests=8,
        requests_per_window=2,
        request_window_seconds=1.0,
    )
    quotas = limits.new_quota_manager()

    quotas.acquire_request("user:a", now=1.0)
    quotas.release_request("user:a")
    quotas.acquire_request("user:a", now=1.1)
    quotas.release_request("user:a")
    with pytest.raises(QuotaExceeded):
        quotas.acquire_request("user:a", now=1.2)

    quotas.acquire_request("user:b", now=1.2)
    quotas.release_request("user:b")
    quotas.acquire_request("user:a", now=2.01)


def test_audit_is_bounded_and_has_no_arbitrary_payload_surface():
    audit = SecurityAuditLog(capacity=2)
    first = SecurityAuditRecord(
        timestamp_ns=1,
        principal_id="user:a",
        device_id="device-a",
        session_id="session-a",
        request_id="request-a",
        turn_id=None,
        action="turn.submit",
        decision="allow",
        error_class=None,
        duration_ns=10,
    )
    audit.append(first)
    audit.append(first.__class__(**{**first.__dict__, "request_id": "request-b"}))
    audit.append(first.__class__(**{**first.__dict__, "request_id": "request-c"}))

    records = audit.snapshot()
    assert [record.request_id for record in records] == ["request-b", "request-c"]
    assert set(records[-1].as_dict()) == {
        "timestamp_ns",
        "principal_id",
        "device_id",
        "session_id",
        "request_id",
        "turn_id",
        "action",
        "decision",
        "error_class",
        "duration_ns",
    }
    assert "body" not in repr(records[-1])
    assert "secret" not in repr(records[-1]).lower()


@pytest.mark.parametrize("endpoint", ["tcp://*:5555", "tcp://0.0.0.0:5555", "tcp://[::]:5555"])
def test_remote_or_wildcard_tcp_requires_secure_auth(endpoint):
    with pytest.raises(ValueError, match="CURVE"):
        validate_listener_security(endpoint, curve_enabled=False, zap_enabled=False)

    assert validate_listener_security(endpoint, curve_enabled=True, zap_enabled=True) == endpoint


def test_loopback_tcp_is_explicit_but_still_requires_curve_for_tcp():
    with pytest.raises(ValueError, match="CURVE"):
        validate_listener_security("tcp://127.0.0.1:5555", curve_enabled=False, zap_enabled=False)


def test_ipc_can_remain_explicit_local_owner_transport():
    assert validate_listener_security("ipc:///tmp/zara-test.sock", curve_enabled=False, zap_enabled=False) == "ipc:///tmp/zara-test.sock"


def test_secret_key_file_requires_owner_only_permissions(tmp_path: Path):
    path = tmp_path / "server.key_secret"
    path.write_text("not-a-real-key\n", encoding="ascii")
    os.chmod(path, 0o600)
    assert validate_secret_key_file(path) == path

    os.chmod(path, 0o640)
    with pytest.raises(PermissionError):
        validate_secret_key_file(path)
