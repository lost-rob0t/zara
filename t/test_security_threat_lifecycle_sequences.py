from __future__ import annotations

from pathlib import Path

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive
from zara.security_state import PersistentSecurityState, SecurityStateError


_CAPABILITIES = {Capability.SESSION_BASIC, Capability.TURN_SUBMIT}


def _key() -> str:
    public, _secret = zmq.curve_keypair()
    return public.decode("ascii")


def test_revoke_before_enrollment_is_closed_and_does_not_create_identity(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()

    with pytest.raises(SecurityStateError, match="not actively enrolled"):
        state.revoke_device("never-enrolled")

    assert state.list_clients() == ()


def test_revoked_device_can_reenroll_new_key_without_resurrecting_old_key(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    old_key = _key()
    new_key = _key()

    state.enroll_client(
        old_key,
        device_id="phone",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    state.revoke_device("phone")
    state.enroll_client(
        new_key,
        device_id="phone",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )

    restarted = PersistentSecurityState(state.directory)
    registry = restarted.load_registry()
    assert registry.resolve_public_key(new_key).device_id == "phone"
    with pytest.raises(KeyNotActive):
        registry.resolve_public_key(old_key)

    records = restarted.list_clients()
    assert [record["active"] for record in records] == [False, True]


def test_duplicate_active_device_with_different_key_cannot_replace_authority(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    first_key = _key()
    attacker_key = _key()

    state.enroll_client(
        first_key,
        device_id="phone",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    with pytest.raises(Exception):
        state.enroll_client(
            attacker_key,
            device_id="phone",
            principal=PrincipalContext.local_owner(),
            capabilities=_CAPABILITIES,
        )

    registry = state.load_registry()
    assert registry.resolve_public_key(first_key).device_id == "phone"
    with pytest.raises(KeyNotActive):
        registry.resolve_public_key(attacker_key)
