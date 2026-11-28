from __future__ import annotations

import json
import os
from pathlib import Path

import pytest
import zmq

from zara.security import Capability, PrincipalContext
from zara.security_state import PersistentSecurityState, SecurityStateError


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _state_with_device(tmp_path: Path, device_id: str = "android-abc123") -> PersistentSecurityState:
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public, _secret = _keypair()
    state.enroll_client(
        public,
        device_id=device_id,
        principal=PrincipalContext.local_owner(),
        capabilities={Capability.SESSION_BASIC},
    )
    return state


def test_enrolled_devices_carry_an_empty_nickname_until_renamed(tmp_path: Path) -> None:
    state = _state_with_device(tmp_path)
    clients = state.list_clients()
    assert len(clients) == 1
    assert clients[0]["nickname"] == ""

    state.rename_device("android-abc123", "Pixel 9")
    reloaded = PersistentSecurityState(tmp_path / "security").list_clients()
    assert reloaded[0]["nickname"] == "Pixel 9"
    assert reloaded[0]["device_id"] == "android-abc123"
    assert reloaded[0]["active"] is True


def test_rename_device_rejects_unknown_or_revoked_devices(tmp_path: Path) -> None:
    state = _state_with_device(tmp_path)
    state.revoke_device("android-abc123")
    with pytest.raises(SecurityStateError, match="not actively enrolled"):
        state.rename_device("android-abc123", "Ghost")
    with pytest.raises(SecurityStateError, match="not actively enrolled"):
        state.rename_device("desktop-does-not-exist", "Ghost")


@pytest.mark.parametrize(
    "nickname",
    ["", "   ", "x" * 49, "bad\nname", "bad\tname", "bad\x00name", "bad\x7fname", 42, None],
)
def test_rename_device_rejects_unsafe_nicknames(tmp_path: Path, nickname) -> None:
    state = _state_with_device(tmp_path)
    with pytest.raises(SecurityStateError):
        state.rename_device("android-abc123", nickname)
    assert state.list_clients()[0]["nickname"] == ""


def test_rename_device_stores_trimmed_unicode_nicknames(tmp_path: Path) -> None:
    state = _state_with_device(tmp_path)
    state.rename_device("android-abc123", "  Küche tablet ✓  ")
    assert state.list_clients()[0]["nickname"] == "Küche tablet ✓"


def test_legacy_records_without_nickname_field_still_load(tmp_path: Path) -> None:
    state = _state_with_device(tmp_path)
    clients_path = state.directory / "clients.json"
    payload = json.loads(clients_path.read_text(encoding="utf-8"))
    for record in payload["clients"]:
        record.pop("nickname", None)
    clients_path.write_text(json.dumps(payload) + "\n", encoding="utf-8")
    os.chmod(clients_path, 0o600)

    reloaded = PersistentSecurityState(tmp_path / "security").list_clients()
    assert reloaded[0]["nickname"] == ""

    renamed = PersistentSecurityState(tmp_path / "security")
    renamed.rename_device("android-abc123", "Legacy")
    assert renamed.list_clients()[0]["nickname"] == "Legacy"


@pytest.mark.parametrize(
    "stored",
    [42, None, [], " spaced ", "too " + "x" * 50, "bad\nname"],
)
def test_tampered_nickname_metadata_fails_closed(tmp_path: Path, stored) -> None:
    state = _state_with_device(tmp_path)
    clients_path = state.directory / "clients.json"
    payload = json.loads(clients_path.read_text(encoding="utf-8"))
    payload["clients"][0]["nickname"] = stored
    clients_path.write_text(json.dumps(payload) + "\n", encoding="utf-8")
    os.chmod(clients_path, 0o600)

    with pytest.raises(SecurityStateError):
        PersistentSecurityState(tmp_path / "security").list_clients()


def test_unknown_fields_besides_nickname_stay_rejected(tmp_path: Path) -> None:
    state = _state_with_device(tmp_path)
    clients_path = state.directory / "clients.json"
    payload = json.loads(clients_path.read_text(encoding="utf-8"))
    payload["clients"][0]["vendor"] = "acme"
    clients_path.write_text(json.dumps(payload) + "\n", encoding="utf-8")
    os.chmod(clients_path, 0o600)

    with pytest.raises(SecurityStateError):
        PersistentSecurityState(tmp_path / "security").load_registry()
