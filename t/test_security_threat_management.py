from __future__ import annotations

import json
import os
from pathlib import Path

import pytest
import zmq

import zara.server as server_module
from zara.security_state import PersistentSecurityState, SecurityStateError


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def test_security_enroll_parser_accepts_dash_prefixed_z85_key():
    public = "-.6(Vfv?M9JT!Y^l)iow0lAtnD(*w[0(d)(@EuI1"
    args = server_module._parser().parse_args(
        [
            "--security-dir",
            "/tmp/zara-security",
            "--security-enroll-key",
            public,
            "--security-device-id",
            "android",
        ]
    )
    assert args.security_enroll_key == public
    assert args.security_device_id == "android"


def test_security_init_prints_public_identity_but_never_server_secret(tmp_path: Path, capsys):
    runtime_dir = tmp_path / "runtime"
    security_dir = tmp_path / "security"
    args = server_module._parser().parse_args(
        [
            "--runtime-dir",
            str(runtime_dir),
            "--security-dir",
            str(security_dir),
            "--security-init",
        ]
    )
    assert server_module._run_security_management(args) == 0
    output = capsys.readouterr()
    payload = json.loads((security_dir / "server-curve.json").read_text(encoding="utf-8"))
    assert payload["public_key"] in output.out
    assert payload["secret_key"] not in output.out
    assert payload["secret_key"] not in output.err


def test_security_list_exposes_no_server_secret_or_zap_internal_user_id(tmp_path: Path, capsys):
    runtime_dir = tmp_path / "runtime"
    security_dir = tmp_path / "security"
    state = PersistentSecurityState(security_dir)
    state.initialize()
    public, _secret = _keypair()
    enroll_args = server_module._parser().parse_args(
        [
            "--runtime-dir",
            str(runtime_dir),
            "--security-dir",
            str(security_dir),
            "--security-enroll-key",
            public,
            "--security-device-id",
            "phone",
        ]
    )
    assert server_module._run_security_management(enroll_args) == 0
    capsys.readouterr()

    list_args = server_module._parser().parse_args(
        [
            "--runtime-dir",
            str(runtime_dir),
            "--security-dir",
            str(security_dir),
            "--security-list-clients",
        ]
    )
    assert server_module._run_security_management(list_args) == 0
    output = capsys.readouterr().out
    listed = json.loads(output)
    server_payload = json.loads((security_dir / "server-curve.json").read_text(encoding="utf-8"))
    assert server_payload["secret_key"] not in output
    assert len(listed) == 1
    assert "user_id" not in listed[0]
    assert set(listed[0]) == {
        "active",
        "capabilities",
        "device_id",
        "principal_id",
        "principal_kind",
        "public_key",
    }


def test_persisted_mismatched_server_keypair_fails_closed(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public, _secret = _keypair()
    _other_public, other_secret = _keypair()
    server_path = state.directory / "server-curve.json"
    server_path.write_text(
        json.dumps(
            {
                "version": 1,
                "public_key": public,
                "secret_key": other_secret,
            }
        )
        + "\n",
        encoding="utf-8",
    )
    os.chmod(server_path, 0o600)
    with pytest.raises(SecurityStateError, match="invalid"):
        state.load_server_config()


def test_registry_client_count_limit_rejects_resource_bomb_before_entry_validation(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    clients_path = state.directory / "clients.json"
    clients_path.write_text(
        json.dumps({"version": 1, "clients": [None] * 257}) + "\n",
        encoding="utf-8",
    )
    os.chmod(clients_path, 0o600)
    with pytest.raises(SecurityStateError, match="invalid client list"):
        state.load_registry()


def test_orphan_atomic_temp_files_are_never_treated_as_authoritative_state(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    orphan = state.directory / ".clients.json.crashed-writer"
    orphan.write_text('{"version":1,"clients":["attacker"]}\n', encoding="utf-8")
    os.chmod(orphan, 0o600)

    assert state.list_clients() == ()
    assert state.load_registry() is not None
    assert orphan.exists()
