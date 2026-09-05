from __future__ import annotations

import os
import socket
import stat
import struct
from pathlib import Path

import pytest
import zmq
from zmq.utils import z85

import zara.security_admin as security_admin
import zara.security_transport as security_transport
from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityRegistry
from zara.security_admin import SecurityAdminError, SecurityAdminServer
from zara.security_state import PersistentSecurityState, SecurityStateError
from zara.security_transport import (
    AuthenticationRequired,
    CurveClientConfig,
    CurveServerConfig,
    RegistryCredentialsProvider,
    authenticated_user_id,
)


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


class _MetadataFrame:
    def __init__(self, value) -> None:
        self.value = value

    def __getitem__(self, key):
        if key != "User-Id":
            raise KeyError(key)
        return self.value


class _PeerCredentialSocket:
    def __init__(self, *, uid: int) -> None:
        self.uid = uid

    def getsockopt(self, level, option, size):
        assert level == socket.SOL_SOCKET
        assert option == socket.SO_PEERCRED
        assert size == struct.calcsize("3i")
        return struct.pack("3i", os.getpid(), self.uid, os.getgid())


def test_curve_decoder_normalizes_invalid_z85_alphabet_to_value_error():
    invalid = "~" * 40
    with pytest.raises(ValueError, match="valid Z85"):
        CurveServerConfig(public_key=invalid, secret_key=invalid)


def test_curve_server_config_rejects_mismatched_public_secret_pair():
    public, _secret = _keypair()
    _other_public, other_secret = _keypair()
    with pytest.raises(ValueError, match="key pair"):
        CurveServerConfig(public_key=public, secret_key=other_secret)


def test_curve_client_config_rejects_mismatched_public_secret_pair():
    client_public, _client_secret = _keypair()
    _other_public, other_secret = _keypair()
    server_public, _server_secret = _keypair()
    with pytest.raises(ValueError, match="key pair"):
        CurveClientConfig(
            public_key=client_public,
            secret_key=other_secret,
            server_public_key=server_public,
        )


@pytest.mark.parametrize(
    "domain",
    [
        "zara\nadmin",
        "zära",
        "x" * 256,
        " zara",
        "zara ",
    ],
)
def test_zap_domain_obeys_bounded_ascii_wire_contract(domain: str):
    public, secret = _keypair()
    with pytest.raises(ValueError, match="domain"):
        CurveServerConfig(public_key=public, secret_key=secret, zap_domain=domain)


@pytest.mark.parametrize(
    "user_id",
    [
        b"\xff",
        "zära",
        "x" * 256,
        "line\nbreak",
    ],
)
def test_authenticated_user_id_obeys_bounded_ascii_wire_contract(user_id):
    with pytest.raises(AuthenticationRequired):
        authenticated_user_id([_MetadataFrame(user_id)])


def test_generated_zap_user_id_is_opaque_ascii_even_for_unicode_device_label():
    public, _secret = _keypair()
    registry = SecurityRegistry()
    enrolled = registry.enroll(
        public,
        principal=PrincipalContext.local_owner(),
        device_id="電話-🙂",
        capabilities={Capability.SESSION_BASIC},
    )
    encoded = enrolled.user_id.encode("ascii")
    assert 0 < len(encoded) <= 255
    assert "電話" not in enrolled.user_id
    assert "🙂" not in enrolled.user_id


@pytest.mark.parametrize(
    ("version", "allowed"),
    [
        ((4, 0, 8), False),
        ((4, 3, 1), False),
        ((4, 3, 2), False),
        ((4, 3, 3), True),
        ((4, 3, 5), True),
        ((5, 0, 0), True),
    ],
)
def test_public_curve_runtime_rejects_known_vulnerable_libzmq_versions(monkeypatch, version, allowed):
    monkeypatch.setattr(security_transport.zmq, "zmq_version_info", lambda: version)
    if allowed:
        security_transport.require_secure_curve_runtime()
    else:
        with pytest.raises(RuntimeError, match="libzmq"):
            security_transport.require_secure_curve_runtime()


def test_credentials_provider_invalid_40_byte_z85_alphabet_fails_closed():
    provider = RegistryCredentialsProvider(SecurityRegistry())
    invalid = b"~" * 40
    assert provider.callback("zara", invalid) is False
    with pytest.raises(KeyNotActive):
        provider.user_id(invalid)


def test_owner_admin_rejects_different_uid_peer():
    if getattr(socket, "SO_PEERCRED", None) is None:
        pytest.skip("Linux SO_PEERCRED unavailable")
    different_uid = os.getuid() + 1
    with pytest.raises(SecurityAdminError, match="not the daemon owner"):
        SecurityAdminServer._require_owner_peer(_PeerCredentialSocket(uid=different_uid))


def test_owner_admin_accepts_current_uid_peer():
    if getattr(socket, "SO_PEERCRED", None) is None:
        pytest.skip("Linux SO_PEERCRED unavailable")
    SecurityAdminServer._require_owner_peer(_PeerCredentialSocket(uid=os.getuid()))


def test_long_admin_path_fails_closed_without_proc_fd_escape_hatch(tmp_path: Path, monkeypatch):
    long_directory = tmp_path / ("a" * 70) / ("b" * 70)
    path = long_directory / "security-admin.sock"
    long_directory.mkdir(parents=True)
    assert len(os.fsencode(path)) > 100

    original_is_dir = security_admin.Path.is_dir

    def fake_is_dir(candidate: Path) -> bool:
        if os.fspath(candidate) == "/proc/self/fd":
            return False
        return original_is_dir(candidate)

    monkeypatch.setattr(security_admin.Path, "is_dir", fake_is_dir)
    with pytest.raises(SecurityAdminError, match="/proc/self/fd is unavailable"):
        security_admin._open_socket_address(path)


def test_security_state_directory_symlink_is_rejected(tmp_path: Path):
    real = tmp_path / "real"
    real.mkdir(mode=0o700)
    linked = tmp_path / "security"
    linked.symlink_to(real, target_is_directory=True)
    with pytest.raises(SecurityStateError, match="not a directory"):
        PersistentSecurityState(linked).initialize()


def test_server_identity_file_symlink_and_broad_permissions_fail_closed(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    server_path = state.directory / "server-curve.json"
    backup = state.directory / "server-backup.json"
    backup.write_bytes(server_path.read_bytes())
    os.chmod(backup, 0o600)

    server_path.unlink()
    server_path.symlink_to(backup)
    with pytest.raises(SecurityStateError, match="unsafe security state file"):
        state.load_server_config()

    server_path.unlink()
    server_path.write_bytes(backup.read_bytes())
    os.chmod(server_path, 0o644)
    with pytest.raises(SecurityStateError, match="unsafe security state file"):
        state.load_server_config()
