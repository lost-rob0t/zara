"""Private desktop CURVE identity and paired-server profile storage."""

from __future__ import annotations

import json
import os
import tempfile
from dataclasses import dataclass
from pathlib import Path

import zmq

from zara.security import validate_secret_key_file
from zara.security_transport import CurveClientConfig

_STATE_VERSION = 1
_MAX_STATE_BYTES = 16 * 1024
_DEFAULT_FILE = "client-curve.json"


class ClientEnrollmentError(RuntimeError):
    """Desktop client enrollment state is unsafe or invalid."""


@dataclass(frozen=True)
class PairedClientProfile:
    endpoint: str
    public_key: str
    secret_key: str
    server_public_key: str

    def curve_config(self) -> CurveClientConfig:
        return CurveClientConfig(
            public_key=self.public_key,
            secret_key=self.secret_key,
            server_public_key=self.server_public_key,
        )


class ClientEnrollmentStore:
    """Owner-private desktop client identity with optional paired server trust."""

    def __init__(self, path: Path | str) -> None:
        self.path = Path(path).expanduser()

    @classmethod
    def for_config(cls, config) -> "ClientEnrollmentStore":
        return cls(Path(config.config_dir) / _DEFAULT_FILE)

    def identity_or_create(self) -> tuple[str, str]:
        payload = self._load_payload()
        if payload is not None:
            return str(payload["public_key"]), str(payload["secret_key"])

        public_key, secret_key = zmq.curve_keypair()
        public_text = public_key.decode("ascii")
        secret_text = secret_key.decode("ascii")
        self._validate_pair(public_text, secret_text)
        self._write_payload(
            {
                "version": _STATE_VERSION,
                "public_key": public_text,
                "secret_key": secret_text,
                "endpoint": None,
                "server_public_key": None,
            }
        )
        return public_text, secret_text

    def complete_pairing(self, *, endpoint: str, server_public_key: str) -> PairedClientProfile:
        public_key, secret_key = self.identity_or_create()
        profile = PairedClientProfile(
            endpoint=str(endpoint).strip(),
            public_key=public_key,
            secret_key=secret_key,
            server_public_key=str(server_public_key).strip(),
        )
        if not profile.endpoint:
            raise ClientEnrollmentError("paired endpoint must not be empty")
        try:
            profile.curve_config()
        except (TypeError, ValueError) as error:
            raise ClientEnrollmentError("paired CURVE credentials are invalid") from error
        self._write_payload(
            {
                "version": _STATE_VERSION,
                "public_key": public_key,
                "secret_key": secret_key,
                "endpoint": profile.endpoint,
                "server_public_key": profile.server_public_key,
            }
        )
        return profile

    def ready_profile(self) -> PairedClientProfile | None:
        payload = self._load_payload()
        if payload is None:
            return None
        endpoint = payload.get("endpoint")
        server_public_key = payload.get("server_public_key")
        if endpoint is None and server_public_key is None:
            return None
        if not isinstance(endpoint, str) or not endpoint.strip():
            raise ClientEnrollmentError("paired endpoint is invalid")
        if not isinstance(server_public_key, str) or not server_public_key.strip():
            raise ClientEnrollmentError("paired server public key is invalid")
        profile = PairedClientProfile(
            endpoint=endpoint.strip(),
            public_key=str(payload["public_key"]),
            secret_key=str(payload["secret_key"]),
            server_public_key=server_public_key.strip(),
        )
        try:
            profile.curve_config()
        except (TypeError, ValueError) as error:
            raise ClientEnrollmentError("stored CURVE credentials are invalid") from error
        return profile

    def reset(self) -> None:
        self.path.unlink(missing_ok=True)

    def _load_payload(self) -> dict[str, object] | None:
        if not self.path.exists():
            return None
        try:
            validate_secret_key_file(self.path)
            size = self.path.stat().st_size
            if size <= 0 or size > _MAX_STATE_BYTES:
                raise ClientEnrollmentError("client enrollment file has invalid size")
            payload = json.loads(self.path.read_text(encoding="utf-8"))
        except ClientEnrollmentError:
            raise
        except (OSError, UnicodeError, ValueError, PermissionError) as error:
            raise ClientEnrollmentError("client enrollment file is invalid or unsafe") from error
        if not isinstance(payload, dict):
            raise ClientEnrollmentError("client enrollment root must be an object")
        if set(payload) != {
            "version",
            "public_key",
            "secret_key",
            "endpoint",
            "server_public_key",
        }:
            raise ClientEnrollmentError("client enrollment fields are invalid")
        if payload.get("version") != _STATE_VERSION:
            raise ClientEnrollmentError("unsupported client enrollment version")
        public_key = payload.get("public_key")
        secret_key = payload.get("secret_key")
        if not isinstance(public_key, str) or not isinstance(secret_key, str):
            raise ClientEnrollmentError("client CURVE identity is invalid")
        self._validate_pair(public_key, secret_key)
        return payload

    @staticmethod
    def _validate_pair(public_key: str, secret_key: str) -> None:
        try:
            public_bytes = public_key.encode("ascii")
            secret_bytes = secret_key.encode("ascii")
            if len(public_bytes) != 40 or len(secret_bytes) != 40:
                raise ValueError("CURVE keys must be 40-character Z85")
            derived = zmq.curve_public(secret_bytes)
            if isinstance(derived, str):
                derived = derived.encode("ascii")
            if derived != public_bytes:
                raise ValueError("CURVE public/secret pair does not match")
        except (UnicodeEncodeError, TypeError, ValueError, zmq.ZMQError) as error:
            raise ClientEnrollmentError("client CURVE identity is invalid") from error

    def _write_payload(self, payload: dict[str, object]) -> None:
        encoded = (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode(
            "utf-8"
        )
        if len(encoded) > _MAX_STATE_BYTES:
            raise ClientEnrollmentError("client enrollment state is too large")
        self.path.parent.mkdir(parents=True, exist_ok=True)
        temporary_path: Path | None = None
        fd = -1
        try:
            fd, temporary = tempfile.mkstemp(
                prefix=f".{self.path.name}.",
                dir=self.path.parent,
            )
            temporary_path = Path(temporary)
            os.fchmod(fd, 0o600)
            with os.fdopen(fd, "wb", closefd=True) as stream:
                fd = -1
                stream.write(encoded)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(temporary_path, self.path)
            temporary_path = None
            os.chmod(self.path, 0o600)
        finally:
            if fd >= 0:
                os.close(fd)
            if temporary_path is not None:
                temporary_path.unlink(missing_ok=True)


__all__ = [
    "ClientEnrollmentError",
    "ClientEnrollmentStore",
    "PairedClientProfile",
]
