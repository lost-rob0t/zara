"""Owner-private persistent security state for authenticated Zara listeners."""

from __future__ import annotations

import json
import os
import stat
import tempfile
from pathlib import Path
from typing import Iterable

import zmq

from zara.principals import PrincipalContext
from zara.security import (
    Capability,
    EnrolledKey,
    KeyAlreadyEnrolled,
    KeyNotActive,
    SecurityRegistry,
    validate_secret_key_file,
)
from zara.security_transport import CurveServerConfig

_STATE_VERSION = 1
_MAX_STATE_BYTES = 1024 * 1024
_MAX_CLIENTS = 256
_SERVER_FILE = "server-curve.json"
_CLIENTS_FILE = "clients.json"
_CONTROL_SOCKET_FILE = "security-admin.sock"


class SecurityStateError(RuntimeError):
    pass


class PersistentSecurityState:
    """Durable owner-local CURVE identity and enrolled client registry."""

    def __init__(self, directory: Path | str) -> None:
        self._directory = Path(directory).expanduser()

    @property
    def directory(self) -> Path:
        return self._directory

    @property
    def control_socket_path(self) -> Path:
        return self._directory / _CONTROL_SOCKET_FILE

    @property
    def _server_path(self) -> Path:
        return self._directory / _SERVER_FILE

    @property
    def _clients_path(self) -> Path:
        return self._directory / _CLIENTS_FILE

    def prepare_directory(self) -> None:
        self._prepare_directory()

    def initialize(self) -> CurveServerConfig:
        self._prepare_directory()
        if self._server_path.exists():
            config = self.load_server_config()
        else:
            public_key, secret_key = zmq.curve_keypair()
            payload = {
                "version": _STATE_VERSION,
                "public_key": public_key.decode("ascii"),
                "secret_key": secret_key.decode("ascii"),
            }
            self._write_private_json(self._server_path, payload)
            config = CurveServerConfig(public_key=public_key, secret_key=secret_key)
        if not self._clients_path.exists():
            self._write_private_json(
                self._clients_path,
                {"version": _STATE_VERSION, "clients": []},
            )
        else:
            self._load_client_records()
        return config

    def load_server_config(self) -> CurveServerConfig:
        payload = self._read_private_json(self._server_path)
        if set(payload) != {"version", "public_key", "secret_key"}:
            raise SecurityStateError("server CURVE state has invalid fields")
        if payload.get("version") != _STATE_VERSION:
            raise SecurityStateError("unsupported server CURVE state version")
        public_key = payload.get("public_key")
        secret_key = payload.get("secret_key")
        if not isinstance(public_key, str) or not isinstance(secret_key, str):
            raise SecurityStateError("server CURVE keys must be strings")
        try:
            return CurveServerConfig(
                public_key=public_key.encode("ascii"),
                secret_key=secret_key.encode("ascii"),
            )
        except (TypeError, ValueError, UnicodeEncodeError) as error:
            raise SecurityStateError("server CURVE key state is invalid") from error

    def server_public_key(self) -> str:
        public_key = self.load_server_config().public_key
        if isinstance(public_key, bytes):
            return public_key.decode("ascii")
        return public_key

    def load_registry(self) -> SecurityRegistry:
        return self._registry_from_records(self._load_client_records())

    def enroll_client(
        self,
        public_key: str | bytes,
        *,
        device_id: str,
        principal: PrincipalContext,
        capabilities: Iterable[Capability],
        live_registry: SecurityRegistry | None = None,
    ) -> EnrolledKey:
        if live_registry is not None and not isinstance(live_registry, SecurityRegistry):
            raise TypeError("live_registry must be SecurityRegistry")
        self._prepare_directory()
        if not self._server_path.exists():
            raise SecurityStateError("server security state is not initialized")
        records = self._load_client_records()
        if len(records) >= _MAX_CLIENTS:
            raise SecurityStateError("enrolled client limit reached")
        candidate = self._registry_from_records(records)
        enrolled = candidate.enroll(
            public_key,
            principal=principal,
            device_id=device_id,
            capabilities=capabilities,
        )
        updated = [*records, self._record_from_enrolled(enrolled)]
        self._save_client_records(updated)
        if live_registry is None:
            return enrolled
        try:
            return live_registry.enroll(
                enrolled.public_key,
                principal=enrolled.principal,
                device_id=enrolled.device_id,
                capabilities=enrolled.capabilities,
            )
        except (KeyAlreadyEnrolled, TypeError, ValueError) as error:
            try:
                self._save_client_records(records)
            except BaseException as rollback_error:
                raise SecurityStateError(
                    "live registry diverged and enrollment persistence rollback failed"
                ) from rollback_error
            raise SecurityStateError("live security registry diverged during enrollment") from error

    def revoke_device(
        self,
        device_id: str,
        *,
        live_registry: SecurityRegistry | None = None,
    ) -> EnrolledKey:
        if live_registry is not None and not isinstance(live_registry, SecurityRegistry):
            raise TypeError("live_registry must be SecurityRegistry")
        records = self._load_client_records()
        candidate = self._registry_from_records(records)
        try:
            revoked = candidate.revoke(device_id)
        except (KeyNotActive, TypeError, ValueError) as error:
            raise SecurityStateError("device is not actively enrolled") from error

        updated = [dict(record) for record in records]
        found = False
        for record in updated:
            if record["device_id"] == device_id and record["active"]:
                record["active"] = False
                found = True
        if not found:
            raise SecurityStateError("device is not actively enrolled")
        self._save_client_records(updated)
        if live_registry is None:
            return revoked
        try:
            return live_registry.revoke(device_id)
        except (KeyNotActive, TypeError, ValueError) as error:
            try:
                self._save_client_records(records)
            except BaseException as rollback_error:
                raise SecurityStateError(
                    "live registry diverged and revocation persistence rollback failed"
                ) from rollback_error
            raise SecurityStateError("live security registry diverged during revocation") from error

    def list_clients(self) -> tuple[dict[str, object], ...]:
        records = self._load_client_records()
        return tuple(
            {
                "public_key": record["public_key"],
                "principal_id": record["principal_id"],
                "principal_kind": record["principal_kind"],
                "device_id": record["device_id"],
                "capabilities": tuple(record["capabilities"]),
                "active": record["active"],
            }
            for record in records
        )

    @staticmethod
    def _record_from_enrolled(enrolled: EnrolledKey) -> dict[str, object]:
        return {
            "public_key": enrolled.public_key,
            "principal_id": enrolled.principal.principal_id,
            "principal_kind": enrolled.principal.kind,
            "device_id": enrolled.device_id,
            "capabilities": sorted(capability.value for capability in enrolled.capabilities),
            "active": enrolled.active,
        }

    def _registry_from_records(self, records: list[dict[str, object]]) -> SecurityRegistry:
        registry = SecurityRegistry()
        for record in records:
            if not record["active"]:
                continue
            try:
                registry.enroll(
                    record["public_key"],
                    principal=PrincipalContext(
                        record["principal_id"],
                        kind=record["principal_kind"],
                    ),
                    device_id=record["device_id"],
                    capabilities={Capability(value) for value in record["capabilities"]},
                )
            except (KeyAlreadyEnrolled, TypeError, ValueError) as error:
                raise SecurityStateError("persisted client registry is inconsistent") from error
        return registry

    def _prepare_directory(self) -> None:
        self._directory.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = os.lstat(self._directory)
        if not stat.S_ISDIR(info.st_mode):
            raise SecurityStateError("security state path is not a directory")
        if info.st_uid != os.getuid():
            raise SecurityStateError("security state directory is not owner-owned")
        if stat.S_IMODE(info.st_mode) != 0o700:
            os.chmod(self._directory, 0o700)

    def _read_private_json(self, path: Path) -> dict[str, object]:
        try:
            validate_secret_key_file(path)
        except (FileNotFoundError, PermissionError, OSError) as error:
            raise SecurityStateError(f"unsafe security state file: {path.name}") from error
        size = path.stat().st_size
        if size <= 0 or size > _MAX_STATE_BYTES:
            raise SecurityStateError(f"security state file has invalid size: {path.name}")
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeError, json.JSONDecodeError) as error:
            raise SecurityStateError(f"invalid security state JSON: {path.name}") from error
        if not isinstance(payload, dict):
            raise SecurityStateError(f"security state root must be an object: {path.name}")
        return payload

    def _write_private_json(self, path: Path, payload: dict[str, object]) -> None:
        self._prepare_directory()
        encoded = (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode(
            "utf-8"
        )
        if len(encoded) > _MAX_STATE_BYTES:
            raise SecurityStateError("security state exceeds byte limit")
        fd, temporary = tempfile.mkstemp(
            prefix=f".{path.name}.",
            dir=self._directory,
        )
        temporary_path = Path(temporary)
        try:
            os.fchmod(fd, 0o600)
            with os.fdopen(fd, "wb", closefd=True) as stream:
                stream.write(encoded)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(temporary_path, path)
            os.chmod(path, 0o600)
        finally:
            try:
                os.close(fd)
            except OSError:
                pass
            if temporary_path.exists():
                temporary_path.unlink()

    def _load_client_records(self) -> list[dict[str, object]]:
        if not self._clients_path.exists():
            return []
        payload = self._read_private_json(self._clients_path)
        if set(payload) != {"version", "clients"}:
            raise SecurityStateError("client registry has invalid fields")
        if payload.get("version") != _STATE_VERSION:
            raise SecurityStateError("unsupported client registry version")
        raw_clients = payload.get("clients")
        if not isinstance(raw_clients, list) or len(raw_clients) > _MAX_CLIENTS:
            raise SecurityStateError("client registry has invalid client list")
        records: list[dict[str, object]] = []
        active_public_keys: set[str] = set()
        active_devices: set[str] = set()
        for raw in raw_clients:
            record = self._validate_client_record(raw)
            if record["active"]:
                public_key = record["public_key"]
                device_id = record["device_id"]
                if public_key in active_public_keys or device_id in active_devices:
                    raise SecurityStateError("client registry contains duplicate active identity")
                active_public_keys.add(public_key)
                active_devices.add(device_id)
            records.append(record)
        return records

    def _validate_client_record(self, raw: object) -> dict[str, object]:
        if not isinstance(raw, dict):
            raise SecurityStateError("client registry entry must be an object")
        required = {
            "public_key",
            "principal_id",
            "principal_kind",
            "device_id",
            "capabilities",
            "active",
        }
        if set(raw) != required:
            raise SecurityStateError("client registry entry has invalid fields")
        public_key = raw.get("public_key")
        principal_id = raw.get("principal_id")
        principal_kind = raw.get("principal_kind")
        device_id = raw.get("device_id")
        capabilities = raw.get("capabilities")
        active = raw.get("active")
        if not isinstance(public_key, str):
            raise SecurityStateError("client public key must be a string")
        try:
            SecurityRegistry._normalize_public_key(public_key)
        except ValueError as error:
            raise SecurityStateError("client public key is invalid") from error
        try:
            principal = PrincipalContext(str(principal_id), kind=str(principal_kind))
            SecurityRegistry._normalize_device_id(str(device_id))
        except (TypeError, ValueError) as error:
            raise SecurityStateError("client principal/device metadata is invalid") from error
        if principal.principal_id != principal_id or principal.kind != principal_kind:
            raise SecurityStateError("client principal metadata must be strings")
        if not isinstance(device_id, str):
            raise SecurityStateError("client device id must be a string")
        if not isinstance(capabilities, list) or len(capabilities) > len(Capability):
            raise SecurityStateError("client capabilities are invalid")
        normalized_capabilities: list[str] = []
        seen: set[str] = set()
        for value in capabilities:
            if not isinstance(value, str) or value in seen:
                raise SecurityStateError("client capabilities are invalid")
            try:
                capability = Capability(value)
            except ValueError as error:
                raise SecurityStateError("client capability is unknown") from error
            seen.add(value)
            normalized_capabilities.append(capability.value)
        if type(active) is not bool:
            raise SecurityStateError("client active flag must be boolean")
        return {
            "public_key": public_key,
            "principal_id": principal_id,
            "principal_kind": principal_kind,
            "device_id": device_id,
            "capabilities": normalized_capabilities,
            "active": active,
        }

    def _save_client_records(self, records: list[dict[str, object]]) -> None:
        if len(records) > _MAX_CLIENTS:
            raise SecurityStateError("client registry exceeds client limit")
        self._write_private_json(
            self._clients_path,
            {"version": _STATE_VERSION, "clients": records},
        )


__all__ = ["PersistentSecurityState", "SecurityStateError"]
