"""Authentication-adjacent security primitives for Zara's daemon transport.

This module deliberately contains no socket ownership. Issue #130 uses these
thread-safe primitives from both the ZAP authenticator and the ZARA/1 gateway.
"""

from __future__ import annotations

import enum
import os
import stat
import threading
import time
from collections import deque
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Optional

import zmq
from zmq.utils import z85

from zara.server import PrincipalContext


class SecurityConfigurationError(RuntimeError):
    pass


class QuotaExceeded(RuntimeError):
    pass


class Capability(str, enum.Enum):
    CONVERSATION = "conversation"
    STATUS = "status"
    ADMIN = "admin"
    TOOL = "tool"
    CONTEXT = "context"
    MEMORY = "memory"
    VOICE = "voice"


def _validate_z85_key(value: str | bytes, *, label: str) -> str:
    if isinstance(value, bytes):
        try:
            value = value.decode("ascii")
        except UnicodeDecodeError as error:
            raise SecurityConfigurationError(f"{label} must be ASCII Z85") from error
    if not isinstance(value, str):
        raise SecurityConfigurationError(f"{label} must be a Z85 string")
    if len(value) != 40:
        raise SecurityConfigurationError(f"{label} must be exactly 40 Z85 characters")
    try:
        encoded = value.encode("ascii")
        decoded = z85.decode(encoded)
    except (UnicodeEncodeError, ValueError, KeyError) as error:
        raise SecurityConfigurationError(f"{label} is not valid Z85") from error
    if len(decoded) != 32:
        raise SecurityConfigurationError(f"{label} must decode to 32 bytes")
    return value


def validate_curve_public_key(value: str | bytes) -> str:
    return _validate_z85_key(value, label="CURVE public key")


def validate_curve_secret_key(value: str | bytes) -> str:
    return _validate_z85_key(value, label="CURVE secret key")


def _validate_curve_keypair(public_key: str, secret_key: str) -> None:
    try:
        derived = zmq.curve_public(secret_key.encode("ascii")).decode("ascii")
    except (ValueError, zmq.ZMQError) as error:
        raise SecurityConfigurationError("CURVE secret key cannot derive a public key") from error
    if derived != public_key:
        raise SecurityConfigurationError("CURVE public and secret keys do not form a keypair")


@dataclass(frozen=True)
class KeyRecord:
    public_key: str
    principal: PrincipalContext
    device_id: str
    capabilities: frozenset[Capability] = field(default_factory=frozenset)
    enabled: bool = True

    def __post_init__(self) -> None:
        object.__setattr__(self, "public_key", validate_curve_public_key(self.public_key))
        if not isinstance(self.principal, PrincipalContext):
            raise SecurityConfigurationError("key record requires PrincipalContext")
        if not isinstance(self.device_id, str) or not self.device_id.strip():
            raise SecurityConfigurationError("device_id must be a non-empty string")
        if self.device_id != self.device_id.strip():
            raise SecurityConfigurationError("device_id must not contain surrounding whitespace")
        try:
            capabilities = frozenset(Capability(capability) for capability in self.capabilities)
        except (TypeError, ValueError) as error:
            raise SecurityConfigurationError("key record contains an unknown capability") from error
        object.__setattr__(self, "capabilities", capabilities)
        if type(self.enabled) is not bool:
            raise SecurityConfigurationError("enabled must be boolean")


class KeyRegistry:
    def __init__(self) -> None:
        self._records: dict[str, KeyRecord] = {}
        self._lock = threading.RLock()

    def enroll(self, record: KeyRecord) -> KeyRecord:
        if not isinstance(record, KeyRecord):
            raise TypeError("record must be KeyRecord")
        with self._lock:
            current = self._records.get(record.public_key)
            if current is not None:
                if current != record:
                    raise SecurityConfigurationError("CURVE public key is already enrolled differently")
                return current
            self._records[record.public_key] = record
            return record

    def lookup(self, public_key: str | bytes) -> Optional[KeyRecord]:
        key = validate_curve_public_key(public_key)
        with self._lock:
            return self._records.get(key)

    def require_enabled(self, public_key: str | bytes) -> Optional[KeyRecord]:
        record = self.lookup(public_key)
        if record is None or not record.enabled:
            return None
        return record

    def revoke(self, public_key: str | bytes) -> KeyRecord:
        key = validate_curve_public_key(public_key)
        with self._lock:
            current = self._records.get(key)
            if current is None:
                raise KeyError(key)
            updated = replace(current, enabled=False)
            self._records[key] = updated
            return updated

    def enable(self, public_key: str | bytes) -> KeyRecord:
        key = validate_curve_public_key(public_key)
        with self._lock:
            current = self._records.get(key)
            if current is None:
                raise KeyError(key)
            updated = replace(current, enabled=True)
            self._records[key] = updated
            return updated


@dataclass(frozen=True)
class CurveServerConfig:
    public_key: str
    secret_key: str = field(repr=False)
    registry: KeyRegistry = field(repr=False)
    zap_domain: str = "zara"

    def __post_init__(self) -> None:
        public_key = validate_curve_public_key(self.public_key)
        secret_key = validate_curve_secret_key(self.secret_key)
        _validate_curve_keypair(public_key, secret_key)
        if not isinstance(self.registry, KeyRegistry):
            raise SecurityConfigurationError("CURVE server requires KeyRegistry")
        if not isinstance(self.zap_domain, str) or not self.zap_domain.strip():
            raise SecurityConfigurationError("ZAP domain must be a non-empty string")
        try:
            self.zap_domain.encode("ascii")
        except UnicodeEncodeError as error:
            raise SecurityConfigurationError("ZAP domain must be ASCII") from error
        if self.zap_domain != self.zap_domain.strip():
            raise SecurityConfigurationError("ZAP domain must not contain surrounding whitespace")
        object.__setattr__(self, "public_key", public_key)
        object.__setattr__(self, "secret_key", secret_key)


@dataclass(frozen=True)
class CurveClientConfig:
    public_key: str
    secret_key: str = field(repr=False)
    server_public_key: str = ""

    def __post_init__(self) -> None:
        public_key = validate_curve_public_key(self.public_key)
        secret_key = validate_curve_secret_key(self.secret_key)
        server_public_key = validate_curve_public_key(self.server_public_key)
        _validate_curve_keypair(public_key, secret_key)
        object.__setattr__(self, "public_key", public_key)
        object.__setattr__(self, "secret_key", secret_key)
        object.__setattr__(self, "server_public_key", server_public_key)


class CurveCredentialsProvider:
    """PyZMQ CURVE callback backed by Zara's live key registry."""

    def __init__(self, registry: KeyRegistry) -> None:
        if not isinstance(registry, KeyRegistry):
            raise TypeError("registry must be KeyRegistry")
        self._registry = registry

    def callback(self, domain: str, key: bytes) -> bool:
        del domain
        if not isinstance(key, bytes) or len(key) != 40:
            return False
        try:
            public_key = validate_curve_public_key(key)
            return self._registry.require_enabled(public_key) is not None
        except (SecurityConfigurationError, ValueError, KeyError, UnicodeDecodeError):
            return False


def apply_curve_server(socket: zmq.Socket, config: CurveServerConfig) -> None:
    if not isinstance(config, CurveServerConfig):
        raise TypeError("config must be CurveServerConfig")
    socket.curve_secretkey = config.secret_key.encode("ascii")
    socket.curve_server = True
    socket.zap_domain = config.zap_domain.encode("ascii")


def apply_curve_client(socket: zmq.Socket, config: CurveClientConfig) -> None:
    if not isinstance(config, CurveClientConfig):
        raise TypeError("config must be CurveClientConfig")
    socket.curve_publickey = config.public_key.encode("ascii")
    socket.curve_secretkey = config.secret_key.encode("ascii")
    socket.curve_serverkey = config.server_public_key.encode("ascii")


class AuthorizationPolicy:
    _REQUIRED = {
        "conversation.open": Capability.CONVERSATION,
        "turn.submit": Capability.CONVERSATION,
        "turn.cancel": Capability.CONVERSATION,
        "ping": Capability.STATUS,
        "runtime.status": Capability.STATUS,
    }

    def authorize(self, record: KeyRecord, message_type: str) -> bool:
        if not isinstance(record, KeyRecord) or not record.enabled:
            return False
        if message_type == "hello":
            return True
        required = self._REQUIRED.get(message_type)
        return required is not None and required in record.capabilities


@dataclass(frozen=True)
class SecurityAuditRecord:
    timestamp_ns: int
    action: str
    decision: str
    principal_id: Optional[str] = None
    key_id: Optional[str] = None
    device_id: Optional[str] = None
    session_id: Optional[str] = None
    request_id: Optional[str] = None
    turn_id: Optional[str] = None
    error_class: Optional[str] = None
    duration_ns: Optional[int] = None


class BoundedAuditSink:
    def __init__(self, max_records: int = 1024) -> None:
        if type(max_records) is not int or max_records < 1:
            raise ValueError("max_records must be a positive integer")
        self._records: deque[SecurityAuditRecord] = deque(maxlen=max_records)
        self._lock = threading.RLock()

    def append(self, record: SecurityAuditRecord) -> None:
        if not isinstance(record, SecurityAuditRecord):
            raise TypeError("record must be SecurityAuditRecord")
        with self._lock:
            self._records.append(record)

    def snapshot(self) -> tuple[SecurityAuditRecord, ...]:
        with self._lock:
            return tuple(self._records)


@dataclass(frozen=True)
class PrincipalQuotaPolicy:
    max_routes: int = 8
    max_requests: int = 120
    request_window_seconds: float = 60.0
    max_concurrent_commands: int = 4

    def __post_init__(self) -> None:
        for name in ("max_routes", "max_requests", "max_concurrent_commands"):
            value = getattr(self, name)
            if type(value) is not int or value < 1:
                raise ValueError(f"{name} must be a positive integer")
        if not isinstance(self.request_window_seconds, (int, float)) or self.request_window_seconds <= 0:
            raise ValueError("request_window_seconds must be positive")


@dataclass(frozen=True)
class QuotaSnapshot:
    routes: int
    concurrent_commands: int
    requests: int


@dataclass
class _QuotaState:
    routes: int = 0
    concurrent_commands: int = 0
    requests: deque[float] = field(default_factory=deque)


class QuotaTracker:
    def __init__(self, policy: Optional[PrincipalQuotaPolicy] = None) -> None:
        self._policy = policy or PrincipalQuotaPolicy()
        self._states: dict[str, _QuotaState] = {}
        self._lock = threading.RLock()

    @staticmethod
    def _principal_id(principal_id: str) -> str:
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise ValueError("principal_id must be a non-empty string")
        if principal_id != principal_id.strip():
            raise ValueError("principal_id must not contain surrounding whitespace")
        return principal_id

    def _state(self, principal_id: str) -> _QuotaState:
        principal_id = self._principal_id(principal_id)
        return self._states.setdefault(principal_id, _QuotaState())

    def acquire_route(self, principal_id: str) -> None:
        with self._lock:
            state = self._state(principal_id)
            if state.routes >= self._policy.max_routes:
                raise QuotaExceeded("route quota exceeded")
            state.routes += 1

    def release_route(self, principal_id: str) -> None:
        with self._lock:
            state = self._state(principal_id)
            if state.routes > 0:
                state.routes -= 1

    def record_request(self, principal_id: str, *, now: Optional[float] = None) -> None:
        timestamp = time.monotonic() if now is None else float(now)
        with self._lock:
            state = self._state(principal_id)
            cutoff = timestamp - float(self._policy.request_window_seconds)
            while state.requests and state.requests[0] <= cutoff:
                state.requests.popleft()
            if len(state.requests) >= self._policy.max_requests:
                raise QuotaExceeded("request quota exceeded")
            state.requests.append(timestamp)

    def acquire_command(self, principal_id: str) -> None:
        with self._lock:
            state = self._state(principal_id)
            if state.concurrent_commands >= self._policy.max_concurrent_commands:
                raise QuotaExceeded("concurrent command quota exceeded")
            state.concurrent_commands += 1

    def release_command(self, principal_id: str) -> None:
        with self._lock:
            state = self._state(principal_id)
            if state.concurrent_commands > 0:
                state.concurrent_commands -= 1

    def snapshot(self, principal_id: str, *, now: Optional[float] = None) -> QuotaSnapshot:
        timestamp = time.monotonic() if now is None else float(now)
        with self._lock:
            state = self._state(principal_id)
            cutoff = timestamp - float(self._policy.request_window_seconds)
            while state.requests and state.requests[0] <= cutoff:
                state.requests.popleft()
            return QuotaSnapshot(
                routes=state.routes,
                concurrent_commands=state.concurrent_commands,
                requests=len(state.requests),
            )


def validate_secret_file(path: str | os.PathLike[str]) -> Path:
    secret = Path(path).expanduser()
    info = secret.stat()
    if not stat.S_ISREG(info.st_mode):
        raise SecurityConfigurationError("secret path must be a regular file")
    if info.st_uid != os.getuid():
        raise SecurityConfigurationError("secret file must be owned by the current user")
    if stat.S_IMODE(info.st_mode) & 0o077:
        raise SecurityConfigurationError("secret file permissions must not grant group/other access")
    return secret


__all__ = [
    "AuthorizationPolicy",
    "BoundedAuditSink",
    "Capability",
    "CurveClientConfig",
    "CurveCredentialsProvider",
    "CurveServerConfig",
    "KeyRecord",
    "KeyRegistry",
    "PrincipalQuotaPolicy",
    "QuotaExceeded",
    "QuotaSnapshot",
    "QuotaTracker",
    "SecurityAuditRecord",
    "SecurityConfigurationError",
    "apply_curve_client",
    "apply_curve_server",
    "validate_curve_public_key",
    "validate_curve_secret_key",
    "validate_secret_file",
]
