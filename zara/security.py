"""Security primitives for authenticated Zara daemon operation.

This module intentionally contains no ZeroMQ socket ownership.  It provides
closed, thread-safe policy/state primitives that the transport layer can use
without making routing ids or request payloads authoritative for identity.
"""

from __future__ import annotations

import enum
import os
import stat
import threading
import time
import uuid
from collections import deque
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Iterable, Optional

from zmq.utils import z85

from zara.server import PrincipalContext


class SecurityError(RuntimeError):
    pass


class KeyAlreadyEnrolled(SecurityError):
    pass


class KeyNotActive(SecurityError):
    pass


class AuthorizationDenied(SecurityError):
    pass


class QuotaExceeded(SecurityError):
    pass


class Capability(str, enum.Enum):
    SESSION_BASIC = "session.basic"
    RUNTIME_STATUS = "runtime.status"
    TURN_SUBMIT = "turn.submit"
    TURN_CANCEL = "turn.cancel"
    TOOL_APPROVE = "tool.approve"
    CONTEXT_READ = "context.read"
    CONTEXT_WRITE = "context.write"
    MEMORY_READ = "memory.read"
    MEMORY_WRITE = "memory.write"
    DAEMON_ADMIN = "daemon.admin"


@dataclass(frozen=True)
class EnrolledKey:
    public_key: str
    principal: PrincipalContext
    device_id: str
    capabilities: frozenset[Capability]
    user_id: str
    generation: int
    active: bool = True


class SecurityRegistry:
    """Thread-safe live mapping from CURVE public keys to principals."""

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._by_public_key: dict[str, EnrolledKey] = {}
        self._by_user_id: dict[str, EnrolledKey] = {}
        self._by_device_id: dict[str, EnrolledKey] = {}

    @staticmethod
    def _normalize_public_key(public_key: str | bytes) -> str:
        if isinstance(public_key, bytes):
            try:
                public_key = public_key.decode("ascii")
            except UnicodeDecodeError as error:
                raise ValueError("CURVE public key must be ASCII Z85") from error
        if not isinstance(public_key, str) or not public_key:
            raise ValueError("CURVE public key must be a non-empty Z85 string")
        try:
            encoded = public_key.encode("ascii")
            decoded = z85.decode(encoded)
        except (UnicodeEncodeError, ValueError) as error:
            raise ValueError("CURVE public key must be valid Z85") from error
        if len(encoded) != 40 or len(decoded) != 32:
            raise ValueError("CURVE public key must encode exactly 32 bytes")
        return public_key

    @staticmethod
    def _normalize_device_id(device_id: str) -> str:
        if not isinstance(device_id, str) or not device_id.strip():
            raise ValueError("device_id must be a non-empty string")
        if device_id != device_id.strip():
            raise ValueError("device_id must not contain leading or trailing whitespace")
        return device_id

    @staticmethod
    def _normalize_capabilities(
        capabilities: Optional[Iterable[Capability]],
    ) -> frozenset[Capability]:
        if capabilities is None:
            return frozenset()
        result: set[Capability] = set()
        for capability in capabilities:
            if not isinstance(capability, Capability):
                raise TypeError("capabilities must contain Capability values")
            result.add(capability)
        return frozenset(result)

    @staticmethod
    def _new_user_id(device_id: str, generation: int) -> str:
        return f"zara:{device_id}:{generation}:{uuid.uuid4().hex}"

    def enroll(
        self,
        public_key: str | bytes,
        *,
        principal: PrincipalContext,
        device_id: str,
        capabilities: Optional[Iterable[Capability]] = None,
    ) -> EnrolledKey:
        if not isinstance(principal, PrincipalContext):
            raise TypeError("principal must be PrincipalContext")
        normalized_key = self._normalize_public_key(public_key)
        normalized_device = self._normalize_device_id(device_id)
        normalized_capabilities = self._normalize_capabilities(capabilities)

        with self._lock:
            existing_key = self._by_public_key.get(normalized_key)
            if existing_key is not None and existing_key.active:
                raise KeyAlreadyEnrolled("CURVE public key is already enrolled")
            current_device = self._by_device_id.get(normalized_device)
            if current_device is not None and current_device.active:
                raise KeyAlreadyEnrolled("device_id already has an active key")
            generation = 1 if current_device is None else current_device.generation + 1
            enrolled = EnrolledKey(
                public_key=normalized_key,
                principal=principal,
                device_id=normalized_device,
                capabilities=normalized_capabilities,
                user_id=self._new_user_id(normalized_device, generation),
                generation=generation,
            )
            self._by_public_key[normalized_key] = enrolled
            self._by_user_id[enrolled.user_id] = enrolled
            self._by_device_id[normalized_device] = enrolled
            return enrolled

    @staticmethod
    def _require_active(record: Optional[EnrolledKey]) -> EnrolledKey:
        if record is None or not record.active:
            raise KeyNotActive("client key is not active")
        return record

    def resolve_public_key(self, public_key: str | bytes) -> EnrolledKey:
        normalized_key = self._normalize_public_key(public_key)
        with self._lock:
            return self._require_active(self._by_public_key.get(normalized_key))

    def resolve_user_id(self, user_id: str | bytes) -> EnrolledKey:
        if isinstance(user_id, bytes):
            try:
                user_id = user_id.decode("utf-8")
            except UnicodeDecodeError as error:
                raise KeyNotActive("authenticated user id is not active") from error
        if not isinstance(user_id, str) or not user_id:
            raise KeyNotActive("authenticated user id is not active")
        with self._lock:
            return self._require_active(self._by_user_id.get(user_id))

    def revoke(self, device_id: str) -> EnrolledKey:
        normalized_device = self._normalize_device_id(device_id)
        with self._lock:
            current = self._require_active(self._by_device_id.get(normalized_device))
            revoked = replace(current, active=False)
            self._by_device_id[normalized_device] = revoked
            self._by_public_key[current.public_key] = revoked
            self._by_user_id[current.user_id] = revoked
            return revoked

    def rotate(self, device_id: str, new_public_key: str | bytes) -> EnrolledKey:
        normalized_device = self._normalize_device_id(device_id)
        normalized_key = self._normalize_public_key(new_public_key)
        with self._lock:
            current = self._require_active(self._by_device_id.get(normalized_device))
            existing_key = self._by_public_key.get(normalized_key)
            if existing_key is not None and existing_key.active:
                raise KeyAlreadyEnrolled("new CURVE public key is already enrolled")
            revoked = replace(current, active=False)
            self._by_public_key[current.public_key] = revoked
            self._by_user_id[current.user_id] = revoked
            generation = current.generation + 1
            rotated = EnrolledKey(
                public_key=normalized_key,
                principal=current.principal,
                device_id=current.device_id,
                capabilities=current.capabilities,
                user_id=self._new_user_id(current.device_id, generation),
                generation=generation,
            )
            self._by_device_id[current.device_id] = rotated
            self._by_public_key[normalized_key] = rotated
            self._by_user_id[rotated.user_id] = rotated
            return rotated


def authorize(enrolled: EnrolledKey, capability: Capability) -> None:
    if not isinstance(enrolled, EnrolledKey):
        raise TypeError("authorization requires an enrolled client record")
    if not enrolled.active:
        raise KeyNotActive("client key is not active")
    if not isinstance(capability, Capability):
        raise AuthorizationDenied("unknown capability")
    if capability not in enrolled.capabilities:
        raise AuthorizationDenied(f"capability denied: {capability.value}")


@dataclass(frozen=True)
class SecurityLimits:
    max_connections: int = 4
    max_concurrent_requests: int = 8
    requests_per_window: int = 60
    request_window_seconds: float = 1.0

    def __post_init__(self) -> None:
        if self.max_connections < 1:
            raise ValueError("max_connections must be at least 1")
        if self.max_concurrent_requests < 1:
            raise ValueError("max_concurrent_requests must be at least 1")
        if self.requests_per_window < 1:
            raise ValueError("requests_per_window must be at least 1")
        if self.request_window_seconds <= 0:
            raise ValueError("request_window_seconds must be positive")

    def new_quota_manager(self) -> "QuotaManager":
        return QuotaManager(self)


@dataclass
class _PrincipalQuota:
    connections: int = 0
    requests: int = 0
    recent_request_times: deque[float] | None = None

    def __post_init__(self) -> None:
        if self.recent_request_times is None:
            self.recent_request_times = deque()


class QuotaManager:
    def __init__(self, limits: SecurityLimits) -> None:
        self._limits = limits
        self._lock = threading.RLock()
        self._states: dict[str, _PrincipalQuota] = {}

    @staticmethod
    def _principal_id(principal_id: str) -> str:
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise ValueError("principal_id must be a non-empty string")
        return principal_id

    def _state(self, principal_id: str) -> _PrincipalQuota:
        return self._states.setdefault(principal_id, _PrincipalQuota())

    def acquire_connection(self, principal_id: str) -> None:
        principal_id = self._principal_id(principal_id)
        with self._lock:
            state = self._state(principal_id)
            if state.connections >= self._limits.max_connections:
                raise QuotaExceeded("connection quota exceeded")
            state.connections += 1

    def release_connection(self, principal_id: str) -> None:
        principal_id = self._principal_id(principal_id)
        with self._lock:
            state = self._state(principal_id)
            state.connections = max(0, state.connections - 1)

    def acquire_request(self, principal_id: str, *, now: Optional[float] = None) -> None:
        principal_id = self._principal_id(principal_id)
        timestamp = time.monotonic() if now is None else float(now)
        with self._lock:
            state = self._state(principal_id)
            if state.requests >= self._limits.max_concurrent_requests:
                raise QuotaExceeded("concurrent request quota exceeded")
            recent = state.recent_request_times
            assert recent is not None
            cutoff = timestamp - self._limits.request_window_seconds
            while recent and recent[0] <= cutoff:
                recent.popleft()
            if len(recent) >= self._limits.requests_per_window:
                raise QuotaExceeded("request rate quota exceeded")
            recent.append(timestamp)
            state.requests += 1

    def release_request(self, principal_id: str) -> None:
        principal_id = self._principal_id(principal_id)
        with self._lock:
            state = self._state(principal_id)
            state.requests = max(0, state.requests - 1)


@dataclass(frozen=True)
class SecurityAuditRecord:
    timestamp_ns: int
    principal_id: Optional[str]
    device_id: Optional[str]
    session_id: Optional[str]
    request_id: Optional[str]
    turn_id: Optional[str]
    action: str
    decision: str
    error_class: Optional[str]
    duration_ns: int

    def as_dict(self) -> dict[str, object]:
        return {
            "timestamp_ns": self.timestamp_ns,
            "principal_id": self.principal_id,
            "device_id": self.device_id,
            "session_id": self.session_id,
            "request_id": self.request_id,
            "turn_id": self.turn_id,
            "action": self.action,
            "decision": self.decision,
            "error_class": self.error_class,
            "duration_ns": self.duration_ns,
        }


class SecurityAuditLog:
    def __init__(self, *, capacity: int = 256) -> None:
        if capacity < 1:
            raise ValueError("audit capacity must be at least 1")
        self._records: deque[SecurityAuditRecord] = deque(maxlen=capacity)
        self._lock = threading.Lock()

    def append(self, record: SecurityAuditRecord) -> None:
        if not isinstance(record, SecurityAuditRecord):
            raise TypeError("audit log accepts SecurityAuditRecord only")
        with self._lock:
            self._records.append(record)

    def snapshot(self) -> tuple[SecurityAuditRecord, ...]:
        with self._lock:
            return tuple(self._records)


def validate_listener_security(
    endpoint: str,
    *,
    curve_enabled: bool,
    zap_enabled: bool,
) -> str:
    if not isinstance(endpoint, str) or "://" not in endpoint:
        raise ValueError("listener endpoint must include a transport scheme")
    scheme = endpoint.split("://", 1)[0].lower()
    if scheme == "tcp":
        if not curve_enabled or not zap_enabled:
            raise ValueError("TCP listeners require CURVE and ZAP authentication")
        return endpoint
    if scheme in {"ipc", "inproc"}:
        return endpoint
    raise ValueError(f"unsupported listener transport: {scheme}")


def validate_secret_key_file(path: Path | str) -> Path:
    key_path = Path(path)
    info = os.lstat(key_path)
    if stat.S_ISLNK(info.st_mode) or not stat.S_ISREG(info.st_mode):
        raise PermissionError("secret key path must be a regular non-symlink file")
    if info.st_uid != os.getuid():
        raise PermissionError("secret key file must be owned by the current user")
    if stat.S_IMODE(info.st_mode) & 0o077:
        raise PermissionError("secret key file must not be accessible by group or other users")
    return key_path


__all__ = [
    "AuthorizationDenied",
    "Capability",
    "EnrolledKey",
    "KeyAlreadyEnrolled",
    "KeyNotActive",
    "QuotaExceeded",
    "QuotaManager",
    "SecurityAuditLog",
    "SecurityAuditRecord",
    "SecurityError",
    "SecurityLimits",
    "SecurityRegistry",
    "authorize",
    "validate_listener_security",
    "validate_secret_key_file",
]
