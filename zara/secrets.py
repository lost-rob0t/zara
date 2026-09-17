"""Plaintext-free secret references, lease metadata, and outbound redaction."""

from __future__ import annotations

import math
import re
import time
import uuid
from dataclasses import dataclass
from enum import Enum
from typing import Mapping, Protocol, runtime_checkable


_NAME_PATTERN = r"[A-Za-z_][A-Za-z0-9_]{0,127}"
_NAME_RE = re.compile(rf"{_NAME_PATTERN}\Z")
_ALIAS_RE = re.compile(rf"§§secret\(({_NAME_PATTERN})\)")
_CONTROL_RE = re.compile(r"[\x00-\x1f\x7f]")
_LEASE_ISSUER = object()
_MAX_LEASE_TTL_SECONDS = 300.0


class SecretAliasError(ValueError):
    """Raised when a secret alias or name is malformed."""


class SecretLeaseError(RuntimeError):
    """Raised when a runtime secret lease is invalid or unusable."""


class SecretScope(str, Enum):
    GLOBAL = "global"
    PRINCIPAL = "principal"
    PROJECT = "project"
    DEVICE = "device"
    SESSION = "session"


class SecretKind(str, Enum):
    API_KEY = "api_key"
    TOKEN = "token"
    OAUTH_REFRESH = "oauth_refresh"
    PASSWORD = "password"
    SYMMETRIC_KEY = "symmetric_key"
    PRIVATE_KEY = "private_key"
    CREDENTIAL = "credential"
    GENERIC = "generic"


class SecretSink(str, Enum):
    """Reviewed execution sinks that may later consume secret material."""

    HTTP_HEADER = "http_header"
    REQUEST_BODY = "request_body"
    ENVIRONMENT = "environment"
    STDIN = "stdin"
    FILE_DESCRIPTOR = "file_descriptor"
    CREDENTIAL_CALLBACK = "credential_callback"


def _canonical_name(name: str) -> str:
    if not isinstance(name, str) or not _NAME_RE.fullmatch(name):
        raise SecretAliasError(
            "Secret names must start with a letter or underscore and contain "
            "only ASCII letters, digits, or underscores (maximum 128 chars)"
        )
    return name.upper()


def _bounded_label(value: str, field: str, *, max_length: int = 256) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{field} must be str")
    if not value or len(value) > max_length:
        raise ValueError(f"{field} must be non-empty text up to {max_length} chars")
    if _CONTROL_RE.search(value):
        raise ValueError(f"{field} must not contain control characters")
    return value


def alias_for_secret(name: str) -> str:
    """Return the canonical inert printable alias for a secret name."""

    return f"§§secret({_canonical_name(name)})"


def parse_secret_alias(alias: str) -> str:
    """Parse one complete alias without resolving any secret material."""

    if not isinstance(alias, str):
        raise SecretAliasError("Secret alias must be text")
    match = _ALIAS_RE.fullmatch(alias)
    if match is None:
        raise SecretAliasError("Invalid secret alias")
    return _canonical_name(match.group(1))


def find_secret_aliases(text: str) -> tuple[str, ...]:
    """Return canonical names referenced by inert aliases in text."""

    if not isinstance(text, str):
        raise TypeError("text must be str")
    return tuple(_canonical_name(match.group(1)) for match in _ALIAS_RE.finditer(text))


@dataclass(frozen=True, slots=True)
class SecretRef:
    """Plaintext-free serializable identity for one secret."""

    id: str
    name: str
    scope: SecretScope
    owner: str
    kind: SecretKind
    revision: int
    generation: int
    configured: bool

    def __post_init__(self) -> None:
        _bounded_label(self.id, "Secret id")
        _bounded_label(self.owner, "Secret owner")
        if not isinstance(self.scope, SecretScope):
            raise TypeError("scope must be SecretScope")
        if not isinstance(self.kind, SecretKind):
            raise TypeError("kind must be SecretKind")
        if type(self.revision) is not int or self.revision < 0:
            raise ValueError("revision must be a non-negative integer")
        if type(self.generation) is not int or self.generation < 0:
            raise ValueError("generation must be a non-negative integer")
        if not isinstance(self.configured, bool):
            raise TypeError("configured must be bool")
        object.__setattr__(self, "name", _canonical_name(self.name))

    @property
    def alias(self) -> str:
        return alias_for_secret(self.name)

    def to_public_dict(self) -> dict[str, object]:
        return {
            "id": self.id,
            "name": self.name,
            "alias": self.alias,
            "scope": self.scope.value,
            "owner": self.owner,
            "kind": self.kind.value,
            "revision": self.revision,
            "generation": self.generation,
            "configured": self.configured,
        }


@dataclass(frozen=True, slots=True)
class SecretUseContext:
    """Plaintext-free context binding one proposed secret use."""

    principal: str
    runtime_generation: int
    consumer: str
    purpose: str
    sink: SecretSink
    request_id: str | None = None

    def __post_init__(self) -> None:
        _bounded_label(self.principal, "principal")
        _bounded_label(self.consumer, "consumer")
        _bounded_label(self.purpose, "purpose")
        if type(self.runtime_generation) is not int or self.runtime_generation < 0:
            raise ValueError("runtime_generation must be a non-negative integer")
        if not isinstance(self.sink, SecretSink):
            raise TypeError("sink must be SecretSink")
        if self.request_id is not None:
            _bounded_label(self.request_id, "request_id")

    def to_public_dict(self) -> dict[str, object]:
        return {
            "principal": self.principal,
            "runtime_generation": self.runtime_generation,
            "consumer": self.consumer,
            "purpose": self.purpose,
            "sink": self.sink.value,
            "request_id": self.request_id,
        }


class SecretLease:
    """Short-lived, non-serializable authorization metadata with no plaintext."""

    __slots__ = (
        "_lease_id",
        "_secret_id",
        "_secret_revision",
        "_secret_generation",
        "_principal",
        "_runtime_generation",
        "_consumer",
        "_purpose",
        "_sink",
        "_request_id",
        "_expires_at_monotonic",
        "_closed",
    )

    def __init__(
        self,
        *,
        _issuer: object | None = None,
        lease_id: str | None = None,
        secret: SecretRef | None = None,
        context: SecretUseContext | None = None,
        expires_at_monotonic: float | None = None,
    ) -> None:
        if _issuer is not _LEASE_ISSUER:
            raise SecretLeaseError("Secret leases are runtime-issued")
        if lease_id is None or secret is None or context is None:
            raise SecretLeaseError("Incomplete runtime lease")
        if expires_at_monotonic is None or not math.isfinite(expires_at_monotonic):
            raise SecretLeaseError("Invalid runtime lease expiry")

        self._lease_id = lease_id
        self._secret_id = secret.id
        self._secret_revision = secret.revision
        self._secret_generation = secret.generation
        self._principal = context.principal
        self._runtime_generation = context.runtime_generation
        self._consumer = context.consumer
        self._purpose = context.purpose
        self._sink = context.sink
        self._request_id = context.request_id
        self._expires_at_monotonic = expires_at_monotonic
        self._closed = False

    @classmethod
    def _issue(
        cls,
        secret: SecretRef,
        context: SecretUseContext,
        *,
        ttl_seconds: float = 30.0,
    ) -> "SecretLease":
        if not isinstance(secret, SecretRef):
            raise TypeError("secret must be SecretRef")
        if not isinstance(context, SecretUseContext):
            raise TypeError("context must be SecretUseContext")
        if not secret.configured:
            raise SecretLeaseError("Secret is not configured")
        if not isinstance(ttl_seconds, (int, float)) or isinstance(ttl_seconds, bool):
            raise ValueError("ttl_seconds must be a finite positive number")
        ttl = float(ttl_seconds)
        if not math.isfinite(ttl) or ttl <= 0 or ttl > _MAX_LEASE_TTL_SECONDS:
            raise ValueError("ttl_seconds must be between 0 and 300 seconds")
        return cls(
            _issuer=_LEASE_ISSUER,
            lease_id=uuid.uuid4().hex,
            secret=secret,
            context=context,
            expires_at_monotonic=time.monotonic() + ttl,
        )

    @property
    def lease_id(self) -> str:
        return self._lease_id

    @property
    def secret_id(self) -> str:
        return self._secret_id

    @property
    def secret_revision(self) -> int:
        return self._secret_revision

    @property
    def secret_generation(self) -> int:
        return self._secret_generation

    @property
    def principal(self) -> str:
        return self._principal

    @property
    def runtime_generation(self) -> int:
        return self._runtime_generation

    @property
    def consumer(self) -> str:
        return self._consumer

    @property
    def purpose(self) -> str:
        return self._purpose

    @property
    def sink(self) -> SecretSink:
        return self._sink

    @property
    def request_id(self) -> str | None:
        return self._request_id

    @property
    def expires_at_monotonic(self) -> float:
        return self._expires_at_monotonic

    @property
    def closed(self) -> bool:
        return self._closed

    def assert_usable(self, *, now: float | None = None) -> None:
        if self._closed:
            raise SecretLeaseError("Secret lease is closed")
        current = time.monotonic() if now is None else now
        if not isinstance(current, (int, float)) or isinstance(current, bool):
            raise ValueError("now must be a finite monotonic timestamp")
        current_value = float(current)
        if not math.isfinite(current_value):
            raise ValueError("now must be a finite monotonic timestamp")
        if current_value >= self._expires_at_monotonic:
            raise SecretLeaseError("Secret lease is expired")

    def close(self) -> None:
        self._closed = True

    def __repr__(self) -> str:
        return f"SecretLease(state={'closed' if self._closed else 'open'!r})"

    __str__ = __repr__

    def __reduce_ex__(self, protocol: int) -> object:
        del protocol
        raise TypeError("SecretLease cannot be serialized")

    def __getstate__(self) -> object:
        raise TypeError("SecretLease cannot be serialized")


@runtime_checkable
class SecretStore(Protocol):
    """Plaintext-free store contract visible to ordinary Core orchestration."""

    def list_refs(self) -> tuple[SecretRef, ...]: ...

    def get_ref(self, secret_id: str) -> SecretRef | None: ...

    def resolve(
        self,
        secret: SecretRef,
        context: SecretUseContext,
        *,
        ttl_seconds: float = 30.0,
    ) -> SecretLease: ...

    def close_lease(self, lease: SecretLease) -> None: ...


@dataclass(frozen=True, slots=True)
class _RedactionEntry:
    value: str
    replacement: str


class _TrieNode:
    __slots__ = ("children", "replacement")

    def __init__(self) -> None:
        self.children: dict[str, _TrieNode] = {}
        self.replacement: str | None = None


class SecretRedactor:
    """Mask known secret values before text leaves a trusted boundary."""

    def __init__(
        self,
        secrets: Mapping[SecretRef, str],
        *,
        min_scan_length: int = 4,
        max_secret_length: int = 4096,
        max_secret_count: int = 512,
        max_total_secret_chars: int = 262_144,
        reveal_aliases: bool = True,
    ) -> None:
        if not isinstance(min_scan_length, int) or min_scan_length < 1:
            raise ValueError("min_scan_length must be a positive integer")
        if not isinstance(max_secret_length, int) or max_secret_length < min_scan_length:
            raise ValueError("max_secret_length must be >= min_scan_length")
        if not isinstance(max_secret_count, int) or max_secret_count < 1:
            raise ValueError("max_secret_count must be a positive integer")
        if not isinstance(max_total_secret_chars, int) or max_total_secret_chars < 1:
            raise ValueError("max_total_secret_chars must be a positive integer")
        if not isinstance(reveal_aliases, bool):
            raise TypeError("reveal_aliases must be bool")
        if len(secrets) > max_secret_count:
            raise ValueError("secret count exceeds maximum redaction set size")

        grouped: dict[str, list[SecretRef]] = {}
        skipped_short = 0
        total_secret_chars = 0
        for secret_ref, value in secrets.items():
            if not isinstance(secret_ref, SecretRef):
                raise TypeError("secret mapping keys must be SecretRef")
            if not isinstance(value, str):
                raise TypeError("secret material must be str for text redaction")
            if not value:
                continue
            if len(value) > max_secret_length:
                raise ValueError("secret material exceeds maximum redaction length")
            total_secret_chars += len(value)
            if total_secret_chars > max_total_secret_chars:
                raise ValueError("total secret material exceeds redaction budget")
            if len(value) < min_scan_length:
                skipped_short += 1
                continue
            grouped.setdefault(value, []).append(secret_ref)

        entries = []
        for value, refs in grouped.items():
            replacement = refs[0].alias if reveal_aliases and len(refs) == 1 else "***"
            entries.append(_RedactionEntry(value=value, replacement=replacement))
        self._entries = tuple(sorted(entries, key=lambda entry: len(entry.value), reverse=True))
        self._skipped_short = skipped_short
        self._trie = _TrieNode()
        for entry in self._entries:
            node = self._trie
            for character in entry.value:
                node = node.children.setdefault(character, _TrieNode())
            node.replacement = entry.replacement

    @property
    def skipped_short_secret_count(self) -> int:
        return self._skipped_short

    def __repr__(self) -> str:
        return (
            f"SecretRedactor(secret_count={len(self._entries)}, "
            f"skipped_short={self._skipped_short})"
        )

    def mask_text(self, text: str) -> str:
        """Mask complete matches in one pass without re-scanning generated aliases."""

        if not isinstance(text, str):
            raise TypeError("text must be str")
        stream = SecretStreamingFilter(self)
        stream._pending = text
        return stream._drain(final=True, mask_incomplete=False)

    def streaming_filter(self) -> "SecretStreamingFilter":
        return SecretStreamingFilter(self)


class SecretStreamingFilter:
    """Stateful leftmost-longest redactor for chunked text."""

    __slots__ = ("_redactor", "_pending")

    def __init__(self, redactor: SecretRedactor) -> None:
        self._redactor = redactor
        self._pending = ""

    @property
    def pending_size(self) -> int:
        return len(self._pending)

    def process_chunk(self, chunk: str) -> str:
        if not isinstance(chunk, str):
            raise TypeError("chunk must be str")
        if not chunk:
            return ""
        self._pending += chunk
        return self._drain(final=False, mask_incomplete=True)

    def finalize(self) -> str:
        return self._drain(final=True, mask_incomplete=True)

    def _drain(self, *, final: bool, mask_incomplete: bool) -> str:
        output: list[str] = []
        root = self._redactor._trie
        while self._pending:
            node = root
            last_end = 0
            last_replacement: str | None = None

            for index, character in enumerate(self._pending):
                next_node = node.children.get(character)
                if next_node is None:
                    if last_end:
                        assert last_replacement is not None
                        output.append(last_replacement)
                        self._pending = self._pending[last_end:]
                    else:
                        output.append(self._pending[0])
                        self._pending = self._pending[1:]
                    break
                node = next_node
                if node.replacement is not None:
                    last_end = index + 1
                    last_replacement = node.replacement
            else:
                if not final:
                    if node.replacement is not None and not node.children:
                        output.append(node.replacement)
                        self._pending = ""
                    break

                if node.replacement is not None:
                    output.append(node.replacement)
                    self._pending = ""
                    continue

                if last_end:
                    assert last_replacement is not None
                    output.append(last_replacement)
                    remainder = self._pending[last_end:]
                    if mask_incomplete and remainder:
                        output.append("***")
                        self._pending = ""
                    else:
                        self._pending = remainder
                    continue

                output.append("***" if mask_incomplete else self._pending)
                self._pending = ""

        return "".join(output)


__all__ = [
    "SecretAliasError",
    "SecretKind",
    "SecretLease",
    "SecretLeaseError",
    "SecretRedactor",
    "SecretRef",
    "SecretScope",
    "SecretSink",
    "SecretStore",
    "SecretStreamingFilter",
    "SecretUseContext",
    "alias_for_secret",
    "find_secret_aliases",
    "parse_secret_alias",
]
