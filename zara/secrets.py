"""Secret references and outbound redaction primitives.

This module deliberately does not implement a persistent secret store.  It owns
only the public, plaintext-free reference ABI and the redaction boundary used by
future Android, desktop, server, provider, and plugin secret-store adapters.

The agent/operator-facing alias syntax mirrors Agent Zero's useful convention::

    §§secret(NAME)

Aliases are inert references.  Nothing in this module expands an alias into
secret material, and ``SecretRef`` never serializes plaintext.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from typing import Mapping


_NAME_PATTERN = r"[A-Za-z_][A-Za-z0-9_]{0,127}"
_NAME_RE = re.compile(rf"{_NAME_PATTERN}\Z")
_ALIAS_RE = re.compile(rf"§§secret\(({_NAME_PATTERN})\)")


class SecretAliasError(ValueError):
    """Raised when a secret alias or name is malformed."""


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


def _canonical_name(name: str) -> str:
    if not isinstance(name, str) or not _NAME_RE.fullmatch(name):
        raise SecretAliasError(
            "Secret names must start with a letter or underscore and contain "
            "only ASCII letters, digits, or underscores (maximum 128 chars)"
        )
    return name.upper()


def alias_for_secret(name: str) -> str:
    """Return the canonical printable alias for ``name``.

    The returned alias is presentation metadata only; it is not a capability
    and must never imply permission to resolve secret material.
    """

    return f"§§secret({_canonical_name(name)})"


def parse_secret_alias(alias: str) -> str:
    """Parse one complete alias and return its canonical secret name."""

    if not isinstance(alias, str):
        raise SecretAliasError("Secret alias must be text")
    match = _ALIAS_RE.fullmatch(alias)
    if match is None:
        raise SecretAliasError("Invalid secret alias")
    return _canonical_name(match.group(1))


def find_secret_aliases(text: str) -> tuple[str, ...]:
    """Return canonical secret names referenced by aliases in ``text``.

    This is discovery only.  It intentionally performs no resolution.
    """

    if not isinstance(text, str):
        raise TypeError("text must be str")
    return tuple(_canonical_name(match.group(1)) for match in _ALIAS_RE.finditer(text))


@dataclass(frozen=True, slots=True)
class SecretRef:
    """Plaintext-free, serializable identity for one secret."""

    id: str
    name: str
    scope: SecretScope
    owner: str
    kind: SecretKind
    revision: int
    generation: int
    configured: bool

    def __post_init__(self) -> None:
        if not isinstance(self.id, str) or not self.id or len(self.id) > 256:
            raise ValueError("Secret id must be non-empty text up to 256 chars")
        if not isinstance(self.owner, str) or not self.owner or len(self.owner) > 256:
            raise ValueError("Secret owner must be non-empty text up to 256 chars")
        if not isinstance(self.scope, SecretScope):
            raise TypeError("scope must be SecretScope")
        if not isinstance(self.kind, SecretKind):
            raise TypeError("kind must be SecretKind")
        if not isinstance(self.revision, int) or self.revision < 0:
            raise ValueError("revision must be a non-negative integer")
        if not isinstance(self.generation, int) or self.generation < 0:
            raise ValueError("generation must be a non-negative integer")
        if not isinstance(self.configured, bool):
            raise TypeError("configured must be bool")
        object.__setattr__(self, "name", _canonical_name(self.name))

    @property
    def alias(self) -> str:
        return alias_for_secret(self.name)

    def to_public_dict(self) -> dict[str, object]:
        """Return the bounded public projection; plaintext cannot appear here."""

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
class _RedactionEntry:
    value: str
    replacement: str


class SecretRedactor:
    """Mask known secret values before text leaves a trusted boundary.

    Values shorter than ``min_scan_length`` are deliberately excluded from
    automatic free-text scanning to avoid pathological over-redaction.  Once a
    value is eligible for scanning, the streaming filter protects prefixes from
    the first character so chunk boundaries cannot leak a short initial slice.
    Typed secret objects/sinks must protect excluded short values structurally
    rather than by substring scanning.
    """

    def __init__(
        self,
        secrets: Mapping[SecretRef, str],
        *,
        min_scan_length: int = 4,
        max_secret_length: int = 4096,
    ) -> None:
        if not isinstance(min_scan_length, int) or min_scan_length < 1:
            raise ValueError("min_scan_length must be a positive integer")
        if not isinstance(max_secret_length, int) or max_secret_length < min_scan_length:
            raise ValueError("max_secret_length must be >= min_scan_length")

        grouped: dict[str, list[SecretRef]] = {}
        skipped_short = 0
        for secret_ref, value in secrets.items():
            if not isinstance(secret_ref, SecretRef):
                raise TypeError("secret mapping keys must be SecretRef")
            if not isinstance(value, str):
                raise TypeError("secret material must be str for text redaction")
            if not value:
                continue
            if len(value) > max_secret_length:
                raise ValueError("secret material exceeds maximum redaction length")
            if len(value) < min_scan_length:
                skipped_short += 1
                continue
            grouped.setdefault(value, []).append(secret_ref)

        entries: list[_RedactionEntry] = []
        for value, refs in grouped.items():
            replacement = refs[0].alias if len(refs) == 1 else "***"
            entries.append(_RedactionEntry(value=value, replacement=replacement))

        self._entries = tuple(
            sorted(entries, key=lambda entry: len(entry.value), reverse=True)
        )
        self._min_scan_length = min_scan_length
        self._max_secret_length = max(
            (len(entry.value) for entry in self._entries),
            default=0,
        )
        self._skipped_short = skipped_short

    @property
    def skipped_short_secret_count(self) -> int:
        return self._skipped_short

    def __repr__(self) -> str:
        return (
            f"SecretRedactor(secret_count={len(self._entries)}, "
            f"skipped_short={self._skipped_short})"
        )

    def mask_text(self, text: str) -> str:
        """Replace complete known secret values with safe aliases/redaction."""

        if not isinstance(text, str):
            raise TypeError("text must be str")
        result = text
        for entry in self._entries:
            result = result.replace(entry.value, entry.replacement)
        return result

    def streaming_filter(self) -> "SecretStreamingFilter":
        return SecretStreamingFilter(self)

    def _longest_suffix_prefix(self, text: str) -> int:
        """Length of the longest suffix that may continue into a secret.

        Complete values are replaced before this is called, so this method only
        retains unresolved prefixes.  For every value admitted to free-text
        scanning, even a one-character prefix is held rather than emitted.  The
        retained suffix remains bounded by the longest configured secret.
        """

        if not text or not self._entries:
            return 0

        max_candidate = min(len(text), self._max_secret_length)
        for length in range(max_candidate, 0, -1):
            suffix = text[-length:]
            if any(entry.value.startswith(suffix) for entry in self._entries):
                return length
        return 0


class SecretStreamingFilter:
    """Stateful redactor that prevents cross-chunk secret-prefix leakage."""

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
        self._pending = self._redactor.mask_text(self._pending)
        hold = self._redactor._longest_suffix_prefix(self._pending)
        if hold:
            emit = self._pending[:-hold]
            self._pending = self._pending[-hold:]
            return emit

        emit = self._pending
        self._pending = ""
        return emit

    def finalize(self) -> str:
        if not self._pending:
            return ""

        self._pending = self._redactor.mask_text(self._pending)
        hold = self._redactor._longest_suffix_prefix(self._pending)
        if hold:
            result = self._pending[:-hold] + "***"
        else:
            result = self._pending
        self._pending = ""
        return result


__all__ = [
    "SecretAliasError",
    "SecretKind",
    "SecretRedactor",
    "SecretRef",
    "SecretScope",
    "SecretStreamingFilter",
    "alias_for_secret",
    "find_secret_aliases",
    "parse_secret_alias",
]
