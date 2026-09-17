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


class _TrieNode:
    __slots__ = ("children", "replacement")

    def __init__(self) -> None:
        self.children: dict[str, _TrieNode] = {}
        self.replacement: str | None = None


class SecretRedactor:
    """Mask known secret values before text leaves a trusted boundary.

    Values shorter than ``min_scan_length`` are deliberately excluded from
    automatic free-text scanning to avoid pathological over-redaction.  Once a
    value is eligible for scanning, the streaming filter protects prefixes from
    the first character so chunk boundaries cannot leak a short initial slice.
    Typed secret objects/sinks must protect excluded short values structurally
    rather than by substring scanning.

    Secret count, individual value length, and aggregate scannable material are
    bounded so redaction cannot become an unbounded lookup/memory workload.
    """

    def __init__(
        self,
        secrets: Mapping[SecretRef, str],
        *,
        min_scan_length: int = 4,
        max_secret_length: int = 4096,
        max_secret_count: int = 512,
        max_total_secret_chars: int = 262_144,
    ) -> None:
        if not isinstance(min_scan_length, int) or min_scan_length < 1:
            raise ValueError("min_scan_length must be a positive integer")
        if not isinstance(max_secret_length, int) or max_secret_length < min_scan_length:
            raise ValueError("max_secret_length must be >= min_scan_length")
        if not isinstance(max_secret_count, int) or max_secret_count < 1:
            raise ValueError("max_secret_count must be a positive integer")
        if not isinstance(max_total_secret_chars, int) or max_total_secret_chars < 1:
            raise ValueError("max_total_secret_chars must be a positive integer")
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

        entries: list[_RedactionEntry] = []
        for value, refs in grouped.items():
            replacement = refs[0].alias if len(refs) == 1 else "***"
            entries.append(_RedactionEntry(value=value, replacement=replacement))

        self._entries = tuple(
            sorted(entries, key=lambda entry: len(entry.value), reverse=True)
        )
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
        """Mask complete known values without reprocessing generated aliases.

        Unlike streaming finalization, a full already-complete text value keeps
        an ordinary suffix that merely happens to be an incomplete secret
        prefix.  Complete secret matches still use the same leftmost/longest
        trie semantics as streaming output.
        """

        if not isinstance(text, str):
            raise TypeError("text must be str")
        stream = SecretStreamingFilter(self)
        stream._pending = text
        return stream._drain(final=True, mask_incomplete=False)

    def streaming_filter(self) -> "SecretStreamingFilter":
        return SecretStreamingFilter(self)


class SecretStreamingFilter:
    """Stateful leftmost-longest redactor for chunked text.

    ``pending`` always contains the earliest not-yet-safe raw text.  A complete
    secret that is also a prefix of a longer configured secret is retained until
    the next input disambiguates it.  This prevents eager replacement at a chunk
    boundary from exposing a longer credential split across chunks.
    """

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
            last_terminal_end = 0
            last_terminal_replacement: str | None = None
            mismatch = False

            for index, character in enumerate(self._pending):
                next_node = node.children.get(character)
                if next_node is None:
                    mismatch = True
                    break
                node = next_node
                if node.replacement is not None:
                    last_terminal_end = index + 1
                    last_terminal_replacement = node.replacement
            else:
                # The entire pending buffer is a valid secret prefix.
                if node.replacement is not None and (final or not node.children):
                    output.append(node.replacement)
                    self._pending = ""
                    continue

                if final:
                    # No more bytes can arrive.  If a shorter complete secret
                    # was observed along this path, emit that safe replacement
                    # and continue classifying the remainder.  Otherwise this is
                    # either an unresolved stream prefix (mask it) or an ordinary
                    # incomplete prefix in already-complete text (keep it).
                    if last_terminal_end:
                        assert last_terminal_replacement is not None
                        output.append(last_terminal_replacement)
                        self._pending = self._pending[last_terminal_end:]
                        continue
                    output.append("***" if mask_incomplete else self._pending)
                    self._pending = ""
                break

            if mismatch and last_terminal_end:
                assert last_terminal_replacement is not None
                output.append(last_terminal_replacement)
                self._pending = self._pending[last_terminal_end:]
                continue

            # The first pending character cannot begin any configured secret.
            # It is therefore safe to emit; the remaining suffix is reevaluated
            # because a secret may start at the next character.
            output.append(self._pending[0])
            self._pending = self._pending[1:]

        return "".join(output)


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
