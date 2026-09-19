"""Human-safe short-code bootstrap for Zara pairing.

The short code is only a one-time lookup alias for an existing ``zara://pair/v1``
bootstrap URI. It never grants long-term authority; CURVE/ZAP enrollment remains
the trust boundary.
"""

from __future__ import annotations

import secrets
import string
import threading
import time
from dataclasses import dataclass

PAIRING_CODE_LENGTH = 16
PAIRING_CODE_GROUP = 3
PAIRING_CODE_ALPHABET = string.ascii_uppercase
MIN_PAIRING_TTL_SECONDS = 15
MAX_PAIRING_TTL_SECONDS = 600


class PairingCodeError(ValueError):
    """A human pairing code is malformed, unknown, or already consumed."""


class PairingCodeExpired(PairingCodeError):
    """A previously issued human pairing code has expired."""


def normalize_pairing_code(raw: str) -> str:
    """Return the canonical 16-letter code, accepting case and display separators."""

    if not isinstance(raw, str):
        raise PairingCodeError("pairing code must be text")

    letters: list[str] = []
    for character in raw:
        if character == "-" or character in " \t\r\n":
            continue
        if "a" <= character <= "z":
            letters.append(character.upper())
            continue
        if "A" <= character <= "Z":
            letters.append(character)
            continue
        raise PairingCodeError("pairing code must contain letters only")

    canonical = "".join(letters)
    if len(canonical) != PAIRING_CODE_LENGTH:
        raise PairingCodeError("pairing code must contain exactly 16 letters")
    return canonical


def render_pairing_code(raw: str) -> str:
    """Render a canonical code in three-letter groups.

    Sixteen letters intentionally render as ``ABC-DEF-GHI-JKL-MNO-P``.
    """

    canonical = normalize_pairing_code(raw)
    return "-".join(
        canonical[index : index + PAIRING_CODE_GROUP]
        for index in range(0, len(canonical), PAIRING_CODE_GROUP)
    )


def generate_pairing_code() -> str:
    """Generate a cryptographically random canonical pairing code."""

    return "".join(secrets.choice(PAIRING_CODE_ALPHABET) for _ in range(PAIRING_CODE_LENGTH))


@dataclass(frozen=True)
class PairingCodeLease:
    code: str
    pairing_uri: str
    expires_at: int


class PairingCodeRegistry:
    """Thread-safe, in-memory one-time alias registry for existing pairing URIs."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._entries: dict[str, PairingCodeLease] = {}

    def issue(
        self,
        pairing_uri: str,
        *,
        ttl_seconds: int = 120,
        now: int | None = None,
    ) -> PairingCodeLease:
        uri = str(pairing_uri).strip()
        if not uri.startswith("zara://pair/v1?") or len(uri.encode("utf-8")) > 4096:
            raise PairingCodeError("pairing code must alias a bounded zara://pair/v1 URI")
        ttl = int(ttl_seconds)
        if ttl < MIN_PAIRING_TTL_SECONDS or ttl > MAX_PAIRING_TTL_SECONDS:
            raise PairingCodeError("pairing code lifetime is outside the allowed range")
        current = int(time.time()) if now is None else int(now)
        expires_at = current + ttl

        with self._lock:
            self._prune_locked(current)
            for _ in range(32):
                code = generate_pairing_code()
                if code not in self._entries:
                    lease = PairingCodeLease(code=code, pairing_uri=uri, expires_at=expires_at)
                    self._entries[code] = lease
                    return lease
        raise PairingCodeError("could not allocate a unique pairing code")

    def claim(self, raw_code: str, *, now: int | None = None) -> str:
        """Atomically consume a code and return its existing pairing URI."""

        canonical = normalize_pairing_code(raw_code)
        current = int(time.time()) if now is None else int(now)
        with self._lock:
            lease = self._entries.pop(canonical, None)
        if lease is None:
            raise PairingCodeError("pairing code is invalid or already used")
        if lease.expires_at <= current:
            raise PairingCodeExpired("pairing code has expired")
        return lease.pairing_uri

    def revoke(self, raw_code: str) -> bool:
        canonical = normalize_pairing_code(raw_code)
        with self._lock:
            return self._entries.pop(canonical, None) is not None

    def prune_expired(self, *, now: int | None = None) -> int:
        current = int(time.time()) if now is None else int(now)
        with self._lock:
            before = len(self._entries)
            self._prune_locked(current)
            return before - len(self._entries)

    def _prune_locked(self, now: int) -> None:
        expired = [code for code, lease in self._entries.items() if lease.expires_at <= now]
        for code in expired:
            self._entries.pop(code, None)


__all__ = [
    "MAX_PAIRING_TTL_SECONDS",
    "MIN_PAIRING_TTL_SECONDS",
    "PAIRING_CODE_ALPHABET",
    "PAIRING_CODE_GROUP",
    "PAIRING_CODE_LENGTH",
    "PairingCodeError",
    "PairingCodeExpired",
    "PairingCodeLease",
    "PairingCodeRegistry",
    "generate_pairing_code",
    "normalize_pairing_code",
    "render_pairing_code",
]
