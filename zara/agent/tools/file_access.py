"""Per-chat file access policy: four modes with a global default.

The policy is the single authority for how file tools behave in a given
conversation. Trusted local surfaces change it through typed CAS mutations;
model output, tool results, plugin payloads and remote peers are data and
never authority to change the mode.

Modes, from narrowest to broadest:

- ``readonly``: reads, listing, search and diffs only; writes refuse.
- ``ask``: sandbox-constrained, plus every write blocks on the canonical
  approval gate before it executes.
- ``sandbox``: the ZARA-013 root confinement; writes run without prompts.
- ``full``: root confinement and symlink policy lifted; regular-file checks,
  UTF-8 decoding and size caps still apply.
"""

from __future__ import annotations

import json
import os
import tempfile
import threading
import time
from collections import deque
from contextlib import contextmanager
from contextvars import ContextVar
from enum import Enum
from pathlib import Path
from typing import Optional

FILE_ACCESS_MODE_NAMES = ("full", "sandbox", "readonly", "ask")
DEFAULT_MODE_NAME = "sandbox"
DEFAULT_GUEST_CEILING_NAME = "readonly"

_MAX_AUDIT = 128
_MAX_STORE_BYTES = 262144
_DEFAULT_MAX_OVERRIDES = 512
_STORE_VERSION = 1

_SCOPE: ContextVar[Optional[str]] = ContextVar(
    "zara_file_access_conversation",
    default=None,
)


class FileAccessMode(Enum):
    FULL = "full"
    SANDBOX = "sandbox"
    READONLY = "readonly"
    ASK = "ask"


# Monotone broadness used to clamp untrusted principals to their ceiling.
_BROADNESS = {
    FileAccessMode.READONLY: 0,
    FileAccessMode.ASK: 1,
    FileAccessMode.SANDBOX: 2,
    FileAccessMode.FULL: 3,
}


def parse_file_access_mode(value) -> FileAccessMode:
    if isinstance(value, FileAccessMode):
        return value
    if isinstance(value, str):
        candidate = value.strip().lower()
        for mode in FileAccessMode:
            if mode.value == candidate:
                return mode
    raise ValueError(
        "file access mode must be one of: " + ", ".join(FILE_ACCESS_MODE_NAMES)
    )


@contextmanager
def bind_file_access_scope(conversation_id: Optional[str]):
    """Bind the acting conversation so tools can resolve their mode."""

    token = _SCOPE.set(conversation_id)
    try:
        yield
    finally:
        _SCOPE.reset(token)


def current_file_access_conversation() -> Optional[str]:
    return _SCOPE.get()


class FileAccessConflict(RuntimeError):
    """A stale revision tried to clobber a newer mode override."""


class FileAccessPolicy:
    """Resolve and mutate per-chat file access modes with CAS safety."""

    def __init__(
        self,
        default_mode: str = DEFAULT_MODE_NAME,
        guest_ceiling: str = DEFAULT_GUEST_CEILING_NAME,
        store_path: Optional[Path] = None,
        max_overrides: int = _DEFAULT_MAX_OVERRIDES,
    ):
        self._mode = parse_file_access_mode(default_mode)
        self._guest_ceiling = parse_file_access_mode(guest_ceiling)
        if isinstance(max_overrides, bool) or not isinstance(max_overrides, int):
            raise ValueError("max_overrides must be an integer")
        if max_overrides < 1:
            raise ValueError("max_overrides must be a positive integer")
        self._max_overrides = max_overrides
        self._store_path = Path(store_path) if store_path is not None else None
        self._lock = threading.RLock()
        self._overrides: dict[str, dict] = {}
        self._audit: deque = deque(maxlen=_MAX_AUDIT)
        self._load_store()

    # ------------------------------------------------------------------
    # Resolution

    @property
    def default_mode(self) -> FileAccessMode:
        return self._mode

    @property
    def guest_ceiling(self) -> FileAccessMode:
        return self._guest_ceiling

    def effective_mode(
        self,
        conversation_id: Optional[str] = None,
        principal_kind: Optional[str] = None,
    ) -> FileAccessMode:
        with self._lock:
            override = (
                self._overrides.get(str(conversation_id))
                if conversation_id
                else None
            )
            mode = parse_file_access_mode(override["mode"]) if override else self._mode
        if principal_kind == "guest":
            ceiling = self._guest_ceiling
            if _BROADNESS[mode] > _BROADNESS[ceiling]:
                return ceiling
        return mode

    def scoped_mode(self) -> str:
        """Mode for the currently bound conversation (tool-call time)."""

        return self.effective_mode(current_file_access_conversation()).value

    def revision(self, conversation_id: str) -> int:
        with self._lock:
            override = self._overrides.get(str(conversation_id))
            return int(override["revision"]) if override else 0

    # ------------------------------------------------------------------
    # Mutations (typed, revisioned, audited)

    def set_mode(
        self,
        conversation_id: str,
        mode,
        *,
        expected_revision: int,
        actor: str,
    ) -> FileAccessMode:
        parsed = parse_file_access_mode(mode)
        key = str(conversation_id)
        with self._lock:
            current = self._overrides.get(key)
            current_revision = int(current["revision"]) if current else 0
            if expected_revision != current_revision:
                raise FileAccessConflict(
                    f"file access override for {key!r} moved to revision "
                    f"{current_revision}; refresh and retry"
                )
            old = parse_file_access_mode(current["mode"]) if current else self._mode
            self._overrides[key] = {
                "mode": parsed.value,
                "revision": current_revision + 1,
            }
            self._evict_bounded()
            self._record(actor=actor, target=key, old_mode=old, new_mode=parsed)
            self._save_store()
        return parsed

    def clear_mode(self, conversation_id: str, *, actor: str = "local") -> None:
        key = str(conversation_id)
        with self._lock:
            current = self._overrides.pop(key, None)
            if current is not None:
                self._record(
                    actor=actor,
                    target=key,
                    old_mode=parse_file_access_mode(current["mode"]),
                    new_mode=self._mode,
                )
                self._save_store()

    def set_default(self, mode) -> FileAccessMode:
        parsed = parse_file_access_mode(mode)
        with self._lock:
            old = self._mode
            self._mode = parsed
            self._record(
                actor="settings", target="default", old_mode=old, new_mode=parsed
            )
            self._save_store()
        return parsed

    def set_guest_ceiling(self, mode) -> FileAccessMode:
        parsed = parse_file_access_mode(mode)
        with self._lock:
            old = self._guest_ceiling
            self._guest_ceiling = parsed
            self._record(
                actor="settings",
                target="guest_ceiling",
                old_mode=old,
                new_mode=parsed,
            )
            self._save_store()
        return parsed

    def audit_log(self) -> tuple[dict, ...]:
        with self._lock:
            return tuple(dict(entry) for entry in self._audit)

    # ------------------------------------------------------------------
    # Internals

    def _record(
        self,
        *,
        actor: str,
        target: str,
        old_mode: FileAccessMode,
        new_mode: FileAccessMode,
    ) -> None:
        self._audit.append(
            {
                "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "actor": str(actor)[:128],
                "target": str(target)[:256],
                "old_mode": old_mode.value,
                "new_mode": new_mode.value,
            }
        )

    def _evict_bounded(self) -> None:
        if len(self._overrides) <= self._max_overrides:
            return
        ordered = sorted(self._overrides.items(), key=lambda item: item[1]["revision"])
        for key, _entry in ordered[: len(self._overrides) - self._max_overrides]:
            self._overrides.pop(key, None)

    def _load_store(self) -> None:
        if self._store_path is None:
            return
        try:
            raw = self._store_path.read_text(encoding="utf-8")
            if len(raw) > _MAX_STORE_BYTES:
                raise ValueError("file access store exceeds size limit")
            document = json.loads(raw)
            if not isinstance(document, dict):
                raise ValueError("file access store is not an object")
            if int(document.get("version", 0)) != _STORE_VERSION:
                raise ValueError("file access store version is unsupported")
            stored_default = document.get("default")
            if stored_default:
                self._mode = parse_file_access_mode(stored_default)
            overrides = document.get("overrides", {})
            if not isinstance(overrides, dict):
                raise ValueError("file access overrides are malformed")
            loaded: dict[str, dict] = {}
            for key, entry in overrides.items():
                if not isinstance(entry, dict):
                    raise ValueError("file access override entry is malformed")
                mode = parse_file_access_mode(entry.get("mode"))
                revision = entry.get("revision")
                if isinstance(revision, bool) or not isinstance(revision, int):
                    raise ValueError("file access override revision is malformed")
                loaded[str(key)[:256]] = {"mode": mode.value, "revision": revision}
            self._overrides = loaded
        except (OSError, ValueError, TypeError, json.JSONDecodeError):
            self._overrides = {}
            self._mode = parse_file_access_mode(DEFAULT_MODE_NAME)

    def _save_store(self) -> None:
        if self._store_path is None:
            return
        document = {
            "version": _STORE_VERSION,
            "default": self._mode.value,
            "overrides": self._overrides,
        }
        payload = json.dumps(document, sort_keys=True)
        if len(payload.encode("utf-8")) > _MAX_STORE_BYTES:
            return
        try:
            self._store_path.parent.mkdir(parents=True, exist_ok=True)
            descriptor, temporary = tempfile.mkstemp(
                prefix=".file-access-", dir=self._store_path.parent
            )
            try:
                with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
                    handle.write(payload)
                    handle.flush()
                    os.fsync(handle.fileno())
                os.replace(temporary, self._store_path)
            except OSError:
                try:
                    os.unlink(temporary)
                except OSError:
                    pass
                raise
        except OSError:
            return
