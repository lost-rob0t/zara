"""Durable, repository-local claims for host effect callbacks."""

from __future__ import annotations

import fcntl
import hashlib
import json
import os
import secrets
import stat
import subprocess
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterator


MAX_RESULT_BYTES = 8192
MAX_ARGS_BYTES = 8192
_DIR_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
_FILE_FLAGS = os.O_NOFOLLOW | os.O_CLOEXEC


@dataclass(frozen=True)
class EffectClaim:
    status: str
    principal_id: str
    attempt_id: str
    effect_fingerprint: str
    name: str
    args_digest: str
    claim_token: str | None = None
    result: str | None = None


class RepositoryEffectJournal:
    """Claim an effect before execution; uncertain outcomes never auto-retry."""

    def __init__(self, repo_root: Path) -> None:
        root = Path(repo_root)
        if root.is_symlink() or not root.is_dir() or root.resolve() != root:
            raise ValueError("effect journal requires an exact Git worktree root without symlinks")
        result = subprocess.run(
            ["git", "-C", str(root), "rev-parse", "--show-toplevel"],
            capture_output=True, text=True, check=False,
        )
        if result.returncode or Path(result.stdout.strip()) != root:
            raise ValueError("effect journal requires an exact Git worktree root")
        self._root = root
        with self._open_directory(root) as root_fd:
            with self._child_directory(root_fd, ".zara") as zara_fd:
                self._ensure_ignore(zara_fd)
                with self._child_directory(zara_fd, "effects") as effects_fd:
                    self._ensure_lock(effects_fd)

    @classmethod
    def from_alias(cls, prolog_engine, alias: str) -> RepositoryEffectJournal:
        from zara.agent.tools.file_tools import resolve_writable_repo

        return cls(resolve_writable_repo(prolog_engine, alias))

    def claim(
        self, principal_id: str, attempt_id: str, effect_fingerprint: str,
        name: str, args: dict[str, Any],
    ) -> EffectClaim:
        self._bounded_text("principal_id", principal_id, 256)
        self._bounded_text("attempt_id", attempt_id, 256)
        self._bounded_text("effect_fingerprint", effect_fingerprint, 256)
        self._bounded_text("name", name, 128)
        if not isinstance(args, dict):
            raise ValueError("effect args must be an object")
        try:
            encoded_args = json.dumps(args, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
        except (TypeError, ValueError) as error:
            raise ValueError("effect args must be JSON serializable") from error
        if len(encoded_args) > MAX_ARGS_BYTES:
            raise ValueError("effect args exceed size limit")
        identity = {
            "principal_id": principal_id,
            "attempt_id": attempt_id,
            "effect_fingerprint": effect_fingerprint,
            "name": name,
            "args_digest": hashlib.sha256(encoded_args).hexdigest(),
        }
        path = self._attempt_name(attempt_id)
        with self._locked_effects() as directory:
            try:
                existing = self._read(directory, path)
            except FileNotFoundError:
                token = secrets.token_hex(32)
                record = {**identity, "status": "in_progress", "claim_token": token}
                try:
                    self._create(directory, path, record)
                except FileExistsError:
                    existing = self._read(directory, path)
                else:
                    return EffectClaim("claimed", **identity, claim_token=token)
            if existing is None:
                return EffectClaim("indeterminate", **identity)
            if any(existing.get(key) != value for key, value in identity.items()):
                return EffectClaim("denied", **identity)
            if existing.get("status") == "completed" and isinstance(existing.get("result"), str):
                return EffectClaim("replay", **identity, result=existing["result"])
            return EffectClaim("indeterminate", **identity)

    def complete(self, claim: EffectClaim, result: str) -> None:
        if claim.status != "claimed" or not claim.claim_token:
            raise PermissionError("effect completion requires the original claim")
        if not isinstance(result, str) or len(result.encode("utf-8")) > MAX_RESULT_BYTES:
            raise ValueError("effect result exceeds size limit")
        identity = {
            "principal_id": claim.principal_id,
            "attempt_id": claim.attempt_id,
            "effect_fingerprint": claim.effect_fingerprint,
            "name": claim.name,
            "args_digest": claim.args_digest,
        }
        path = self._attempt_name(claim.attempt_id)
        with self._locked_effects() as directory:
            existing = self._read(directory, path)
            if existing is None or existing.get("status") != "in_progress" or existing.get("claim_token") != claim.claim_token or any(
                existing.get(key) != value for key, value in identity.items()
            ):
                raise PermissionError("effect claim is stale or mismatched")
            replacement = {**identity, "status": "completed", "result": result}
            temporary = f".complete-{secrets.token_hex(16)}"
            try:
                self._create(directory, temporary, replacement)
                os.replace(temporary, path, src_dir_fd=directory, dst_dir_fd=directory)
                os.fsync(directory)
            finally:
                try:
                    os.unlink(temporary, dir_fd=directory)
                except FileNotFoundError:
                    pass

    @staticmethod
    def _bounded_text(label: str, value: str, limit: int) -> None:
        if not isinstance(value, str) or not value or len(value.encode("utf-8")) > limit:
            raise ValueError(f"{label} must be bounded non-empty text")

    @staticmethod
    def _attempt_name(attempt_id: str) -> str:
        return hashlib.sha256(attempt_id.encode("utf-8")).hexdigest() + ".json"

    @staticmethod
    @contextmanager
    def _open_directory(path: Path) -> Iterator[int]:
        descriptor = os.open(path, _DIR_FLAGS)
        try:
            yield descriptor
        finally:
            os.close(descriptor)

    @staticmethod
    @contextmanager
    def _child_directory(parent: int, name: str) -> Iterator[int]:
        try:
            os.mkdir(name, mode=0o700, dir_fd=parent)
        except FileExistsError:
            pass
        try:
            descriptor = os.open(name, _DIR_FLAGS, dir_fd=parent)
        except OSError as error:
            raise ValueError("effect journal directory is a symlink or invalid") from error
        try:
            if os.fstat(descriptor).st_mode & 0o777 != 0o700:
                raise ValueError("effect journal directory must be private")
            yield descriptor
        finally:
            os.close(descriptor)

    @staticmethod
    def _ensure_ignore(zara_fd: int) -> None:
        name = ".gitignore"
        try:
            descriptor = os.open(name, os.O_RDWR | _FILE_FLAGS, dir_fd=zara_fd)
        except FileNotFoundError:
            descriptor = os.open(name, os.O_RDWR | os.O_CREAT | os.O_EXCL | _FILE_FLAGS, 0o600, dir_fd=zara_fd)
        with os.fdopen(descriptor, "r+", encoding="utf-8") as handle:
            if not stat.S_ISREG(os.fstat(handle.fileno()).st_mode):
                raise ValueError("effect journal ignore file is invalid")
            content = handle.read()
            missing = [rule for rule in ("/effects/", "/.gitignore") if rule not in content.splitlines()]
            if missing:
                handle.seek(0, os.SEEK_END)
                if content and not content.endswith("\n"):
                    handle.write("\n")
                handle.write("\n".join(missing) + "\n")
                handle.flush()
                os.fsync(handle.fileno())

    @staticmethod
    def _ensure_lock(directory: int) -> None:
        descriptor = os.open(".lock", os.O_RDWR | os.O_CREAT | _FILE_FLAGS, 0o600, dir_fd=directory)
        try:
            info = os.fstat(descriptor)
            if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1 or info.st_mode & 0o777 != 0o600:
                raise ValueError("effect journal lock file is invalid")
        finally:
            os.close(descriptor)

    @contextmanager
    def _locked_effects(self) -> Iterator[int]:
        with self._open_directory(self._root) as root_fd:
            with self._child_directory(root_fd, ".zara") as zara_fd:
                with self._child_directory(zara_fd, "effects") as effects_fd:
                    self._ensure_lock(effects_fd)
                    lock_fd = os.open(".lock", os.O_RDWR | _FILE_FLAGS, dir_fd=effects_fd)
                    try:
                        fcntl.flock(lock_fd, fcntl.LOCK_EX)
                        yield effects_fd
                    finally:
                        fcntl.flock(lock_fd, fcntl.LOCK_UN)
                        os.close(lock_fd)

    @staticmethod
    def _read(directory: int, name: str) -> dict[str, Any] | None:
        try:
            descriptor = os.open(name, os.O_RDONLY | _FILE_FLAGS, dir_fd=directory)
        except FileNotFoundError:
            raise
        try:
            info = os.fstat(descriptor)
            if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1 or info.st_mode & 0o777 != 0o600 or info.st_size > 16384:
                return None
            try:
                value = json.loads(os.read(descriptor, 16385))
            except (ValueError, UnicodeDecodeError):
                return None
            return value if isinstance(value, dict) else None
        finally:
            os.close(descriptor)

    @staticmethod
    def _create(directory: int, name: str, record: dict[str, Any]) -> None:
        descriptor = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | _FILE_FLAGS, 0o600, dir_fd=directory)
        try:
            encoded = json.dumps(record, sort_keys=True, separators=(",", ":")).encode()
            with os.fdopen(descriptor, "wb") as handle:
                handle.write(encoded)
                handle.flush()
                os.fsync(handle.fileno())
        finally:
            os.fsync(directory)
