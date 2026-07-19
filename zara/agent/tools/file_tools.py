"""Sandboxed file tooling for LLM access."""

from __future__ import annotations

import difflib
import os
import stat
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Sequence

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field


DEFAULT_MAX_BYTES = 20000
DEFAULT_MAX_LINES = 200
DEFAULT_MAX_ENTRIES = 200
POLICY_ERROR = "path rejected by file access policy"


class FileReadArgs(BaseModel):
    path: str = Field(
        ...,
        description="File within a configured readable root.",
    )
    max_lines: int = Field(DEFAULT_MAX_LINES, ge=1, le=4000)


class FileWriteArgs(BaseModel):
    path: str = Field(
        ...,
        description="File within a configured writable root.",
    )
    content: str = Field(..., description="Complete UTF-8 file content.")
    allow_overwrite: bool = Field(
        False,
        description="Replace an existing regular file atomically when true.",
    )


class FileDiffArgs(BaseModel):
    path: str = Field(
        ...,
        description="Existing file within a configured readable root.",
    )
    content: str = Field(..., description="Complete proposed UTF-8 content.")
    max_lines: int = Field(DEFAULT_MAX_LINES, ge=1, le=8000)


class FileListArgs(BaseModel):
    path: str = Field(".", description="Directory within a configured readable root.")
    max_entries: int = Field(DEFAULT_MAX_ENTRIES, ge=1, le=2000)
    include_hidden: bool = Field(False)


@dataclass(frozen=True)
class FileToolingConfig:
    base_dir: Path
    readable_roots: tuple[Path, ...] = ()
    writable_roots: tuple[Path, ...] = ()
    max_bytes: int = DEFAULT_MAX_BYTES


class FilePolicyError(ValueError):
    pass


class FileTooling:
    def __init__(self, config: FileToolingConfig):
        if config.max_bytes < 1:
            raise ValueError("file tool max_bytes must be positive")

        self.max_bytes = config.max_bytes
        self.base_dir = self._normalize_root(config.base_dir)
        readable = config.readable_roots or (self.base_dir,)
        writable = config.writable_roots or (self.base_dir,)
        self.readable_roots = tuple(self._normalize_root(root) for root in readable)
        self.writable_roots = tuple(self._normalize_root(root) for root in writable)

    @staticmethod
    def _normalize_root(root: Path) -> Path:
        expanded = Path(root).expanduser()
        if expanded.is_symlink():
            raise ValueError("file tool roots cannot be symlinks")
        try:
            resolved = expanded.resolve(strict=True)
        except OSError as error:
            raise ValueError("file tool root must be an existing directory") from error
        if not resolved.is_dir():
            raise ValueError("file tool root must be an existing directory")
        return resolved

    @staticmethod
    def _is_within(path: Path, root: Path) -> bool:
        return path == root or root in path.parents

    def _resolve_path(self, path: str, roots: tuple[Path, ...]) -> Path:
        if not isinstance(path, str) or not path or "\x00" in path:
            raise FilePolicyError(POLICY_ERROR)

        candidate = Path(path).expanduser()
        if not candidate.is_absolute():
            candidate = self.base_dir / candidate
        candidate = Path(os.path.abspath(candidate))

        root = next((root for root in roots if self._is_within(candidate, root)), None)
        if root is None:
            raise FilePolicyError(POLICY_ERROR)

        current = root
        for part in candidate.relative_to(root).parts:
            current /= part
            try:
                mode = current.lstat().st_mode
            except FileNotFoundError:
                continue
            except OSError as error:
                raise FilePolicyError(POLICY_ERROR) from error
            if stat.S_ISLNK(mode):
                raise FilePolicyError("symlinks are not allowed")

        resolved = candidate.resolve(strict=False)
        if not self._is_within(resolved, root):
            raise FilePolicyError(POLICY_ERROR)
        return resolved

    @staticmethod
    def _file_mode(path: Path) -> int:
        try:
            return path.lstat().st_mode
        except FileNotFoundError as error:
            raise FilePolicyError("file does not exist") from error
        except OSError as error:
            raise FilePolicyError(POLICY_ERROR) from error

    def _read_text(self, target: Path) -> str:
        mode = self._file_mode(target)
        if not stat.S_ISREG(mode):
            raise FilePolicyError("path is not a regular file")

        descriptor = os.open(target, os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0))
        with os.fdopen(descriptor, "rb") as file_handle:
            file_stat = os.fstat(file_handle.fileno())
            if not stat.S_ISREG(file_stat.st_mode):
                raise FilePolicyError("path is not a regular file")
            if file_stat.st_size > self.max_bytes:
                raise FilePolicyError("file exceeds the configured size limit")
            data = file_handle.read(self.max_bytes + 1)
        if len(data) > self.max_bytes:
            raise FilePolicyError("file exceeds the configured size limit")
        try:
            return data.decode("utf-8")
        except UnicodeDecodeError as error:
            raise FilePolicyError("file is not valid UTF-8 text") from error

    def read_file(self, path: str, max_lines: int = DEFAULT_MAX_LINES) -> str:
        try:
            target = self._resolve_path(path, self.readable_roots)
            return "\n".join(self._read_text(target).splitlines()[:max_lines])
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

    def write_file(self, path: str, content: str, allow_overwrite: bool = False) -> str:
        encoded = content.encode("utf-8")
        if len(encoded) > self.max_bytes:
            return "Error: content exceeds the configured size limit"

        temporary: Optional[Path] = None
        try:
            target = self._resolve_path(path, self.writable_roots)
            target.parent.mkdir(parents=True, exist_ok=True)
            parent = self._resolve_path(str(target.parent), self.writable_roots)
            if not stat.S_ISDIR(self._file_mode(parent)):
                raise FilePolicyError("parent is not a directory")

            try:
                target_mode = target.lstat().st_mode
            except FileNotFoundError:
                target_mode = None
            if target_mode is not None:
                if not stat.S_ISREG(target_mode):
                    raise FilePolicyError("path is not a regular file")
                if not allow_overwrite:
                    raise FilePolicyError("file exists; allow_overwrite is required")

            descriptor, temporary_name = tempfile.mkstemp(prefix=".zara-write-", dir=parent)
            temporary = Path(temporary_name)
            with os.fdopen(descriptor, "wb") as file_handle:
                file_handle.write(encoded)
                file_handle.flush()
                os.fsync(file_handle.fileno())

            if allow_overwrite:
                os.replace(temporary, target)
            else:
                os.link(temporary, target, follow_symlinks=False)
                temporary.unlink()
            temporary = None
            return f"Wrote {self._display_path(target)}"
        except FileExistsError:
            return "Error: file exists; allow_overwrite is required"
        except FilePolicyError as error:
            return f"Error: {error}"
        except OSError:
            return "Error: atomic write failed"
        finally:
            if temporary is not None:
                try:
                    temporary.unlink()
                except FileNotFoundError:
                    pass

    def diff_file(self, path: str, content: str, max_lines: int = DEFAULT_MAX_LINES) -> str:
        if len(content.encode("utf-8")) > self.max_bytes:
            return "Error: content exceeds the configured size limit"
        try:
            target = self._resolve_path(path, self.readable_roots)
            current = self._read_text(target).splitlines()
            proposed = content.splitlines()
            return "\n".join(
                _unified_diff(current, proposed, self._display_path(target))[:max_lines]
            )
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

    def list_dir(
        self,
        path: str = ".",
        max_entries: int = DEFAULT_MAX_ENTRIES,
        include_hidden: bool = False,
    ) -> str:
        try:
            target = self._resolve_path(path, self.readable_roots)
            if not stat.S_ISDIR(self._file_mode(target)):
                raise FilePolicyError("path is not a directory")

            entries = []
            for entry in sorted(target.iterdir(), key=lambda item: item.name.casefold()):
                if not include_hidden and entry.name.startswith("."):
                    continue
                mode = entry.lstat().st_mode
                if stat.S_ISLNK(mode) or not (stat.S_ISREG(mode) or stat.S_ISDIR(mode)):
                    continue
                entries.append(f"{entry.name}/" if stat.S_ISDIR(mode) else entry.name)
                if len(entries) >= max_entries:
                    break
            return "\n".join(entries)
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

    def _display_path(self, path: Path) -> str:
        roots = self.readable_roots + self.writable_roots
        root = next(root for root in roots if self._is_within(path, root))
        relative = path.relative_to(root)
        return "." if not relative.parts else relative.as_posix()


def _unified_diff(current: list[str], proposed: list[str], path: str) -> list[str]:
    return list(
        difflib.unified_diff(
            current,
            proposed,
            fromfile=f"a/{path}",
            tofile=f"b/{path}",
            lineterm="",
        )
    )


def build_file_tools(
    base_dir: Optional[Path] = None,
    readable_roots: Optional[Sequence[Path]] = None,
    writable_roots: Optional[Sequence[Path]] = None,
    max_bytes: Optional[int] = None,
) -> list[StructuredTool]:
    base = base_dir or Path.cwd()
    tooling = FileTooling(
        FileToolingConfig(
            base_dir=base,
            readable_roots=tuple(readable_roots or (base,)),
            writable_roots=tuple(writable_roots or (base,)),
            max_bytes=max_bytes if max_bytes is not None else DEFAULT_MAX_BYTES,
        )
    )

    return [
        StructuredTool.from_function(
            func=tooling.read_file,
            name="read_file",
            description="Read UTF-8 text within configured readable roots.",
            args_schema=FileReadArgs,
        ),
        StructuredTool.from_function(
            func=tooling.write_file,
            name="write_file",
            description="Atomically write UTF-8 text within configured writable roots.",
            args_schema=FileWriteArgs,
        ),
        StructuredTool.from_function(
            func=tooling.diff_file,
            name="diff_file",
            description="Diff UTF-8 text within configured readable roots.",
            args_schema=FileDiffArgs,
        ),
        StructuredTool.from_function(
            func=tooling.list_dir,
            name="list_dir",
            description="List regular files and directories within configured readable roots.",
            args_schema=FileListArgs,
        ),
    ]
