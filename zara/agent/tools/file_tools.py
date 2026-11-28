"""Sandboxed file tooling for LLM access."""

from __future__ import annotations

import difflib
import fnmatch
import os
import re
import stat
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional, Sequence

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from .file_access import FileAccessMode, parse_file_access_mode


DEFAULT_MAX_BYTES = 20000
DEFAULT_MAX_LINES = 200
DEFAULT_MAX_ENTRIES = 200
SEARCH_FILE_CAP = 2000
POLICY_ERROR = "path rejected by file access policy"

FILE_WRITE_TOOL_NAMES = ("write_file", "edit_file")


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


class FileEditArgs(BaseModel):
    path: str = Field(
        ...,
        description="Existing file within a configured writable root.",
    )
    old_text: str = Field(..., description="Exact existing text to replace.")
    new_text: str = Field(..., description="Replacement text.")
    replace_all: bool = Field(
        False,
        description="Replace every occurrence instead of requiring a unique match.",
    )


class FileFindArgs(BaseModel):
    pattern: str = Field(
        ...,
        description="fnmatch glob matched against file names and relative paths.",
    )
    path: str = Field(".", description="Directory within a configured readable root.")
    max_entries: int = Field(DEFAULT_MAX_ENTRIES, ge=1, le=2000)
    include_hidden: bool = Field(False)


class FileSearchArgs(BaseModel):
    query: str = Field(..., description="Literal substring or regular expression.")
    path: str = Field(".", description="Directory within a configured readable root.")
    regex: bool = Field(False, description="Treat the query as a regular expression.")
    max_results: int = Field(50, ge=1, le=500)
    include_hidden: bool = Field(False)


@dataclass(frozen=True)
class FileToolingConfig:
    base_dir: Path
    readable_roots: tuple[Path, ...] = ()
    writable_roots: tuple[Path, ...] = ()
    max_bytes: int = DEFAULT_MAX_BYTES
    mode: str = "sandbox"
    mode_provider: Optional[Callable[[], str]] = None


class FilePolicyError(ValueError):
    pass


class FileTooling:
    def __init__(self, config: FileToolingConfig):
        if config.max_bytes < 1:
            raise ValueError("file tool max_bytes must be positive")

        self.max_bytes = config.max_bytes
        self._static_mode = parse_file_access_mode(config.mode)
        self._mode_provider = config.mode_provider
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

    def _mode(self) -> FileAccessMode:
        provider = self._mode_provider
        if provider is not None:
            return parse_file_access_mode(provider())
        return self._static_mode

    def _resolve_path(self, path: str, roots: tuple[Path, ...]) -> Path:
        if not isinstance(path, str) or not path or "\x00" in path:
            raise FilePolicyError(POLICY_ERROR)

        candidate = Path(path).expanduser()
        if not candidate.is_absolute():
            candidate = self.base_dir / candidate
        candidate = Path(os.path.abspath(candidate))

        if self._mode() is FileAccessMode.FULL:
            return candidate

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
        if self._mode() is FileAccessMode.READONLY:
            return "Error: file access is read-only for this conversation"
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

    def edit_file(
        self,
        path: str,
        old_text: str,
        new_text: str,
        *,
        replace_all: bool = False,
    ) -> str:
        if self._mode() is FileAccessMode.READONLY:
            return "Error: file access is read-only for this conversation"
        if not isinstance(old_text, str) or not old_text:
            return "Error: old_text must be a non-empty string"
        if not isinstance(new_text, str):
            return "Error: new_text must be a string"
        try:
            target = self._resolve_path(path, self.writable_roots)
            current = self._read_text(target)
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

        occurrences = current.count(old_text)
        if occurrences == 0:
            return f"Error: text not found in {self._display_path(target)}"
        if occurrences > 1 and not replace_all:
            return (
                f"Error: text matches {occurrences} places; "
                "replace_all is required to change them all"
            )
        updated = (
            current.replace(old_text, new_text)
            if replace_all
            else current.replace(old_text, new_text, 1)
        )
        if len(updated.encode("utf-8")) > self.max_bytes:
            return "Error: content exceeds the configured size limit"

        temporary: Optional[Path] = None
        try:
            target.parent.mkdir(parents=True, exist_ok=True)
            descriptor, temporary_name = tempfile.mkstemp(
                prefix=".zara-edit-", dir=target.parent
            )
            temporary = Path(temporary_name)
            with os.fdopen(descriptor, "wb") as file_handle:
                file_handle.write(updated.encode("utf-8"))
                file_handle.flush()
                os.fsync(file_handle.fileno())
            os.replace(temporary, target)
            temporary = None
            return f"Edited {self._display_path(target)}"
        except OSError:
            return "Error: atomic edit failed"
        finally:
            if temporary is not None:
                try:
                    temporary.unlink()
                except FileNotFoundError:
                    pass

    def _iter_regular_files(self, start: Path, include_hidden: bool, cap: int):
        stack = [start]
        visited = 0
        while stack and visited < cap:
            current = stack.pop()
            try:
                entries = sorted(current.iterdir(), key=lambda item: item.name)
            except OSError:
                continue
            for entry in entries:
                visited += 1
                if visited > cap:
                    return
                if not include_hidden and entry.name.startswith("."):
                    continue
                try:
                    entry_mode = entry.lstat().st_mode
                except OSError:
                    continue
                if stat.S_ISLNK(entry_mode):
                    continue
                if stat.S_ISDIR(entry_mode):
                    stack.append(entry)
                elif stat.S_ISREG(entry_mode):
                    yield entry

    def find_files(
        self,
        pattern: str,
        path: str = ".",
        max_entries: int = DEFAULT_MAX_ENTRIES,
        include_hidden: bool = False,
    ) -> str:
        if not isinstance(pattern, str) or not pattern or "\x00" in pattern:
            return "Error: pattern must be a non-empty string"
        try:
            start = self._resolve_path(path, self.readable_roots)
            if not stat.S_ISDIR(self._file_mode(start)):
                raise FilePolicyError("path is not a directory")
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

        found = []
        for entry in self._iter_regular_files(start, include_hidden, SEARCH_FILE_CAP):
            display = self._display_path(entry)
            if fnmatch.fnmatch(entry.name, pattern) or fnmatch.fnmatch(display, pattern):
                found.append(display)
                if len(found) >= max_entries:
                    break
        return "\n".join(found)

    def search_text(
        self,
        query: str,
        path: str = ".",
        *,
        regex: bool = False,
        max_results: int = 50,
        include_hidden: bool = False,
    ) -> str:
        if not isinstance(query, str) or not query or "\x00" in query:
            return "Error: query must be a non-empty string"
        matcher = None
        if regex:
            try:
                matcher = re.compile(query)
            except re.error as error:
                return f"Error: invalid regular expression: {error}"
        try:
            start = self._resolve_path(path, self.readable_roots)
            if not stat.S_ISDIR(self._file_mode(start)):
                raise FilePolicyError("path is not a directory")
        except (FilePolicyError, OSError) as error:
            return f"Error: {error if isinstance(error, FilePolicyError) else POLICY_ERROR}"

        results = []
        for entry in self._iter_regular_files(start, include_hidden, SEARCH_FILE_CAP):
            try:
                if entry.lstat().st_size > self.max_bytes:
                    continue
                text = entry.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            display = self._display_path(entry)
            for lineno, line in enumerate(text.splitlines(), start=1):
                if matcher is not None:
                    hit = matcher.search(line)
                else:
                    hit = query in line
                if hit:
                    results.append(f"{display}:{lineno}: {line.strip()[:200]}")
                    if len(results) >= max_results:
                        return "\n".join(results)
        return "\n".join(results)

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
        root = next((root for root in roots if self._is_within(path, root)), None)
        if root is None:
            return str(path)
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
    mode: Optional[str] = None,
    mode_provider: Optional[Callable[[], str]] = None,
) -> list[StructuredTool]:
    base = base_dir or Path.cwd()
    tooling = FileTooling(
        FileToolingConfig(
            base_dir=base,
            readable_roots=tuple(readable_roots or (base,)),
            writable_roots=tuple(writable_roots or (base,)),
            max_bytes=max_bytes if max_bytes is not None else DEFAULT_MAX_BYTES,
            mode=mode if mode is not None else "sandbox",
            mode_provider=mode_provider,
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
            func=tooling.edit_file,
            name="edit_file",
            description=(
                "Replace exact text inside an existing file. The match must be "
                "unique unless replace_all is true; the write is atomic."
            ),
            args_schema=FileEditArgs,
        ),
        StructuredTool.from_function(
            func=tooling.find_files,
            name="find_files",
            description="Find files by fnmatch glob within configured readable roots.",
            args_schema=FileFindArgs,
        ),
        StructuredTool.from_function(
            func=tooling.search_text,
            name="search_text",
            description="Search file contents for a literal query or regex.",
            args_schema=FileSearchArgs,
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
