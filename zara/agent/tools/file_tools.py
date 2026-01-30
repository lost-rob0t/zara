"""
File tooling for LLM access.

Provides read/write/diff/list utilities for filesystem access.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field


DEFAULT_MAX_BYTES = 20000
DEFAULT_MAX_LINES = 200
DEFAULT_MAX_ENTRIES = 200


class FileReadArgs(BaseModel):
    path: str = Field(
        ...,
        description=(
            "Path to read. Relative paths are resolved against the tool base directory; "
            "absolute paths are allowed."
        ),
    )
    max_lines: int = Field(
        DEFAULT_MAX_LINES,
        description=(
            "Maximum number of lines to return. Use a smaller value to avoid large responses; "
            "output is truncated after this limit."
        ),
        ge=1,
        le=4000,
    )


class FileWriteArgs(BaseModel):
    path: str = Field(
        ...,
        description=(
            "Path to write. Relative paths are resolved against the tool base directory; "
            "absolute paths are allowed."
        ),
    )
    content: str = Field(
        ...,
        description=(
            "Full file content to write. If content exceeds the size cap, the tool refuses."
        ),
    )
    allow_overwrite: bool = Field(
        False,
        description=(
            "Allow overwriting existing files. Use true only when replacing the entire file."
        ),
    )


class FileDiffArgs(BaseModel):
    path: str = Field(
        ...,
        description=(
            "Path to existing file to diff against. Relative paths are resolved against the tool "
            "base directory; absolute paths are allowed."
        ),
    )
    content: str = Field(
        ...,
        description=(
            "Proposed full file content to diff against the current file. Provide the entire file "
            "content so the diff is accurate."
        ),
    )
    max_lines: int = Field(
        DEFAULT_MAX_LINES,
        description=(
            "Maximum number of diff lines to return. Use a smaller value to avoid large output; "
            "diff output is truncated after this limit."
        ),
        ge=1,
        le=8000,
    )


class FileListArgs(BaseModel):
    path: str = Field(
        ".",
        description=(
            "Directory path to list. Relative paths are resolved against the tool base directory; "
            "absolute paths are allowed."
        ),
    )
    max_entries: int = Field(
        DEFAULT_MAX_ENTRIES,
        description=(
            "Maximum number of entries to return. Use a smaller value to keep responses short."
        ),
        ge=1,
        le=2000,
    )
    include_hidden: bool = Field(
        False,
        description=(
            "Include dotfiles and dot-directories in the listing when true."
        ),
    )


@dataclass(frozen=True)
class FileToolingConfig:
    base_dir: Path
    max_bytes: int = DEFAULT_MAX_BYTES


class FileTooling:
    def __init__(self, config: FileToolingConfig):
        self.config = config

    def _resolve_path(self, path: str) -> Path:
        candidate = Path(path).expanduser()
        if not candidate.is_absolute():
            candidate = self.config.base_dir / candidate
        return candidate.resolve()

    def read_file(self, path: str, max_lines: int = DEFAULT_MAX_LINES) -> str:
        target = self._resolve_path(path)
        if not target.exists():
            return "Error: file does not exist"
        if target.is_dir():
            return "Error: path is a directory"
        if target.stat().st_size > self.config.max_bytes:
            return "Error: file too large to read"

        lines = target.read_text(encoding="utf-8").splitlines()
        limited = lines[:max_lines]
        return "\n".join(limited)

    def write_file(self, path: str, content: str, allow_overwrite: bool = False) -> str:
        target = self._resolve_path(path)
        if target.exists() and not allow_overwrite:
            return "Error: file exists (set allow_overwrite=true to replace)"
        if target.exists() and target.is_dir():
            return "Error: path is a directory"
        if len(content.encode("utf-8")) > self.config.max_bytes:
            return "Error: content too large to write"

        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content, encoding="utf-8")
        return f"Wrote {target}"

    def diff_file(self, path: str, content: str, max_lines: int = DEFAULT_MAX_LINES) -> str:
        target = self._resolve_path(path)
        if not target.exists():
            return "Error: file does not exist"
        if target.is_dir():
            return "Error: path is a directory"

        current = target.read_text(encoding="utf-8").splitlines()
        proposed = content.splitlines()
        diff_lines = _unified_diff(current, proposed, path)
        limited = diff_lines[:max_lines]
        return "\n".join(limited)

    def list_dir(
        self,
        path: str = ".",
        max_entries: int = DEFAULT_MAX_ENTRIES,
        include_hidden: bool = False,
    ) -> str:
        target = self._resolve_path(path)
        if not target.exists():
            return "Error: directory does not exist"
        if target.is_file():
            return "Error: path is a file"

        entries = []
        for entry in sorted(target.iterdir(), key=lambda item: item.name.lower()):
            if not include_hidden and entry.name.startswith("."):
                continue
            name = f"{entry.name}/" if entry.is_dir() else entry.name
            entries.append(name)
            if len(entries) >= max_entries:
                break

        return "\n".join(entries)


def _unified_diff(current: list[str], proposed: list[str], path: str) -> list[str]:
    import difflib

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
    max_bytes: Optional[int] = None,
) -> list[StructuredTool]:
    config = FileToolingConfig(
        base_dir=base_dir or Path.cwd(),
        max_bytes=max_bytes or DEFAULT_MAX_BYTES,
    )
    tooling = FileTooling(config)

    return [
        StructuredTool.from_function(
            func=tooling.read_file,
            name="read_file",
            description=(
                "Read a text file. Relative paths resolve against the tool base directory and "
                "absolute paths are allowed. The tool refuses directories, missing files, or files "
                "over the size cap. Use `max_lines` to limit output volume."
            ),
            args_schema=FileReadArgs,
        ),
        StructuredTool.from_function(
            func=tooling.write_file,
            name="write_file",
            description=(
                "Write a text file. Relative paths resolve against the tool base directory and "
                "absolute paths are allowed. Provide full `content`; if it exceeds the size cap the "
                "tool refuses. Existing files are protected unless `allow_overwrite` is true."
            ),
            args_schema=FileWriteArgs,
        ),
        StructuredTool.from_function(
            func=tooling.diff_file,
            name="diff_file",
            description=(
                "Generate a unified diff between an existing file and proposed content. Relative "
                "paths resolve against the tool base directory and absolute paths are allowed. "
                "Provide full `content` for accurate diffs and use `max_lines` to cap output."
            ),
            args_schema=FileDiffArgs,
        ),
        StructuredTool.from_function(
            func=tooling.list_dir,
            name="list_dir",
            description=(
                "List directory entries. Relative paths resolve against the tool base directory and "
                "absolute paths are allowed. Use `include_hidden` to show dotfiles and `max_entries` "
                "to cap output."
            ),
            args_schema=FileListArgs,
        ),
    ]
