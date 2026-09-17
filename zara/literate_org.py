"""Literate Org configuration tangling for trusted Zara configuration.

The Org file is canonical. Source blocks are parsed as inert text and only
explicit ``:tangle`` targets are written. Python and Prolog are the first-class
configuration languages because those are Zara's executable extension layers.
"""

from __future__ import annotations

import re
import shlex
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

_SUPPORTED = frozenset({"python", "prolog"})
_PROPERTY_RE = re.compile(
    r"^#\+property:\s+header-args(?::([A-Za-z0-9_+.-]+))?\s+(.*)$",
    re.IGNORECASE,
)
_BEGIN_RE = re.compile(r"^#\+begin_src\s+(\S+)(.*)$", re.IGNORECASE)
_END_RE = re.compile(r"^#\+end_src\s*$", re.IGNORECASE)


class LiterateOrgConfigError(ValueError):
    pass


@dataclass(frozen=True)
class TangledConfig:
    path: str
    language: str
    content: str


def _headers(raw: str) -> dict[str, str]:
    try:
        tokens = shlex.split(raw, comments=False, posix=True)
    except ValueError as exc:
        raise LiterateOrgConfigError(f"invalid Org header args: {exc}") from exc
    result: dict[str, str] = {}
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if not token.startswith(":"):
            index += 1
            continue
        key = token[1:].strip().casefold()
        value = "yes"
        if index + 1 < len(tokens) and not tokens[index + 1].startswith(":"):
            value = tokens[index + 1]
            index += 1
        if key:
            result[key] = value
        index += 1
    return result


def _safe_target(raw: str) -> str:
    target = raw.strip().replace("\\", "/")
    while target.startswith("./"):
        target = target[2:]
    path = Path(target)
    if not target or path.is_absolute() or any(part == ".." for part in path.parts):
        raise LiterateOrgConfigError(f"unsafe :tangle target: {raw!r}")
    return path.as_posix()


def tangle_text(text: str, *, source_name: str = "config.org") -> tuple[TangledConfig, ...]:
    global_headers: dict[str, str] = {}
    language_headers: dict[str, dict[str, str]] = {}
    lines = text.splitlines()

    for line in lines:
        match = _PROPERTY_RE.match(line.strip())
        if not match:
            continue
        language = (match.group(1) or "").casefold()
        parsed = _headers(match.group(2))
        if language:
            language_headers.setdefault(language, {}).update(parsed)
        else:
            global_headers.update(parsed)

    buckets: dict[tuple[str, str], list[str]] = {}
    index = 0
    while index < len(lines):
        match = _BEGIN_RE.match(lines[index])
        if not match:
            index += 1
            continue
        language = match.group(1).casefold()
        local_headers = _headers(match.group(2))
        body: list[str] = []
        index += 1
        while index < len(lines) and not _END_RE.match(lines[index]):
            body.append(lines[index])
            index += 1
        if index >= len(lines):
            raise LiterateOrgConfigError("unterminated #+begin_src block")
        index += 1

        if language not in _SUPPORTED:
            continue
        effective = dict(global_headers)
        effective.update(language_headers.get(language, {}))
        effective.update(local_headers)
        tangle = effective.get("tangle")
        if not tangle or tangle.casefold() == "no":
            continue
        if tangle.casefold() == "yes":
            stem = Path(source_name).stem
            suffix = "py" if language == "python" else "pl"
            target = f"{stem}.{suffix}"
        else:
            target = _safe_target(tangle)
        buckets.setdefault((target, language), []).append("\n".join(body))

    return tuple(
        TangledConfig(path=path, language=language, content="\n\n".join(parts) + "\n")
        for (path, language), parts in buckets.items()
    )


def tangle_file(org_file: Path | str, *, output_root: Path | str | None = None) -> tuple[Path, ...]:
    source = Path(org_file).expanduser().resolve()
    root = Path(output_root).expanduser().resolve() if output_root is not None else source.parent
    root.mkdir(parents=True, exist_ok=True)
    written: list[Path] = []
    for output in tangle_text(source.read_text(encoding="utf-8"), source_name=source.name):
        destination = (root / output.path).resolve()
        try:
            destination.relative_to(root)
        except ValueError as exc:
            raise LiterateOrgConfigError(f"tangle escaped output root: {output.path}") from exc
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(output.content, encoding="utf-8")
        written.append(destination)
    return tuple(written)


def supported_languages() -> Iterable[str]:
    return tuple(sorted(_SUPPORTED))
