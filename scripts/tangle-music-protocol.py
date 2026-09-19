#!/usr/bin/env python3
"""Tangle the allowlisted Zara Music protocol blocks from canonical Org."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "docs" / "music-protocol.org"
ALLOWED_TARGETS = {
    "kb/music_protocol.pl",
    "schemas/zara-music-v1.json",
}
BEGIN = re.compile(
    r"^#\+begin_src\s+\S+(?:\s+.*)?\s+:tangle\s+([^\s]+)\s*$",
    re.IGNORECASE,
)
END = re.compile(r"^#\+end_src\s*$", re.IGNORECASE)


class TangleError(ValueError):
    pass


def extract(text: str) -> dict[str, str]:
    outputs: dict[str, str] = {}
    target: str | None = None
    body: list[str] = []

    for line_number, line in enumerate(text.splitlines(), start=1):
        begin = BEGIN.match(line)
        if begin:
            if target is not None:
                raise TangleError(f"nested source block at line {line_number}")
            candidate = begin.group(1)
            if candidate not in ALLOWED_TARGETS:
                raise TangleError(f"unapproved tangle target: {candidate}")
            if candidate in outputs:
                raise TangleError(f"duplicate tangle target: {candidate}")
            target = candidate
            body = []
            continue

        if END.match(line):
            if target is None:
                continue
            outputs[target] = "\n".join(body) + "\n"
            target = None
            body = []
            continue

        if target is not None:
            body.append(line)

    if target is not None:
        raise TangleError(f"unterminated source block for {target}")
    missing = ALLOWED_TARGETS - set(outputs)
    if missing:
        raise TangleError(f"missing tangle targets: {sorted(missing)!r}")
    return outputs


def check(outputs: dict[str, str]) -> list[str]:
    stale: list[str] = []
    for relative, expected in sorted(outputs.items()):
        path = ROOT / relative
        if not path.is_file() or path.read_text(encoding="utf-8") != expected:
            stale.append(relative)
    return stale


def write(outputs: dict[str, str]) -> None:
    for relative, content in sorted(outputs.items()):
        path = ROOT / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail when generated files differ from docs/music-protocol.org",
    )
    args = parser.parse_args()

    outputs = extract(SOURCE.read_text(encoding="utf-8"))
    if args.check:
        stale = check(outputs)
        if stale:
            parser.error("stale generated music protocol files: " + ", ".join(stale))
        return 0

    write(outputs)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
