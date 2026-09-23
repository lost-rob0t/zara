#!/usr/bin/env python3
"""Validate and materialize canonical notes for a versioned Zara release."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path
from typing import Sequence


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from zara.version_context import load_version_context


CHANGELOG = ROOT / "CHANGELOG.md"
VERSION_FILE = ROOT / "version.properties"
BULLET_RE = re.compile(r"(?m)^-\s+\S")


class ReleaseChangelogError(ValueError):
    pass


def extract_version_notes(markdown: str, version: str) -> str:
    heading = f"## {version}"
    lines = markdown.splitlines()
    starts = [index for index, line in enumerate(lines) if line.strip() == heading]
    if len(starts) != 1:
        raise ReleaseChangelogError(
            f"CHANGELOG.md must contain exactly one {heading!r} section"
        )

    start = starts[0] + 1
    end = next(
        (index for index in range(start, len(lines)) if lines[index].startswith("## ")),
        len(lines),
    )
    notes = "\n".join(lines[start:end]).strip()
    if not notes:
        raise ReleaseChangelogError(f"{heading} has no release notes")
    if BULLET_RE.search(notes) is None:
        raise ReleaseChangelogError(f"{heading} must contain at least one changelog entry")
    return notes + "\n"


def _git(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=False,
    )


def _path_changed(base: str, path: Path) -> bool:
    if not base or set(base) == {"0"}:
        return True
    if _git("rev-parse", "--verify", f"{base}^{{commit}}").returncode != 0:
        return True
    return _git(
        "diff",
        "--quiet",
        base,
        "HEAD",
        "--",
        str(path.relative_to(ROOT)),
    ).returncode != 0


def changelog_changed(base: str) -> bool:
    return _path_changed(base, CHANGELOG)


def version_context_changed(base: str) -> bool:
    return _path_changed(base, VERSION_FILE)


def existing_version_tag(version: str) -> str | None:
    result = _git(
        "rev-parse",
        "--verify",
        "--quiet",
        f"refs/tags/v{version}^{{commit}}",
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def validate(
    *,
    version: str | None = None,
    base: str | None = None,
    changelog: Path = CHANGELOG,
    output: Path | None = None,
) -> str | None:
    context = load_version_context(ROOT / "version.properties")

    if base is not None:
        if not changelog_changed(base):
            raise ReleaseChangelogError(
                "CHANGELOG.md must change on every master-bound change so humans can follow master"
            )
        if not version_context_changed(base):
            print("master changelog gate: CHANGELOG.md updated; version context unchanged")
            return None
        if not context.release_ready:
            print("versioned changelog gate: release target is staged but not promoted; skipped")
            return None
        tagged_sha = existing_version_tag(context.version)
        if tagged_sha is not None:
            raise ReleaseChangelogError(
                f"version {context.version!r} is already tagged at {tagged_sha}; choose a new version"
            )

    requested = version or context.version
    if requested != context.version:
        raise ReleaseChangelogError(
            f"release notes version {requested!r} does not match current source {context.version!r}"
        )

    notes = extract_version_notes(changelog.read_text(encoding="utf-8"), requested)
    if output is not None:
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(notes, encoding="utf-8")
    print(f"versioned changelog gate: {requested} notes verified")
    return notes


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--version")
    parser.add_argument("--base")
    parser.add_argument("--changelog", type=Path, default=CHANGELOG)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args(argv)

    try:
        validate(
            version=args.version,
            base=args.base,
            changelog=args.changelog,
            output=args.output,
        )
    except ReleaseChangelogError as error:
        parser.error(str(error))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
