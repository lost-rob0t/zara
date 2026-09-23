"""Canonical Zara changelog loading and version-section parsing."""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Mapping, MutableMapping, Optional

from zara.version_context import VersionContextError, load_version_context

LAST_SHOWN_KEY = "desktop/changelog/last-shown-version"


def parse_changelog(markdown: str) -> dict[str, str]:
    """Return version -> normalized notes from the canonical Markdown changelog."""
    sections: dict[str, list[str]] = {}
    current: Optional[str] = None
    for raw_line in markdown.splitlines():
        line = raw_line.rstrip()
        if line.startswith("## "):
            current = line[3:].strip()
            sections.setdefault(current, [])
            continue
        if current is not None:
            sections[current].append(line)

    parsed: dict[str, str] = {}
    for version, lines in sections.items():
        notes = _normalize_notes(lines)
        if notes:
            parsed[version] = notes
    return parsed


def _normalize_notes(lines: list[str]) -> str:
    rendered: list[str] = []
    blank = False
    previous_was_heading = False
    for raw in lines:
        line = raw.strip()
        if not line:
            blank = bool(rendered)
            continue

        is_heading = line.startswith("### ")
        if (
            blank
            and rendered
            and rendered[-1] != ""
            and (is_heading or not previous_was_heading)
        ):
            rendered.append("")
        blank = False

        if is_heading:
            rendered.append(line[4:].strip())
        elif line.startswith("- "):
            rendered.append("• " + line[2:].strip())
        else:
            rendered.append(line)
        previous_was_heading = is_heading
    return "\n".join(rendered).strip()


def find_install_resource(name: str) -> Optional[Path]:
    candidates = (
        Path(__file__).resolve().parents[1] / name,
        Path(sys.prefix) / "share" / "zarathushtra" / name,
        Path("/usr/share/zarathushtra") / name,
    )
    return next((path for path in candidates if path.is_file()), None)


def current_version() -> Optional[str]:
    path = find_install_resource("version.properties")
    if path is None:
        return None
    try:
        return load_version_context(path).version
    except VersionContextError:
        return None


def notes_for_version(version: str, *, path: Optional[Path] = None) -> Optional[str]:
    changelog_path = path or find_install_resource("CHANGELOG.md")
    if changelog_path is None:
        return None
    return parse_changelog(changelog_path.read_text(encoding="utf-8")).get(version)


def current_release_notes() -> tuple[Optional[str], Optional[str]]:
    version = current_version()
    if version is None:
        return None, None
    return version, notes_for_version(version)


def should_show_release_notes(
    settings: Mapping[str, object],
    version: Optional[str],
    notes: Optional[str],
) -> bool:
    if not version or not notes:
        return False
    return str(settings.get(LAST_SHOWN_KEY, "")) != version


def mark_release_notes_shown(settings: MutableMapping[str, object], version: str) -> None:
    settings[LAST_SHOWN_KEY] = version
