"""Read-only discovery of ChatGPT/Codex pets installed on this machine.

Discovery is strictly read-only: we never modify, delete, or depend on
ChatGPT running. We copy imported assets into Zarathushtra storage; the
original ChatGPT files are untouched.

We search several documented application-data locations rather than one
hardcoded path, and we verify each candidate by inspecting its contents
(not just its filename) so future ChatGPT storage-layout changes degrade
gracefully into a manual import fallback.

Known locations (all under user home):

    ~/.codex/pets/<pet-id>/pet.json + spritesheet.webp     (Codex CLI)
    ~/.config/ChatGPT/pets/<pet-id>/...                    (desktop app, Linux)
    ~/Library/Application Support/ChatGPT/pets/<pet-id>/... (macOS)
    %APPDATA%\\ChatGPT\\pets\\<pet-id>\\...                    (Windows)

Each location is optional. Missing directories are silently skipped.
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator, List, Optional

from .importer import identify_candidate, ImportError_
from .manifest import MANIFEST_FILENAME
from .formats import FormatError

logger = logging.getLogger(__name__)


@dataclass
class DiscoveredPet:
    """A ChatGPT/Codex pet found on the local machine."""

    pet_id: str
    display_name: str
    source_path: Path
    source_format: str
    sprite_path: Optional[Path] = None


def _candidate_dirs() -> Iterator[Path]:
    home = Path.home()
    yield home / ".codex" / "pets"
    xdg_config = os.getenv("XDG_CONFIG_HOME")
    if xdg_config:
        yield Path(xdg_config) / "ChatGPT" / "pets"
    else:
        yield home / ".config" / "ChatGPT" / "pets"
    # macOS
    mac_home = home / "Library" / "Application Support" / "ChatGPT" / "pets"
    yield mac_home
    # Windows
    appdata = os.getenv("APPDATA")
    if appdata:
        yield Path(appdata) / "ChatGPT" / "pets"


class ChatGPTPetDiscovery:
    """Read-only scanner for locally installed ChatGPT/Codex pets."""

    def discover(self) -> List[DiscoveredPet]:
        found: list[DiscoveredPet] = []
        seen_paths: set[Path] = set()
        for base in _candidate_dirs():
            if not base.exists() or not base.is_dir():
                continue
            logger.debug("[ChatGPTDiscovery] scanning %s", base)
            for child in sorted(base.iterdir()):
                if not child.is_dir():
                    continue
                if child in seen_paths:
                    continue
                result = self.inspect(child)
                if result is not None:
                    seen_paths.add(child)
                    found.append(result)
        return found

    def inspect(self, candidate_dir: Path) -> Optional[DiscoveredPet]:
        """Inspect one candidate directory, verifying its contents."""
        pet_json = candidate_dir / MANIFEST_FILENAME
        # ChatGPT desktop packages use pet.json; Codex CLI uses pet.json too.
        if pet_json.exists():
            entry = pet_json
        else:
            # Fall back to any spritesheet image present.
            images = [
                p for p in candidate_dir.iterdir()
                if p.is_file() and p.suffix.lower() in {".png", ".webp"}
            ]
            if not images:
                return None
            entry = images[0]
        try:
            adapter, result = identify_candidate(entry)
        except (ImportError_, FormatError, OSError) as exc:
            logger.debug(
                "[ChatGPTDiscovery] rejecting %s: %s", candidate_dir, exc
            )
            return None
        sprite = None
        if entry.name == MANIFEST_FILENAME:
            sprite = entry.parent / result.manifest.sprite_asset
            if not sprite.exists():
                sprite = None
        else:
            sprite = entry
        return DiscoveredPet(
            pet_id=result.manifest.id,
            display_name=result.manifest.name,
            source_path=entry,
            source_format=result.source_format,
            sprite_path=sprite,
        )