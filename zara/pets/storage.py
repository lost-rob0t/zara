"""Pet storage — Zarathushtra-managed application data location.

Pets live under ``$XDG_DATA_HOME/zarathushtra/pets/<pet-id>/`` (with a
``~/.local/share/zarathushtra/pets`` fallback) and never under the source
checkout. Removing an imported pet deletes only this managed copy; the
original ChatGPT source asset is never touched.
"""

from __future__ import annotations

import logging
import os
import re
import shutil
from pathlib import Path
from typing import List, Optional

from .manifest import MANIFEST_FILENAME, ManifestError, PetManifest

logger = logging.getLogger(__name__)

PETS_SUBDIR = "pets"


def pets_dir() -> Path:
    """Return the managed pets directory, creating it if needed."""
    xdg_data = os.getenv("XDG_DATA_HOME")
    if xdg_data:
        base = Path(xdg_data) / "zarathushtra" / PETS_SUBDIR
    else:
        base = Path.home() / ".local" / "share" / "zarathushtra" / PETS_SUBDIR
    base.mkdir(parents=True, exist_ok=True)
    return base


def pet_dir(pet_id: str) -> Path:
    from .manifest import _ID_RE
    if not _ID_RE.match(pet_id):
        raise ManifestError(f"unsafe pet id for storage: {pet_id!r}")
    return pets_dir() / pet_id


def list_pets() -> List[PetManifest]:
    """List all installed pets, ignoring malformed packages."""
    found: list[PetManifest] = []
    base = pets_dir()
    for child in base.iterdir():
        if not child.is_dir():
            continue
        manifest_path = child / MANIFEST_FILENAME
        if not manifest_path.exists():
            continue
        try:
            manifest = PetManifest.load(manifest_path)
        except ManifestError as exc:
            logger.warning("[PetStorage] skipping malformed pet %s: %s", child, exc)
            continue
        found.append(manifest)
    found.sort(key=lambda m: m.name.lower())
    return found


def load_pet(pet_id: str) -> Optional[PetManifest]:
    path = pet_dir(pet_id) / MANIFEST_FILENAME
    if not path.exists():
        return None
    return PetManifest.load(path)


def install_pet(manifest: PetManifest, sprite_source: Path) -> Path:
    """Install a pet: write manifest and copy the sprite asset into storage.

    The source sprite is never mutated; a copy lands in the managed dir.
    Returns the installed pet directory.
    """
    target_dir = pet_dir(manifest.id)
    target_dir.mkdir(parents=True, exist_ok=True)
    sprite_target = target_dir / manifest.sprite_asset
    if sprite_target.resolve().parent != target_dir.resolve():
        raise ManifestError(
            f"sprite_asset {manifest.sprite_asset!r} escapes pet directory"
        )
    shutil.copyfile(sprite_source, sprite_target)
    manifest.save(target_dir / MANIFEST_FILENAME)
    logger.info("[PetStorage] installed pet %s at %s", manifest.id, target_dir)
    return target_dir


def remove_pet(pet_id: str) -> bool:
    """Remove a managed pet package. Returns True if something was deleted."""
    target = pet_dir(pet_id)
    if not target.exists():
        return False
    shutil.rmtree(target)
    logger.info("[PetStorage] removed pet %s at %s", pet_id, target)
    return True


def sprite_path_for(manifest: PetManifest) -> Path:
    return pet_dir(manifest.id) / manifest.sprite_asset