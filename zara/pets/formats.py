"""Pet format adapters.

ChatGPT/Codex compatibility is an *import* format, not Zarathushtra's
canonical representation. Each foreign format is adapted into a
``PetManifest`` via a small adapter rather than leaking its layout into the
core. There are three formats:

``ZarathuraNative``
    The canonical Zarathushtra manifest (see ``manifest.py``).

``ChatGPTSpriteV1``
    The single-image upload format used by ChatGPT on the web: a
    transparent PNG/WebP, exactly 1536x1872, 8 columns x 9 rows of
    192x208 cells. Rows are: idle, run-right, run-left, wave, jump,
    failure, waiting, active-work, review.

``ChatGPTSpriteV2``
    The desktop package format: ``pet.json`` + ``spritesheet.webp``, a
    1536x2288 atlas (8 columns x 11 rows). ``pet.json`` carries
    ``spriteVersionNumber: 2`` plus id/displayName/description. The V2
    sheet extends V1 with two extra rows of look-direction cells.

Frame mappings are documented from the public ChatGPT/Codex pet docs and
the community-published V2 format. We do not guess undocumented rows: V2's
extra rows (10 and 11, 0-indexed 9 and 10) are look-direction cells that
are not part of the five-state animation set, so they are not mapped to a
pet state by default. They remain available in the manifest metadata for
future use.
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Optional

from .manifest import (
    MANIFEST_VERSION,
    Animation,
    FrameGeometry,
    ManifestError,
    PetManifest,
    sanitize_id,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Shared geometry constants
# ---------------------------------------------------------------------------

CELL_WIDTH = 192
CELL_HEIGHT = 208
COLUMNS = 8

# V1 (web upload): 8 cols x 9 rows = 1536 x 1872
V1_ROWS = 9
V1_WIDTH = 1536
V1_HEIGHT = 1872
V1_MAX_BYTES = 20 * 1024 * 1024  # 20 MiB per ChatGPT docs

# V2 (desktop package): 8 cols x 11 rows = 1536 x 2288
V2_ROWS = 11
V2_WIDTH = 1536
V2_HEIGHT = 2288

# Row index -> Zarathushtra animation name. Rows 0-8 are the nine
# standard action rows documented by ChatGPT/Codex:
#   0 idle, 1 run-right, 2 run-left, 3 wave, 4 jump,
#   5 failure reaction, 6 waiting, 7 active work/processing, 8 review.
#
# Row 7 (active work) maps to the ``running`` *state* (task work). Row 1
# (run-right) is physical movement — we map it to a separate ``drag``
# animation used only while the user carries the window, so the pet
# "runs" when moved but does NOT run on tasks. V2's extra rows (9, 10)
# are look-direction cells not mapped to a state animation.
CHATGPT_ROW_TO_STATE: Dict[int, str] = {
    0: "idle",
    1: "drag",           # run right — physical movement, not task work
    2: "drag",           # run left — collapsed onto drag (first wins)
    3: "idle",           # wave (idle flourish)
    4: "idle",           # jump (idle flourish)
    5: "blocked",        # failure reaction
    6: "needs-input",    # waiting
    7: "running",        # active work / processing (the task animation)
    8: "ready",          # review / inspection
}


class FormatError(ValueError):
    """Raised when a foreign pet format cannot be identified or adapted."""


@dataclass
class ImportResult:
    manifest: PetManifest
    source_format: str
    warnings: list[str]


# ---------------------------------------------------------------------------
# Format protocol
# ---------------------------------------------------------------------------

class PetFormat:
    """Adapts a foreign pet asset into a Zarathushtra ``PetManifest``."""

    name: str = "unknown"

    def identify(self, candidate: Any) -> bool:
        raise NotImplementedError

    def adapt(self, candidate: Any, *, pet_id: Optional[str] = None,
              display_name: Optional[str] = None) -> ImportResult:
        raise NotImplementedError


# ---------------------------------------------------------------------------
# ChatGPT V1 (single-image web upload)
# ---------------------------------------------------------------------------

@dataclass
class _ImageCandidate:
    path: Path
    width: int
    height: int
    mime: str
    size_bytes: int


class ChatGPTSpriteV1(PetFormat):
    name = "chatgpt-v1"

    def identify(self, candidate: Any) -> bool:
        if not isinstance(candidate, _ImageCandidate):
            return False
        return (
            candidate.width == V1_WIDTH
            and candidate.height == V1_HEIGHT
            and candidate.mime in {"image/png", "image/webp"}
        )

    def adapt(self, candidate: Any, *, pet_id: Optional[str] = None,
              display_name: Optional[str] = None) -> ImportResult:
        if not self.identify(candidate):
            raise FormatError("not a ChatGPT V1 sprite")
        warnings: list[str] = []
        # Some V1 uploads are 1536x2288 (V2 geometry) despite the docs
        # stating 1872. We accept the documented 1872 height here; V2
        # geometry is handled by the V2 adapter.
        manifest = _build_manifest_from_rows(
            pet_id=pet_id or _derive_id(candidate.path, display_name),
            display_name=display_name or candidate.path.stem,
            source_format="chatgpt-v1",
            sprite_asset="spritesheet.webp",
            rows=V1_ROWS,
            row_to_state=CHATGPT_ROW_TO_STATE,
            warnings=warnings,
        )
        return ImportResult(manifest=manifest, source_format=self.name, warnings=warnings)


# ---------------------------------------------------------------------------
# ChatGPT V2 (desktop package: pet.json + spritesheet.webp)
# ---------------------------------------------------------------------------

@dataclass
class _V2Candidate:
    pet_json_path: Path
    sprite_path: Path
    pet_json: Dict[str, Any]
    image: _ImageCandidate


class ChatGPTSpriteV2(PetFormat):
    name = "chatgpt-v2"

    def identify(self, candidate: Any) -> bool:
        if not isinstance(candidate, _V2Candidate):
            return False
        version = candidate.pet_json.get("spriteVersionNumber")
        if version != 2:
            return False
        return (
            candidate.image.width == V2_WIDTH
            and candidate.image.height == V2_HEIGHT
            and candidate.image.mime in {"image/png", "image/webp"}
        )

    def adapt(self, candidate: Any, *, pet_id: Optional[str] = None,
              display_name: Optional[str] = None) -> ImportResult:
        if not self.identify(candidate):
            raise FormatError("not a ChatGPT V2 sprite")
        warnings: list[str] = []
        source_id = str(candidate.pet_json.get("id") or "")
        source_name = display_name or candidate.pet_json.get("displayName") or source_id
        manifest = _build_manifest_from_rows(
            pet_id=pet_id or sanitize_id(source_id or candidate.pet_json_path.parent.name),
            display_name=str(source_name),
            source_format="chatgpt-v2",
            sprite_asset="spritesheet.webp",
            rows=V2_ROWS,
            row_to_state=CHATGPT_ROW_TO_STATE,
            warnings=warnings,
        )
        manifest.metadata["chatgpt_description"] = candidate.pet_json.get("description", "")
        manifest.metadata["chatgpt_sprite_version"] = 2
        # V2's extra rows (9, 10) are look-direction cells, not state
        # animations. We record them as metadata for future use but do not
        # map them to a pet state, since the docs do not define their
        # semantics as state animations.
        manifest.metadata["look_direction_rows"] = [9, 10]
        return ImportResult(manifest=manifest, source_format=self.name, warnings=warnings)


# ---------------------------------------------------------------------------
# Native
# ---------------------------------------------------------------------------

class ZarathuraNative(PetFormat):
    name = "native"

    def identify(self, candidate: Any) -> bool:
        return isinstance(candidate, _NativeCandidate)

    def adapt(self, candidate: Any, *, pet_id: Optional[str] = None,
              display_name: Optional[str] = None) -> ImportResult:
        if not self.identify(candidate):
            raise FormatError("not a native pet package")
        manifest = PetManifest.load(candidate.manifest_path)
        if pet_id:
            manifest = PetManifest(
                id=pet_id,
                name=display_name or manifest.name,
                version=manifest.version,
                source=manifest.source,
                source_format="native",
                sprite_asset=manifest.sprite_asset,
                frame_geometry=manifest.frame_geometry,
                animations=manifest.animations,
                anchor=manifest.anchor,
                scale=manifest.scale,
                metadata=manifest.metadata,
            )
        return ImportResult(manifest=manifest, source_format=self.name, warnings=[])


@dataclass
class _NativeCandidate:
    manifest_path: Path


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _build_manifest_from_rows(
    *,
    pet_id: str,
    display_name: str,
    source_format: str,
    sprite_asset: str,
    rows: int,
    row_to_state: Dict[int, str],
    warnings: list[str],
) -> PetManifest:
    """Build a manifest by collapsing ChatGPT rows onto Zarathushtra states.

    Multiple rows may map to the same state (e.g. run-left and run-right
    both -> running). For each state we keep the first row that maps to it
    so the animation is deterministic.
    """
    frame_geometry = FrameGeometry(
        width=CELL_WIDTH, height=CELL_HEIGHT, columns=COLUMNS, rows=rows,
    )
    seen: set[str] = set()
    animations: list[Animation] = []
    for row, state in row_to_state.items():
        if row >= rows:
            continue
        if state in seen:
            continue
        seen.add(state)
        animations.append(
            Animation(
                name=state,
                row=row,
                frames=COLUMNS,
                fps=8.0,
                loop=True,
            )
        )
    # Ensure all five states are present. If a state is missing from the
    # source layout, fall back to row 0 (idle) so the pet still renders.
    for state in ("idle", "running", "needs-input", "ready", "blocked"):
        if state not in seen:
            warnings.append(
                f"source layout missing {state!r}; falling back to idle row"
            )
            animations.append(
                Animation(name=state, row=0, frames=COLUMNS, fps=8.0, loop=True)
            )
    return PetManifest(
        id=pet_id,
        name=display_name,
        version=MANIFEST_VERSION,
        source=source_format,
        source_format=source_format,
        sprite_asset=sprite_asset,
        frame_geometry=frame_geometry,
        animations=animations,
        anchor=(0.5, 1.0),
        scale=1.0,
        metadata={},
    )


def _derive_id(path: Path, display_name: Optional[str]) -> str:
    raw = display_name or path.stem
    try:
        return sanitize_id(raw)
    except ManifestError:
        return sanitize_id("imported-pet")


def load_pet_json(path: Path) -> Dict[str, Any]:
    """Read a ChatGPT ``pet.json`` defensively."""
    try:
        raw = path.read_text(encoding="utf-8")
    except OSError as exc:
        raise FormatError(f"cannot read pet.json: {exc}") from exc
    try:
        data = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise FormatError(f"pet.json is not valid JSON: {exc}") from exc
    if not isinstance(data, dict):
        raise FormatError("pet.json must be a JSON object")
    return data