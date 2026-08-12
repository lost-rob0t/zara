"""Zarathushtra-native pet manifest.

A pet package is DATA, not code. The manifest describes the sprite asset,
frame geometry, animation timing, anchor, scale, and provenance. It is
forward-compatible: unknown keys are preserved on round-trip and a
``manifest_version`` field gates schema evolution.

Package layout on disk::

    <pet-data-dir>/<pet-id>/
        pet.json          # this manifest
        spritesheet.webp  # or .png

No executable scripts are permitted in a pet package.
"""

from __future__ import annotations

import json
import logging
import re
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)

MANIFEST_VERSION = 1
MANIFEST_FILENAME = "pet.json"

# Pet IDs are restricted to a safe subset so they can be used as directory
# names without path-traversal risk. Anything outside this set is rejected.
_ID_RE = re.compile(r"^[a-z0-9][a-z0-9-]{0,63}$")

# Animation name aliases mapping to ChatGPT/Codex standard row semantics.
# These are the canonical Zarathushtra animation names; importers map
# foreign layouts onto these names.
STATE_ANIMATIONS = ("idle", "running", "needs-input", "ready", "blocked")

# Safe image MIME types we will decode. Anything else is rejected at import.
SAFE_IMAGE_MIMES = {"image/png", "image/webp"}


class ManifestError(ValueError):
    """Raised when a pet manifest is malformed, unsafe, or unsupported."""


@dataclass
class FrameGeometry:
    """Geometry of one sprite cell within the sheet."""

    width: int
    height: int
    columns: int
    rows: int

    def __post_init__(self) -> None:
        for name, value in (
            ("width", self.width), ("height", self.height),
            ("columns", self.columns), ("rows", self.rows),
        ):
            if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
                raise ManifestError(f"frame_geometry.{name} must be a positive integer")

    def frame_count(self) -> int:
        return self.columns * self.rows

    def cell_index(self, col: int, row: int) -> int:
        if not (0 <= col < self.columns and 0 <= row < self.rows):
            raise ManifestError(f"cell ({col},{row}) out of bounds {self.columns}x{self.rows}")
        return row * self.columns + col


@dataclass
class Animation:
    """One named animation: a row index, frame range, FPS, and loop flag."""

    name: str
    row: int
    frames: int = 8
    fps: float = 8.0
    loop: bool = True

    def __post_init__(self) -> None:
        if not self.name or not isinstance(self.name, str):
            raise ManifestError("animation.name must be a non-empty string")
        if not isinstance(self.row, int) or isinstance(self.row, bool) or self.row < 0:
            raise ManifestError(f"animation {self.name!r}: row must be a non-negative integer")
        if not isinstance(self.frames, int) or isinstance(self.frames, bool) or self.frames <= 0:
            raise ManifestError(f"animation {self.name!r}: frames must be a positive integer")
        if (
            isinstance(self.fps, bool)
            or not isinstance(self.fps, (int, float))
            or float(self.fps) <= 0
            or float(self.fps) > 1000
        ):
            raise ManifestError(f"animation {self.name!r}: fps must be a positive number")
        if not isinstance(self.loop, bool):
            raise ManifestError(f"animation {self.name!r}: loop must be a boolean")


@dataclass
class PetManifest:
    """Canonical Zarathushtra pet description.

    ``source_format`` records where the pet came from so the picker and
    documentation can show provenance ("native", "chatgpt-v1", "chatgpt-v2").
    """

    id: str
    name: str
    version: int = MANIFEST_VERSION
    source: str = "native"
    source_format: str = "native"
    sprite_asset: str = "spritesheet.webp"
    frame_geometry: FrameGeometry = field(
        default_factory=lambda: FrameGeometry(192, 208, 8, 9)
    )
    animations: List[Animation] = field(default_factory=list)
    anchor: tuple[float, float] = (0.5, 1.0)
    scale: float = 1.0
    metadata: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not _ID_RE.match(self.id):
            raise ManifestError(
                f"pet id {self.id!r} must match {_ID_RE.pattern}"
            )
        if not self.name or not isinstance(self.name, str):
            raise ManifestError("pet name must be a non-empty string")
        if (
            isinstance(self.version, bool)
            or not isinstance(self.version, int)
            or self.version < 1
        ):
            raise ManifestError("version must be a positive integer")
        if not isinstance(self.source, str) or not self.source:
            raise ManifestError("source must be a non-empty string")
        if self.source_format not in {"native", "chatgpt-v1", "chatgpt-v2"}:
            raise ManifestError(f"unsupported source_format: {self.source_format!r}")
        if not isinstance(self.sprite_asset, str) or not self.sprite_asset:
            raise ManifestError("sprite_asset must be a non-empty string")
        if _is_unsafe_path(self.sprite_asset):
            raise ManifestError(f"sprite_asset path is unsafe: {self.sprite_asset!r}")
        if (
            isinstance(self.scale, bool)
            or not isinstance(self.scale, (int, float))
            or float(self.scale) <= 0
        ):
            raise ManifestError("scale must be a positive number")
        if not self.animations:
            raise ManifestError("at least one animation is required")
        names = [a.name for a in self.animations]
        if len(set(names)) != len(names):
            raise ManifestError("animation names must be unique")
        for anim in self.animations:
            if anim.row >= self.frame_geometry.rows:
                raise ManifestError(
                    f"animation {anim.name!r}: row {anim.row} exceeds sheet rows "
                    f"{self.frame_geometry.rows}"
                )
            if anim.frames > self.frame_geometry.columns:
                raise ManifestError(
                    f"animation {anim.name!r}: frames {anim.frames} exceed columns "
                    f"{self.frame_geometry.columns}"
                )

    def animation_for(self, state: str) -> Optional[Animation]:
        for anim in self.animations:
            if anim.name == state:
                return anim
        return None

    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data["anchor"] = list(self.anchor)
        data["frame_geometry"] = asdict(self.frame_geometry)
        data["animations"] = [asdict(a) for a in self.animations]
        return data

    def to_json(self, indent: int = 2) -> str:
        return json.dumps(self.to_dict(), indent=indent, sort_keys=True)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "PetManifest":
        if not isinstance(data, dict):
            raise ManifestError("manifest must be a JSON object")
        version = int(data.get("version", MANIFEST_VERSION))
        if version > MANIFEST_VERSION:
            raise ManifestError(
                f"manifest version {version} is newer than supported "
                f"{MANIFEST_VERSION}"
            )
        fg_data = data.get("frame_geometry") or {}
        try:
            frame_geometry = FrameGeometry(
                width=int(fg_data.get("width", 192)),
                height=int(fg_data.get("height", 208)),
                columns=int(fg_data.get("columns", 8)),
                rows=int(fg_data.get("rows", 9)),
            )
        except (TypeError, ValueError) as exc:
            raise ManifestError(f"invalid frame_geometry: {exc}") from exc
        anim_data = data.get("animations", [])
        if not isinstance(anim_data, list):
            raise ManifestError("animations must be a list")
        animations: List[Animation] = []
        for item in anim_data:
            if not isinstance(item, dict):
                raise ManifestError("each animation must be a JSON object")
            try:
                animations.append(
                    Animation(
                        name=str(item["name"]),
                        row=int(item["row"]),
                        frames=int(item.get("frames", 8)),
                        fps=float(item.get("fps", 8.0)),
                        loop=bool(item.get("loop", True)),
                    )
                )
            except KeyError as exc:
                raise ManifestError(f"animation missing key: {exc}") from exc
            except (TypeError, ValueError) as exc:
                raise ManifestError(f"invalid animation: {exc}") from exc
        anchor_data = data.get("anchor", [0.5, 1.0])
        if not isinstance(anchor_data, (list, tuple)) or len(anchor_data) != 2:
            raise ManifestError("anchor must be a [x, y] pair")
        anchor = (float(anchor_data[0]), float(anchor_data[1]))
        return cls(
            id=str(data["id"]),
            name=str(data.get("name", data["id"])),
            version=version,
            source=str(data.get("source", "native")),
            source_format=str(data.get("source_format", "native")),
            sprite_asset=str(data.get("sprite_asset", "spritesheet.webp")),
            frame_geometry=frame_geometry,
            animations=animations,
            anchor=anchor,
            scale=float(data.get("scale", 1.0)),
            metadata=dict(data.get("metadata", {})),
        )

    @classmethod
    def load(cls, path: Path) -> "PetManifest":
        try:
            raw = path.read_text(encoding="utf-8")
        except OSError as exc:
            raise ManifestError(f"cannot read manifest {path}: {exc}") from exc
        try:
            data = json.loads(raw)
        except json.JSONDecodeError as exc:
            raise ManifestError(f"manifest {path} is not valid JSON: {exc}") from exc
        return cls.from_dict(data)

    def save(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_json(), encoding="utf-8")


def _is_unsafe_path(name: str) -> bool:
    """Reject path traversal and absolute paths in asset references."""
    if not name:
        return True
    if name.startswith("/") or name.startswith("\\"):
        return True
    if ":" in name and (len(name) > 1 and name[1] == ":"):
        return True
    parts = Path(name).parts
    if ".." in parts or "." in parts:
        return True
    return False


def sanitize_id(candidate: str) -> str:
    """Produce a safe pet id from arbitrary text.

    Lowercases, replaces unsafe runs with ``-``, trims, and enforces the
    id regex. Raises ``ManifestError`` if the result is empty.
    """
    cleaned = re.sub(r"[^a-z0-9-]+", "-", candidate.lower().strip())
    cleaned = re.sub(r"-+", "-", cleaned).strip("-")
    if not cleaned:
        raise ManifestError(f"cannot sanitize id from {candidate!r}")
    if len(cleaned) > 64:
        cleaned = cleaned[:64].rstrip("-")
    if not cleaned[0].isalnum():
        cleaned = "z" + cleaned
    if not _ID_RE.match(cleaned):
        raise ManifestError(f"sanitized id {cleaned!r} still invalid")
    return cleaned