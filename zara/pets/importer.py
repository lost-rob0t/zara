"""Pet import flow — validates untrusted pet files and installs them.

Pet files are untrusted data. This module enforces:

- supported file types only (PNG/WebP)
- safe image decoding via header magic numbers (no extension trust)
- size limits (20 MiB)
- dimension limits (exact V1/V2 geometry)
- path traversal protection (manifest ids/asset names are sanitized)
- sanitized generated identifiers
- no arbitrary script execution
- safe handling of malformed images/manifests

The source file is never mutated. A copy lands in managed storage only
after all validation passes.
"""

from __future__ import annotations

import logging
import struct
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

from .formats import (
    ChatGPTSpriteV1,
    ChatGPTSpriteV2,
    ImportResult,
    PetFormat,
    ZarathuraNative,
    _ImageCandidate,
    _NativeCandidate,
    _V2Candidate,
    V1_HEIGHT,
    V1_MAX_BYTES,
    V1_WIDTH,
    V2_HEIGHT,
    V2_WIDTH,
    load_pet_json,
)
from .manifest import PetManifest, sanitize_id
from .storage import install_pet

logger = logging.getLogger(__name__)

MAX_BYTES = V1_MAX_BYTES  # 20 MiB; applies to both V1 and V2 imports.


class ImportError_(ValueError):
    """Raised when an import candidate is rejected."""


@dataclass
class ImportPreview:
    """Preview of a candidate import before the user confirms."""

    source_format: str
    pet_id: str
    display_name: str
    warnings: List[str]
    manifest: PetManifest
    sprite_source: Path


# Ordered registry of adapters. The first adapter that identifies the
# candidate wins. Native is checked first so a real pet.json isn't
# misidentified as a foreign sprite.
ADAPTERS: List[PetFormat] = [
    ZarathuraNative(),
    ChatGPTSpriteV2(),
    ChatGPTSpriteV1(),
]


def detect_mime(path: Path) -> str:
    """Detect the image MIME type from file magic numbers, not extensions."""
    if not path.exists() or not path.is_file():
        raise ImportError_(f"not a file: {path}")
    with path.open("rb") as handle:
        header = handle.read(12)
    if header.startswith(b"\x89PNG\r\n\x1a\n"):
        return "image/png"
    if header[0:4] == b"RIFF" and header[8:12] == b"WEBP":
        return "image/webp"
    raise ImportError_(f"unsupported file type (not PNG or WebP): {path.name}")


def _image_dimensions(path: Path) -> Tuple[int, int]:
    """Read PNG/WebP dimensions from headers without a third-party library.

    Raises ``ImportError_`` on truncated or malformed headers. We only
    parse the fixed headers, so malformed later chunks cannot trick us.
    """
    with path.open("rb") as handle:
        header = handle.read(30)
    if header.startswith(b"\x89PNG\r\n\x1a\n"):
        if len(header) < 24:
            raise ImportError_("truncated PNG header")
        width, height = struct.unpack(">II", header[16:24])
        return width, height
    if header[0:4] == b"RIFF" and header[8:12] == b"WEBP":
        if len(header) < 30:
            raise ImportError_("truncated WebP header")
        form = header[12:16]
        if form == b"VP8 ":
            width = int.from_bytes(header[26:28], "little") & 0x3FFF
            height = int.from_bytes(header[28:30], "little") & 0x3FFF
            return width, height
        if form == b"VP8L":
            val = int.from_bytes(header[21:25], "little")
            width = (val & 0x3FFF) + 1
            height = ((val >> 14) & 0x3FFF) + 1
            return width, height
        if form == b"VP8X":
            width = int.from_bytes(header[24:27], "little") + 1
            height = int.from_bytes(header[27:30], "little") + 1
            return width, height
        raise ImportError_("unsupported WebP variant")
    raise ImportError_(f"unsupported image format: {path.name}")


def inspect_image(path: Path) -> _ImageCandidate:
    """Validate and inspect an image file, returning geometry + MIME."""
    mime = detect_mime(path)
    size_bytes = path.stat().st_size
    if size_bytes > MAX_BYTES:
        raise ImportError_(f"file too large: {size_bytes} bytes (limit {MAX_BYTES})")
    width, height = _image_dimensions(path)
    return _ImageCandidate(
        path=path, width=width, height=height, mime=mime, size_bytes=size_bytes,
    )


def _resolve_sprite_source(adapter: PetFormat, path: Path,
                            manifest: PetManifest) -> Path:
    if isinstance(adapter, ChatGPTSpriteV1):
        return path
    if isinstance(adapter, ChatGPTSpriteV2):
        data = load_pet_json(path)
        sprite_name = str(data.get("spritesheetPath") or "spritesheet.webp")
        return path.parent / sprite_name
    if isinstance(adapter, ZarathuraNative):
        return path.parent / manifest.sprite_asset
    raise ImportError_("cannot determine sprite source for adapter")


def identify_candidate(path: Path) -> Tuple[PetFormat, ImportResult]:
    """Identify a candidate file and adapt it, without installing.

    Accepts:
      - a single PNG/WebP sprite sheet (V1 or V2 geometry)
      - a ChatGPT V2 ``pet.json`` (its sibling sprite is loaded)
      - a Zarathushtra native ``pet.json`` package
    """
    if not path.exists():
        raise ImportError_(f"path does not exist: {path}")

    # pet.json (native or ChatGPT V2)
    if path.is_file() and path.name == "pet.json":
        data = load_pet_json(path)
        if data.get("spriteVersionNumber") == 2:
            sprite_name = str(data.get("spritesheetPath") or "spritesheet.webp")
            sprite_path = path.parent / sprite_name
            if not sprite_path.exists():
                raise ImportError_(f"V2 pet.json references missing sprite: {sprite_path}")
            image = inspect_image(sprite_path)
            candidate = _V2Candidate(
                pet_json_path=path, sprite_path=sprite_path,
                pet_json=data, image=image,
            )
            adapter = next((a for a in ADAPTERS if a.identify(candidate)), None)
            if adapter is None:
                raise ImportError_("unrecognized ChatGPT V2 package")
            return adapter, adapter.adapt(candidate)
        # Otherwise treat as a native manifest.
        candidate = _NativeCandidate(manifest_path=path)
        adapter = next((a for a in ADAPTERS if a.identify(candidate)), None)
        if adapter is None:
            raise ImportError_("unrecognized pet.json manifest")
        return adapter, adapter.adapt(candidate)

    if path.is_file() and path.suffix.lower() == ".json":
        raise ImportError_("unrecognized JSON file")

    # Single image sprite sheet (V1 or V2 geometry)
    if path.is_file() and path.suffix.lower() in {".png", ".webp"}:
        image = inspect_image(path)
        adapter = next((a for a in ADAPTERS if a.identify(image)), None)
        if adapter is None:
            raise ImportError_(
                f"image dimensions {image.width}x{image.height} do not match "
                f"V1 ({V1_WIDTH}x{V1_HEIGHT}) or V2 ({V2_WIDTH}x{V2_HEIGHT})"
            )
        return adapter, adapter.adapt(image)

    raise ImportError_(f"unsupported candidate: {path}")


def preview(
    path: Path,
    *,
    pet_id: Optional[str] = None,
    display_name: Optional[str] = None,
) -> ImportPreview:
    """Preview an import without installing. Validates everything."""
    adapter, result = identify_candidate(path)
    pid = sanitize_id(pet_id) if pet_id else result.manifest.id
    name = display_name or result.manifest.name
    manifest = result.manifest
    manifest.id = pid
    manifest.name = name
    sprite_source = _resolve_sprite_source(adapter, path, manifest)
    if not sprite_source.exists():
        raise ImportError_(f"sprite source not found: {sprite_source}")
    return ImportPreview(
        source_format=result.source_format,
        pet_id=pid,
        display_name=name,
        warnings=list(result.warnings),
        manifest=manifest,
        sprite_source=sprite_source,
    )


def import_pet(
    path: Path,
    *,
    pet_id: Optional[str] = None,
    display_name: Optional[str] = None,
) -> PetManifest:
    """Identify, validate, and install a pet from a foreign asset.

    The source file is never mutated. The stored sprite asset name is
    normalized to match the source's actual image format (PNG or WebP)
    so the manifest always points at the file we actually copied.
    """
    adapter, result = identify_candidate(path)
    pid = sanitize_id(pet_id) if pet_id else result.manifest.id
    name = display_name or result.manifest.name
    manifest = result.manifest
    manifest.id = pid
    manifest.name = name
    sprite_source = _resolve_sprite_source(adapter, path, manifest)
    if not sprite_source.exists():
        raise ImportError_(f"sprite source not found: {sprite_source}")
    # Normalize the stored asset name to the actual format. WebP sources
    # are converted to PNG because Qt's nixpkgs build lacks the WebP image
    # plugin; PNG is always loadable. This keeps the manifest honest and
    # guarantees the overlay can decode the sprite.
    mime = detect_mime(sprite_source)
    if mime == "image/webp":
        manifest.sprite_asset = "spritesheet.png"
        png_target = _webp_to_png(sprite_source)
        install_pet(manifest, png_target)
        png_target.unlink(missing_ok=True)
    else:
        manifest.sprite_asset = "spritesheet.png"
        install_pet(manifest, sprite_source)
    logger.info(
        "[PetImport] imported %s pet %s from %s (sprite=%s)",
        result.source_format, manifest.id, path, sprite_source,
    )
    return manifest


def _webp_to_png(source: Path) -> Path:
    """Convert a WebP sprite to a temporary PNG using Pillow."""
    from PIL import Image  # lazy import; Pillow is in pythonLibs
    import tempfile

    image = Image.open(source)
    if image.mode != "RGBA":
        image = image.convert("RGBA")
    fd, tmp = tempfile.mkstemp(suffix=".png")
    import os
    os.close(fd)
    image.save(tmp, format="PNG")
    return Path(tmp)