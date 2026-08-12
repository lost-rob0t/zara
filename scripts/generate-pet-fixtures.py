#!/usr/bin/env python3
"""Generate synthetic pet sprite fixtures for tests.

Produces PNG/WebP files with the exact ChatGPT V1 (1536x1872) and V2
(1536x2288) geometries plus a native Zarathushtra package. No copyrighted
artwork is committed — these are programmatically generated solid-color
grids with row/col markers so tests can verify frame extraction.

Usage:
    python scripts/generate-pet-fixtures.py <output-dir>
"""

from __future__ import annotations

import json
import struct
import sys
import zlib
from pathlib import Path

CELL_W = 192
CELL_H = 208
COLS = 8
V1_ROWS = 9
V2_ROWS = 11
V1_W = COLS * CELL_W
V1_H = V1_ROWS * CELL_H
V2_H = V2_ROWS * CELL_H


def _png_chunk(name: bytes, data: bytes) -> bytes:
    return struct.pack(">I", len(data)) + name + data + struct.pack(">I", zlib.crc32(name + data) & 0xFFFFFFFF)


def _png_image(width: int, height: int, pixels: bytes) -> bytes:
    raw = b""
    stride = width * 4
    for y in range(height):
        raw += b"\x00" + pixels[y * stride:(y + 1) * stride]
    compressed = zlib.compress(raw)
    return (
        b"\x89PNG\r\n\x1a\n"
        + _png_chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0))
        + _png_chunk(b"IDAT", compressed)
        + _png_chunk(b"IEND", b"")
    )


def _row_color(row: int) -> tuple[int, int, int, int]:
    palette = [
        (200, 200, 200, 255),  # idle gray
        (120, 180, 255, 255),  # running blue
        (255, 120, 120, 255),  # needs-input red
        (120, 255, 120, 255),  # ready green
        (255, 200, 80, 255),   # blocked amber
        (180, 180, 255, 255),
        (255, 180, 220, 255),
        (200, 255, 255, 255),
        (255, 255, 200, 255),
        (220, 220, 220, 255),
        (240, 240, 240, 255),
    ]
    return palette[row % len(palette)]


def _make_grid(width: int, height: int, rows: int) -> bytes:
    pixels = bytearray(width * height * 4)
    for row in range(rows):
        color = _row_color(row)
        for col in range(COLS):
            x0 = col * CELL_W
            y0 = row * CELL_H
            for y in range(y0, y0 + CELL_H):
                for x in range(x0, x0 + CELL_W):
                    offset = (y * width + x) * 4
                    pixels[offset:offset + 4] = bytes(color)
            # mark cell with a diagonal stripe so frames differ per col
            stripe = (color[0] ^ (col * 20), color[1] ^ (col * 20), color[2] ^ (col * 20), 255)
            for y in range(y0, y0 + CELL_H):
                if (x0 + (y - y0)) % CELL_W < 4 or (y - y0) % 8 == 0 and col % 2 == 0:
                    offset = (y * width + x0) * 4
                    pixels[offset:offset + 4] = bytes(stripe)
    return bytes(pixels)


def write_v1(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pixels = _make_grid(V1_W, V1_H, V1_ROWS)
    path.write_bytes(_png_image(V1_W, V1_H, pixels))


def write_v2_sprite(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pixels = _make_grid(V1_W, V2_H, V2_ROWS)
    path.write_bytes(_png_image(V1_W, V2_H, pixels))


def write_v2_pet_json(path: Path, pet_id: str = "synthetic-v2", name: str = "Synthetic V2") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    data = {
        "id": pet_id,
        "displayName": name,
        "description": "Synthetic V2 fixture pet",
        "spriteVersionNumber": 2,
        "spritesheetPath": "spritesheet.png",
    }
    path.write_text(json.dumps(data, indent=2), encoding="utf-8")


def write_native(path: Path, pet_id: str = "synthetic-native", name: str = "Synthetic Native") -> None:
    sprite = path.parent / "spritesheet.png"
    write_v2_sprite(sprite)
    manifest = {
        "id": pet_id,
        "name": name,
        "version": 1,
        "source": "native",
        "source_format": "native",
        "sprite_asset": "spritesheet.png",
        "frame_geometry": {"width": CELL_W, "height": CELL_H, "columns": COLS, "rows": V2_ROWS},
        "animations": [
            {"name": "idle", "row": 0, "frames": 8, "fps": 8.0, "loop": True},
            {"name": "running", "row": 1, "frames": 8, "fps": 8.0, "loop": True},
            {"name": "needs-input", "row": 6, "frames": 8, "fps": 8.0, "loop": True},
            {"name": "ready", "row": 8, "frames": 8, "fps": 8.0, "loop": True},
            {"name": "blocked", "row": 5, "frames": 8, "fps": 8.0, "loop": True},
        ],
        "anchor": [0.5, 1.0],
        "scale": 1.0,
        "metadata": {},
    }
    path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: generate-pet-fixtures.py <output-dir>", file=sys.stderr)
        return 1
    out = Path(sys.argv[1])
    write_v1(out / "v1" / "sprite.png")
    write_v2_sprite(out / "v2" / "spritesheet.png")
    write_v2_pet_json(out / "v2" / "pet.json")
    write_native(out / "native" / "pet.json")
    # Invalid fixtures for negative tests
    (out / "bad" / "wrong-dims.png").parent.mkdir(parents=True, exist_ok=True)
    (out / "bad" / "wrong-dims.png").write_bytes(_png_image(100, 100, _make_grid(100, 100, 1)))
    (out / "bad" / "not-image.txt").write_text("this is not an image\n")
    (out / "bad" / "corrupt.png").write_bytes(b"\x89PNG\r\n\x1a\n\x00\x00\x00\x00broken")
    (out / "bad" / "pet.json").parent.mkdir(parents=True, exist_ok=True)
    (out / "bad" / "pet.json").write_text('{"spriteVersionNumber": 2, "id": "../escape"}')
    print(f"fixtures written to {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())