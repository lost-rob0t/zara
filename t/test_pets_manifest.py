"""Tests for the Zarathushtra-native pet manifest and ChatGPT import."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from zara.pets.manifest import (
    Animation,
    FrameGeometry,
    MANIFEST_FILENAME,
    ManifestError,
    PetManifest,
    sanitize_id,
)


def _valid_manifest(**overrides) -> PetManifest:
    base = dict(
        id="test-pet",
        name="Test Pet",
        source_format="native",
        sprite_asset="spritesheet.webp",
        frame_geometry=FrameGeometry(192, 208, 8, 9),
        animations=[
            Animation(name="idle", row=0, frames=8, fps=8.0, loop=True),
            Animation(name="running", row=1, frames=8, fps=8.0, loop=True),
            Animation(name="needs-input", row=6, frames=8, fps=8.0, loop=True),
            Animation(name="ready", row=8, frames=8, fps=8.0, loop=True),
            Animation(name="blocked", row=5, frames=8, fps=8.0, loop=True),
        ],
    )
    base.update(overrides)
    return PetManifest(**base)


def test_valid_native_manifest_round_trips(tmp_path):
    manifest = _valid_manifest()
    path = tmp_path / MANIFEST_FILENAME
    manifest.save(path)
    loaded = PetManifest.load(path)
    assert loaded.id == "test-pet"
    assert loaded.name == "Test Pet"
    assert loaded.source_format == "native"
    assert loaded.frame_geometry.columns == 8
    assert loaded.frame_geometry.rows == 9
    assert len(loaded.animations) == 5
    assert loaded.animation_for("idle").row == 0


def test_invalid_id_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(id="../escape")


def test_invalid_id_with_spaces_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(id="bad id")


def test_path_traversal_in_sprite_asset_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(sprite_asset="../escape.png")


def test_absolute_sprite_asset_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(sprite_asset="/etc/passwd")


def test_empty_animations_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(animations=[])


def test_animation_row_out_of_bounds_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(animations=[Animation(name="idle", row=99, frames=8)])


def test_animation_frames_exceeding_columns_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(animations=[Animation(name="idle", row=0, frames=99)])


def test_duplicate_animation_names_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(animations=[
            Animation(name="idle", row=0, frames=8),
            Animation(name="idle", row=1, frames=8),
        ])


def test_unsupported_source_format_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(source_format="unknown")


def test_zero_scale_rejected():
    with pytest.raises(ManifestError):
        _valid_manifest(scale=0)


def test_manifest_version_too_new_rejected(tmp_path):
    manifest = _valid_manifest()
    data = manifest.to_dict()
    data["version"] = 999
    path = tmp_path / MANIFEST_FILENAME
    path.write_text(json.dumps(data))
    with pytest.raises(ManifestError, match="newer than supported"):
        PetManifest.load(path)


def test_malformed_json_rejected(tmp_path):
    path = tmp_path / MANIFEST_FILENAME
    path.write_text("{not json")
    with pytest.raises(ManifestError, match="not valid JSON"):
        PetManifest.load(path)


def test_sanitize_id_lowercases_and_replaces_unsafe():
    assert sanitize_id("My Cool Pet!") == "my-cool-pet"


def test_sanitize_id_rejects_empty():
    with pytest.raises(ManifestError):
        sanitize_id("!!!")


def test_sanitize_id_truncates_long_names():
    long = "a" * 100
    result = sanitize_id(long)
    assert len(result) <= 64


def test_frame_geometry_cell_index():
    fg = FrameGeometry(192, 208, 8, 9)
    assert fg.cell_index(0, 0) == 0
    assert fg.cell_index(1, 0) == 1
    assert fg.cell_index(0, 1) == 8
    assert fg.frame_count() == 72


def test_frame_geometry_rejects_zero():
    with pytest.raises(ManifestError):
        FrameGeometry(0, 208, 8, 9)


def test_look_frame_requires_v2_metadata():
    assert _valid_manifest().look_frame(0) is None


def test_look_frame_rejects_out_of_bounds_rows():
    manifest = _valid_manifest(metadata={"look_direction_rows": [9, 10]})
    assert manifest.look_frame(0) is None


def test_manifest_from_dict_loads_full_structure():
    data = {
        "id": "loaded",
        "name": "Loaded",
        "version": 1,
        "source": "native",
        "source_format": "native",
        "sprite_asset": "spritesheet.webp",
        "frame_geometry": {"width": 192, "height": 208, "columns": 8, "rows": 9},
        "animations": [{"name": "idle", "row": 0, "frames": 8, "fps": 8.0, "loop": True}],
        "anchor": [0.5, 1.0],
        "scale": 2.0,
        "metadata": {"foo": "bar"},
    }
    manifest = PetManifest.from_dict(data)
    assert manifest.id == "loaded"
    assert manifest.scale == 2.0
    assert manifest.metadata == {"foo": "bar"}
    assert manifest.anchor == (0.5, 1.0)
