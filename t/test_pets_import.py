"""Tests for the pet import flow — ChatGPT V1/V2, native, and rejections.

Fixtures are generated synthetically by ``scripts/generate-pet-fixtures.py``
so no copyrighted artwork is committed. The test module regenerates them
into a temp directory on setup.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from zara.pets.importer import (
    ImportError_,
    detect_mime,
    import_pet,
    preview,
)
from zara.pets.manifest import PetManifest
from zara.pets.storage import list_pets, load_pet, pet_dir, remove_pet

REPO_ROOT = Path(__file__).resolve().parent.parent
GENERATOR = REPO_ROOT / "scripts" / "generate-pet-fixtures.py"


@pytest.fixture
def fixtures(tmp_path):
    out = tmp_path / "fixtures"
    result = subprocess.run(
        [sys.executable, str(GENERATOR), str(out)],
        capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stderr
    return out


@pytest.fixture(autouse=True)
def isolated_pets_dir(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_DATA_HOME", str(tmp_path / "data"))
    yield


def test_detect_mime_png(fixtures):
    assert detect_mime(fixtures / "v1" / "sprite.png") == "image/png"


def test_detect_mime_webp_not_available_uses_png(fixtures):
    # The V2 fixture is PNG content (magic-number detected); the importer
    # accepts both PNG and WebP, so this validates magic-number detection.
    assert detect_mime(fixtures / "v2" / "spritesheet.png") == "image/png"


def test_detect_mime_rejects_text(fixtures):
    with pytest.raises(ImportError_):
        detect_mime(fixtures / "bad" / "not-image.txt")


def test_import_chatgpt_v1_sprite(fixtures):
    manifest = import_pet(fixtures / "v1" / "sprite.png")
    assert manifest.source_format == "chatgpt-v1"
    assert manifest.frame_geometry.rows == 9
    assert manifest.frame_geometry.columns == 8
    assert manifest.animation_for("idle") is not None
    assert manifest.animation_for("running") is not None
    assert manifest.animation_for("needs-input") is not None
    assert manifest.animation_for("ready") is not None
    assert manifest.animation_for("blocked") is not None
    assert manifest.animation_for("idle").frames == 6
    assert manifest.animation_for("drag").frames == 8
    assert manifest.animation_for("wave").frames == 4
    assert manifest.animation_for("jump").frames == 5
    assert manifest.animation_for("running").frames == 6
    # Installed into managed storage.
    installed = load_pet(manifest.id)
    assert installed is not None
    assert (pet_dir(manifest.id) / "spritesheet.png").exists()


def test_import_chatgpt_v2_package(fixtures):
    manifest = import_pet(fixtures / "v2" / "pet.json")
    assert manifest.source_format == "chatgpt-v2"
    assert manifest.frame_geometry.rows == 11
    assert manifest.metadata.get("chatgpt_sprite_version") == 2
    assert manifest.animation_for("wave").row == 3
    assert manifest.animation_for("idle").frames == 6
    assert manifest.animation_for("wave").loop is False
    assert manifest.animation_for("jump").row == 4
    assert manifest.animation_for("jump").loop is False
    assert manifest.look_frame(0) == (9, 0)
    assert manifest.look_frame(15) == (10, 7)
    installed = load_pet(manifest.id)
    assert installed is not None


def test_import_native_package(fixtures):
    manifest = import_pet(fixtures / "native" / "pet.json")
    assert manifest.source_format == "native"
    installed = load_pet(manifest.id)
    assert installed is not None


def test_import_invalid_dimensions_rejected(fixtures):
    with pytest.raises(ImportError_, match="do not match"):
        import_pet(fixtures / "bad" / "wrong-dims.png")


def test_import_oversized_image_rejected(tmp_path, fixtures):
    # Take the V1 sprite and pad it past the 20 MiB limit by appending bytes
    # after the IEND chunk. detect_mime still sees a PNG; the size check
    # rejects it.
    src = fixtures / "v1" / "sprite.png"
    big = tmp_path / "big.png"
    shutil.copyfile(src, big)
    with big.open("ab") as handle:
        handle.write(b"\x00" * (21 * 1024 * 1024))
    with pytest.raises(ImportError_, match="file too large"):
        import_pet(big)


def test_import_invalid_mime_rejected(fixtures):
    with pytest.raises(ImportError_):
        import_pet(fixtures / "bad" / "not-image.txt")


def test_import_corrupt_image_rejected(fixtures):
    with pytest.raises((ImportError_, ValueError)):
        import_pet(fixtures / "bad" / "corrupt.png")


def test_import_path_traversal_manifest_rejected(fixtures):
    # The bad/pet.json has id="../escape"; the manifest should reject it
    # during adapt because the id fails the regex.
    with pytest.raises((ImportError_, ValueError)):
        import_pet(fixtures / "bad" / "pet.json")


def test_preview_does_not_install(fixtures):
    pre = preview(fixtures / "v1" / "sprite.png")
    assert pre.source_format == "chatgpt-v1"
    assert pre.pet_id
    # No pet should be installed yet.
    assert load_pet(pre.pet_id) is None


def test_preview_with_custom_name_and_id(fixtures):
    pre = preview(
        fixtures / "v1" / "sprite.png",
        pet_id="my-custom",
        display_name="My Custom Pet",
    )
    assert pre.pet_id == "my-custom"
    assert pre.display_name == "My Custom Pet"


def test_list_pets_after_import(fixtures):
    import_pet(fixtures / "v1" / "sprite.png")
    import_pet(fixtures / "v2" / "pet.json")
    pets = list_pets()
    assert len(pets) >= 2
    ids = {p.id for p in pets}
    assert any("synthetic" in i or "v1" in i for i in ids)


def test_remove_pet_deletes_only_managed_copy(fixtures, tmp_path):
    src_sprite = fixtures / "v1" / "sprite.png"
    manifest = import_pet(src_sprite)
    # The original source file must still exist.
    assert src_sprite.exists()
    removed = remove_pet(manifest.id)
    assert removed is True
    assert not pet_dir(manifest.id).exists()
    # The original source must STILL exist after removal.
    assert src_sprite.exists()


def test_remove_pet_returns_false_for_missing(fixtures):
    assert remove_pet("does-not-exist") is False


def test_import_copies_source_without_mutating(fixtures):
    src = fixtures / "v1" / "sprite.png"
    size_before = src.stat().st_size
    import_pet(src)
    assert src.stat().st_size == size_before


def test_import_v2_preserves_original_sprite(fixtures):
    src_sprite = fixtures / "v2" / "spritesheet.png"
    size_before = src_sprite.stat().st_size
    import_pet(fixtures / "v2" / "pet.json")
    assert src_sprite.stat().st_size == size_before
