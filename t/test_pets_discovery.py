"""Tests for reduced-motion behavior, discovery, and deletion safety."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

from zara.pets.animation import AnimationController
from zara.pets.discovery import ChatGPTPetDiscovery
from zara.pets.importer import import_pet
from zara.pets.manifest import Animation, FrameGeometry, PetManifest
from zara.pets.state import PetState
from zara.pets.storage import pet_dir, remove_pet

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


def _manifest() -> PetManifest:
    return PetManifest(
        id="rm-test",
        name="RM Test",
        source_format="native",
        sprite_asset="spritesheet.png",
        frame_geometry=FrameGeometry(192, 208, 8, 9),
        animations=[
            Animation(name="idle", row=0, frames=8, fps=8.0, loop=True),
            Animation(name="running", row=1, frames=8, fps=8.0, loop=True),
            Animation(name="needs-input", row=6, frames=8, fps=8.0, loop=True),
            Animation(name="ready", row=8, frames=8, fps=8.0, loop=True),
            Animation(name="blocked", row=5, frames=8, fps=8.0, loop=True),
        ],
    )


class _FakeClock:
    def __init__(self):
        self.now = 0.0

    def __call__(self):
        return self.now

    def advance(self, seconds):
        self.now += seconds


def test_reduced_motion_renders_static_frame_for_running():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), reduced_motion=True, clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(10.0)
    assert ctrl.current_frame() == (1, 0)


def test_reduced_motion_renders_static_frame_for_each_state():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), reduced_motion=True, clock=clock)
    for state, expected_row in [
        (PetState.IDLE, 0), (PetState.RUNNING, 1), (PetState.NEEDS_INPUT, 6),
        (PetState.READY, 8), (PetState.BLOCKED, 5),
    ]:
        ctrl.set_state(state)
        clock.advance(10.0)
        row, col = ctrl.current_frame()
        assert row == expected_row
        assert col == 0


def test_reduced_motion_toggled_off_resumes_animation():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), reduced_motion=True, clock=clock)
    ctrl.set_state(PetState.RUNNING)
    ctrl.set_reduced_motion(False)
    clock.advance(0.25)
    row, col = ctrl.current_frame()
    assert row == 1
    assert col == 2


def test_discovery_finds_installed_chatgpt_v2(tmp_path, monkeypatch, fixtures):
    # Simulate a ChatGPT install location under XDG_CONFIG_HOME.
    chatgpt_dir = tmp_path / "config" / "ChatGPT" / "pets" / "synthetic-v2"
    chatgpt_dir.mkdir(parents=True)
    (chatgpt_dir / "pet.json").write_text(
        (fixtures / "v2" / "pet.json").read_text()
    )
    (chatgpt_dir / "spritesheet.png").write_bytes(
        (fixtures / "v2" / "spritesheet.png").read_bytes()
    )
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "config"))
    monkeypatch.setenv("HOME", str(tmp_path))
    discovered = ChatGPTPetDiscovery().discover()
    ids = [d.pet_id for d in discovered]
    assert any("synthetic" in i for i in ids)


def test_discovery_is_read_only(tmp_path, monkeypatch, fixtures):
    chatgpt_dir = tmp_path / "config" / "ChatGPT" / "pets" / "synthetic-v2"
    chatgpt_dir.mkdir(parents=True)
    (chatgpt_dir / "pet.json").write_text(
        (fixtures / "v2" / "pet.json").read_text()
    )
    sprite = fixtures / "v2" / "spritesheet.png"
    (chatgpt_dir / "spritesheet.png").write_bytes(sprite.read_bytes())
    size_before = (chatgpt_dir / "spritesheet.png").stat().st_size
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "config"))
    monkeypatch.setenv("HOME", str(tmp_path))
    discovered = ChatGPTPetDiscovery().discover()
    assert discovered
    # Discovery must not modify the source files.
    assert (chatgpt_dir / "pet.json").exists()
    assert (chatgpt_dir / "spritesheet.png").stat().st_size == size_before


def test_discovery_handles_missing_dirs(monkeypatch, tmp_path):
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "empty"))
    assert ChatGPTPetDiscovery().discover() == []


def test_discovery_rejects_malformed(tmp_path, monkeypatch, fixtures):
    chatgpt_dir = tmp_path / "config" / "ChatGPT" / "pets" / "broken"
    chatgpt_dir.mkdir(parents=True)
    (chatgpt_dir / "pet.json").write_text("{not json")
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "config"))
    monkeypatch.setenv("HOME", str(tmp_path))
    discovered = ChatGPTPetDiscovery().discover()
    assert discovered == []


def test_deletion_preserves_original_source_asset(fixtures, tmp_path):
    src = fixtures / "v1" / "sprite.png"
    manifest = import_pet(src)
    assert src.exists()
    remove_pet(manifest.id)
    assert src.exists()
    assert not pet_dir(manifest.id).exists()


def test_deletion_preserves_chatgpt_v2_source(fixtures, tmp_path):
    src_dir = fixtures / "v2"
    pet_json = src_dir / "pet.json"
    sprite = src_dir / "spritesheet.png"
    size_before_json = pet_json.stat().st_size
    size_before_sprite = sprite.stat().st_size
    manifest = import_pet(pet_json)
    remove_pet(manifest.id)
    assert pet_json.stat().st_size == size_before_json
    assert sprite.stat().st_size == size_before_sprite