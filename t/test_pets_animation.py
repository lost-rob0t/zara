"""Tests for the AnimationController and pet persistence."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from zara.pets.animation import AnimationController
from zara.pets.manifest import Animation, FrameGeometry, PetManifest
from zara.pets.settings import PetSettings, PetWindowState
from zara.pets.state import PetState


def _manifest() -> PetManifest:
    return PetManifest(
        id="anim-test",
        name="Anim Test",
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


class _FakeClock:
    def __init__(self):
        self.now = 0.0

    def __call__(self):
        return self.now

    def advance(self, seconds):
        self.now += seconds


def test_animation_starts_idle_row_0():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    row, col = ctrl.current_frame()
    assert row == 0
    assert col == 0


def test_animation_frame_advances_with_elapsed_time():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    ctrl.set_state(PetState.RUNNING)
    # 8 fps -> frame duration 0.125s
    clock.advance(0.125)
    assert ctrl.current_frame() == (1, 1)
    clock.advance(0.125)
    assert ctrl.current_frame() == (1, 2)
    clock.advance(0.5)  # 4 more frames
    assert ctrl.current_frame() == (1, 6)


def test_animation_loops_back_to_start():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(1.0)  # 8 frames at 8fps = exactly one loop
    assert ctrl.current_frame() == (1, 0)


def test_non_loop_animation_holds_last_frame():
    manifest = _manifest()
    # Override running animation to non-looping
    manifest.animations[1] = Animation(name="running", row=1, frames=4, fps=8.0, loop=False)
    clock = _FakeClock()
    ctrl = AnimationController(manifest, clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(10.0)  # well past 4 frames
    row, col = ctrl.current_frame()
    assert row == 1
    assert col == 3  # last frame held


def test_set_state_changes_animation_row():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    assert ctrl.set_state(PetState.RUNNING) is True
    assert ctrl.current_frame()[0] == 1
    assert ctrl.set_state(PetState.BLOCKED) is True
    assert ctrl.current_frame()[0] == 5


def test_repeated_identical_state_does_not_restart():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(0.25)  # frame 2
    assert ctrl.current_frame() == (1, 2)
    # Re-set the same state; the timeline must NOT reset.
    assert ctrl.set_state(PetState.RUNNING) is False
    assert ctrl.current_frame() == (1, 2)


def test_reduced_motion_renders_static_frame():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), reduced_motion=True, clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(10.0)
    assert ctrl.current_frame() == (1, 0)


def test_reduced_motion_can_be_toggled_on():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    ctrl.set_state(PetState.RUNNING)
    clock.advance(0.5)
    assert ctrl.current_frame() != (1, 0)
    ctrl.set_reduced_motion(True)
    assert ctrl.current_frame() == (1, 0)


def test_frame_changed_reports_transitions():
    clock = _FakeClock()
    ctrl = AnimationController(_manifest(), clock=clock)
    ctrl.set_state(PetState.RUNNING)
    assert ctrl.frame_changed()  # first observation
    assert ctrl.frame_changed() is False  # same frame
    clock.advance(0.125)
    assert ctrl.frame_changed() is True


def test_dispose_clears_state():
    ctrl = AnimationController(_manifest())
    ctrl.dispose()
    assert ctrl._animation is None


def test_settings_round_trip(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    settings.update(
        selected_pet="custom",
        x=200,
        y=300,
        scale=1.5,
        reduced_motion="on",
        enabled=True,
        monitor_key="monitor-1",
    )
    settings.save()
    loaded = PetSettings(path=path)
    assert loaded.state.selected_pet == "custom"
    assert loaded.state.x == 200
    assert loaded.state.y == 300
    assert loaded.state.scale == 1.5
    assert loaded.state.reduced_motion == "on"
    assert loaded.state.enabled is True
    assert loaded.state.monitor_key == "monitor-1"


def test_settings_defaults_when_missing(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    assert settings.state.enabled is False
    assert settings.state.selected_pet == "zara-default"
    assert settings.state.scale == 1.0
    assert settings.state.reduced_motion == "system"


def test_settings_recovers_from_corrupt_file(tmp_path):
    path = tmp_path / "pet-state.json"
    path.write_text("{not json")
    settings = PetSettings(path=path)
    assert settings.state.enabled is False


def test_settings_partial_state_round_trips(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    settings.update(x=None, y=None, enabled=True)
    settings.save()
    data = json.loads(path.read_text())
    assert data["enabled"] is True
    assert data["x"] is None
    assert data["y"] is None


def test_position_persistence_round_trip(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    settings.update(x=42, y=99)
    settings.save()
    loaded = PetSettings(path=path)
    assert loaded.state.x == 42
    assert loaded.state.y == 99


def test_scale_persistence_round_trip(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    settings.update(scale=2.5)
    settings.save()
    loaded = PetSettings(path=path)
    assert loaded.state.scale == 2.5


def test_selected_pet_persistence(tmp_path):
    path = tmp_path / "pet-state.json"
    settings = PetSettings(path=path)
    settings.update(selected_pet="my-imported-pet")
    settings.save()
    loaded = PetSettings(path=path)
    assert loaded.state.selected_pet == "my-imported-pet"


def test_pet_window_state_from_dict_handles_none():
    state = PetWindowState.from_dict({})
    assert state.x is None
    assert state.y is None
    assert state.scale == 1.0