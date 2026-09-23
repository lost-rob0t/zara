from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_overlay_test", DEVICE_ACCEPTANCE
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_release_notes_survive_pixel_launcher_anr_overlay(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][500,90]" />'
    )
    launcher_anr = module.ET.fromstring(
        '<node text="Pixel Launcher isn\'t responding" bounds="[10,10][90,90]" />'
    )
    wait = module.ET.fromstring('<node text="Wait" bounds="[20,30][80,70]" />')
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    state = {"launcher_anr": True}
    adb_calls: list[tuple[str, ...]] = []
    monotonic_values = iter((0.0, 0.0, 0.5, 1.1))

    def find_contains(fragment: str):
        if fragment == "What's new in Zara ":
            return release_notes
        if fragment == "Pixel Launcher isn't responding" and state["launcher_anr"]:
            return launcher_anr
        return None

    def find(label: str):
        if label == "Wait" and state["launcher_anr"]:
            return wait
        if label == "Continue" and not state["launcher_anr"]:
            return continue_button
        return None

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments == ("shell", "input", "tap", "50", "50"):
            state["launcher_anr"] = False
        return ""

    monkeypatch.setattr(device, "find_contains", find_contains)
    monkeypatch.setattr(device, "find", find)
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "monotonic", lambda: next(monotonic_values))
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes(timeout=1.0) is True
    assert adb_calls == [
        ("shell", "input", "tap", "50", "50"),
        ("shell", "input", "tap", "600", "1550"),
    ]


def test_await_label_dismisses_pixel_launcher_anr_before_accepting_background_label(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    chat = module.ET.fromstring('<node text="Chat" bounds="[10,10][100,80]" />')
    launcher_anr = module.ET.fromstring(
        '<node text="Pixel Launcher isn\'t responding" bounds="[10,10][90,90]" />'
    )
    wait = module.ET.fromstring('<node text="Wait" bounds="[20,30][80,70]" />')
    state = {"launcher_anr": True}
    adb_calls: list[tuple[str, ...]] = []

    def find_contains(fragment: str):
        if fragment == "Pixel Launcher isn't responding" and state["launcher_anr"]:
            return launcher_anr
        return None

    def find(label: str):
        if label == "Wait" and state["launcher_anr"]:
            return wait
        if label == "Chat":
            return chat
        return None

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments == ("shell", "input", "tap", "50", "50"):
            state["launcher_anr"] = False
        return ""

    monkeypatch.setattr(device, "find_contains", find_contains)
    monkeypatch.setattr(device, "find", find)
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(device, "dismiss_release_notes", lambda: False)
    monkeypatch.setattr(module.time, "monotonic", lambda: 0.0)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.await_label("Chat", timeout=1.0)

    assert state["launcher_anr"] is False
    assert adb_calls == [("shell", "input", "tap", "50", "50")]
