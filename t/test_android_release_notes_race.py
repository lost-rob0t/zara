from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_release_notes_race_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_await_label_dismisses_release_notes_that_appear_after_launch(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    chat = module.ET.fromstring('<node text="Chat" bounds="[0,0][100,100]" />')
    state = {"dismissed": False}
    monotonic_values = iter((0.0, 0.0, 0.5, 1.1))

    monkeypatch.setattr(
        device,
        "find",
        lambda label: chat if label == "Chat" and state["dismissed"] else None,
    )

    def dismiss_release_notes() -> bool:
        state["dismissed"] = True
        return True

    monkeypatch.setattr(device, "dismiss_release_notes", dismiss_release_notes)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)
    monkeypatch.setattr(module.time, "monotonic", lambda: next(monotonic_values))
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.await_label("Chat", timeout=1.0)

    assert state["dismissed"] is True


def test_await_label_dismisses_launcher_anr_before_release_notes(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    chat = module.ET.fromstring('<node text="Chat" bounds="[0,0][100,100]" />')
    state = {"anr": True, "release_notes": True}

    monkeypatch.setattr(
        device,
        "find",
        lambda label: (
            chat
            if label == "Chat" and not state["anr"] and not state["release_notes"]
            else None
        ),
    )

    def dismiss_launcher_anr() -> bool:
        if not state["anr"]:
            return False
        state["anr"] = False
        return True

    def dismiss_release_notes() -> bool:
        assert not state["anr"], "release notes must not be inspected through an OS ANR overlay"
        if not state["release_notes"]:
            return False
        state["release_notes"] = False
        return True

    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", dismiss_launcher_anr)
    monkeypatch.setattr(device, "dismiss_release_notes", dismiss_release_notes)
    monkeypatch.setattr(module.time, "monotonic", lambda: 0.0)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.await_label("Chat", timeout=1.0)

    assert state == {"anr": False, "release_notes": False}


def test_launch_surface_dismisses_launcher_anr_before_release_notes(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    events: list[str] = []

    monkeypatch.setattr(device, "adb", lambda *_args, **_kwargs: "")

    def dismiss_launcher_anr() -> bool:
        events.append("anr")
        return True

    def dismiss_release_notes() -> bool:
        assert events == ["anr"], "launcher ANR must be cleared before release notes"
        events.append("release-notes")
        return True

    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", dismiss_launcher_anr)
    monkeypatch.setattr(device, "dismiss_release_notes", dismiss_release_notes)
    monkeypatch.setattr(
        device,
        "await_label",
        lambda label: events.append(f"await:{label}"),
    )

    device.launch_surface("ai.zara.app/.MainActivity", "Chat")

    assert events == ["anr", "release-notes", "await:Chat"]


def test_dismiss_release_notes_accepts_wrapped_title_semantics(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    title = module.ET.fromstring(
        '<node text="What\'s new in Zara&#10;0.2.2-alpha" bounds="[0,0][200,80]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[10,20][110,80]" />'
    )
    taps: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: iter((title, continue_button)))

    def adb(*args: str, **_kwargs):
        taps.append(args)
        return ""

    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes() is True
    assert taps == [("shell", "input", "tap", "60", "50")]
