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


def test_dismiss_release_notes_taps_continue_for_visible_changelog(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    title = module.ET.fromstring(
        '<node text="What&apos;s new in Zara 0.2.2-alpha" bounds="[0,0][100,40]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[20,40][100,80]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: title if fragment == "What's new in Zara " else None,
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: continue_button if label == "Continue" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes() is True
    assert adb_calls == [("shell", "input", "tap", "60", "60")]


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
