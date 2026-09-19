from __future__ import annotations

import importlib.util
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_release_notes_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_surface_wait_dismisses_only_zara_release_notes(monkeypatch, tmp_path: Path) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][200,90]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[20,30][100,70]" />'
    )
    chat = module.ET.fromstring('<node text="Chat" bounds="[20,30][100,70]" />')
    state = {"dismissed": False}
    adb_calls: list[tuple[str, ...]] = []

    def find(label: str):
        if label == "Chat" and state["dismissed"]:
            return chat
        if label == "Continue" and not state["dismissed"]:
            return continue_button
        return None

    def find_contains(fragment: str):
        if fragment == "What's new in Zara " and not state["dismissed"]:
            return release_notes
        return None

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments == ("shell", "input", "tap", "60", "50"):
            state["dismissed"] = True
        return ""

    monkeypatch.setattr(device, "find", find)
    monkeypatch.setattr(device, "find_contains", find_contains)
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.await_label("Chat", timeout=0.5)

    assert state["dismissed"] is True
    assert adb_calls == [("shell", "input", "tap", "60", "50")]

    state["dismissed"] = False
    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    adb_calls.clear()
    assert device.dismiss_release_notes() is False
    assert adb_calls == []
