from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_release_notes_regression",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_release_notes_fallback_accepts_continue_plus_changelog_section(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    added_heading = module.ET.fromstring(
        '<node text="Added" bounds="[120,460][280,520]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)

    def fake_find(label: str):
        if label == "Continue":
            return continue_button
        if label == "Added":
            return added_heading
        return None

    monkeypatch.setattr(device, "find", fake_find)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes() is True
    assert adb_calls == [("shell", "input", "tap", "600", "1550")]


def test_release_notes_fallback_does_not_dismiss_unrelated_continue(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    monkeypatch.setattr(
        device,
        "find",
        lambda label: continue_button if label == "Continue" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )

    assert device.dismiss_release_notes() is False
    assert adb_calls == []
