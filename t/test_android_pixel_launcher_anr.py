from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_pixel_anr_test", DEVICE_ACCEPTANCE
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_pixel_launcher_anr_closes_hung_launcher_instead_of_waiting(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    launcher_anr = module.ET.fromstring(
        '<node text="Pixel Launcher isn\'t responding" bounds="[10,10][90,90]" />'
    )
    close_app = module.ET.fromstring(
        '<node text="Close app" bounds="[20,30][80,70]" />'
    )
    wait = module.ET.fromstring('<node text="Wait" bounds="[20,80][80,120]" />')
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: launcher_anr
        if fragment == "Pixel Launcher isn't responding"
        else None,
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: close_app if label == "Close app" else wait if label == "Wait" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_pixel_launcher_anr() is True
    assert adb_calls == [("shell", "input", "tap", "50", "50")]


def test_non_launcher_anr_is_never_dismissed(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )

    assert device.dismiss_pixel_launcher_anr() is False
    assert adb_calls == []
