from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_overlay_order_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_release_notes_clear_pixel_launcher_anr_before_probing_zara_dialog(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    events: list[str] = []

    monkeypatch.setattr(
        device,
        "dismiss_pixel_launcher_anr",
        lambda: events.append("pixel") or True,
    )
    monkeypatch.setattr(
        device,
        "find_contains",
        lambda _fragment: events.append("release-probe") or None,
    )

    assert device.dismiss_release_notes() is False
    assert events == ["pixel"]


def test_release_notes_probe_zara_dialog_after_system_overlay_is_clear(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    release_notes = module.ET.fromstring(
        '<node text="What\'s new in Zara 0.2.2-alpha" bounds="[10,10][500,90]" />'
    )
    continue_button = module.ET.fromstring(
        '<node text="Continue" bounds="[500,1500][700,1600]" />'
    )
    events: list[str] = []

    monkeypatch.setattr(
        device,
        "dismiss_pixel_launcher_anr",
        lambda: events.append("pixel") or False,
    )
    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: (
            events.append("release-probe") or release_notes
            if fragment == "What's new in Zara "
            else None
        ),
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: continue_button if label == "Continue" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: events.append("tap") or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_release_notes() is True
    assert events == ["pixel", "release-probe", "tap"]
