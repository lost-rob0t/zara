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


def test_await_label_clears_pixel_launcher_anr_before_release_notes(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    events: list[str] = []

    monkeypatch.setattr(
        device,
        "find",
        lambda label: object() if label == "Chat" and events == ["pixel"] else None,
    )
    monkeypatch.setattr(
        device,
        "dismiss_pixel_launcher_anr",
        lambda: events.append("pixel") or True,
    )
    monkeypatch.setattr(
        device,
        "dismiss_release_notes",
        lambda: events.append("release") or False,
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    device.await_label("Chat", timeout=1.0)

    assert events == ["pixel"]


def test_launch_surface_delegates_overlay_recovery_to_await_label(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    events: list[str] = []

    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: events.append("launch") or "",
    )
    monkeypatch.setattr(
        device,
        "dismiss_release_notes",
        lambda: events.append("eager-release-notes") or True,
    )
    monkeypatch.setattr(
        device,
        "await_label",
        lambda label: events.append(f"await:{label}"),
    )

    device.launch_surface("ai.zara.app/.MainActivity", "Chat")

    assert events == ["launch", "await:Chat"]
