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
