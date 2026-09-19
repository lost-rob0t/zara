from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
ORG_ACCEPTANCE = INTEGRATION / "org_device_acceptance.py"


def _load_org_acceptance_module():
    sys.path.insert(0, str(INTEGRATION))
    try:
        spec = importlib.util.spec_from_file_location(
            "zara_org_device_acceptance_test",
            ORG_ACCEPTANCE,
        )
        assert spec is not None
        assert spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.pop(0)


def test_org_saf_control_tap_recovers_only_from_pixel_launcher_anr(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    tap_attempts = 0
    dismiss_attempts = 0

    def tap(label: str) -> None:
        nonlocal tap_attempts
        tap_attempts += 1
        assert label == "Choose Org directory"
        if tap_attempts == 1:
            raise AssertionError(
                "Control is not reachable after scrolling: Choose Org directory"
            )

    def dismiss_launcher_anr() -> bool:
        nonlocal dismiss_attempts
        dismiss_attempts += 1
        return True

    monkeypatch.setattr(device, "tap", tap)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", dismiss_launcher_anr)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    module._tap_app_control(device, "Choose Org directory")

    assert tap_attempts == 2
    assert dismiss_attempts == 1


def test_org_saf_control_tap_does_not_mask_real_missing_control(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)

    monkeypatch.setattr(
        device,
        "tap",
        lambda _label: (_ for _ in ()).throw(
            AssertionError("Control is not reachable after scrolling: Choose Org directory")
        ),
    )
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    with pytest.raises(AssertionError, match="Choose Org directory"):
        module._tap_app_control(device, "Choose Org directory")
