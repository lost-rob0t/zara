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


def test_org_saf_control_tap_clears_pixel_launcher_anr_then_taps_control(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    dismiss_results = iter((True, False))
    adb_calls: list[tuple[str, ...]] = []
    monotonic_values = iter((0.0, 0.0, 0.1))
    control = object()

    monkeypatch.setattr(
        device,
        "dismiss_pixel_launcher_anr",
        lambda: next(dismiss_results),
    )
    monkeypatch.setattr(device, "find", lambda label: control if label == "Choose Org directory" else None)
    monkeypatch.setattr(device, "bounds", lambda _node: (10, 20, 90, 100))
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "monotonic", lambda: next(monotonic_values))
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    module._tap_app_control_through_launcher_anr(
        device,
        "Choose Org directory",
        timeout=1.0,
    )

    assert adb_calls == [("shell", "input", "tap", "50", "60")]


def test_org_saf_control_tap_does_not_mask_real_missing_control(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    monotonic_values = iter((0.0, 0.0, 2.0))

    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(module.time, "monotonic", lambda: next(monotonic_values))
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(
        AssertionError,
        match="Control is not reachable after launcher ANR recovery: Choose Org directory",
    ):
        module._tap_app_control_through_launcher_anr(
            device,
            "Choose Org directory",
            timeout=1.0,
        )
