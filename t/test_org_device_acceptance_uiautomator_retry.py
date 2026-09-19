from __future__ import annotations

import importlib.util
from pathlib import Path
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
ORG_DEVICE_ACCEPTANCE = INTEGRATION / "org_device_acceptance.py"


def _load_org_device_acceptance_module():
    module_name = "zara_org_device_acceptance_uiautomator_retry_test"
    sys.path.insert(0, str(INTEGRATION))
    try:
        spec = importlib.util.spec_from_file_location(module_name, ORG_DEVICE_ACCEPTANCE)
        assert spec is not None
        assert spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.remove(str(INTEGRATION))


def test_org_evidence_retries_transient_missing_uiautomator_hierarchy(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_device_acceptance_module()
    calls = 0
    sleeps: list[float] = []
    sentinel = object()

    def flaky_nodes(_device):
        nonlocal calls
        calls += 1
        if calls < 3:
            raise AssertionError(
                "UIAutomator did not create /data/local/tmp/zara-acceptance.xml: "
                "no uiautomator diagnostic"
            )
        return iter((sentinel,))

    monkeypatch.setattr(module.Device, "nodes", flaky_nodes)
    monkeypatch.setattr(module.time, "sleep", sleeps.append)
    device = module.OrgEvidenceDevice("emulator-5554", tmp_path)

    assert list(device.nodes()) == [sentinel]
    assert calls == 3
    assert sleeps == [
        module.UIAUTOMATOR_RETRY_DELAY_SECONDS,
        module.UIAUTOMATOR_RETRY_DELAY_SECONDS,
    ]


def test_org_evidence_uiautomator_retry_is_bounded_and_fail_closed(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_device_acceptance_module()
    calls = 0

    def missing_nodes(_device):
        nonlocal calls
        calls += 1
        raise AssertionError(
            "UIAutomator did not create /data/local/tmp/zara-acceptance.xml: "
            "no uiautomator diagnostic"
        )

    monkeypatch.setattr(module.Device, "nodes", missing_nodes)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    device = module.OrgEvidenceDevice("emulator-5554", tmp_path)

    with pytest.raises(AssertionError, match=r"UIAutomator did not create"):
        list(device.nodes())

    assert calls == module.UIAUTOMATOR_RETRY_ATTEMPTS


def test_org_evidence_does_not_retry_unrelated_acceptance_failures(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_org_device_acceptance_module()
    calls = 0

    def unrelated_failure(_device):
        nonlocal calls
        calls += 1
        raise AssertionError("Control is not reachable: Use this folder")

    monkeypatch.setattr(module.Device, "nodes", unrelated_failure)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    device = module.OrgEvidenceDevice("emulator-5554", tmp_path)

    with pytest.raises(AssertionError, match=r"Control is not reachable"):
        list(device.nodes())

    assert calls == 1


def test_org_evidence_navigates_to_fixture_root_before_accepting_saf(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _load_org_device_acceptance_module()
    events: list[tuple] = []

    class FakeDevice:
        def adb(self, *arguments, **_kwargs):
            events.append(("adb", *arguments))

        def launch_surface(self, component, label):
            events.append(("launch", component, label))

        def await_contains(self, fragment, timeout=20.0):
            events.append(("await", fragment, timeout))

        def tap(self, label):
            events.append(("tap", label))

    monkeypatch.setattr(
        module,
        "_tap_contains",
        lambda _device, fragment: events.append(("tap_contains", fragment)),
    )

    module.connect_fixture_through_saf(FakeDevice())

    assert events == [
        ("adb", "shell", "am", "force-stop", module.PACKAGE),
        ("launch", module.COMPONENT, "Org"),
        ("await", "Shared Org workspace is unavailable", 20.0),
        ("tap", "Choose Org directory"),
        ("await", "Documents", 20.0),
        ("tap_contains", "Documents"),
        ("await", "ZaraOrgAcceptance", 20.0),
        ("tap_contains", "ZaraOrgAcceptance"),
        ("await", module.SAF_PICKER_CONFIRM_LABEL, 20.0),
        ("tap_contains", module.SAF_PICKER_CONFIRM_LABEL),
        ("await", module.SAF_PICKER_ALLOW_LABEL, 20.0),
        ("tap_contains", module.SAF_PICKER_ALLOW_LABEL),
        ("await", "Acceptance task", 20.0),
    ]
