from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"
PNG_FIXTURE = b"\x89PNG\r\n\x1a\n" + b"w10-rendered-state"
SOURCE_SHA = "a" * 40
APK_SHA256 = "b" * 64


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_scenario_evidence_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _device(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    device.source_sha = SOURCE_SHA
    device.apk_sha256 = APK_SHA256
    device.device_api = "35"
    device.current_profile = "default"
    device.current_route = "chat"
    device.runtime_evidence = {
        "mode": None,
        "runtime_id": None,
        "model": None,
        "quantization": None,
        "phase": None,
    }
    hierarchy = (
        '<hierarchy rotation="0">'
        '<node text="Chat" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" />'
        '<node text="" content-desc="Open navigation menu" class="android.widget.Button" '
        'enabled="true" clickable="true" selected="false" focused="false" '
        'bounds="[0,0][48,48]" />'
        "</hierarchy>"
    )

    def fake_adb(*arguments: str, binary: bool = False):
        if arguments[:2] == ("exec-out", "screencap"):
            assert binary is True
            return PNG_FIXTURE
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(device, "_hierarchy_text", lambda: hierarchy)
    return module, device


def test_capture_emits_exact_source_scenario_bundle(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _module, device = _device(monkeypatch, tmp_path)
    device.record_action("tap:Open navigation menu")
    device.record_assertion("chat-visible", passed=True, detail="Chat is visible")

    screenshot = device.capture("contract-state")

    assert screenshot == tmp_path / "contract-state.png"
    assert len(device.scenario_evidence) == 1
    record = device.scenario_evidence[0]
    assert record["scenario_id"] == "android.ui.contract-state"
    assert record["source_sha"] == SOURCE_SHA
    assert record["apk_sha256"] == APK_SHA256
    assert record["device_api"] == "35"
    assert record["profile"] == "default"
    assert record["route"] == "chat"
    assert record["runtime"] == {
        "mode": None,
        "runtime_id": None,
        "model": None,
        "quantization": None,
        "phase": None,
    }
    assert record["actions"] == [
        "tap:Open navigation menu",
        "capture:contract-state",
    ]
    assert record["assertions"] == [
        {"name": "chat-visible", "passed": True, "detail": "Chat is visible"},
        {
            "name": "screenshot-png",
            "passed": True,
            "detail": "device returned PNG screenshot evidence",
        },
    ]

    for key in ("screenshot", "text_evidence", "assertion_evidence"):
        evidence = record[key]
        path = tmp_path / evidence["file"]
        assert path.is_file()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == evidence["sha256"]

    persisted = json.loads((tmp_path / "contract-state.json").read_text(encoding="utf-8"))
    assert persisted == record
    ui_text = (tmp_path / "contract-state.ui.txt").read_text(encoding="utf-8")
    assert 'route="chat"' in ui_text
    assert "runtime=" in ui_text
    assert "Open navigation menu" in ui_text
    assertion_text = (tmp_path / "contract-state.assertions.txt").read_text(
        encoding="utf-8"
    )
    assert "ACTION 1 tap:Open navigation menu" in assertion_text
    assert "ACTION 2 capture:contract-state" in assertion_text
    assert "ASSERT PASS chat-visible Chat is visible" in assertion_text
    assert (
        "ASSERT PASS screenshot-png device returned PNG screenshot evidence"
        in assertion_text
    )


def test_capture_rejects_duplicate_scenario_without_overwriting_first_bundle(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _module, device = _device(monkeypatch, tmp_path)
    device.record_action("first-action")
    device.record_assertion("first-assertion", passed=True, detail="first")
    device.capture("duplicate-state")
    original = {
        suffix: (tmp_path / f"duplicate-state{suffix}").read_bytes()
        for suffix in (".png", ".ui.txt", ".assertions.txt", ".json")
    }

    device.record_action("second-action")
    with pytest.raises(AssertionError, match="Duplicate rendered-state scenario"):
        device.capture("duplicate-state")

    for suffix, expected in original.items():
        assert (tmp_path / f"duplicate-state{suffix}").read_bytes() == expected
    assert len(device.scenario_evidence) == 1


def test_normalized_ui_text_is_deterministic_across_node_order() -> None:
    module = _load_device_acceptance_module()
    node_a = (
        '<node text="Chat" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" />'
    )
    node_b = (
        '<node text="" content-desc="Open navigation menu" class="android.widget.Button" '
        'enabled="true" clickable="true" selected="false" focused="false" '
        'bounds="[0,0][48,48]" />'
    )
    first = f"<hierarchy>{node_a}{node_b}</hierarchy>"
    second = f"<hierarchy>{node_b}{node_a}</hierarchy>"

    normalized_first = module.normalized_ui_text(first)
    normalized_second = module.normalized_ui_text(second)

    assert normalized_first == normalized_second
    assert "android.widget.Button" in normalized_first
    assert "Open navigation menu" in normalized_first
    assert "clickable=true" in normalized_first
    assert "selected=true" in normalized_first
    assert "[20,40][180,96]" in normalized_first


def test_typed_input_action_trace_records_only_length_not_fixture_text(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _module, device = _device(monkeypatch, tmp_path)
    fixture_text = "private_fixture_123"

    device.type_text(fixture_text)
    device.record_assertion("composer-visible", passed=True, detail="composer retained input")
    device.capture("typed-input")

    record = device.scenario_evidence[0]
    serialized = json.dumps(record, sort_keys=True)
    assertion_text = (tmp_path / "typed-input.assertions.txt").read_text(encoding="utf-8")
    assert fixture_text not in serialized
    assert fixture_text not in assertion_text
    assert f"type_text:length={len(fixture_text)}" in record["actions"]


def test_failed_assertion_can_be_retained_in_failure_scenario_bundle(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _module, device = _device(monkeypatch, tmp_path)
    device.record_action("recreate-process")
    device.record_assertion(
        "restart-postcondition",
        passed=False,
        detail="expected durable state was absent",
    )

    device.capture("failure")

    record = device.scenario_evidence[0]
    assert record["scenario_id"] == "android.ui.failure"
    assert record["assertions"] == [
        {
            "name": "restart-postcondition",
            "passed": False,
            "detail": "expected durable state was absent",
        },
        {
            "name": "screenshot-png",
            "passed": True,
            "detail": "device returned PNG screenshot evidence",
        },
    ]
    assert (tmp_path / "failure.png").is_file()
    assert (tmp_path / "failure.ui.txt").is_file()
    assert (tmp_path / "failure.assertions.txt").is_file()
    assert (tmp_path / "failure.json").is_file()


def test_capture_rejects_ui_change_across_screenshot_boundary_without_persisting_bundle(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _module, device = _device(monkeypatch, tmp_path)
    before = (
        '<hierarchy><node text="Chat" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" /></hierarchy>'
    )
    after = (
        '<hierarchy><node text="Settings" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" /></hierarchy>'
    )
    hierarchies = iter((before, after))
    monkeypatch.setattr(device, "_hierarchy_text", lambda: next(hierarchies))

    with pytest.raises(AssertionError, match="UI changed while screenshot evidence was captured"):
        device.capture("unstable-state")

    assert device.scenario_evidence == []
    assert device.screenshots == []
    assert device._captured_scenarios == set()
    for suffix in (".png", ".ui.txt", ".assertions.txt", ".json"):
        assert not (tmp_path / f"unstable-state{suffix}").exists()
