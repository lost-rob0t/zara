from __future__ import annotations

import hashlib
import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"
PNG_FIXTURE = b"\x89PNG\r\n\x1a\n" + b"w10-text-twin-trace"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_text_twin_trace_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _prepared_device(module, monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    device = module.Device("emulator-5554", tmp_path)
    device.source_sha = "a" * 40
    device.apk_sha256 = "b" * 64
    device.device_api = "35"
    device.current_profile = "default"
    device.current_route = "chat"
    device.runtime_evidence = {
        "mode": "local",
        "runtime_id": "local-zara-server",
        "model": None,
        "quantization": None,
        "phase": "ready",
    }
    hierarchy = (
        '<hierarchy rotation="0">'
        '<node text="Chat" content-desc="" class="android.widget.TextView" '
        'enabled="true" clickable="false" selected="true" focused="false" '
        'bounds="[20,40][180,96]" />'
        "</hierarchy>"
    )

    def fake_adb(*arguments: str, binary: bool = False):
        if arguments[:2] == ("exec-out", "screencap"):
            assert binary is True
            return PNG_FIXTURE
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(device, "_hierarchy_text", lambda: hierarchy)
    return device


def test_text_twin_contains_bounded_action_and_assertion_trace(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = _prepared_device(module, monkeypatch, tmp_path)

    device.record_action("tap:Runtime")
    device.record_assertion("local-ready", passed=True, detail="runtime phase is ready")
    device.capture("local-ready")

    text_twin = (tmp_path / "local-ready.ui.txt").read_text(encoding="utf-8")
    assert "ACTION 1 tap:Runtime" in text_twin
    assert "ACTION 2 capture:local-ready" in text_twin
    assert "ASSERT PASS local-ready runtime phase is ready" in text_twin
    assert "ASSERT PASS screenshot-png device returned PNG screenshot evidence" in text_twin


def test_text_twin_and_hash_track_late_visual_assertion(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = _prepared_device(module, monkeypatch, tmp_path)
    device.capture("overflow")
    record = device.scenario_evidence[0]
    original_hash = record["text_evidence"]["sha256"]

    device._append_scenario_assertion(
        "overflow",
        assertion_name="transient-surface-visible",
        detail="actions=Pin,Rename,Move to project viewport=1080x2400",
    )

    text_path = tmp_path / "overflow.ui.txt"
    text_twin = text_path.read_text(encoding="utf-8")
    assert (
        "ASSERT PASS transient-surface-visible "
        "actions=Pin,Rename,Move to project viewport=1080x2400"
    ) in text_twin
    assert record["text_evidence"]["sha256"] == hashlib.sha256(text_path.read_bytes()).hexdigest()
    assert record["text_evidence"]["sha256"] != original_hash
