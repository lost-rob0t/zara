from __future__ import annotations

import hashlib
import sys
from pathlib import Path

import pytest

INTEGRATION = Path(__file__).resolve().parents[1] / "android" / "integration"
sys.path.insert(0, str(INTEGRATION))

from device_acceptance import Device  # noqa: E402


PNG_HEADER = b"\x89PNG\r\n\x1a\n"


def test_device_capture_rejects_duplicate_evidence_state_without_overwrite(tmp_path):
    device = Device("emulator-test", tmp_path)
    hierarchy = '<hierarchy><node text="Chat" /></hierarchy>'

    def fake_adb(*arguments, **_kwargs):
        if arguments[:3] == ("exec-out", "screencap", "-p"):
            return PNG_HEADER + b"first"
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return "dumped"
        if arguments[:2] == ("shell", "cat"):
            return hierarchy
        if arguments[:2] == ("shell", "pidof"):
            return "1234"
        if arguments[:2] == ("shell", "logcat"):
            return "captured app log"
        return ""

    device.adb = fake_adb  # type: ignore[method-assign]

    device.capture("same-state")
    first_path = tmp_path / "same-state.png"
    first_bytes = first_path.read_bytes()
    first_hash = hashlib.sha256(first_bytes).hexdigest()

    with pytest.raises(AssertionError, match="duplicate screenshot evidence state"):
        device.capture("same-state")

    assert first_path.read_bytes() == first_bytes
    assert len(device.screenshots) == 1
    evidence = device.screenshots[0]
    assert evidence["state"] == "same-state"
    assert evidence["scenario_id"] == "same-state"
    assert evidence["outcome"] == "captured"
    assert evidence["screenshot"] == {"file": "same-state.png", "sha256": first_hash}
    assert (tmp_path / evidence["ui_hierarchy"]["file"]).read_text() == hierarchy
    assert (tmp_path / evidence["logs"]["file"]).read_text() == "captured app log"
