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
    payloads = iter((PNG_HEADER + b"first", PNG_HEADER + b"second"))
    device.adb = lambda *_args, **_kwargs: next(payloads)  # type: ignore[method-assign]

    device.capture("same-state")
    first_path = tmp_path / "same-state.png"
    first_bytes = first_path.read_bytes()
    first_hash = hashlib.sha256(first_bytes).hexdigest()

    with pytest.raises(AssertionError, match="duplicate screenshot evidence state"):
        device.capture("same-state")

    assert first_path.read_bytes() == first_bytes
    assert device.screenshots == [
        {
            "state": "same-state",
            "file": "same-state.png",
            "sha256": first_hash,
        }
    ]
