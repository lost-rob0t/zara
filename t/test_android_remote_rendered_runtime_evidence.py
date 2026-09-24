from __future__ import annotations

import importlib
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
RUNTIME_FIELDS = ("mode", "runtime_id", "model", "quantization", "phase")


@pytest.fixture
def remote_acceptance(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.syspath_prepend(str(INTEGRATION))
    sys.modules.pop("device_remote_acceptance", None)
    return importlib.import_module("device_remote_acceptance")


class FakeDevice:
    def __init__(self) -> None:
        self.runtime_evidence = {field: None for field in RUNTIME_FIELDS}
        self.captures: dict[str, dict[str, str | None]] = {}

    def adb(self, *arguments: str, binary: bool = False):
        assert binary is False
        return ""

    def start(self) -> None:
        return None

    def tap_tab(self, label: str) -> None:
        return None

    def await_contains(self, fragment: str, timeout: float = 20.0) -> None:
        return None

    def await_label(self, label: str, timeout: float = 20.0) -> None:
        return None

    def tap(self, label: str) -> None:
        return None

    def press_back(self) -> None:
        return None

    def type_text(self, text: str) -> None:
        return None

    def capture(self, name: str) -> None:
        self.captures[name] = dict(self.runtime_evidence)


def test_remote_rendered_states_bind_runtime_identity_before_capture(
    remote_acceptance,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    device = FakeDevice()
    fixture = {
        "endpoint": "tcp://127.0.0.1:5555",
        "server_public": "S" * 40,
        "security_admin_path": "/tmp/unused.sock",
        "acceptance_host": "127.0.0.1",
        "acceptance_port": "5556",
    }

    monkeypatch.setattr(remote_acceptance, "require_reverse_mapping", lambda *_: "tcp:5555 tcp:5555")
    monkeypatch.setattr(remote_acceptance, "open_menu", lambda *_: None)
    monkeypatch.setattr(remote_acceptance, "type_printable_ascii", lambda *_: None)
    monkeypatch.setattr(remote_acceptance, "find_curve_public_key", lambda *_: "C" * 40)
    monkeypatch.setattr(remote_acceptance, "enroll_live_server", lambda *_: None)
    monkeypatch.setattr(
        remote_acceptance,
        "visible_device_text",
        lambda *_: [fixture["server_public"]],
    )
    monkeypatch.setattr(remote_acceptance, "signal_turn_acceptance", lambda *_: None)

    result = remote_acceptance.exercise_remote_connection(device, fixture)

    assert result["local_turn_completed"] is True
    assert result["remote_turn_completed"] is True
    assert device.captures["local-text-turn"] == {
        "mode": "local",
        "runtime_id": "local-zara-server",
        "model": None,
        "quantization": None,
        "phase": "ready",
    }
    assert device.captures["remote-connected"] == {
        "mode": "local",
        "runtime_id": "local-zara-server",
        "model": None,
        "quantization": None,
        "phase": "connected",
    }
    assert device.captures["remote-text-turn"] == {
        "mode": "remote",
        "runtime_id": "stock-zara-server",
        "model": None,
        "quantization": None,
        "phase": "connected",
    }
