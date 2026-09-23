from __future__ import annotations

import importlib.util
from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
REMOTE_ACCEPTANCE = INTEGRATION / "device_remote_acceptance.py"


def _load_remote_acceptance_module():
    sys.path.insert(0, str(INTEGRATION))
    try:
        spec = importlib.util.spec_from_file_location(
            "zara_device_remote_acceptance_test",
            REMOTE_ACCEPTANCE,
        )
        assert spec is not None
        assert spec.loader is not None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.remove(str(INTEGRATION))


class _TransientLogcatDevice:
    def __init__(self) -> None:
        self.logcat_calls = 0
        self.wait_calls = 0

    def adb(self, *arguments: str) -> str:
        if arguments[:4] == ("shell", "run-as", "ai.zara.app", "cat"):
            return "diagnostics-ok\n"
        if arguments == ("shell", "pidof", "ai.zara.app"):
            return "4242\n"
        if arguments == ("wait-for-device",):
            self.wait_calls += 1
            return ""
        if arguments and arguments[0] == "logcat":
            self.logcat_calls += 1
            if self.logcat_calls <= 2:
                raise RuntimeError("adb logcat exited 255")
            return "09-23 21:05:00.000 4242 4242 I Zara: healthy\n"
        raise AssertionError(f"unexpected adb call: {arguments!r}")


class _BrokenLogcatDevice(_TransientLogcatDevice):
    def adb(self, *arguments: str) -> str:
        if arguments and arguments[0] == "logcat":
            self.logcat_calls += 1
            raise RuntimeError("adb logcat exited 255")
        return super().adb(*arguments)


def test_collect_app_diagnostics_recovers_from_transient_logcat_transport_failure(
    tmp_path: Path,
) -> None:
    remote = _load_remote_acceptance_module()
    device = _TransientLogcatDevice()

    evidence = remote.collect_app_diagnostics(device, tmp_path)

    assert evidence["app_diagnostics"] == "remote-app-diagnostics.log"
    assert evidence["logcat"] == "remote-logcat.log"
    assert evidence["fatal_log_markers"] == []
    assert "logcat_failure" not in evidence
    assert device.logcat_calls == 3
    assert device.wait_calls >= 1
    assert (tmp_path / "remote-logcat.log").read_text(encoding="utf-8").endswith(
        "Zara: healthy\n"
    )


def test_collect_app_diagnostics_still_fails_closed_when_logcat_never_recovers(
    tmp_path: Path,
) -> None:
    remote = _load_remote_acceptance_module()
    device = _BrokenLogcatDevice()

    evidence = remote.collect_app_diagnostics(device, tmp_path)

    assert evidence["app_diagnostics"] == "remote-app-diagnostics.log"
    assert "logcat" not in evidence
    assert "fatal_log_markers" not in evidence
    assert "logcat_failure" in evidence
    assert device.logcat_calls >= 3
