from __future__ import annotations

import subprocess

import pytest

from zara.plugins.builtin.android_adb import (
    AdbCommandError,
    AdbTransport,
    AndroidAdbPlugin,
)


class FakeRunner:
    def __init__(self):
        self.calls = []
        self.responses = []

    def queue(self, stdout=b"", stderr=b"", returncode=0):
        self.responses.append((stdout, stderr, returncode))

    def __call__(self, argv, **kwargs):
        self.calls.append((tuple(argv), kwargs))
        stdout, stderr, returncode = self.responses.pop(0)
        return subprocess.CompletedProcess(argv, returncode, stdout, stderr)


def test_transport_selects_only_connected_device():
    runner = FakeRunner()
    runner.queue(b"List of devices attached\nSERIAL\tdevice product:x model:y\n")
    transport = AdbTransport("adb", runner=runner)

    assert transport.selected_serial() == "SERIAL"


def test_transport_rejects_ambiguous_target():
    runner = FakeRunner()
    runner.queue(b"List of devices attached\nA\tdevice\nB\tdevice\n")
    transport = AdbTransport("adb", runner=runner)

    with pytest.raises(AdbCommandError, match="exactly one"):
        transport.selected_serial()


def test_screenshot_is_png_and_uses_exec_out():
    runner = FakeRunner()
    runner.queue(b"List of devices attached\nSERIAL\tdevice\n")
    runner.queue(b"\x89PNG\r\n\x1a\npayload")
    transport = AdbTransport("adb", runner=runner)

    assert transport.screenshot_png().startswith(b"\x89PNG")
    assert runner.calls[-1][0] == (
        "adb",
        "-s",
        "SERIAL",
        "exec-out",
        "screencap",
        "-p",
    )


def test_text_input_rejects_remote_shell_metacharacters():
    transport = AdbTransport("adb", serial="SERIAL", runner=FakeRunner())

    with pytest.raises(ValueError):
        transport.type_text("hello; id")
    with pytest.raises(ValueError):
        transport.type_text("$(id)")


def test_vision_policy_fails_closed_without_explicit_policy():
    plugin = AndroidAdbPlugin()

    class Engine:
        def query_once(self, _goal):
            return {"Policy": "observe_only"}

    plugin._engine = Engine()

    with pytest.raises(PermissionError, match="observe_only"):
        plugin._require_vision_mutation()
