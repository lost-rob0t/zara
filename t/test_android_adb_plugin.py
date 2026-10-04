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


class PrologTerm:
    def __init__(self, name, *args):
        self.name = name
        self.args = args


class RecordingTransport:
    def __init__(self):
        self.calls = []

    def tap(self, *args):
        self.calls.append(("tap", *args))

    def swipe(self, *args):
        self.calls.append(("swipe", *args))

    def type_text(self, *args):
        self.calls.append(("text", *args))

    def key(self, *args):
        self.calls.append(("key", *args))

    def wait(self, *args):
        self.calls.append(("wait", *args))

    def open_package(self, *args):
        self.calls.append(("open_package", *args))


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


def test_prolog_plan_executes_every_typed_adb_action_in_order():
    transport = RecordingTransport()

    class Engine:
        def query_once(self, goal):
            if goal == "kb_android_control:android_adb_plan(daily_check, Actions)":
                return {
                    "Actions": [
                        PrologTerm("tap", 10, 20),
                        PrologTerm("swipe", 10, 20, 30, 40, 300),
                        PrologTerm("text", b"hello world"),
                        PrologTerm("key", "home"),
                        PrologTerm("wait", 250),
                        PrologTerm("open_app", "zara"),
                    ]
                }
            if goal == "kb_android_control:android_app_package(zara, Package)":
                return {"Package": "ai.zara.app"}
            raise AssertionError(goal)

    plugin = AndroidAdbPlugin()
    plugin._transport = transport
    plugin._engine = Engine()

    assert plugin.android_adb_run_plan("daily_check") == {
        "plan": "daily_check",
        "actions": 6,
        "completed": True,
    }
    assert transport.calls == [
        ("tap", 10, 20),
        ("swipe", 10, 20, 30, 40, 300),
        ("text", "hello world"),
        ("key", "home"),
        ("wait", 250),
        ("open_package", "ai.zara.app"),
    ]
