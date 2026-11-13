from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from zara.plugins import StartupUnavailable
from zara.plugins.builtin import android_adb
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


class OnlineRunner:
    def __init__(self):
        self.calls = []

    def __call__(self, argv, **kwargs):
        self.calls.append((tuple(argv), kwargs))
        command = tuple(argv)
        if command[-2:] == ("devices", "-l"):
            stdout = b"List of devices attached\nSERIAL\tdevice product:zara model:phone\n"
        elif command[-3:] == ("exec-out", "screencap", "-p"):
            stdout = b"\x89PNG\r\n\x1a\npayload"
        elif command[-4:] == ("exec-out", "uiautomator", "dump", "/dev/tty"):
            stdout = b"UI dump complete\n<?xml version='1.0'?><hierarchy/>\n"
        else:
            stdout = b""
        return subprocess.CompletedProcess(argv, 0, stdout, b"")


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


def test_transport_observes_and_executes_bounded_device_actions():
    runner = OnlineRunner()
    sleeps = []
    transport = AdbTransport("adb", serial="SERIAL", runner=runner, sleeper=sleeps.append)

    assert transport.ui_tree() == "<?xml version='1.0'?><hierarchy/>"
    assert transport.screenshot_png().endswith(b"payload")
    transport.tap(10, 20)
    transport.swipe(1, 2, 3, 4, 500)
    transport.type_text("hello world")
    transport.key(" HOME ")
    transport.open_package("ai.zara.app")
    transport.wait(250)

    commands = [call[0] for call in runner.calls]
    assert ("adb", "-s", "SERIAL", "shell", "input", "tap", "10", "20") in commands
    assert (
        "adb",
        "-s",
        "SERIAL",
        "shell",
        "input",
        "swipe",
        "1",
        "2",
        "3",
        "4",
        "500",
    ) in commands
    assert ("adb", "-s", "SERIAL", "shell", "input", "text", "hello%sworld") in commands
    assert ("adb", "-s", "SERIAL", "shell", "input", "keyevent", "3") in commands
    assert sleeps == [0.25]


@pytest.mark.parametrize("value", [-1, True, 16385, 1.5])
def test_transport_rejects_invalid_coordinates(value):
    with pytest.raises(ValueError, match="coordinate"):
        AdbTransport._coordinate(value)


def test_transport_rejects_invalid_action_arguments():
    transport = AdbTransport("adb", serial="SERIAL", runner=OnlineRunner())

    for duration in (0, 5001, True):
        with pytest.raises(ValueError, match="swipe duration"):
            transport.swipe(0, 0, 1, 1, duration)
    for duration in (-1, 5001, True):
        with pytest.raises(ValueError, match="wait duration"):
            transport.wait(duration)
    with pytest.raises(TypeError, match="text must be"):
        transport.type_text(12)
    with pytest.raises(ValueError, match="limited"):
        transport.type_text("bad;command")
    with pytest.raises(ValueError, match="unsupported Android key"):
        transport.key("power")
    with pytest.raises(ValueError, match="package"):
        transport.open_package("not-a-package")


def test_transport_fails_closed_on_bad_payloads_and_adb_errors():
    runner = FakeRunner()
    runner.queue(b"List of devices attached\nSERIAL\tdevice\n")
    runner.queue(b"not png")
    transport = AdbTransport("adb", runner=runner)
    with pytest.raises(AdbCommandError, match="PNG"):
        transport.screenshot_png()

    runner.queue(b"List of devices attached\nSERIAL\tdevice\n")
    runner.queue(b"no hierarchy")
    with pytest.raises(AdbCommandError, match="hierarchy"):
        transport.ui_tree()

    runner.queue(b"x" * 10)
    with pytest.raises(AdbCommandError, match="byte limit"):
        transport._run_host(("version",), max_stdout=4)

    runner.queue(stderr=b"permission denied", returncode=1)
    with pytest.raises(AdbCommandError, match="permission denied"):
        transport._run_host(("version",))


def test_configured_transport_requires_its_exact_online_serial():
    runner = FakeRunner()
    runner.queue(b"List of devices attached\nOTHER\tdevice\nSERIAL\toffline\n")
    transport = AdbTransport("adb", serial="SERIAL", runner=runner)

    with pytest.raises(AdbCommandError, match="configured"):
        transport.selected_serial()


def test_plugin_observation_and_confirmed_mutation_surfaces():
    class Transport(RecordingTransport):
        def devices(self):
            return (
                android_adb.AdbDevice("SERIAL", "device", "model:phone"),
                android_adb.AdbDevice("OFFLINE", "offline", ""),
            )

        def selected_serial(self):
            return "SERIAL"

        def screenshot_png(self):
            return b"\x89PNG\r\n\x1a\n"

        def ui_tree(self):
            return "<?xml version='1.0'?><hierarchy/>"

    class Engine:
        def query_once(self, goal):
            if goal == "kb_android_control:android_vision_policy(Policy)":
                return {"Policy": "confirm_each_action"}
            if goal == "kb_android_control:android_app_package(zara, Package)":
                return {"Package": b"ai.zara.app"}
            raise AssertionError(goal)

    plugin = AndroidAdbPlugin()
    transport = Transport()
    plugin._transport = transport
    plugin._engine = Engine()

    assert plugin.android_adb_devices()["selected"] == "SERIAL"
    assert plugin.android_adb_screenshot()["data_url"].startswith("data:image/png;base64,")
    assert plugin.android_adb_ui_tree()["serial"] == "SERIAL"
    assert plugin.android_adb_tap(1, 2)["completed"] is True
    assert plugin.android_adb_swipe(1, 2, 3, 4)["action"] == "swipe"
    assert plugin.android_adb_type_text("hello")["action"] == "text"
    assert plugin.android_adb_key("home")["action"] == "key"
    assert plugin.android_adb_open_app("zara")["alias"] == "zara"
    assert transport.calls[-1] == ("open_package", "ai.zara.app")

    tools = {tool.name: tool for tool in plugin.tools()}
    assert set(tools) == {
        "android_adb_devices",
        "android_adb_screenshot",
        "android_adb_ui_tree",
        "android_adb_run_plan",
        "android_adb_tap",
        "android_adb_swipe",
        "android_adb_type_text",
        "android_adb_key",
        "android_adb_open_app",
    }
    assert tools["android_adb_run_plan"].metadata["zara_requires_approval"] is True


def test_plugin_start_stop_and_unavailable_adb(monkeypatch, tmp_path):
    consulted = []

    class Runtime:
        configuration = {"adb_path": str(tmp_path / "adb"), "timeout_seconds": 5}

    class Engine:
        def consult(self, path):
            consulted.append(path)

    adb_path = tmp_path / "adb"
    adb_path.write_text("adb", encoding="utf-8")
    monkeypatch.setattr(android_adb, "PrologEngine", Engine)
    monkeypatch.setattr(android_adb, "locate_main_pl", lambda: Path("/repo/main.pl"))
    monkeypatch.setattr(AdbTransport, "_run_host", lambda self, *args, **kwargs: None)
    plugin = AndroidAdbPlugin()

    assert plugin.start(Runtime()) is None
    assert plugin._transport is not None
    assert consulted == [Path("/repo/kb/android_control.pl")]
    plugin.stop()
    with pytest.raises(RuntimeError, match="not running"):
        plugin._require_transport()
    with pytest.raises(RuntimeError, match="policy"):
        plugin._require_engine()

    monkeypatch.setattr(AndroidAdbPlugin, "_resolve_adb", staticmethod(lambda _value: None))
    unavailable = plugin.start(Runtime())
    assert isinstance(unavailable, StartupUnavailable)
    assert unavailable.reason == "adb_unavailable"
