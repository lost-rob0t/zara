from __future__ import annotations

import sys

import pytest

from zara import __main__ as cli
from zara.desktop import app as desktop_app
from zara.desktop.control import DesktopControlAlreadyRunning


class FakeController:
    def __init__(self) -> None:
        self.start_calls = 0
        self.quick_calls = 0

    def start(self):
        self.start_calls += 1
        return None

    def show_quick_copilot(self) -> None:
        self.quick_calls += 1


class FakeApp:
    pass


def test_default_desktop_client_uses_canonical_daemon_endpoint(monkeypatch):
    expected_client = object()
    seen = {}

    monkeypatch.setattr(
        desktop_app,
        "_default_daemon_endpoint",
        lambda: "ipc:///run/user/test/zara.sock",
        raising=False,
    )

    def fake_zmq_client(endpoint):
        seen["endpoint"] = endpoint
        return expected_client

    monkeypatch.setattr(desktop_app, "ZmqZaraClient", fake_zmq_client, raising=False)
    monkeypatch.setattr(
        desktop_app,
        "InProcessZaraClient",
        lambda: pytest.fail("normal desktop startup must not create a private runtime"),
    )

    client = desktop_app._default_desktop_client()

    assert client is expected_client
    assert seen == {"endpoint": "ipc:///run/user/test/zara.sock"}


def test_explicit_desktop_start_summons_quick_once(monkeypatch):
    fake_app = FakeApp()
    controller = FakeController()
    seen = {}

    def fake_create_application(argv=None, *, host=None):
        seen["argv"] = argv
        seen["host"] = host
        return fake_app, controller

    monkeypatch.setattr(desktop_app, "create_application", fake_create_application)

    app, returned_controller = desktop_app.start_desktop(["zara-desktop"])

    assert app is fake_app
    assert returned_controller is controller
    assert seen == {"argv": ["zara-desktop"], "host": None}
    assert controller.start_calls == 1
    assert controller.quick_calls == 1


def test_headless_desktop_start_can_skip_initial_summon(monkeypatch):
    controller = FakeController()
    monkeypatch.setattr(
        desktop_app,
        "create_application",
        lambda argv=None, *, host=None: (FakeApp(), controller),
    )

    desktop_app.start_desktop([], summon_quick=False)

    assert controller.start_calls == 1
    assert controller.quick_calls == 0


def test_zara_desktop_flag_routes_to_canonical_desktop_main(monkeypatch):
    monkeypatch.setattr(sys, "argv", ["zara", "--desktop"])
    monkeypatch.setattr(cli, "init_config", lambda: None)
    seen = {}

    def fake_desktop_main(argv=None):
        seen["argv"] = argv
        return 23

    monkeypatch.setattr(desktop_app, "main", fake_desktop_main)

    with pytest.raises(SystemExit) as exc_info:
        cli.main()

    assert exc_info.value.code == 23
    assert seen["argv"] == ["zara"]


def test_toggle_desktop_sends_to_existing_owner_without_starting_runtime(monkeypatch, tmp_path):
    monkeypatch.setattr(sys, "argv", ["zara", "--toggle-desktop"])
    monkeypatch.setattr(cli, "init_config", lambda: None)
    monkeypatch.setattr(cli, "_desktop_control_runtime_dir", lambda: tmp_path, raising=False)
    seen = []
    monkeypatch.setattr(
        cli,
        "send_desktop_control",
        lambda command, *, runtime_dir: seen.append((command, runtime_dir)) or "ok",
        raising=False,
    )
    monkeypatch.setattr(
        desktop_app,
        "main",
        lambda *args, **kwargs: pytest.fail("existing owner must not start another desktop"),
    )

    with pytest.raises(SystemExit) as exc_info:
        cli.main()

    assert exc_info.value.code == 0
    assert seen == [("toggle", tmp_path)]


def test_toggle_desktop_no_owner_starts_canonical_desktop_visible(monkeypatch, tmp_path):
    monkeypatch.setattr(sys, "argv", ["zara", "--toggle-desktop"])
    monkeypatch.setattr(cli, "init_config", lambda: None)
    monkeypatch.setattr(cli, "_desktop_control_runtime_dir", lambda: tmp_path, raising=False)
    monkeypatch.setattr(
        cli,
        "send_desktop_control",
        lambda *_args, **_kwargs: (_ for _ in ()).throw(ConnectionError("no owner")),
        raising=False,
    )
    seen = {}

    def fake_desktop_main(argv=None, *, initial_command="show"):
        seen["argv"] = argv
        seen["initial_command"] = initial_command
        return 0

    monkeypatch.setattr(desktop_app, "main", fake_desktop_main)

    with pytest.raises(SystemExit) as exc_info:
        cli.main()

    assert exc_info.value.code == 0
    assert seen == {"argv": ["zara"], "initial_command": "show"}


def test_desktop_main_losing_owner_race_relays_show_without_starting_client(monkeypatch, tmp_path):
    app = FakeApp()
    controller = FakeController()
    monkeypatch.setattr(
        desktop_app,
        "create_application",
        lambda argv=None: (app, controller),
    )
    monkeypatch.setattr(desktop_app, "_desktop_control_runtime_dir", lambda: tmp_path, raising=False)
    monkeypatch.setattr(
        desktop_app,
        "_install_desktop_control",
        lambda *_args, **_kwargs: (_ for _ in ()).throw(DesktopControlAlreadyRunning("race")),
        raising=False,
    )
    relayed = []
    monkeypatch.setattr(
        desktop_app,
        "send_desktop_control",
        lambda command, *, runtime_dir: relayed.append((command, runtime_dir)) or "ok",
        raising=False,
    )

    assert desktop_app.main(["zara"], initial_command="show") == 0
    assert controller.start_calls == 0
    assert controller.quick_calls == 0
    assert relayed == [("show", tmp_path)]
