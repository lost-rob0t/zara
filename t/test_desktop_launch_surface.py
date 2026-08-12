from __future__ import annotations

import sys

import pytest

from zara import __main__ as cli
from zara.desktop import app as desktop_app


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
    monkeypatch.setattr(desktop_app, "main", lambda argv=None: 23)

    with pytest.raises(SystemExit) as exc_info:
        cli.main()

    assert exc_info.value.code == 23
