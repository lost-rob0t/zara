"""Wiring tests for the task status announcer in ZaraServer."""

from types import SimpleNamespace

from zara.server_core import ZaraServer


class _Config:
    def __init__(self, sections):
        self._sections = sections

    def get_section(self, name):
        return self._sections.get(name)


class _Supervisor:
    def __init__(self):
        self.subscribed = False

    def subscribe(self, principal, *, maxsize=0):
        self.subscribed = True
        return SimpleNamespace()


class _NoSubscribeSupervisor:
    pass


def _server(sections):
    return ZaraServer(
        supervisor=_Supervisor(),
        lease=SimpleNamespace(),
        config=_Config(sections),
    )


def test_build_task_announcer_when_enabled():
    server = _server({"tasks": {"status_announcements": True}})
    supervisor = server._supervisor
    announcer = server._build_task_announcer(supervisor, None, 24000)
    assert announcer is not None
    assert supervisor.subscribed is True


def test_build_task_announcer_disabled_by_config():
    server = _server({"tasks": {"status_announcements": False}})
    assert server._build_task_announcer(server._supervisor, None, 24000) is None


def test_build_task_announcer_without_subscribe_degrades_to_none():
    server = _server({"tasks": {}})
    assert server._build_task_announcer(_NoSubscribeSupervisor(), None, 24000) is None


def test_new_server_has_no_announcer_until_gateway_build():
    server = _server({"tasks": {"status_announcements": True}})
    assert server._task_announcer is None
