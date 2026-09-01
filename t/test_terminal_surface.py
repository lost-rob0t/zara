from __future__ import annotations

import concurrent.futures
from types import SimpleNamespace

import zara.__main__ as cli
from zara.runtime import events
from zara.runtime.commands import CommandReceipt, SubmitTurn


class FakeConfig:
    def get_section(self, _name):
        return {}


def resolved(value):
    future = concurrent.futures.Future()
    future.set_result(value)
    return future


def test_bare_zara_launches_tui(monkeypatch):
    calls = []
    config = FakeConfig()
    monkeypatch.setattr(cli, "init_config", lambda: config)

    import zara.terminal as terminal

    monkeypatch.setattr(
        terminal,
        "run_tui",
        lambda **kwargs: calls.append(kwargs) or 0,
    )

    assert cli.run([]) == 0
    assert calls == [{"endpoint": None, "config": config}]


def test_agent_without_task_launches_same_tui(monkeypatch):
    calls = []
    config = FakeConfig()
    monkeypatch.setattr(cli, "init_config", lambda: config)

    import zara.terminal as terminal

    monkeypatch.setattr(
        terminal,
        "run_tui",
        lambda **kwargs: calls.append(kwargs) or 0,
    )

    assert cli.run(["--agent"]) == 0
    assert calls == [{"endpoint": None, "config": config}]


def test_positional_task_and_agent_task_use_same_runner(monkeypatch):
    calls = []
    config = FakeConfig()
    monkeypatch.setattr(cli, "init_config", lambda: config)

    import zara.terminal as terminal

    def fake_run_task(text, **kwargs):
        calls.append((text, kwargs))
        return 0

    monkeypatch.setattr(terminal, "run_task", fake_run_task)

    assert cli.run(["remember", "this"]) == 0
    assert cli.run(["--agent", "remember", "this"]) == 0
    assert calls == [
        ("remember this", {"endpoint": None, "config": config}),
        ("remember this", {"endpoint": None, "config": config}),
    ]


def test_console_alias_launches_tui(monkeypatch):
    calls = []
    config = FakeConfig()
    monkeypatch.setattr(cli, "init_config", lambda: config)

    import zara.terminal as terminal

    monkeypatch.setattr(
        terminal,
        "run_tui",
        lambda **kwargs: calls.append(kwargs) or 0,
    )

    assert cli.run(["--console"]) == 0
    assert calls == [{"endpoint": None, "config": config}]


def test_agent_entrypoint_delegates_to_main_cli(monkeypatch):
    import zara.agent_cli as agent_cli

    calls = []
    monkeypatch.setattr(cli, "run", lambda argv: calls.append(argv) or 7)

    assert agent_cli.run(["remember", "this"]) == 7
    assert calls == [["--agent", "remember", "this"]]


def test_main_without_args_in_noninteractive_process_prints_help(monkeypatch, capsys):
    config = FakeConfig()
    monkeypatch.setattr(cli, "init_config", lambda: config)
    monkeypatch.setattr(cli.sys, "argv", ["zara"])
    monkeypatch.setattr(cli.sys.stdin, "isatty", lambda: False)
    monkeypatch.setattr(cli.sys.stdout, "isatty", lambda: False)

    assert cli.run() == 1
    assert "Zarathustra Voice Assistant" in capsys.readouterr().out


class FakeSubscription:
    def __init__(self, event_items):
        self._items = [SimpleNamespace(event=event) for event in event_items]
        self.closed = False

    def get(self, timeout=None):
        assert timeout is not None
        return self._items.pop(0)

    def close(self):
        self.closed = True


class FakeClient:
    def __init__(self, events_for_turn):
        self.subscription = FakeSubscription(events_for_turn)
        self.commands = []
        self.started = False
        self.closed = False

    def start(self):
        self.started = True
        return resolved(None)

    def subscribe(self, *, maxsize=0):
        assert maxsize == 0
        return self.subscription

    def submit(self, command):
        self.commands.append(command)
        return resolved(
            CommandReceipt(request_id=command.request_id, turn_id="turn-1")
        )

    def close(self, timeout=None):
        self.closed = True


def test_run_task_uses_zara_client_submit_turn_boundary(monkeypatch, capsys):
    import zara.terminal as terminal

    client = FakeClient(
        [events.ResponseText(turn_id="turn-1", text="same runtime response")]
    )
    monkeypatch.setattr(terminal, "make_client", lambda **_kwargs: client)

    assert terminal.run_task("hello", endpoint=None, config=FakeConfig()) == 0
    assert client.started is True
    assert client.closed is True
    assert client.subscription.closed is True
    assert len(client.commands) == 1
    assert isinstance(client.commands[0], SubmitTurn)
    assert client.commands[0].text == "hello"
    assert capsys.readouterr().out.strip() == "same runtime response"


def test_run_task_reports_runtime_failure(monkeypatch, capsys):
    import zara.terminal as terminal

    client = FakeClient(
        [events.AgentFailed(turn_id="turn-1", reason="provider exploded")]
    )
    monkeypatch.setattr(terminal, "make_client", lambda **_kwargs: client)

    assert terminal.run_task("hello", endpoint=None, config=FakeConfig()) == 2
    assert "provider exploded" in capsys.readouterr().err
    assert client.closed is True
    assert client.subscription.closed is True
