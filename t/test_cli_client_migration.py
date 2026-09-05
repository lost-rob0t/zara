from __future__ import annotations

import concurrent.futures
from types import SimpleNamespace
import sys

import pytest

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


def run_main(monkeypatch, argv):
    monkeypatch.setattr(cli, "init_config", lambda: FakeConfig())
    monkeypatch.setattr(sys, "argv", ["zara", *argv])
    with pytest.raises(SystemExit) as stopped:
        cli.main()
    return stopped.value.code


class FakeSubscription:
    def __init__(self, event_items):
        self._items = [SimpleNamespace(event=event) for event in event_items]
        self.closed = False

    def get(self, timeout=None):
        assert timeout is not None
        if not self._items:
            raise TimeoutError("no more fake events")
        return self._items.pop(0)

    def close(self):
        self.closed = True


def test_explicit_standalone_text_command_preserves_local_console(monkeypatch):
    calls = []

    class FakeConsole:
        def execute_command(self, text):
            calls.append(text)
            return True

    import zara.console as console_module

    monkeypatch.setattr(console_module, "ZaraConsole", FakeConsole)

    assert run_main(monkeypatch, ["--standalone", "hello", "there"]) == 0
    assert calls == ["hello there"]


def test_connect_text_command_uses_zara_client_boundary(monkeypatch):
    calls = []
    subscription = FakeSubscription(
        [events.AssistantComplete(turn_id="turn-1", text="done", success=True)]
    )

    class FakeClient:
        def __init__(self, endpoint):
            calls.append(("construct", endpoint))

        def start(self):
            calls.append(("start",))
            return resolved(None)

        def subscribe(self, *, maxsize=0):
            calls.append(("subscribe", maxsize))
            return subscription

        def submit(self, command):
            calls.append(("submit", command))
            return resolved(CommandReceipt(request_id=command.request_id, turn_id="turn-1"))

        def close(self, timeout=None):
            calls.append(("close", timeout))

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)

    assert run_main(monkeypatch, ["--connect", "ipc:///tmp/zara.sock", "hello"]) == 0
    assert calls[0] == ("construct", "ipc:///tmp/zara.sock")
    assert calls[1] == ("start",)
    assert calls[2] == ("subscribe", 0)
    assert isinstance(calls[3][1], SubmitTurn)
    assert calls[3][1].text == "hello"
    assert calls[4][0] == "close"
    assert subscription.closed is True


def test_connect_failure_is_reported_without_silent_standalone_fallback(monkeypatch, capsys):
    class FailingClient:
        def __init__(self, endpoint):
            self.endpoint = endpoint

        def start(self):
            future = concurrent.futures.Future()
            future.set_exception(RuntimeError("daemon unavailable"))
            return future

        def close(self, timeout=None):
            return None

    import zara.console as console_module
    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FailingClient)
    monkeypatch.setattr(
        console_module,
        "ZaraConsole",
        lambda: pytest.fail("connect failure must not silently start standalone runtime"),
    )

    assert run_main(monkeypatch, ["--connect", "ipc:///tmp/missing.sock", "hello"]) == 2
    assert "daemon unavailable" in capsys.readouterr().err


def test_connect_and_standalone_are_mutually_exclusive(monkeypatch, capsys):
    assert (
        run_main(
            monkeypatch,
            ["--connect", "ipc:///tmp/zara.sock", "--standalone", "hello"],
        )
        == 2
    )
    assert "not allowed with argument" in capsys.readouterr().err


def test_connect_text_waits_for_matching_completion_and_prints_response(monkeypatch, capsys):
    calls = []
    subscription = FakeSubscription(
        [
            events.AssistantComplete(turn_id="other-turn", text="wrong response", success=True),
            events.AssistantComplete(turn_id="turn-1", text="daemon response", success=True),
        ]
    )

    class FakeClient:
        def __init__(self, endpoint):
            calls.append(("construct", endpoint))

        def start(self):
            calls.append(("start",))
            return resolved(None)

        def subscribe(self, *, maxsize=0):
            calls.append(("subscribe", maxsize))
            return subscription

        def submit(self, command):
            calls.append(("submit", command))
            return resolved(CommandReceipt(request_id=command.request_id, turn_id="turn-1"))

        def close(self, timeout=None):
            calls.append(("close", timeout))

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)

    assert run_main(monkeypatch, ["--connect", "ipc:///tmp/zara.sock", "hello"]) == 0
    output = capsys.readouterr()
    assert output.out.strip() == "daemon response"
    assert [call[0] for call in calls] == ["construct", "start", "subscribe", "submit", "close"]
    assert subscription.closed is True


def test_connect_constructor_failure_is_bounded_and_never_falls_back(monkeypatch, capsys):
    class FailingClient:
        def __init__(self, endpoint):
            raise RuntimeError(f"invalid daemon endpoint: {endpoint}")

    import zara.console as console_module
    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FailingClient)
    monkeypatch.setattr(
        console_module,
        "ZaraConsole",
        lambda: pytest.fail("constructor failure must not silently start standalone runtime"),
    )

    assert run_main(monkeypatch, ["--connect", "ipc:///tmp/bad.sock", "hello"]) == 2
    error = capsys.readouterr().err
    assert "invalid daemon endpoint" in error
    assert "Traceback" not in error


def test_text_command_defaults_to_daemon_client_for_timer(monkeypatch):
    calls = []

    def fake_connected(endpoint, text):
        calls.append((endpoint, text))
        return 0

    import zara.console as console_module

    monkeypatch.setattr(cli, "_run_connected_text", fake_connected)
    monkeypatch.setattr(
        console_module,
        "ZaraConsole",
        lambda: pytest.fail("default command must not create a standalone runtime"),
    )

    assert run_main(monkeypatch, ["set", "a", "timer", "for", "10", "seconds"]) == 0
    assert len(calls) == 1
    endpoint, text = calls[0]
    assert endpoint
    assert text == "set a timer for 10 seconds"
