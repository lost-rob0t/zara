from __future__ import annotations

import builtins
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
        [events.ResponseText(turn_id="turn-1", text="done")]
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
            events.ResponseText(turn_id="other-turn", text="wrong response"),
            events.ResponseText(turn_id="turn-1", text="daemon response"),
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


def test_connected_text_waits_past_model_completion_for_final_runtime_response(
    monkeypatch,
    capsys,
):
    subscription = FakeSubscription(
        [
            events.AssistantComplete(turn_id="turn-1", text="", success=True),
            events.ToolCompleted(
                turn_id="turn-1",
                tool_name="query_prolog",
                success=True,
            ),
            events.ResponseText(turn_id="turn-1", text="Timer set."),
        ]
    )

    class FakeClient:
        def __init__(self, _endpoint):
            pass

        def start(self):
            return resolved(None)

        def subscribe(self, *, maxsize=0):
            assert maxsize == 0
            return subscription

        def submit(self, command):
            return resolved(CommandReceipt(request_id=command.request_id, turn_id="turn-1"))

        def close(self, timeout=None):
            return None

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)

    assert run_main(monkeypatch, ["--connect", "ipc:///tmp/zara.sock", "timer 10 seconds"]) == 0
    assert capsys.readouterr().out.strip() == "Timer set."


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


def test_text_command_uses_owner_local_daemon_by_default(monkeypatch):
    calls = []
    subscription = FakeSubscription(
        [events.ResponseText(turn_id="turn-1", text="Timer set.")]
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
            calls.append(("submit", command.text))
            return resolved(CommandReceipt(request_id=command.request_id, turn_id="turn-1"))

        def close(self, timeout=None):
            calls.append(("close", timeout))

    import zara.console as console_module
    import zara.zmq_transport as transport_module

    monkeypatch.setenv("XDG_RUNTIME_DIR", "/run/user/1000")
    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)
    monkeypatch.setattr(
        console_module,
        "ZaraConsole",
        lambda: pytest.fail("default commands must not create a standalone runtime"),
    )

    assert run_main(monkeypatch, ["set a timer for 10 seconds"]) == 0
    assert calls == [
        ("construct", "ipc:///run/user/1000/zarathushtra/zara-server.sock"),
        ("start",),
        ("subscribe", 0),
        ("submit", "set a timer for 10 seconds"),
        ("close", None),
    ]
    assert subscription.closed is True


def test_bare_zara_opens_connected_console_on_default_daemon(monkeypatch, capsys):
    calls = []
    subscription = FakeSubscription(
        [events.ResponseText(turn_id="turn-1", text="Timer set.")]
    )
    inputs = iter(["set a timer for 10 seconds", "exit"])

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
            calls.append(("submit", command.text))
            return resolved(CommandReceipt(request_id=command.request_id, turn_id="turn-1"))

        def close(self, timeout=None):
            calls.append(("close", timeout))

    import zara.zmq_transport as transport_module

    monkeypatch.setenv("XDG_RUNTIME_DIR", "/run/user/1000")
    monkeypatch.setattr(transport_module, "ZmqZaraClient", FakeClient)
    monkeypatch.setattr(builtins, "input", lambda _prompt: next(inputs))

    assert run_main(monkeypatch, []) == 0
    output = capsys.readouterr().out
    assert "Connected to Zara" in output
    assert "Timer set." in output
    assert calls == [
        ("construct", "ipc:///run/user/1000/zarathushtra/zara-server.sock"),
        ("start",),
        ("subscribe", 0),
        ("submit", "set a timer for 10 seconds"),
        ("close", None),
    ]
    assert subscription.closed is True


def test_default_daemon_failure_is_actionable_and_never_falls_back(
    monkeypatch,
    tmp_path,
    capsys,
):
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

    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    monkeypatch.setattr(transport_module, "ZmqZaraClient", FailingClient)
    monkeypatch.setattr(
        console_module,
        "ZaraConsole",
        lambda: pytest.fail("daemon failure must not silently start standalone runtime"),
    )

    assert run_main(monkeypatch, ["set a timer for 10 seconds"]) == 2
    error = capsys.readouterr().err
    assert "daemon unavailable" in error
    assert "zara-server" in error
    assert "--standalone" in error


def test_daemon_start_error_is_not_masked_by_client_cleanup_failure(monkeypatch, capsys):
    class FailingClient:
        def __init__(self, _endpoint):
            pass

        def start(self):
            future = concurrent.futures.Future()
            future.set_exception(RuntimeError("handshake failed"))
            return future

        def close(self, timeout=None):
            raise RuntimeError("cleanup failed")

    import zara.zmq_transport as transport_module

    monkeypatch.setattr(transport_module, "ZmqZaraClient", FailingClient)

    assert run_main(monkeypatch, ["hello"]) == 2
    error = capsys.readouterr().err
    assert "handshake failed" in error
    assert "cleanup failed" not in error
