from __future__ import annotations

import concurrent.futures
import sys

import pytest

import zara.__main__ as cli
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

    class FakeClient:
        def __init__(self, endpoint):
            calls.append(("construct", endpoint))

        def start(self):
            calls.append(("start",))
            return resolved(None)

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
    assert isinstance(calls[2][1], SubmitTurn)
    assert calls[2][1].text == "hello"
    assert calls[3][0] == "close"


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
