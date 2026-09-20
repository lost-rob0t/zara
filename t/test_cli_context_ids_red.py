from __future__ import annotations

import concurrent.futures
import sys

import pytest

import zara.__main__ as cli
from zara.runtime.commands import SubmitTurn


class _Subscription:
    def close(self) -> None:
        pass


class _Client:
    def __init__(self) -> None:
        self.commands = []
        self.subscription = _Subscription()

    @staticmethod
    def _done(value=None):
        future = concurrent.futures.Future()
        future.set_result(value)
        return future

    def start(self):
        return self._done(None)

    def subscribe(self):
        return self.subscription

    def submit(self, command):
        self.commands.append(command)
        receipt = type(
            "Receipt",
            (),
            {"turn_id": "turn-context-1", "request_id": command.request_id, "detail": ""},
        )()
        return self._done(receipt)

    def close(self) -> None:
        pass


def _install_client(monkeypatch, client: _Client) -> None:
    import zara.daemon_client as daemon_client

    monkeypatch.setattr(daemon_client, "create_daemon_client", lambda endpoint: client)


def test_connected_text_forwards_context_ids_to_canonical_submit_turn(monkeypatch):
    client = _Client()
    _install_client(monkeypatch, client)
    monkeypatch.setattr(cli, "_wait_for_daemon_turn", lambda subscription, turn_id: "ok")

    exit_code = cli._run_connected_text(
        "ipc:///tmp/zara.sock",
        "continue with these",
        conversation_id="emacs-main",
        context_ids=(" doc:alpha ", "project:zara"),
        emit_json=True,
    )

    assert exit_code == 0
    assert len(client.commands) == 1
    command = client.commands[0]
    assert isinstance(command, SubmitTurn)
    assert command.context_ids == ("doc:alpha", "project:zara")


def test_main_accepts_repeatable_context_id_and_forwards_without_parallel_state(
    monkeypatch,
):
    captured = {}

    class _Config:
        @staticmethod
        def get_section(name):
            return {}

    def fake_run(endpoint, command_text, **kwargs):
        captured["endpoint"] = endpoint
        captured["command_text"] = command_text
        captured.update(kwargs)
        return 0

    monkeypatch.setattr(cli, "init_config", lambda: _Config())
    monkeypatch.setattr(cli, "_default_daemon_endpoint", lambda: "ipc:///tmp/zara.sock")
    monkeypatch.setattr(cli, "_run_connected_text", fake_run)
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "zara",
            "--conversation-id",
            "emacs-main",
            "--context-id",
            " doc:alpha ",
            "--context-id",
            "project:zara",
            "continue",
        ],
    )

    with pytest.raises(SystemExit) as exit_info:
        cli.main()

    assert exit_info.value.code == 0
    assert captured == {
        "endpoint": "ipc:///tmp/zara.sock",
        "command_text": "continue",
        "conversation_id": "emacs-main",
        "context_ids": [" doc:alpha ", "project:zara"],
        "emit_json": False,
    }
