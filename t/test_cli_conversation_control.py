from __future__ import annotations

import concurrent.futures
import json

import pytest

import zara.__main__ as cli
from zara.runtime.commands import CancelTurn, SubmitTurn


class FakeSubscription:
    def __init__(self) -> None:
        self.closed = False

    def close(self) -> None:
        self.closed = True


class FakeClient:
    def __init__(self, *, turn_id: str = "turn-1") -> None:
        self.turn_id = turn_id
        self.commands = []
        self.subscription = FakeSubscription()
        self.started = False
        self.closed = False

    @staticmethod
    def _done(value=None):
        future = concurrent.futures.Future()
        future.set_result(value)
        return future

    def start(self):
        self.started = True
        return self._done(None)

    def subscribe(self):
        return self.subscription

    def submit(self, command):
        self.commands.append(command)
        receipt = type(
            "Receipt",
            (),
            {"turn_id": self.turn_id, "request_id": command.request_id, "detail": ""},
        )()
        return self._done(receipt)

    def close(self) -> None:
        self.closed = True


def install_client(monkeypatch, client: FakeClient) -> None:
    import zara.daemon_client as daemon_client

    monkeypatch.setattr(
        daemon_client,
        "create_daemon_client",
        lambda endpoint: client,
    )


def test_connected_text_preserves_explicit_conversation_and_emits_receipt(
    monkeypatch, capsys
):
    client = FakeClient()
    install_client(monkeypatch, client)
    monkeypatch.setattr(cli, "_wait_for_daemon_turn", lambda subscription, turn_id: "hi")

    exit_code = cli._run_connected_text(
        "ipc:///tmp/zara.sock",
        "hello again",
        conversation_id="conv-emacs-1",
        emit_json=True,
    )

    assert exit_code == 0
    assert client.started is True
    assert client.closed is True
    assert client.subscription.closed is True
    assert len(client.commands) == 1
    command = client.commands[0]
    assert isinstance(command, SubmitTurn)
    assert command.text == "hello again"
    assert command.conversation_id == "conv-emacs-1"

    lines = capsys.readouterr().out.splitlines()
    assert [json.loads(line) for line in lines] == [
        {
            "type": "turn.accepted",
            "turn_id": "turn-1",
            "conversation_id": "conv-emacs-1",
        },
        {
            "type": "assistant.complete",
            "turn_id": "turn-1",
            "conversation_id": "conv-emacs-1",
            "text": "hi",
        },
    ]


def test_connected_text_plain_output_stays_backward_compatible(monkeypatch, capsys):
    client = FakeClient()
    install_client(monkeypatch, client)
    monkeypatch.setattr(cli, "_wait_for_daemon_turn", lambda subscription, turn_id: "plain")

    exit_code = cli._run_connected_text("ipc:///tmp/zara.sock", "hello")

    assert exit_code == 0
    assert capsys.readouterr().out == "plain\n"
    assert isinstance(client.commands[0], SubmitTurn)
    assert client.commands[0].conversation_id is None


def test_connected_cancel_uses_canonical_cancel_turn(monkeypatch):
    client = FakeClient(turn_id="ignored")
    install_client(monkeypatch, client)

    exit_code = cli._run_connected_cancel("ipc:///tmp/zara.sock", "turn-42")

    assert exit_code == 0
    assert client.started is True
    assert client.closed is True
    assert len(client.commands) == 1
    command = client.commands[0]
    assert isinstance(command, CancelTurn)
    assert command.turn_id == "turn-42"


@pytest.mark.parametrize(
    "value",
    ["", "   ", "bad\x00id", "x" * 129],
)
def test_cli_identifier_validation_fails_closed(value):
    with pytest.raises(ValueError):
        cli._validate_cli_identifier(value, "conversation id")


def test_cli_identifier_validation_preserves_bounded_value():
    assert cli._validate_cli_identifier("conv-123", "conversation id") == "conv-123"
