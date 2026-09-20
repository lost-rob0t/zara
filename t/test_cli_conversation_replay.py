from __future__ import annotations

import json
import sys
from types import SimpleNamespace

import pytest

import zara.__main__ as cli
from zara.desktop.conversation.models import MessageRole, MessageStatus


class _ReplayStore:
    def __init__(self, *, missing: bool = False) -> None:
        self.missing = missing
        self.requested: list[tuple[str, str]] = []

    def get_conversation(self, conversation_id: str):
        self.requested.append(("conversation", conversation_id))
        if self.missing:
            return None
        return SimpleNamespace(
            id=conversation_id,
            title="Emacs main",
            created_at="2026-09-20T18:00:00",
            updated_at="2026-09-20T18:05:00",
        )

    def load_messages(self, conversation_id: str):
        self.requested.append(("messages", conversation_id))
        return [
            SimpleNamespace(
                sequence=1,
                role=MessageRole.USER,
                content="hello",
                status=MessageStatus.COMPLETE,
                turn_id="turn-1",
                error="",
                tool_run_id=None,
            ),
            SimpleNamespace(
                sequence=2,
                role=MessageRole.ASSISTANT,
                content="hi there",
                status=MessageStatus.COMPLETE,
                turn_id="turn-1",
                error="",
                tool_run_id=None,
            ),
        ]


def _install_config(monkeypatch) -> None:
    class _Config:
        @staticmethod
        def get_section(name):
            return {}

    monkeypatch.setattr(cli, "init_config", lambda: _Config())


def test_replay_reads_only_the_canonical_conversation_store(monkeypatch, capsys):
    import zara.desktop.conversation as conversation

    store = _ReplayStore()
    monkeypatch.setattr(conversation, "ConversationStore", lambda: store)

    assert cli._run_conversation_replay(" emacs-main ") == 0

    payload = json.loads(capsys.readouterr().out)
    assert payload == {
        "conversation": {
            "created_at": "2026-09-20T18:00:00",
            "id": "emacs-main",
            "title": "Emacs main",
            "updated_at": "2026-09-20T18:05:00",
        },
        "messages": [
            {
                "content": "hello",
                "error": "",
                "role": "user",
                "sequence": 1,
                "status": "complete",
                "tool_run_id": None,
                "turn_id": "turn-1",
            },
            {
                "content": "hi there",
                "error": "",
                "role": "assistant",
                "sequence": 2,
                "status": "complete",
                "tool_run_id": None,
                "turn_id": "turn-1",
            },
        ],
        "version": "ZARA-CONVERSATION-REPLAY/1",
    }
    assert store.requested == [
        ("conversation", "emacs-main"),
        ("messages", "emacs-main"),
    ]


def test_replay_missing_conversation_fails_closed(monkeypatch, capsys):
    import zara.desktop.conversation as conversation

    monkeypatch.setattr(
        conversation,
        "ConversationStore",
        lambda: _ReplayStore(missing=True),
    )

    assert cli._run_conversation_replay("missing") == 2
    captured = capsys.readouterr()
    assert captured.out == ""
    assert "unknown conversation" in captured.err


def test_main_dispatches_replay_without_daemon_or_provider_path(monkeypatch):
    captured: list[str] = []
    _install_config(monkeypatch)
    monkeypatch.setattr(
        cli,
        "_run_conversation_replay",
        lambda conversation_id: captured.append(conversation_id) or 0,
    )
    monkeypatch.setattr(
        cli,
        "_default_daemon_endpoint",
        lambda: (_ for _ in ()).throw(AssertionError("daemon path must not run")),
    )
    monkeypatch.setattr(
        sys,
        "argv",
        ["zara", "--replay-conversation", "emacs-main"],
    )

    with pytest.raises(SystemExit) as exit_info:
        cli.main()

    assert exit_info.value.code == 0
    assert captured == ["emacs-main"]


@pytest.mark.parametrize(
    "argv",
    [
        ["zara", "--replay-conversation", "emacs-main", "hello"],
        ["zara", "--replay-conversation", "emacs-main", "--connect", "tcp://host:1"],
        ["zara", "--replay-conversation", "emacs-main", "--standalone"],
        ["zara", "--replay-conversation", "emacs-main", "--cancel-turn", "turn-1"],
        ["zara", "--replay-conversation", "emacs-main", "--conversation-id", "other"],
        ["zara", "--replay-conversation", "emacs-main", "--context-id", "doc:1"],
        ["zara", "--replay-conversation", "emacs-main", "--json-events"],
    ],
)
def test_main_rejects_replay_with_turn_or_network_modes(monkeypatch, argv):
    _install_config(monkeypatch)
    monkeypatch.setattr(sys, "argv", argv)

    with pytest.raises(SystemExit) as exit_info:
        cli.main()

    assert exit_info.value.code == 2
