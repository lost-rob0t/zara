from __future__ import annotations

import asyncio
from types import SimpleNamespace

import zara.agent_cli as agent_cli


class Agent:
    def __init__(self, config=None):
        self.config = config
        self.conversation_manager = SimpleNamespace(enter_conversation=lambda: None)
        self.shutdown = False

    async def process_async(self, text):
        if text == "boom":
            raise RuntimeError("bad")
        return {"response": text.upper(), "tool_results": [{"tool": "demo"}] if text == "tools" else []}

    async def shutdown_async(self):
        self.shutdown = True


def test_chat_loop_processes_blank_tools_errors_and_exit(monkeypatch, capsys):
    agent = Agent()
    monkeypatch.setattr(agent_cli, "get_config", lambda: object())
    monkeypatch.setattr(agent_cli, "AgentManager", lambda config=None: agent)
    inputs = iter([" ", "hello", "tools", "boom", "quit"])
    monkeypatch.setattr("builtins.input", lambda _prompt: next(inputs))
    asyncio.run(agent_cli.chat_loop())
    captured = capsys.readouterr()
    assert "Zara: HELLO" in captured.out
    assert "[Tools used: demo]" in captured.out
    assert "Goodbye!" in captured.out
    assert "Error: bad" in captured.err
    assert agent.shutdown is True


def test_chat_loop_keyboard_interrupt_still_shuts_down(monkeypatch, capsys):
    agent = Agent()
    monkeypatch.setattr(agent_cli, "get_config", lambda: object())
    monkeypatch.setattr(agent_cli, "AgentManager", lambda config=None: agent)
    monkeypatch.setattr("builtins.input", lambda _prompt: (_ for _ in ()).throw(KeyboardInterrupt()))
    asyncio.run(agent_cli.chat_loop())
    assert "Goodbye!" in capsys.readouterr().out
    assert agent.shutdown is True


def test_main_returns_zero_or_one(monkeypatch, capsys):
    def ok(coro):
        coro.close()
        return None

    monkeypatch.setattr(agent_cli.asyncio, "run", ok)
    assert agent_cli.main() == 0

    def interrupt(coro):
        coro.close()
        raise KeyboardInterrupt

    monkeypatch.setattr(agent_cli.asyncio, "run", interrupt)
    assert agent_cli.main() == 0

    def explode(coro):
        coro.close()
        raise RuntimeError("boom")

    monkeypatch.setattr(agent_cli.asyncio, "run", explode)
    assert agent_cli.main() == 1
    assert "Fatal error: boom" in capsys.readouterr().err
