from __future__ import annotations

import asyncio
from types import SimpleNamespace

import pytest

import zara.mcp.cli as cli


class Server:
    def __init__(self, *, enabled=True, transport="stdio", command="cmd", url=None):
        self.enabled = enabled
        self.transport = transport
        self.command = command
        self.url = url

    def safe_dict(self):
        return {"enabled": self.enabled, "transport": self.transport}


class Store:
    def __init__(self):
        self.path = "/tmp/mcp.toml"
        self.configs = {
            "alpha": Server(),
            "beta": Server(enabled=False, transport="http", command=None, url="https://mcp.invalid"),
        }
        self.saved = []
        self.removed = True
        self.enabled = []

    def load(self):
        return self.configs

    def save_server(self, name, values):
        self.saved.append((name, values))

    def remove_server(self, _name):
        return self.removed

    def set_enabled(self, name, enabled):
        self.enabled.append((name, enabled))


class Manager:
    def __init__(self):
        self.configs = {"alpha": Server()}
        self.tools = {
            "one": SimpleNamespace(server="alpha", name="one"),
            "other": SimpleNamespace(server="beta", name="other"),
        }
        self.resources = {"alpha": [{"uri": "x"}]}
        self.resource_templates = {"alpha": [{"uriTemplate": "x/{id}"}]}
        self.prompts = {"alpha": [{"name": "p"}]}
        self.started = 0
        self.stopped = 0

    async def ensure_started(self):
        self.started += 1

    async def shutdown(self):
        self.stopped += 1

    def status(self):
        return [{
            "name": "alpha", "state": "ready", "transport": "stdio",
            "protocol_version": "1", "capabilities": ["tools"], "error": None,
        }]


@pytest.fixture
def store(monkeypatch):
    value = Store()
    monkeypatch.setattr(cli, "MCPConfigStore", lambda _config: value)
    return value


def test_pairs_accepts_and_rejects_values():
    assert cli._pairs(["A=1", "B=two=three"], "--env") == {"A": "1", "B": "two=three"}
    with pytest.raises(cli.MCPConfigError, match="expects KEY=VALUE"):
        cli._pairs(["bad"], "--env")
    with pytest.raises(cli.MCPConfigError, match="key cannot be empty"):
        cli._pairs(["=bad"], "--header")


def test_main_list_add_remove_and_toggle(store, capsys):
    config = object()
    assert cli.main(["list"], config=config) == 0
    listed = capsys.readouterr().out
    assert "alpha" in listed and "enabled" in listed
    assert "beta" in listed and "https://mcp.invalid" in listed

    assert cli.main([
        "add", "local", "--transport", "stdio", "--command", "python",
        "--arg", "-m", "--cwd", "/tmp", "--env", "A=1", "--disabled",
        "--connect-timeout", "2", "--request-timeout", "3",
    ], config=config) == 0
    name, values = store.saved[-1]
    assert name == "local"
    assert values["enabled"] is False and values["command"] == "python"
    assert values["args"] == ["-m"] and values["env"] == {"A": "1"}

    assert cli.main([
        "add", "remote", "--transport", "http", "--url", "https://mcp.invalid",
        "--header", "Authorization=Bearer x",
    ], config=config) == 0
    assert store.saved[-1][1]["headers"] == {"Authorization": "Bearer x"}

    store.removed = False
    assert cli.main(["remove", "inherited"], config=config) == 2
    assert "not managed" in capsys.readouterr().err
    store.removed = True
    assert cli.main(["remove", "alpha"], config=config) == 0
    assert cli.main(["enable", "alpha"], config=config) == 0
    assert cli.main(["disable", "alpha"], config=config) == 0
    assert store.enabled == [("alpha", True), ("alpha", False)]

    assert cli.main(["add", "bad", "--transport", "stdio", "--env", "oops"], config=config) == 2
    assert "MCP configuration error" in capsys.readouterr().err


def test_connected_diagnostics_cover_all_actions(monkeypatch, capsys):
    manager = Manager()
    monkeypatch.setattr(cli, "MCPManager", lambda _config: manager)

    assert asyncio.run(cli._connected(object(), "status", None)) == 0
    assert "alpha" in capsys.readouterr().out
    assert manager.started == 1 and manager.stopped == 1

    for action, needle in [
        ("inspect", '"config"'),
        ("tools", '"name": "one"'),
        ("resources", '"templates"'),
        ("prompts", '"name": "p"'),
    ]:
        manager.configs = {"alpha": Server()}
        assert asyncio.run(cli._connected(object(), action, "alpha")) == 0
        assert needle in capsys.readouterr().out

    manager.configs = {"alpha": Server()}
    assert asyncio.run(cli._connected(object(), "inspect", "missing")) == 2
    assert "Unknown MCP server" in capsys.readouterr().err


def test_main_dispatches_connected_action(store, monkeypatch):
    seen = []

    async def fake_connected(config, action, server):
        seen.append((config, action, server))
        return 7

    monkeypatch.setattr(cli, "_connected", fake_connected)
    marker = object()
    assert cli.main(["inspect", "alpha"], config=marker) == 7
    assert seen == [(marker, "inspect", "alpha")]
