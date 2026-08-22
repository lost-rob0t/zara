from __future__ import annotations

from pathlib import Path

import pytest

from zara.mcp.config import MCPConfigError, MCPConfigStore, MCPServerConfig


class FakeConfig:
    def __init__(self, root: Path, mcp: dict | None = None):
        self.config_dir = root
        self._mcp = mcp or {}

    def get_section(self, name: str):
        return self._mcp if name == "mcp" else {}


def test_valid_stdio_and_http_config(monkeypatch):
    monkeypatch.setenv("MCP_TOKEN", "secret-value")
    stdio = MCPServerConfig.from_mapping(
        "filesystem",
        {
            "transport": "stdio",
            "command": "npx",
            "args": ["-y", "server"],
            "env": {"TOKEN": "${MCP_TOKEN}"},
        },
    )
    assert stdio.transport == "stdio"
    assert stdio.resolved_env()["TOKEN"] == "secret-value"

    http = MCPServerConfig.from_mapping(
        "research",
        {
            "transport": "streamable-http",
            "url": "https://example.test/mcp",
            "headers": {"Authorization": "Bearer ${MCP_TOKEN}"},
        },
    )
    assert http.transport == "http"
    assert http.resolved_headers()["Authorization"] == "Bearer secret-value"


def test_invalid_config_rejected():
    with pytest.raises(MCPConfigError):
        MCPServerConfig.from_mapping("bad name", {"transport": "stdio", "command": "x"})
    with pytest.raises(MCPConfigError):
        MCPServerConfig.from_mapping("x", {"transport": "stdio"})
    with pytest.raises(MCPConfigError):
        MCPServerConfig.from_mapping("x", {"transport": "http", "url": "file:///tmp/x"})
    with pytest.raises(MCPConfigError):
        MCPServerConfig.from_mapping("x", {"transport": "http", "url": "https://u:p@example.test/mcp"})


def test_repr_and_safe_dict_never_expose_configured_values():
    config = MCPServerConfig.from_mapping(
        "secret",
        {
            "transport": "stdio",
            "command": "server",
            "env": {"ORDINARY": "still-sensitive", "API_TOKEN": "super-secret"},
        },
    )
    rendered = repr(config)
    assert "still-sensitive" not in rendered
    assert "super-secret" not in rendered
    assert config.safe_dict()["env"] == {
        "ORDINARY": "<redacted>",
        "API_TOKEN": "<redacted>",
    }


def test_managed_config_round_trip_and_literal_dotted_server_name(tmp_path):
    store = MCPConfigStore(FakeConfig(tmp_path))
    store.save_server(
        "dev.files",
        {
            "transport": "stdio",
            "command": "python",
            "args": ["server.py"],
            "env": {"TOKEN": "${TOKEN}"},
        },
    )
    loaded = store.load()
    assert set(loaded) == {"dev.files"}
    assert loaded["dev.files"].args == ("server.py",)
    text = store.path.read_text()
    assert '[mcp.servers."dev.files"]' in text


def test_managed_config_overrides_main_config(tmp_path):
    base = {
        "servers": {
            "dev": {"transport": "stdio", "command": "old"},
        }
    }
    store = MCPConfigStore(FakeConfig(tmp_path, base))
    store.save_server("dev", {"transport": "stdio", "command": "new"})
    assert store.load()["dev"].command == "new"
    store.set_enabled("dev", False)
    assert store.load()["dev"].enabled is False
    assert store.remove_server("dev") is True
    assert store.load()["dev"].command == "old"
