"""Model Context Protocol client integration for Zara."""

from __future__ import annotations

from typing import Any

__all__ = [
    "MCPConfigError",
    "MCPConfigStore",
    "MCPManager",
    "MCPServerConfig",
]


def __getattr__(name: str) -> Any:
    # Keep MCP protocol/LangChain dependencies out of unrelated Zara startup
    # paths. They are imported only when MCP configuration is actually used.
    if name in {"MCPConfigError", "MCPConfigStore", "MCPServerConfig"}:
        from .config import MCPConfigError, MCPConfigStore, MCPServerConfig

        return {
            "MCPConfigError": MCPConfigError,
            "MCPConfigStore": MCPConfigStore,
            "MCPServerConfig": MCPServerConfig,
        }[name]
    if name == "MCPManager":
        from .manager import MCPManager

        return MCPManager
    raise AttributeError(name)
