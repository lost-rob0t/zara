"""Persistent configuration for Zara MCP client connections."""

from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Mapping
from urllib.parse import urlparse

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - Python < 3.11
    import tomli as tomllib  # type: ignore


_SERVER_NAME_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")
_ENV_REF_RE = re.compile(r"\$\{([A-Za-z_][A-Za-z0-9_]*)\}")


class MCPConfigError(ValueError):
    """Raised when MCP configuration is invalid."""


def _expand(value: str) -> str:
    """Expand ``~``, ``$VAR`` and ``${VAR}`` without logging resolved secrets."""
    return os.path.expanduser(os.path.expandvars(value))


def _safe_mapping(values: Mapping[str, str]) -> dict[str, str]:
    # Treat every configured header/environment value as potentially secret.
    # Diagnostics expose keys only; resolved values never enter repr/log output.
    return {key: "<redacted>" for key in values}


def _quote_toml(value: str) -> str:
    escaped = value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
    return f'"{escaped}"'


def _toml_value(value: Any) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return repr(value)
    if isinstance(value, str):
        return _quote_toml(value)
    if isinstance(value, list):
        return "[" + ", ".join(_toml_value(item) for item in value) + "]"
    if isinstance(value, dict):
        pairs = [f"{_quote_toml(str(k))} = {_toml_value(v)}" for k, v in value.items()]
        return "{ " + ", ".join(pairs) + " }"
    raise TypeError(f"Unsupported TOML value: {type(value).__name__}")


@dataclass(frozen=True)
class MCPServerConfig:
    """Validated configuration for one MCP server."""

    name: str
    transport: str
    enabled: bool = True
    command: str | None = None
    args: tuple[str, ...] = ()
    env: Mapping[str, str] = field(default_factory=dict)
    cwd: str | None = None
    url: str | None = None
    headers: Mapping[str, str] = field(default_factory=dict)
    connect_timeout: float = 10.0
    request_timeout: float = 60.0

    @classmethod
    def from_mapping(
        cls,
        name: str,
        raw: Mapping[str, Any],
        *,
        default_connect_timeout: float = 10.0,
        default_request_timeout: float = 60.0,
    ) -> "MCPServerConfig":
        if not _SERVER_NAME_RE.fullmatch(name):
            raise MCPConfigError(
                f"Invalid MCP server name {name!r}; use letters, numbers, '.', '_' or '-'"
            )

        transport = str(raw.get("transport", "")).lower().strip()
        if transport == "streamable-http":
            transport = "http"
        if transport not in {"stdio", "http"}:
            raise MCPConfigError(
                f"MCP server {name!r}: transport must be 'stdio' or 'http'"
            )

        enabled = raw.get("enabled", True)
        if not isinstance(enabled, bool):
            raise MCPConfigError(f"MCP server {name!r}: enabled must be true or false")

        args = raw.get("args", [])
        if not isinstance(args, list) or any(not isinstance(item, str) for item in args):
            raise MCPConfigError(f"MCP server {name!r}: args must be a string list")

        env = raw.get("env", {})
        headers = raw.get("headers", {})
        for field_name, mapping in (("env", env), ("headers", headers)):
            if not isinstance(mapping, dict) or any(
                not isinstance(key, str) or not isinstance(value, str)
                for key, value in mapping.items()
            ):
                raise MCPConfigError(
                    f"MCP server {name!r}: {field_name} must be a string-to-string table"
                )

        connect_timeout = raw.get("connect_timeout", default_connect_timeout)
        request_timeout = raw.get("request_timeout", default_request_timeout)
        for field_name, value in (
            ("connect_timeout", connect_timeout),
            ("request_timeout", request_timeout),
        ):
            if isinstance(value, bool) or not isinstance(value, (int, float)) or value <= 0:
                raise MCPConfigError(
                    f"MCP server {name!r}: {field_name} must be a positive number"
                )

        command = raw.get("command")
        cwd = raw.get("cwd")
        url = raw.get("url")
        if command is not None and not isinstance(command, str):
            raise MCPConfigError(f"MCP server {name!r}: command must be a string")
        if cwd is not None and not isinstance(cwd, str):
            raise MCPConfigError(f"MCP server {name!r}: cwd must be a string")
        if url is not None and not isinstance(url, str):
            raise MCPConfigError(f"MCP server {name!r}: url must be a string")

        if transport == "stdio":
            if not command:
                raise MCPConfigError(f"MCP server {name!r}: stdio transport requires command")
        else:
            if not url:
                raise MCPConfigError(f"MCP server {name!r}: http transport requires url")
            parsed = urlparse(url)
            if parsed.scheme not in {"http", "https"} or not parsed.netloc:
                raise MCPConfigError(f"MCP server {name!r}: invalid HTTP URL")
            if parsed.username is not None or parsed.password is not None:
                raise MCPConfigError(
                    f"MCP server {name!r}: credentials in URLs are not allowed; use headers"
                )

        return cls(
            name=name,
            transport=transport,
            enabled=enabled,
            command=command,
            args=tuple(args),
            env=dict(env),
            cwd=cwd,
            url=url,
            headers=dict(headers),
            connect_timeout=float(connect_timeout),
            request_timeout=float(request_timeout),
        )

    def resolved_env(self) -> dict[str, str]:
        return {key: _expand(value) for key, value in self.env.items()}

    def resolved_headers(self) -> dict[str, str]:
        return {key: _expand(value) for key, value in self.headers.items()}

    def safe_dict(self) -> dict[str, Any]:
        result: dict[str, Any] = {
            "name": self.name,
            "transport": self.transport,
            "enabled": self.enabled,
            "connect_timeout": self.connect_timeout,
            "request_timeout": self.request_timeout,
        }
        if self.transport == "stdio":
            result.update(
                command=self.command,
                args=list(self.args),
                cwd=self.cwd,
                env=_safe_mapping(self.env),
            )
        else:
            result.update(url=self.url, headers=_safe_mapping(self.headers))
        return result

    def __repr__(self) -> str:
        return f"MCPServerConfig({self.safe_dict()!r})"


class MCPConfigStore:
    """Read and write ``mcp.toml`` next to Zara's main configuration."""

    def __init__(self, zara_config: Any):
        self.zara_config = zara_config
        self.path = Path(zara_config.config_dir) / "mcp.toml"

    def _base_config(self) -> dict[str, Any]:
        section = self.zara_config.get_section("mcp")
        return dict(section) if isinstance(section, dict) else {}

    def _managed_config(self) -> dict[str, Any]:
        if not self.path.exists():
            return {}
        try:
            with self.path.open("rb") as handle:
                loaded = tomllib.load(handle)
        except (OSError, tomllib.TOMLDecodeError) as error:
            raise MCPConfigError(f"Failed to load MCP config {self.path}: {error}") from error
        section = loaded.get("mcp", {})
        if not isinstance(section, dict):
            raise MCPConfigError("Invalid mcp.toml: [mcp] must be a table")
        return section

    def raw(self) -> dict[str, Any]:
        base = self._base_config()
        managed = self._managed_config()
        merged = dict(base)
        for key, value in managed.items():
            if key == "servers":
                servers = dict(base.get("servers", {})) if isinstance(base.get("servers"), dict) else {}
                if isinstance(value, dict):
                    servers.update(value)
                merged["servers"] = servers
            else:
                merged[key] = value
        return merged

    def load(self) -> dict[str, MCPServerConfig]:
        raw = self.raw()
        default_connect = raw.get("connect_timeout", 10.0)
        default_request = raw.get("request_timeout", 60.0)
        servers = raw.get("servers", {})
        if not isinstance(servers, dict):
            raise MCPConfigError("mcp.servers must be a table")
        result: dict[str, MCPServerConfig] = {}
        for name, server in servers.items():
            if not isinstance(server, dict):
                raise MCPConfigError(f"mcp.servers.{name} must be a table")
            result[name] = MCPServerConfig.from_mapping(
                name,
                server,
                default_connect_timeout=float(default_connect),
                default_request_timeout=float(default_request),
            )
        return result

    def _managed_raw(self) -> dict[str, Any]:
        managed = self._managed_config()
        servers = managed.setdefault("servers", {})
        if not isinstance(servers, dict):
            raise MCPConfigError("mcp.servers must be a table")
        return managed

    def save_server(self, name: str, values: Mapping[str, Any]) -> None:
        raw = self._managed_raw()
        servers = raw["servers"]
        assert isinstance(servers, dict)
        # Validate before mutating persistent state.
        MCPServerConfig.from_mapping(
            name,
            dict(values),
            default_connect_timeout=float(raw.get("connect_timeout", 10.0)),
            default_request_timeout=float(raw.get("request_timeout", 60.0)),
        )
        servers[name] = dict(values)
        self._write(raw)

    def remove_server(self, name: str) -> bool:
        raw = self._managed_raw()
        servers = raw["servers"]
        assert isinstance(servers, dict)
        if name not in servers:
            return False
        del servers[name]
        self._write(raw)
        return True

    def set_enabled(self, name: str, enabled: bool) -> None:
        raw = self._managed_raw()
        servers = raw["servers"]
        assert isinstance(servers, dict)
        if name not in servers:
            # A server may originate from config.toml. Copy it into managed
            # config so the enable/disable override is persistent.
            inherited = self.raw().get("servers", {}).get(name)
            if not isinstance(inherited, dict):
                raise MCPConfigError(f"Unknown MCP server {name!r}")
            servers[name] = dict(inherited)
        servers[name]["enabled"] = enabled
        self._write(raw)

    def _write(self, raw: Mapping[str, Any]) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        lines = ["# Managed by `zara mcp`; values in this file override [mcp] in config.toml.", ""]
        for key in ("connect_timeout", "request_timeout", "refresh_interval"):
            if key in raw:
                lines.append(f"mcp.{key} = {_toml_value(raw[key])}")
        if any(key in raw for key in ("connect_timeout", "request_timeout", "refresh_interval")):
            lines.append("")
        servers = raw.get("servers", {})
        if isinstance(servers, dict):
            for name in sorted(servers):
                values = servers[name]
                if not isinstance(values, dict):
                    continue
                lines.append(f"[mcp.servers.{_quote_toml(name)}]")
                preferred = (
                    "enabled",
                    "transport",
                    "command",
                    "args",
                    "cwd",
                    "url",
                    "env",
                    "headers",
                    "connect_timeout",
                    "request_timeout",
                )
                for key in preferred:
                    if key in values and values[key] is not None:
                        lines.append(f"{key} = {_toml_value(values[key])}")
                for key in values:
                    if key not in preferred and values[key] is not None:
                        lines.append(f"{key} = {_toml_value(values[key])}")
                lines.append("")
        self.path.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")


def find_env_references(config: MCPServerConfig) -> set[str]:
    """Return referenced ``${ENV}`` names for diagnostics without exposing values."""
    refs: set[str] = set()
    for value in (*config.env.values(), *config.headers.values()):
        refs.update(_ENV_REF_RE.findall(value))
    return refs
