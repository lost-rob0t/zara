"""`zara mcp` management and diagnostics."""

from __future__ import annotations

import argparse
import asyncio
import json
import sys
from typing import Any

from .config import MCPConfigError, MCPConfigStore
from .manager import MCPManager


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="zara mcp", description="Manage MCP server connections")
    sub = parser.add_subparsers(dest="action", required=True)

    sub.add_parser("list", help="List configured MCP servers")
    sub.add_parser("status", help="Connect and show MCP server status")

    for action in ("inspect", "tools", "resources", "prompts", "enable", "disable", "remove"):
        command = sub.add_parser(action)
        command.add_argument("server")

    add = sub.add_parser("add", help="Add or replace a managed MCP server")
    add.add_argument("server")
    add.add_argument("--transport", choices=("stdio", "http"), required=True)
    add.add_argument("--command")
    add.add_argument("--arg", action="append", default=[])
    add.add_argument("--cwd")
    add.add_argument("--env", action="append", default=[], metavar="KEY=VALUE")
    add.add_argument("--url")
    add.add_argument("--header", action="append", default=[], metavar="KEY=VALUE")
    add.add_argument("--connect-timeout", type=float, default=10.0)
    add.add_argument("--request-timeout", type=float, default=60.0)
    add.add_argument("--disabled", action="store_true")
    return parser


def _pairs(values: list[str], field: str) -> dict[str, str]:
    result: dict[str, str] = {}
    for value in values:
        if "=" not in value:
            raise MCPConfigError(f"{field} expects KEY=VALUE, got {value!r}")
        key, item = value.split("=", 1)
        if not key:
            raise MCPConfigError(f"{field} key cannot be empty")
        result[key] = item
    return result


async def _connected(config: Any, action: str, server: str | None) -> int:
    manager = MCPManager(config)
    if server is not None:
        if server not in manager.configs:
            print(f"Unknown MCP server: {server}", file=sys.stderr)
            return 2
        # Keep diagnostics focused: disable other configs in-memory.
        selected = manager.configs[server]
        manager.configs = {server: selected}
    try:
        await manager.ensure_started()
        if action == "status":
            for row in manager.status():
                caps = ",".join(row["capabilities"]) or "-"
                proto = row["protocol_version"] or "-"
                error = f"  {row['error']}" if row["error"] else ""
                print(f"{row['name']:<20} {row['state']:<10} {row['transport']:<6} {proto:<12} {caps}{error}")
            return 0

        assert server is not None
        if action == "inspect":
            row = manager.status()[0]
            row["config"] = manager.configs[server].safe_dict()
            print(json.dumps(row, indent=2, sort_keys=True))
        elif action == "tools":
            bindings = [binding for binding in manager.tools.values() if binding.server == server]
            print(json.dumps([binding.__dict__ for binding in bindings], indent=2, sort_keys=True))
        elif action == "resources":
            payload = {
                "resources": manager.resources.get(server, []),
                "templates": manager.resource_templates.get(server, []),
            }
            print(json.dumps(payload, indent=2, sort_keys=True))
        elif action == "prompts":
            print(json.dumps(manager.prompts.get(server, []), indent=2, sort_keys=True))
        return 0
    finally:
        await manager.shutdown()


def main(argv: list[str] | None = None, *, config: Any) -> int:
    args = _parser().parse_args(argv)
    store = MCPConfigStore(config)
    try:
        if args.action == "list":
            configs = store.load()
            for name, server in sorted(configs.items()):
                state = "enabled" if server.enabled else "disabled"
                target = server.command if server.transport == "stdio" else server.url
                print(f"{name:<20} {state:<8} {server.transport:<6} {target or ''}")
            return 0
        if args.action == "add":
            values: dict[str, Any] = {
                "enabled": not args.disabled,
                "transport": args.transport,
                "connect_timeout": args.connect_timeout,
                "request_timeout": args.request_timeout,
            }
            if args.transport == "stdio":
                values.update(
                    command=args.command,
                    args=args.arg,
                    cwd=args.cwd,
                    env=_pairs(args.env, "--env"),
                )
            else:
                values.update(url=args.url, headers=_pairs(args.header, "--header"))
            store.save_server(args.server, values)
            print(f"Saved MCP server {args.server} in {store.path}")
            return 0
        if args.action == "remove":
            if not store.remove_server(args.server):
                print(
                    f"MCP server {args.server!r} is not managed in {store.path}; remove it from config.toml if inherited",
                    file=sys.stderr,
                )
                return 2
            print(f"Removed MCP server {args.server}")
            return 0
        if args.action in {"enable", "disable"}:
            enabled = args.action == "enable"
            store.set_enabled(args.server, enabled)
            print(f"{'Enabled' if enabled else 'Disabled'} MCP server {args.server}")
            return 0
        return asyncio.run(_connected(config, args.action, getattr(args, "server", None)))
    except MCPConfigError as error:
        print(f"MCP configuration error: {error}", file=sys.stderr)
        return 2


__all__ = ["main"]
