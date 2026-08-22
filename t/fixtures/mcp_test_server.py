"""Small real MCP server used by Zara MCP integration tests."""

from __future__ import annotations

import os
import sys

import anyio
from mcp.server import MCPServer

mcp = MCPServer("zara-test-server")


@mcp.tool()
def echo(text: str) -> dict[str, str]:
    """Echo text back as structured content."""
    return {"echo": text}


@mcp.tool()
def fail() -> str:
    """Return a normal MCP tool error without killing the connection."""
    raise ValueError("fixture tool failure")


@mcp.tool()
async def slow(seconds: float = 1.0) -> str:
    """Sleep so cancellation and timeout behavior can be tested."""
    await anyio.sleep(seconds)
    return "done"


@mcp.tool()
def pid() -> int:
    """Return this MCP subprocess PID."""
    return os.getpid()


@mcp.tool()
def crash() -> None:
    """Abruptly exit to test failure isolation and reconnect."""
    os._exit(19)


@mcp.resource("test://hello")
def hello_resource() -> str:
    """A fixed test resource."""
    return "hello from MCP"


@mcp.resource("greeting://{name}")
def greeting_resource(name: str) -> str:
    """A templated test resource."""
    return f"hello {name}"


@mcp.prompt()
def welcome(name: str) -> str:
    """A test prompt."""
    return f"Welcome, {name}."


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--http":
        mcp.run(
            "streamable-http",
            host="127.0.0.1",
            port=int(sys.argv[2]),
            streamable_http_path="/mcp",
        )
    else:
        mcp.run()
