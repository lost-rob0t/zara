from __future__ import annotations

import asyncio
import json
import os
from importlib.metadata import version
import socket
import subprocess
import sys
from pathlib import Path

import httpx2
import mcp
import pytest

assert int(version("mcp").split(".", 1)[0]) >= 2

from zara.agent.tools.registry import ToolRegistry
from zara.mcp.manager import MCPManager
from zara.mcp.session import MCPRequestError


FIXTURE = Path(__file__).parent / "fixtures" / "mcp_test_server.py"


def _find_int(value):
    if isinstance(value, int) and not isinstance(value, bool):
        return value
    if isinstance(value, dict):
        for item in value.values():
            found = _find_int(item)
            if found is not None:
                return found
    if isinstance(value, list):
        for item in value:
            found = _find_int(item)
            if found is not None:
                return found
    return None


def _free_port() -> int:
    sock = socket.socket()
    sock.bind(("127.0.0.1", 0))
    port = sock.getsockname()[1]
    sock.close()
    return port


async def _wait_for_port(port: int) -> None:
    for _ in range(200):
        try:
            reader, writer = await asyncio.open_connection("127.0.0.1", port)
        except OSError:
            await asyncio.sleep(0.02)
            continue
        writer.close()
        await writer.wait_closed()
        return
    raise AssertionError(f"HTTP MCP fixture did not listen on port {port}")


class FakeConfig:
    def __init__(self, root: Path, servers: dict):
        self.config_dir = root
        self._mcp = {"refresh_interval": 0.05, "servers": servers}

    def get_section(self, name: str):
        return self._mcp if name == "mcp" else {}


def server_config(*, timeout: float = 2.0) -> dict:
    return {
        "transport": "stdio",
        "command": sys.executable,
        "args": [str(FIXTURE)],
        "connect_timeout": 5.0,
        "request_timeout": timeout,
    }


@pytest.mark.asyncio
async def test_real_stdio_server_discovers_and_executes_native_capabilities(tmp_path):
    config = FakeConfig(tmp_path, {"fixture": server_config()})
    registry = ToolRegistry(config=config)
    manager = MCPManager(config, registry)
    try:
        await manager.ensure_started()
        identities = set(manager.tools)
        assert "mcp.fixture.echo" in identities
        assert any(tool.name.startswith("mcp__fixture__echo") for tool in registry.to_langchain_tools())
        assert manager.resources["fixture"]
        assert manager.resource_templates["fixture"]
        assert manager.prompts["fixture"]

        tool_result = await manager.call_tool("fixture", "echo", {"text": "rete"})
        assert tool_result["is_error"] is False
        assert "rete" in json.dumps(tool_result)

        resource = await manager.read_resource("fixture", "test://hello")
        assert "hello from MCP" in json.dumps(resource)

        prompt = await manager.get_prompt("fixture", "welcome", {"name": "Zara"})
        assert "Welcome, Zara" in json.dumps(prompt)
    finally:
        await manager.shutdown()


@pytest.mark.asyncio
async def test_two_servers_are_isolated_and_namespaced(tmp_path):
    config = FakeConfig(
        tmp_path,
        {"alpha": server_config(), "beta": server_config()},
    )
    manager = MCPManager(config)
    try:
        await manager.ensure_started()
        assert "mcp.alpha.echo" in manager.tools
        assert "mcp.beta.echo" in manager.tools

        alpha, beta = await asyncio.gather(
            manager.call_tool("alpha", "echo", {"text": "A"}),
            manager.call_tool("beta", "echo", {"text": "B"}),
        )
        assert "A" in json.dumps(alpha)
        assert "B" in json.dumps(beta)

        with pytest.raises(MCPRequestError):
            await manager.call_tool("alpha", "crash", {})
        beta_after = await manager.call_tool("beta", "echo", {"text": "still-alive"})
        assert "still-alive" in json.dumps(beta_after)

        # Recovery prepares a future call without replaying the failed crash() side effect.
        await manager.ensure_started()
        alpha_after = await manager.call_tool("alpha", "echo", {"text": "reconnected"})
        assert "reconnected" in json.dumps(alpha_after)
    finally:
        await manager.shutdown()


@pytest.mark.asyncio
async def test_tool_error_is_data_not_session_failure(tmp_path):
    config = FakeConfig(tmp_path, {"fixture": server_config()})
    manager = MCPManager(config)
    try:
        await manager.ensure_started()
        result = await manager.call_tool("fixture", "fail", {})
        assert result["is_error"] is True
        followup = await manager.call_tool("fixture", "echo", {"text": "alive"})
        assert "alive" in json.dumps(followup)
    finally:
        await manager.shutdown()


@pytest.mark.asyncio
async def test_timeout_and_cancellation_do_not_kill_healthy_session(tmp_path):
    config = FakeConfig(tmp_path, {"fixture": server_config(timeout=0.1)})
    manager = MCPManager(config)
    try:
        await manager.ensure_started()
        with pytest.raises(MCPRequestError, match="timed out"):
            await manager.call_tool("fixture", "slow", {"seconds": 1.0})
        followup = await manager.call_tool("fixture", "echo", {"text": "after-timeout"})
        assert "after-timeout" in json.dumps(followup)

        task = asyncio.create_task(manager.call_tool("fixture", "slow", {"seconds": 1.0}))
        await asyncio.sleep(0.02)
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
        followup = await manager.call_tool("fixture", "echo", {"text": "after-cancel"})
        assert "after-cancel" in json.dumps(followup)
    finally:
        await manager.shutdown()


@pytest.mark.asyncio
async def test_shutdown_reaps_stdio_child(tmp_path):
    config = FakeConfig(tmp_path, {"fixture": server_config()})
    manager = MCPManager(config)
    await manager.ensure_started()
    pid_result = await manager.call_tool("fixture", "pid", {})
    child_pid = _find_int(pid_result.get("structured_content"))
    assert child_pid is not None
    await manager.shutdown()

    for _ in range(100):
        try:
            os.kill(child_pid, 0)
        except ProcessLookupError:
            break
        await asyncio.sleep(0.01)
    else:
        pytest.fail(f"MCP child process {child_pid} survived manager shutdown")


@pytest.mark.asyncio
async def test_real_streamable_http_server(tmp_path):
    port = _free_port()
    process = subprocess.Popen(
        [sys.executable, str(FIXTURE), "--http", str(port)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    manager = None
    try:
        await _wait_for_port(port)
        config = FakeConfig(
            tmp_path,
            {
                "http": {
                    "transport": "http",
                    "url": f"http://127.0.0.1:{port}/mcp",
                    "connect_timeout": 5.0,
                    "request_timeout": 2.0,
                }
            },
        )
        manager = MCPManager(config)
        await manager.ensure_started()
        result = await manager.call_tool("http", "echo", {"text": "streamable"})
        assert "streamable" in json.dumps(result)
    finally:
        if manager is not None:
            await manager.shutdown()
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=3)
