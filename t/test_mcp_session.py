from __future__ import annotations

import asyncio
from contextlib import asynccontextmanager
from types import SimpleNamespace

import pytest

from zara.mcp.config import MCPServerConfig
from zara.mcp.session import (
    MCPRequestError,
    MCPServerActor,
    MCPSessionState,
    _RedactingWriter,
    normalize_tool_result,
)


class FakeClient:
    protocol_version = "2026-07-28"
    server_info = SimpleNamespace(model_dump=lambda **_: {"name": "fake"})
    server_capabilities = SimpleNamespace(
        tools=SimpleNamespace(list_changed=False),
        resources=None,
        prompts=None,
    )
    instructions = None

    async def echo(self, value):
        return value

    async def slow(self):
        await asyncio.sleep(60)

    async def explode(self):
        raise ConnectionError("gone")


class FakeActor(MCPServerActor):
    @asynccontextmanager
    async def _open_client(self):
        yield FakeClient()


def make_actor(timeout: float = 1.0) -> FakeActor:
    config = MCPServerConfig.from_mapping(
        "fake",
        {
            "transport": "stdio",
            "command": "ignored",
            "connect_timeout": 1,
            "request_timeout": timeout,
        },
    )
    return FakeActor(config)


@pytest.mark.asyncio
async def test_actor_lifecycle_and_request():
    actor = make_actor()
    await actor.start()
    assert actor.state is MCPSessionState.READY
    assert await actor.request("echo", "ok") == "ok"
    await actor.stop()
    assert actor.state is MCPSessionState.STOPPED


@pytest.mark.asyncio
async def test_timeout_cancels_only_operation_and_keeps_session_ready():
    actor = make_actor(timeout=0.02)
    await actor.start()
    with pytest.raises(MCPRequestError, match="timed out"):
        await actor.request("slow")
    assert actor.state is MCPSessionState.READY
    assert await actor.request("echo", "alive") == "alive"
    await actor.stop()


@pytest.mark.asyncio
async def test_caller_cancellation_keeps_session_ready():
    actor = make_actor()
    await actor.start()
    task = asyncio.create_task(actor.request("slow"))
    await asyncio.sleep(0)
    task.cancel()
    with pytest.raises(asyncio.CancelledError):
        await task
    assert actor.state is MCPSessionState.READY
    assert await actor.request("echo", "alive") == "alive"
    await actor.stop()


@pytest.mark.asyncio
async def test_protocol_or_transport_exception_marks_actor_failed():
    actor = make_actor()
    await actor.start()
    with pytest.raises(MCPRequestError, match="ConnectionError"):
        await actor.request("explode")
    await asyncio.sleep(0)
    assert actor.state is MCPSessionState.FAILED


def test_tool_result_normalization_preserves_all_content_and_structure():
    class Block:
        def __init__(self, kind, value):
            self.kind = kind
            self.value = value

        def model_dump(self, **_):
            return {"type": self.kind, "value": self.value}

    result = SimpleNamespace(
        is_error=False,
        content=[Block("text", "hello"), Block("image", "opaque")],
        structured_content={"answer": 42},
        meta={"origin": "test"},
    )
    normalized = normalize_tool_result(result)
    assert normalized["content"][1]["type"] == "image"
    assert normalized["structured_content"] == {"answer": 42}
    assert normalized["meta"] == {"origin": "test"}


def test_stdio_secret_errlog_exposes_real_suppressed_fd(tmp_path):
    stderr_path = tmp_path / "stderr.log"
    with stderr_path.open("w+", encoding="utf-8") as target:
        writer = _RedactingWriter(target, ["super-secret"])
        try:
            assert writer.fileno() != target.fileno()
            assert writer.write("super-secret\n") == len("super-secret\n")
            writer.flush()
        finally:
            writer.close()
        target.seek(0)
        assert target.read() == ""


def test_stdio_errlog_without_secrets_keeps_normal_stderr_fd(tmp_path):
    stderr_path = tmp_path / "stderr.log"
    with stderr_path.open("w+", encoding="utf-8") as target:
        writer = _RedactingWriter(target, [])
        try:
            assert writer.fileno() == target.fileno()
            writer.write("diagnostic\n")
            writer.flush()
        finally:
            writer.close()
        target.seek(0)
        assert target.read() == "diagnostic\n"
