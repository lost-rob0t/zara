from __future__ import annotations

import asyncio
from dataclasses import dataclass

import pytest

from zara.agent.approval import ToolApprovalController
from zara.runtime import events
from zara.runtime.prolog_rlm_backend import (
    PrologRLMProtocolError,
    PrologRLMRuntimeBackend,
)


class FakeConfig:
    def __init__(self, *, max_steps: int = 4) -> None:
        self.max_steps = max_steps

    def get(self, section: str, key: str, default=None):
        if section == "agent" and key == "backend":
            return "prolog_rlm"
        return default

    def get_section(self, section: str):
        if section == "agent":
            return {"max_steps": self.max_steps, "conversation_timeout": 60}
        if section == "prolog_rlm":
            return {}
        if section == "memory":
            return {"enabled": False, "top_k": 5, "max_chars": 1200}
        if section == "tool_approval":
            return {"required_tools": [], "timeout_seconds": 5.0, "max_pending": 4}
        return {}

    def get_module_search_paths(self):
        return []

    def get_agent_system_prompt(self):
        return "Test Zara system prompt"


@dataclass
class FakeTool:
    name: str
    description: str = "test tool"
    args: dict = None

    def __post_init__(self):
        if self.args is None:
            self.args = {"value": {"type": "integer"}}


class FakeToolRegistry:
    def __init__(self, *, required=()):
        self._tools = {"double": FakeTool("double"), "danger": FakeTool("danger")}
        self._required = set(required)
        self.calls = []
        self.prepared = False
        self.stopped = False

    def list_tools(self):
        return list(self._tools)

    def get_tool(self, name):
        return self._tools.get(name)

    def requires_approval(self, name):
        return name in self._required

    def execute_tool(self, name, **kwargs):
        self.calls.append((name, kwargs))
        if name == "double":
            return str(int(kwargs["value"]) * 2)
        return "done"

    def register_tools(self, tools):
        for tool in tools:
            self._tools[tool.name] = tool

    def unregister_tools(self, names):
        for name in names:
            self._tools.pop(name, None)

    async def prepare_async(self):
        self.prepared = True

    async def shutdown_async(self):
        self.stopped = True


class FakeMemory:
    def __init__(self):
        self.messages = []

    def start_session(self):
        return "session-1"

    def add_message(self, session_id, role, text):
        self.messages.append((session_id, role, text))

    def retrieve(self, _query, k=5):
        return []

    def summarise_session(self, _session_id):
        return None


class ScriptedRunner:
    def __init__(self, actions):
        self.actions = list(actions)
        self.calls = []
        self.cancelled = []
        self.stopped = False

    async def run(self, *, prompt, context, turn_id):
        self.calls.append((prompt, context, turn_id))
        if not self.actions:
            raise AssertionError("runner received more steps than scripted")
        return self.actions.pop(0)

    async def cancel(self, turn_id):
        self.cancelled.append(turn_id)

    async def stop(self):
        self.stopped = True


class BlockingRunner:
    def __init__(self):
        self.started = asyncio.Event()
        self.cancelled = asyncio.Event()

    async def run(self, *, prompt, context, turn_id):
        self.started.set()
        await self.cancelled.wait()
        raise asyncio.CancelledError

    async def cancel(self, turn_id):
        self.cancelled.set()

    async def stop(self):
        self.cancelled.set()


@pytest.mark.asyncio
async def test_prolog_rlm_backend_returns_final_action() -> None:
    runner = ScriptedRunner([{"type": "final", "text": "hello"}])
    memory = FakeMemory()
    backend = PrologRLMRuntimeBackend(
        FakeConfig(),
        runner=runner,
        tool_registry=FakeToolRegistry(),
        memory_manager=memory,
    )

    await backend.start()
    result = await backend.submit_turn("hi", turn_id="turn-1", conversation_id="conv-1")

    assert result.response == "hello"
    assert result.tool_results == ()
    assert runner.calls[0][1]["user_input"] == "hi"
    assert memory.messages[-2:] == [
        ("session-1", "user", "hi"),
        ("session-1", "assistant", "hello"),
    ]


@pytest.mark.asyncio
async def test_prolog_rlm_backend_executes_canonical_tool_then_replans() -> None:
    registry = FakeToolRegistry()
    runner = ScriptedRunner(
        [
            {"type": "tool", "name": "double", "arguments": {"value": 21}},
            {"type": "final", "text": "42"},
        ]
    )
    backend = PrologRLMRuntimeBackend(
        FakeConfig(),
        runner=runner,
        tool_registry=registry,
        memory_manager=FakeMemory(),
    )

    await backend.start()
    result = await backend.submit_turn("double 21", turn_id="turn-2")

    assert result.response == "42"
    assert registry.calls == [("double", {"value": 21})]
    assert result.tool_results == (
        {"tool": "double", "arguments": {"value": 21}, "result": "42", "success": True},
    )
    assert runner.calls[1][1]["observations"][0]["result"] == "42"


@pytest.mark.asyncio
async def test_prolog_rlm_backend_rejects_unknown_tool_without_execution() -> None:
    registry = FakeToolRegistry()
    runner = ScriptedRunner(
        [{"type": "tool", "name": "not_registered", "arguments": {}}]
    )
    backend = PrologRLMRuntimeBackend(
        FakeConfig(),
        runner=runner,
        tool_registry=registry,
        memory_manager=FakeMemory(),
    )

    await backend.start()
    with pytest.raises(PrologRLMProtocolError, match="unknown tool"):
        await backend.submit_turn("do something", turn_id="turn-3")
    assert registry.calls == []


@pytest.mark.asyncio
async def test_prolog_rlm_backend_uses_existing_tool_approval_controller() -> None:
    published = []
    controller = ToolApprovalController(
        timeout_seconds=5.0,
        max_pending=2,
        publisher=published.append,
    )
    registry = FakeToolRegistry(required={"danger"})
    runner = ScriptedRunner(
        [
            {"type": "tool", "name": "danger", "arguments": {}},
            {"type": "final", "text": "approved"},
        ]
    )
    backend = PrologRLMRuntimeBackend(
        FakeConfig(),
        runner=runner,
        tool_registry=registry,
        approval_controller=controller,
        memory_manager=FakeMemory(),
    )

    await backend.start()
    task = asyncio.create_task(backend.submit_turn("do it", turn_id="turn-4"))
    for _ in range(20):
        await asyncio.sleep(0)
        waiting = [event for event in published if isinstance(event, events.ToolWaitingForUser)]
        if waiting:
            break
    assert waiting
    assert registry.calls == []

    await backend.approve_tool(waiting[0].tool_run_id)
    result = await task

    assert result.response == "approved"
    assert registry.calls == [("danger", {})]
    assert any(isinstance(event, events.ToolStarted) for event in published)
    assert any(isinstance(event, events.ToolCompleted) for event in published)


@pytest.mark.asyncio
async def test_prolog_rlm_backend_cancellation_reaches_sidecar() -> None:
    runner = BlockingRunner()
    backend = PrologRLMRuntimeBackend(
        FakeConfig(),
        runner=runner,
        tool_registry=FakeToolRegistry(),
        memory_manager=FakeMemory(),
    )

    await backend.start()
    task = asyncio.create_task(backend.submit_turn("wait", turn_id="turn-5"))
    await runner.started.wait()
    await backend.cancel_turn("turn-5")

    with pytest.raises(asyncio.CancelledError):
        await task
    assert runner.cancelled.is_set()
