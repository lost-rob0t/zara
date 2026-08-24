from __future__ import annotations

import threading

from langchain_core.messages import AIMessage, HumanMessage, ToolMessage
from langchain_core.tools import tool

from zara.agent import AgentManager
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.runtime.commands import ApproveTool, SubmitTurn
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.server import PrincipalContext


class ApprovalConfig:
    def get_llm_config(self):
        return {}

    def get_section(self, name: str):
        sections = {
            "agent": {"conversation_timeout": 60, "max_steps": 4},
            "memory": {"max_chars": 1200, "top_k": 5},
            "tool_approval": {
                "required_tools": ["approval_effect"],
                "timeout_seconds": 5.0,
                "max_pending": 4,
            },
        }
        return sections.get(name, {})

    def get_module_search_paths(self):
        return []

    def get_tool_config(self):
        return {"file_tools": False}

    def get_agent_system_prompt(self):
        return "Test assistant."


class EmptyMemory:
    def retrieve(self, _query, k=5):
        return []


class ApprovalLLM:
    def __init__(self, secret: str) -> None:
        self.secret = secret

    def bind_tools(self, _tools):
        return self

    async def ainvoke(self, messages):
        if isinstance(messages[-1], ToolMessage):
            return AIMessage(content=f"finished: {messages[-1].content}")
        assert isinstance(messages[-1], HumanMessage)
        return AIMessage(
            content="",
            tool_calls=[
                {
                    "name": "approval_effect",
                    "args": {"value": self.secret},
                    "id": "approval-call-1",
                    "type": "tool_call",
                }
            ],
        )


class EventRecorder:
    def __init__(self) -> None:
        self.bus = runtime_bridge.RuntimeEventBus()
        self.events: list[events.RuntimeEvent] = []
        self.waiting = threading.Event()
        self.output_ready = threading.Event()

    def __call__(self, event: events.RuntimeEvent):
        self.events.append(event)
        if isinstance(event, events.ToolWaitingForUser):
            self.waiting.set()
        if isinstance(event, events.OutputReady):
            self.output_ready.set()
        return self.bus.publish(event)


def stop_host(host: RuntimeHost) -> None:
    if host.state not in {
        RuntimeHostState.NEW,
        RuntimeHostState.STOPPED,
        RuntimeHostState.FAILED,
    }:
        host.shutdown("test cleanup").result(timeout=5)
    host.join(timeout=5)


def test_required_tool_waits_for_runtime_approval_and_runs_once(monkeypatch):
    effects: list[str] = []
    secret = "PRIVATE-TOOL-ARGUMENT"

    @tool("approval_effect")
    def approval_effect(value: str) -> str:
        """Record one deterministic approval-protected side effect."""
        effects.append(value)
        return "effect complete"

    monkeypatch.setattr(
        "zara.agent.tools.builtin_tools.get_builtin_tools",
        lambda *_args, **_kwargs: [],
    )
    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda _self, _config: ApprovalLLM(secret),
    )
    principal = PrincipalContext("test:approval-owner", kind="test")
    manager = AgentManager(
        config=ApprovalConfig(),
        memory_manager=EmptyMemory(),
        principal=principal,
    )
    manager.tool_registry.register_tool(approval_effect)
    recorder = EventRecorder()
    host = RuntimeHost(
        lambda: LangGraphRuntimeBackend(lambda: manager),
        publisher=recorder,
    )

    try:
        host.start().result(timeout=5)
        receipt = host.submit(
            SubmitTurn(text="perform the protected effect", conversation_id="conversation-1")
        ).result(timeout=5)

        assert receipt.turn_id is not None
        assert recorder.waiting.wait(timeout=3)
        assert effects == []

        approved = host.submit(
            ApproveTool(tool_run_id="approval-call-1")
        ).result(timeout=5)
        assert approved.detail == "tool approved"
        assert recorder.output_ready.wait(timeout=5)
        assert effects == [secret]

        lifecycle = [
            type(event)
            for event in recorder.events
            if getattr(event, "tool_run_id", None) == "approval-call-1"
        ]
        assert lifecycle == [
            events.ToolQueued,
            events.ToolWaitingForUser,
            events.ToolStarted,
            events.ToolCompleted,
        ]
        assert all(secret not in repr(event) for event in recorder.events)
    finally:
        stop_host(host)
