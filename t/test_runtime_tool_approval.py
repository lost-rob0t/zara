import asyncio
from pathlib import Path
import threading
import time

import pytest

from langchain_core.messages import AIMessage, HumanMessage, ToolMessage
from langchain_core.tools import tool

from zara.agent import AgentManager
from zara.agent.approval import ApprovalRequest, ToolApprovalController, ToolApprovalError
from zara.agent.tools.registry import ToolRegistry
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.runtime.commands import (
    ApproveTool,
    CancelTurn,
    RejectTool,
    RestartRuntime,
    SubmitTurn,
)
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.server import PrincipalContext


class ApprovalConfig:
    config_dir = Path("/nonexistent-zara-approval-test-config")

    def __init__(
        self,
        *,
        required_tools=("approval_effect",),
        timeout_seconds=5.0,
        max_pending=4,
    ) -> None:
        self.required_tools = list(required_tools)
        self.timeout_seconds = timeout_seconds
        self.max_pending = max_pending

    def get_llm_config(self):
        return {}

    def get_section(self, name: str):
        sections = {
            "agent": {"conversation_timeout": 60, "max_steps": 4},
            "memory": {"max_chars": 1200, "top_k": 5},
            "tool_approval": {
                "required_tools": self.required_tools,
                "timeout_seconds": self.timeout_seconds,
                "max_pending": self.max_pending,
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


class MultipleApprovalLLM:
    def __init__(self, *, duplicate_ids=False) -> None:
        self.duplicate_ids = duplicate_ids

    def bind_tools(self, _tools):
        return self

    async def ainvoke(self, messages):
        if isinstance(messages[-1], ToolMessage):
            return AIMessage(content="all effects finished")
        second_id = "approval-call-1" if self.duplicate_ids else "approval-call-2"
        return AIMessage(
            content="",
            tool_calls=[
                {
                    "name": "approval_effect",
                    "args": {"value": "first"},
                    "id": "approval-call-1",
                    "type": "tool_call",
                },
                {
                    "name": "approval_effect",
                    "args": {"value": "second"},
                    "id": second_id,
                    "type": "tool_call",
                },
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


def build_runtime(
    monkeypatch,
    *,
    secret="PRIVATE-TOOL-ARGUMENT",
    config=None,
    principal_id="test:approval-owner",
    llm=None,
    effect_error=None,
):
    effects: list[str] = []

    @tool("approval_effect")
    def approval_effect(value: str) -> str:
        """Record one deterministic approval-protected side effect."""
        if effect_error is not None:
            raise RuntimeError(effect_error)
        effects.append(value)
        return "effect complete"

    monkeypatch.setattr(
        "zara.agent.tools.builtin_tools.get_builtin_tools",
        lambda *_args, **_kwargs: [],
    )
    llm = llm or ApprovalLLM(secret)
    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda _self, _config: llm,
    )
    principal = PrincipalContext(principal_id, kind="test")
    manager = AgentManager(
        config=config or ApprovalConfig(),
        memory_manager=EmptyMemory(),
        principal=principal,
    )
    manager.tool_registry.register_tool(approval_effect)
    recorder = EventRecorder()
    host = RuntimeHost(
        lambda: LangGraphRuntimeBackend(lambda: manager),
        publisher=recorder,
    )
    return host, recorder, manager, effects


def wait_for_tool_event(recorder, event_type, tool_run_id, timeout=3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if any(
            isinstance(event, event_type)
            and getattr(event, "tool_run_id", None) == tool_run_id
            for event in recorder.events
        ):
            return True
        time.sleep(0.01)
    return False


def test_required_tool_waits_for_runtime_approval_and_runs_once(monkeypatch):
    secret = "PRIVATE-TOOL-ARGUMENT"
    host, recorder, _manager, effects = build_runtime(monkeypatch, secret=secret)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(
            SubmitTurn(text="perform the protected effect", conversation_id="conversation-1")
        ).result(timeout=5)

        assert receipt.turn_id is not None
        assert recorder.waiting.wait(timeout=3), recorder.events
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


def test_reject_never_runs_tool_and_replay_fails_closed(monkeypatch):
    secret = "PRIVATE-REJECTION-ARGUMENT"
    rejection = "PRIVATE-REJECTION-REASON"
    host, recorder, _manager, effects = build_runtime(monkeypatch, secret=secret)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="reject the effect")).result(timeout=5)
        assert recorder.waiting.wait(timeout=3), recorder.events

        rejected = host.submit(
            RejectTool(tool_run_id="approval-call-1", reason=rejection)
        ).result(timeout=5)
        assert rejected.detail == "tool rejected"
        assert recorder.output_ready.wait(timeout=5)
        assert effects == []
        assert any(
            isinstance(event, events.ToolCancelled)
            and event.tool_run_id == "approval-call-1"
            and event.reason == "tool rejected"
            for event in recorder.events
        )

        with pytest.raises(ToolApprovalError, match="not pending"):
            host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        rendered_events = repr(recorder.events)
        assert secret not in rendered_events
        assert rejection not in rendered_events
    finally:
        stop_host(host)


def test_wrong_id_does_not_release_pending_tool(monkeypatch):
    host, recorder, _manager, effects = build_runtime(monkeypatch)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="keep waiting")).result(timeout=5)
        assert recorder.waiting.wait(timeout=3), recorder.events

        with pytest.raises(ToolApprovalError, match="not pending"):
            host.submit(ApproveTool(tool_run_id="wrong-id")).result(timeout=5)
        assert effects == []

        host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert recorder.output_ready.wait(timeout=5)
        assert len(effects) == 1
    finally:
        stop_host(host)


def test_cancelled_pending_turn_cannot_be_approved(monkeypatch):
    host, recorder, manager, effects = build_runtime(monkeypatch)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="cancel before effect")).result(timeout=5)
        assert receipt.turn_id is not None
        assert recorder.waiting.wait(timeout=3), recorder.events

        cancelled = host.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=5)
        assert cancelled.detail == "turn cancelled"
        with pytest.raises(ToolApprovalError, match="not pending"):
            host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert effects == []
        assert manager.approval_controller.pending_count == 0
        assert any(
            isinstance(event, events.ToolCancelled)
            and event.tool_run_id == "approval-call-1"
            for event in recorder.events
        )
    finally:
        stop_host(host)


def test_approval_timeout_rejects_without_side_effect(monkeypatch):
    config = ApprovalConfig(timeout_seconds=0.05)
    host, recorder, manager, effects = build_runtime(monkeypatch, config=config)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="let approval expire")).result(timeout=5)
        assert recorder.waiting.wait(timeout=3), recorder.events
        assert recorder.output_ready.wait(timeout=5)
        assert effects == []
        assert manager.approval_controller.pending_count == 0
        assert any(
            isinstance(event, events.ToolCancelled)
            and event.reason == "approval timeout"
            for event in recorder.events
        )
    finally:
        stop_host(host)


def test_tool_without_approval_policy_preserves_direct_execution(monkeypatch):
    config = ApprovalConfig(required_tools=())
    host, recorder, _manager, effects = build_runtime(monkeypatch, config=config)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="run directly")).result(timeout=5)
        assert recorder.output_ready.wait(timeout=5)
        assert len(effects) == 1
        assert not any(
            isinstance(event, events.ToolWaitingForUser)
            for event in recorder.events
        )
        lifecycle = [
            type(event)
            for event in recorder.events
            if getattr(event, "tool_run_id", None) == "approval-call-1"
        ]
        assert lifecycle == [
            events.ToolQueued,
            events.ToolStarted,
            events.ToolCompleted,
        ]
    finally:
        stop_host(host)


def test_all_approvals_are_collected_before_any_tool_runs(monkeypatch):
    host, recorder, _manager, effects = build_runtime(
        monkeypatch,
        llm=MultipleApprovalLLM(),
    )

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="run both effects")).result(timeout=5)
        assert wait_for_tool_event(
            recorder, events.ToolWaitingForUser, "approval-call-1"
        )
        host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert wait_for_tool_event(
            recorder, events.ToolWaitingForUser, "approval-call-2"
        )
        assert effects == []

        host.submit(ApproveTool(tool_run_id="approval-call-2")).result(timeout=5)
        assert recorder.output_ready.wait(timeout=5)
        assert effects == ["first", "second"]
    finally:
        stop_host(host)


def test_duplicate_tool_call_ids_fail_before_waiting_or_execution(monkeypatch):
    host, recorder, _manager, effects = build_runtime(
        monkeypatch,
        llm=MultipleApprovalLLM(duplicate_ids=True),
    )

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="invalid duplicate calls")).result(timeout=5)
        assert receipt.turn_id is not None
        assert wait_for_tool_event(
            recorder, events.AgentFailed, None
        )
        assert effects == []
        assert not any(
            isinstance(event, events.ToolWaitingForUser)
            for event in recorder.events
        )
    finally:
        stop_host(host)


def test_removed_tool_cannot_run_after_approval(monkeypatch):
    host, recorder, manager, effects = build_runtime(monkeypatch)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="remove before approval")).result(timeout=5)
        assert recorder.waiting.wait(timeout=3), recorder.events
        manager.tool_registry.unregister_tool("approval_effect")

        host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert recorder.output_ready.wait(timeout=5)
        assert effects == []
        assert any(
            isinstance(event, events.ToolFailed)
            and event.reason == "tool is no longer available"
            for event in recorder.events
        )
    finally:
        stop_host(host)


def test_tool_failure_does_not_expose_provider_error(monkeypatch):
    secret_error = "PRIVATE-PROVIDER-FAILURE"
    host, recorder, _manager, effects = build_runtime(
        monkeypatch,
        effect_error=secret_error,
    )

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="failing approved effect")).result(timeout=5)
        assert receipt.turn_id is not None
        assert recorder.waiting.wait(timeout=3), recorder.events
        host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert wait_for_tool_event(recorder, events.AgentFailed, None)
        assert effects == []
        assert secret_error not in repr(recorder.events)
    finally:
        stop_host(host)


def test_cancellation_after_tool_start_emits_tool_terminal(monkeypatch):
    host, recorder, manager, effects = build_runtime(monkeypatch)
    manager.tool_registry.unregister_tool("approval_effect")

    @tool("approval_effect")
    async def approval_effect(value: str) -> str:
        """Wait until the runtime cancels this approved invocation."""
        await asyncio.Event().wait()
        effects.append(value)
        return "unreachable"

    manager.tool_registry.register_tool(approval_effect)

    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="cancel running tool")).result(timeout=5)
        assert receipt.turn_id is not None
        assert recorder.waiting.wait(timeout=3), recorder.events
        host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert wait_for_tool_event(recorder, events.ToolStarted, "approval-call-1")

        host.submit(CancelTurn(turn_id=receipt.turn_id)).result(timeout=5)
        assert wait_for_tool_event(recorder, events.ToolCancelled, "approval-call-1")
        assert effects == []
    finally:
        stop_host(host)


def test_restart_cancels_pending_approval_without_running_tool(monkeypatch):
    host, recorder, manager, effects = build_runtime(monkeypatch)

    try:
        host.start().result(timeout=5)
        host.submit(SubmitTurn(text="restart while waiting")).result(timeout=5)
        assert recorder.waiting.wait(timeout=3), recorder.events
        restarted = host.submit(RestartRuntime(reason="test restart")).result(timeout=5)
        assert restarted.detail == "runtime restarted"
        assert effects == []
        assert manager.approval_controller.pending_count == 0
        with pytest.raises(ToolApprovalError, match="not pending"):
            host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
    finally:
        stop_host(host)


def test_principal_runtime_approval_state_is_isolated(monkeypatch):
    first_host, first_events, _first_manager, first_effects = build_runtime(
        monkeypatch,
        principal_id="test:first-principal",
    )
    second_host, second_events, _second_manager, second_effects = build_runtime(
        monkeypatch,
        principal_id="test:second-principal",
    )

    try:
        first_host.start().result(timeout=5)
        second_host.start().result(timeout=5)
        first_host.submit(SubmitTurn(text="first effect")).result(timeout=5)
        second_host.submit(SubmitTurn(text="second effect")).result(timeout=5)
        assert first_events.waiting.wait(timeout=3), first_events.events
        assert second_events.waiting.wait(timeout=3), second_events.events

        first_host.submit(ApproveTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert first_events.output_ready.wait(timeout=5)
        assert len(first_effects) == 1
        assert second_effects == []

        second_host.submit(RejectTool(tool_run_id="approval-call-1")).result(timeout=5)
        assert second_events.output_ready.wait(timeout=5)
        assert second_effects == []
    finally:
        stop_host(first_host)
        stop_host(second_host)


@pytest.mark.asyncio
async def test_pending_approval_capacity_is_bounded():
    controller = ToolApprovalController(timeout_seconds=5.0, max_pending=1)
    first = asyncio.create_task(
        controller.wait_for_decision(
            ApprovalRequest("call-1", "tool", "turn-1")
        )
    )
    await asyncio.sleep(0)

    with pytest.raises(ToolApprovalError, match="capacity"):
        await controller.wait_for_decision(
            ApprovalRequest("call-2", "tool", "turn-2")
        )

    await controller.cancel_turn("turn-1")
    assert (await first).decision == "cancel"
    assert controller.pending_count == 0


@pytest.mark.asyncio
async def test_rejected_tool_result_is_reported_as_unsuccessful(monkeypatch):
    host, _recorder, manager, effects = build_runtime(monkeypatch)
    task = asyncio.create_task(
        manager.process_async(
            "reject direct manager turn",
            turn_id="turn-direct",
            conversation_id="conversation-direct",
        )
    )
    deadline = time.monotonic() + 3.0
    while manager.approval_controller.pending_count == 0:
        assert time.monotonic() < deadline
        await asyncio.sleep(0.01)

    await manager.reject_tool("approval-call-1")
    result = await asyncio.wait_for(task, timeout=5)

    assert effects == []
    assert result["tool_results"] == [
        {
            "tool": "approval_effect",
            "success": False,
            "result": "Tool approval_effect was not approved.",
        }
    ]
    await manager.shutdown_async()
    assert host.state is RuntimeHostState.NEW


def test_registry_rejects_unbounded_or_unsafe_tool_names():
    @tool("bad name")
    def invalid_tool() -> str:
        """Never register."""
        return "unreachable"

    with pytest.raises(ValueError, match="tool name is invalid"):
        ToolRegistry().register_tool(invalid_tool)


@pytest.mark.parametrize(
    ("approval_request", "message"),
    [
        (ApprovalRequest("call", "tool", ""), "active turn"),
        (ApprovalRequest("", "tool", "turn"), "identifier"),
        (ApprovalRequest("x" * 257, "tool", "turn"), "identifier"),
        (ApprovalRequest("call", "bad name", "turn"), "metadata"),
    ],
)
@pytest.mark.asyncio
async def test_invalid_approval_request_fails_before_publication(
    approval_request, message
):
    published = []
    controller = ToolApprovalController(publisher=published.append)

    with pytest.raises(ToolApprovalError, match=message):
        await controller.wait_for_decision(approval_request)

    assert published == []
    assert controller.pending_count == 0


@pytest.mark.asyncio
async def test_shutdown_cancels_pending_once_and_replay_fails_closed():
    published = []
    controller = ToolApprovalController(publisher=published.append)
    pending = asyncio.create_task(
        controller.wait_for_decision(ApprovalRequest("call", "tool", "turn"))
    )
    await asyncio.sleep(0)

    with pytest.raises(ToolApprovalError, match="already pending"):
        await controller.wait_for_decision(ApprovalRequest("call", "tool", "turn"))
    await controller.shutdown()

    assert (await pending).decision == "cancel"
    assert controller.pending_count == 0
    assert sum(isinstance(event, events.ToolCancelled) for event in published) == 1
    assert published[-1].reason == "runtime shutdown"
    with pytest.raises(ToolApprovalError, match="not pending"):
        await controller.approve("call")


@pytest.mark.asyncio
async def test_first_terminal_decision_fences_approve_cancel_race():
    approved_controller = ToolApprovalController()
    approved = asyncio.create_task(
        approved_controller.wait_for_decision(
            ApprovalRequest("approved", "tool", "turn-approved")
        )
    )
    await asyncio.sleep(0)
    await approved_controller.approve("approved")
    await approved_controller.cancel_turn("turn-approved")
    assert (await approved).decision == "approve"

    cancelled_controller = ToolApprovalController()
    cancelled = asyncio.create_task(
        cancelled_controller.wait_for_decision(
            ApprovalRequest("cancelled", "tool", "turn-cancelled")
        )
    )
    await asyncio.sleep(0)
    await cancelled_controller.cancel_turn("turn-cancelled")
    with pytest.raises(ToolApprovalError, match="not pending"):
        await cancelled_controller.approve("cancelled")
    assert (await cancelled).decision == "cancel"
