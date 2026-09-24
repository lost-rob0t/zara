from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import AsyncMock, MagicMock

import pytest

from zara.agent import stream_events
from zara.runtime import backend as backend_module
from zara.runtime import events
from zara.runtime.backend import (
    DETERMINISTIC_COMMAND_FAILED,
    DETERMINISTIC_COMMAND_UNAVAILABLE,
    AgentRuntimeBackend,
    LangGraphRuntimeBackend,
    RuntimeBackend,
    RuntimeTurnResult,
    UnsupportedRuntimeCommand,
)


class Conversation:
    def __init__(self) -> None:
        self.in_conversation = False
        self.conversation_history = ["stale"]
        self.entered = self.exited = 0

    def enter_conversation(self):
        self.in_conversation = True
        self.entered += 1

    def exit_conversation(self):
        self.in_conversation = False
        self.exited += 1


class Tools:
    def __init__(self) -> None:
        self.registered = []
        self.unregistered = []

    def register_tools(self, values):
        self.registered.append(tuple(values))

    def unregister_tools(self, values):
        self.unregistered.append(tuple(values))

    def requires_approval(self, name):
        return name == "danger"

    def invoke_composed_tool(self, name, request):
        return {"name": name, "request": request}


class Advice:
    def register(self, kind, *, owner, priority, callback):
        self.last = (kind, owner, priority, callback)
        return 41

    def unregister(self, registration_id):
        return registration_id == 41


class Manager:
    def __init__(self, memory=None, *, advice=True) -> None:
        self.principal = SimpleNamespace(principal_id="principal-a")
        self.conversation_manager = Conversation()
        self.memory_manager = memory
        self.tool_registry = Tools()
        self.agent_loop_advice = Advice() if advice else None
        self.process_async = AsyncMock(
            return_value={"response": "agent reply", "tool_results": [{"ok": True}, "raw"]}
        )
        self.cancel_turn = AsyncMock()
        self.approve_tool = AsyncMock()
        self.reject_tool = AsyncMock()
        self.shutdown_async = AsyncMock()
        self.bound = []

    def bind_event_publisher(self, publisher):
        self.bound.append(publisher)

    def customization_diagnostics(self):
        return {"custom": "ok"}


class Router:
    def __init__(self, action, response="router reply") -> None:
        self.action = action
        self.response = response
        self.calls = []

    async def route(self, text, **kwargs):
        self.calls.append((text, kwargs))
        return SimpleNamespace(action=self.action, response=self.response)


@pytest.mark.asyncio
async def test_runtime_backend_base_surface_fails_closed():
    backend = RuntimeBackend()
    with pytest.raises(UnsupportedRuntimeCommand, match="principal identity"):
        _ = backend.principal_id
    backend.bind_event_publisher(lambda _event: None)
    await backend.start()
    with pytest.raises(NotImplementedError):
        await backend.submit_turn("hello", turn_id="t")
    backend.commit_turn_result(RuntimeTurnResult(), turn_id="t")
    await backend.cancel_turn("t")
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.register_tools(["tool"])
    backend.unregister_tools(["tool"])
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.requires_composed_tool_approval("tool")
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.invoke_composed_tool("p", "tool", {})
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.register_agent_loop_advice("before", "owner", 1, lambda: None)
    assert backend.unregister_agent_loop_advice(1) is False
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.customization_diagnostics()
    for call in (
        backend.start_voice(),
        backend.stop_voice(),
        backend.mute_speech(True),
        backend.approve_tool("run"),
        backend.reject_tool("run", "no"),
    ):
        with pytest.raises(UnsupportedRuntimeCommand):
            await call
    await backend.stop()


@pytest.mark.asyncio
async def test_langgraph_start_identity_context_cancel_and_semantic_fence(monkeypatch):
    manager = Manager()
    factory = MagicMock(return_value=manager)
    backend = LangGraphRuntimeBackend(factory, semantic_first=True)
    with pytest.raises(RuntimeError, match="not started"):
        _ = backend.principal_id
    with pytest.raises(RuntimeError, match="not started"):
        await backend.submit_turn("hello", turn_id="t0")

    publisher = MagicMock()
    backend.bind_event_publisher(publisher)
    await backend.start()
    await backend.start()
    assert factory.call_count == 1
    assert manager.bound == [publisher]
    assert backend.principal_id == "principal-a"
    backend.bind_event_publisher(MagicMock())
    assert len(manager.bound) == 2

    with pytest.raises(UnsupportedRuntimeCommand, match="context attachments"):
        await backend.submit_turn("hello", turn_id="t1", context_ids=("ctx",))
    await backend.cancel_turn("t1")
    manager.cancel_turn.assert_awaited_once_with("t1")

    monkeypatch.setattr(backend_module.command_gate, "looks_like_command", lambda _text: True)
    result = await backend.submit_turn("open terminal", turn_id="t2")
    assert result.response == DETERMINISTIC_COMMAND_UNAVAILABLE
    assert result.metadata == {"route": "deterministic_unavailable"}
    manager.process_async.assert_not_awaited()

    manager.principal = None
    with pytest.raises(RuntimeError, match="no principal identity"):
        _ = backend.principal_id


@pytest.mark.asyncio
@pytest.mark.parametrize(
    ("action", "command_like", "expected"),
    [
        ("greeting", False, "router reply"),
        ("end_conversation", False, "router reply"),
        ("respond", False, "router reply"),
        ("delegate", True, DETERMINISTIC_COMMAND_FAILED),
    ],
)
async def test_router_terminal_paths(monkeypatch, action, command_like, expected):
    memory = MagicMock()
    memory.start_session.return_value = "memory-1"
    manager = Manager(memory)
    router = Router(action)
    backend = LangGraphRuntimeBackend(lambda: manager, router=router, semantic_first=True)
    await backend.start()
    monkeypatch.setattr(backend_module.command_gate, "looks_like_command", lambda _text: command_like)
    if action == "end_conversation":
        manager.conversation_manager.in_conversation = True
        backend._memory_session = "memory-old"

    result = await backend.submit_turn("utterance", turn_id="t", conversation_id="c")
    assert result.response == expected
    assert len(router.calls) == 1
    if action == "greeting":
        assert manager.conversation_manager.entered == 1
        assert manager.conversation_manager.conversation_history == []
    elif action == "end_conversation":
        assert manager.conversation_manager.exited == 1
        memory.summarise_session.assert_called_once_with("memory-old")
        assert backend._memory_session == "memory-1"
    elif action == "respond":
        assert [call.args for call in memory.add_message.call_args_list] == [
            ("memory-1", "user", "utterance"),
            ("memory-1", "assistant", "router reply"),
        ]
    else:
        assert result.metadata == {"route": "deterministic_failed"}
        manager.process_async.assert_not_awaited()


@pytest.mark.asyncio
async def test_delegate_task_memory_and_stream_paths(monkeypatch):
    memory = MagicMock()
    manager = Manager(memory)
    router = Router("delegate")
    backend = LangGraphRuntimeBackend(lambda: manager, router=router)
    await backend.start()
    monkeypatch.setattr(backend_module.command_gate, "looks_like_command", lambda _text: False)

    result = await backend.submit_turn("normal", turn_id="t1", conversation_id="c")
    assert result.tool_results == ({"ok": True}, {"result": "raw"})
    assert manager.conversation_manager.entered == 1

    router.calls.clear()
    manager.process_async.reset_mock()
    await backend.submit_turn(
        "task",
        turn_id="t2",
        conversation_id="c",
        conversation_history=[],
        system_context="bounded",
    )
    assert router.calls == []
    manager.process_async.assert_awaited_once()

    events_seen = []
    backend.bind_event_publisher(events_seen.append)
    publish = backend._stream_publisher("turn", "conversation")
    publish(stream_events.SentenceReady(text="One."))
    publish(stream_events.SentenceReady(text="Two."))
    publish(stream_events.Completed(full_text="One. Two."))
    publish(object())
    assert [type(event) for event in events_seen] == [
        events.AssistantStarted,
        events.AssistantDelta,
        events.AssistantDelta,
        events.AssistantComplete,
    ]
    backend.bind_event_publisher(MagicMock(side_effect=RuntimeError("sink failed")))
    backend._stream_publisher("turn", None)(stream_events.SentenceReady(text="bounded"))

    memory.start_session.side_effect = RuntimeError("memory unavailable")
    backend._memory_session = None
    await backend._persist_turn("ignored", "ignored")
    assert backend._memory_session is None
    backend._memory_session = "existing"
    memory.summarise_session.side_effect = RuntimeError("summary unavailable")
    await backend._rotate_memory_session()
    assert backend._memory_session == "existing"


@pytest.mark.asyncio
async def test_manager_extensions_delegate_or_fail_closed_and_stop_is_idempotent():
    manager = Manager()
    backend = LangGraphRuntimeBackend(lambda: manager)
    assert backend._stream_publisher("t", "c") is None
    await backend.start()
    await backend.approve_tool("a")
    await backend.reject_tool("b", "no")
    backend.register_tools(["x"])
    backend.unregister_tools(["x"])
    assert backend.requires_composed_tool_approval("danger") is True
    assert backend.requires_composed_tool_approval("safe") is False
    with pytest.raises(PermissionError):
        backend.invoke_composed_tool("other", "x", {})
    assert backend.invoke_composed_tool("principal-a", "x", {"v": 1})["name"] == "x"
    assert backend.register_agent_loop_advice("before", "owner", 1, lambda: None) == 41
    assert backend.unregister_agent_loop_advice(41) is True
    assert backend.customization_diagnostics() == {"custom": "ok"}

    absent = Manager(advice=False)
    del absent.approve_tool
    del absent.reject_tool
    absent.customization_diagnostics = None
    missing = LangGraphRuntimeBackend(lambda: absent)
    await missing.start()
    with pytest.raises(UnsupportedRuntimeCommand):
        await missing.approve_tool("run")
    with pytest.raises(UnsupportedRuntimeCommand):
        await missing.reject_tool("run")
    with pytest.raises(UnsupportedRuntimeCommand):
        missing.register_agent_loop_advice("before", "owner", 1, lambda: None)
    assert missing.unregister_agent_loop_advice(1) is False
    with pytest.raises(UnsupportedRuntimeCommand):
        missing.customization_diagnostics()

    await backend.stop()
    manager.shutdown_async.assert_awaited_once()
    await backend.stop()
    fallback_manager = Manager()
    del fallback_manager.shutdown_async
    fallback_manager.exit_conversation = MagicMock()
    fallback = LangGraphRuntimeBackend(lambda: fallback_manager)
    await fallback.start()
    await fallback.stop()
    fallback_manager.exit_conversation.assert_called_once_with()


@pytest.mark.asyncio
async def test_agent_runtime_facade_forwards_complete_contract():
    delegate = MagicMock()
    delegate.principal_id = "principal"
    for name in ("start", "submit_turn", "cancel_turn", "start_voice", "stop_voice", "mute_speech", "approve_tool", "reject_tool", "stop"):
        setattr(delegate, name, AsyncMock())
    delegate.submit_turn.return_value = RuntimeTurnResult(response="ok")
    delegate.requires_composed_tool_approval.return_value = True
    delegate.invoke_composed_tool.return_value = {"ok": True}
    delegate.register_agent_loop_advice.return_value = 9
    delegate.unregister_agent_loop_advice.return_value = True
    delegate.customization_diagnostics.return_value = ("diag",)

    facade = AgentRuntimeBackend(lambda: Manager())
    facade._delegate = delegate
    assert facade.principal_id == "principal"
    facade.bind_event_publisher("publisher")
    await facade.start()
    assert (await facade.submit_turn("hello", turn_id="t")).response == "ok"
    await facade.cancel_turn("t")
    facade.register_tools(["x"])
    facade.unregister_tools(["x"])
    assert facade.requires_composed_tool_approval("x") is True
    assert facade.invoke_composed_tool("principal", "x", {}) == {"ok": True}
    assert facade.register_agent_loop_advice("before", "owner", 3, lambda: None) == 9
    assert facade.unregister_agent_loop_advice(9) is True
    assert facade.customization_diagnostics() == ("diag",)
    await facade.start_voice()
    await facade.stop_voice()
    await facade.mute_speech(True)
    await facade.approve_tool("run")
    await facade.reject_tool("run", "no")
    await facade.stop()
    delegate.reject_tool.assert_awaited_once_with("run", "no")
