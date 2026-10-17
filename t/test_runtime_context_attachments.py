from __future__ import annotations

import asyncio
from types import SimpleNamespace

import pytest

from zara.agent import AgentManager
from zara.context import ContextAttachmentStore, ContextAttachmentScope, ContextAttachmentNotFound
from zara.principals import PrincipalContext
from zara.runtime.backend import LangGraphRuntimeBackend, UnsupportedRuntimeCommand
from test_task_turn_context import CapturingLLM, ContextConfig


class Memory:
    def __init__(self):
        self.messages = []

    def retrieve(self, *_args, **_kwargs):
        return []

    def start_session(self):
        return "session"

    def add_message(self, session, role, text):
        self.messages.append((session, role, text))

    def summarise_session(self, _session):
        pass


def setup(monkeypatch, *, llm=None, principal=None, store_principal=None, **kwargs):
    llm = llm or CapturingLLM("answer")
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *_args: llm)
    principal = principal or PrincipalContext("owner")
    memory = Memory()
    manager = AgentManager(config=ContextConfig(), memory_manager=memory, principal=principal)
    owner = ContextAttachmentStore(principal=store_principal or principal)
    backend = LangGraphRuntimeBackend(lambda: manager, attachment_store=owner, **kwargs)
    return backend, manager, owner, memory, llm


@pytest.mark.asyncio
async def test_attachment_reaches_canonical_context_without_task_mode_or_persistence_leak(monkeypatch):
    backend, manager, owner, memory, llm = setup(monkeypatch)
    await backend.start()
    item = owner.acquire_text("snapshot text", conversation_id="chat", turn_id="turn")
    result = await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,))
    assert result.response == "answer"
    assert any("snapshot text" in str(message.content) for message in llm.calls[0])
    assert [str(m.content) for m in manager.context_manager.history] == ["inspect", "answer"]
    assert memory.messages == [("session", "user", "inspect"), ("session", "assistant", "answer")]
    assert owner.diagnostics().item_count == 0
    await backend.submit_turn("again", turn_id="next", conversation_id="chat")
    assert not any("snapshot text" in str(m.content) for m in llm.calls[-1])
    await backend.stop()


@pytest.mark.asyncio
async def test_attachment_task_context_remains_ephemeral_and_skips_raw_memory(monkeypatch):
    backend, manager, owner, memory, llm = setup(monkeypatch)
    await backend.start()
    item = owner.acquire_text("task snapshot", conversation_id="task-chat", turn_id="task")
    await backend.submit_turn("inspect", turn_id="task", conversation_id="task-chat",
                              context_ids=(item.id,), system_context="TASK", conversation_history=[])
    assert any("task snapshot" in str(m.content) for m in llm.calls[0])
    assert not manager.context_manager.history
    assert memory.messages == []
    assert owner.diagnostics().item_count == 0
    await backend.stop()


@pytest.mark.asyncio
async def test_missing_reference_fails_before_provider_and_releases_turn(monkeypatch):
    backend, _manager, owner, memory, llm = setup(monkeypatch)
    await backend.start()
    owner.acquire_text("unused", conversation_id="chat", turn_id="turn")
    with pytest.raises(ContextAttachmentNotFound):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=("ctx-missing",))
    assert llm.calls == [] and memory.messages == []
    assert owner.diagnostics().item_count == 0
    await backend.stop()


@pytest.mark.asyncio
async def test_foreign_principal_store_fails_before_start_and_preserves_owner_data(monkeypatch):
    backend, _manager, owner, _memory, llm = setup(monkeypatch, store_principal=PrincipalContext("other"))
    owner.acquire_text("foreign text", conversation_id="chat", turn_id="turn")
    with pytest.raises(ValueError, match="principal"):
        await backend.start()
    await backend.stop()
    assert owner.diagnostics().item_count == 1 and llm.calls == []


@pytest.mark.asyncio
@pytest.mark.parametrize("cancel", [False, True])
async def test_provider_error_or_cancel_releases_turn(monkeypatch, cancel):
    entered = asyncio.Event()

    class Interrupted(CapturingLLM):
        async def ainvoke(self, messages):
            entered.set()
            if cancel:
                await asyncio.Event().wait()
            raise RuntimeError("synthetic provider failure")

    backend, _manager, owner, _memory, _llm = setup(monkeypatch, llm=Interrupted())
    await backend.start()
    item = owner.acquire_text("private", conversation_id="chat", turn_id="turn")
    task = asyncio.create_task(backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,)))
    await asyncio.wait_for(entered.wait(), timeout=2)
    if cancel:
        task.cancel()
    with pytest.raises(asyncio.CancelledError if cancel else RuntimeError):
        await task
    assert owner.diagnostics().item_count == 0
    await backend.stop()


@pytest.mark.asyncio
async def test_conversation_attachment_reuses_only_same_chat_and_stop_clears(monkeypatch):
    backend, _manager, owner, _memory, llm = setup(monkeypatch)
    await backend.start()
    item = owner.acquire_text("shared snapshot", conversation_id="chat", scope=ContextAttachmentScope.CONVERSATION)
    for turn in ("one", "two"):
        await backend.submit_turn("inspect", turn_id=turn, conversation_id="chat", context_ids=(item.id,))
    assert len(llm.calls) == 2
    with pytest.raises(ContextAttachmentNotFound):
        await backend.submit_turn("inspect", turn_id="three", conversation_id="other", context_ids=(item.id,))
    assert len(llm.calls) == 2
    await backend.stop()
    await backend.start()
    with pytest.raises(ContextAttachmentNotFound):
        await backend.submit_turn("inspect", turn_id="four", conversation_id="chat", context_ids=(item.id,))
    await backend.stop()


@pytest.mark.asyncio
async def test_deterministic_route_retains_single_memory_owner_and_turn_cleanup(monkeypatch):
    routed = []

    class Router:
        async def route(self, text, **_kwargs):
            routed.append(text)
            return SimpleNamespace(action="respond", response="routed")

    backend, _manager, owner, memory, llm = setup(monkeypatch, router=Router())
    await backend.start()
    item = owner.acquire_text("delete all files", conversation_id="chat", turn_id="turn")
    result = await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,))
    assert result.response == "routed" and llm.calls == []
    assert routed == ["inspect"]
    assert len(memory.messages) == 2 and owner.diagnostics().item_count == 0
    await backend.stop()


@pytest.mark.asyncio
async def test_unconfigured_store_still_rejects_ids_before_manager_call():
    manager = SimpleNamespace()
    backend = LangGraphRuntimeBackend(lambda: manager)
    await backend.start()
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=("ctx-any",))


@pytest.mark.asyncio
async def test_changed_manager_principal_cannot_read_previously_bound_store(monkeypatch):
    backend, manager, owner, _memory, llm = setup(monkeypatch)
    await backend.start()
    item = owner.acquire_text("private", conversation_id="chat", turn_id="turn")
    manager.principal = PrincipalContext("replacement")
    with pytest.raises(ValueError, match="principal"):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,))
    assert owner.diagnostics().item_count == 1 and llm.calls == []
    await backend.stop()


@pytest.mark.asyncio
async def test_stop_clears_attachments_even_if_manager_shutdown_fails(monkeypatch):
    backend, manager, owner, _memory, _llm = setup(monkeypatch)
    await backend.start()
    owner.acquire_text("private", conversation_id="chat", scope=ContextAttachmentScope.CONVERSATION)

    async def fail():
        raise RuntimeError("shutdown failed")

    monkeypatch.setattr(manager, "shutdown_async", fail)
    with pytest.raises(RuntimeError, match="shutdown failed"):
        await backend.stop()
    assert owner.diagnostics().item_count == 0


@pytest.mark.asyncio
async def test_bad_reference_cannot_release_other_turns_or_conversations(monkeypatch):
    backend, _manager, owner, _memory, _llm = setup(monkeypatch)
    await backend.start()
    other = owner.acquire_text("private", conversation_id="other", turn_id="other")
    with pytest.raises(ContextAttachmentNotFound):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(other.id,))
    assert owner.diagnostics().item_count == 1
    await backend.stop()


@pytest.mark.asyncio
async def test_store_is_never_used_by_stopped_backend(monkeypatch):
    backend, _manager, owner, _memory, llm = setup(monkeypatch)
    item = owner.acquire_text("private", conversation_id="chat", turn_id="turn")
    with pytest.raises(RuntimeError, match="not started"):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,))
    assert llm.calls == []


@pytest.mark.asyncio
async def test_rejected_store_binding_disposes_new_manager_without_clearing_foreign_data():
    closed = []

    async def shutdown():
        closed.append(True)

    manager = SimpleNamespace(principal=PrincipalContext("owner"), shutdown_async=shutdown)
    owner = ContextAttachmentStore(principal=PrincipalContext("other"))
    owner.acquire_text("private", conversation_id="chat", turn_id="turn")
    backend = LangGraphRuntimeBackend(lambda: manager, attachment_store=owner)
    with pytest.raises(ValueError, match="principal"):
        await backend.start()
    assert closed == [True]
    assert owner.diagnostics().item_count == 1


@pytest.mark.asyncio
async def test_old_completion_cannot_release_new_restart_generation_snapshot():
    entered, release = asyncio.Event(), asyncio.Event()

    class Manager:
        principal = PrincipalContext("owner")

        async def process_async(self, *_args, **_kwargs):
            entered.set()
            await release.wait()
            return {"response": "done"}

        async def shutdown_async(self):
            pass

    owner = ContextAttachmentStore(principal=PrincipalContext("owner"))
    backend = LangGraphRuntimeBackend(Manager, attachment_store=owner)
    await backend.start()
    old = owner.acquire_text("old", conversation_id="chat", turn_id="turn")
    running = asyncio.create_task(backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(old.id,)))
    await asyncio.wait_for(entered.wait(), timeout=2)
    await backend.stop()
    await backend.start()
    fresh = owner.acquire_text("new", conversation_id="chat", turn_id="turn")
    release.set()
    await running
    assert "new" in owner.render([fresh.id], conversation_id="chat", turn_id="turn")
    await backend.stop()


@pytest.mark.asyncio
async def test_duplicate_active_turn_rejected_without_revoking_first_turn(monkeypatch):
    entered, release = asyncio.Event(), asyncio.Event()

    class Blocked(CapturingLLM):
        async def ainvoke(self, messages):
            entered.set()
            await release.wait()
            return await super().ainvoke(messages)

    backend, _manager, owner, _memory, llm = setup(monkeypatch, llm=Blocked())
    await backend.start()
    item = owner.acquire_text("private", conversation_id="chat", turn_id="turn")
    running = asyncio.create_task(backend.submit_turn("first", turn_id="turn", conversation_id="chat", context_ids=(item.id,)))
    try:
        await asyncio.wait_for(entered.wait(), timeout=2)
        from zara.context import ContextAttachmentError
        with pytest.raises(ContextAttachmentError, match="active"):
            await asyncio.wait_for(backend.submit_turn("duplicate", turn_id="turn", conversation_id="chat", context_ids=(item.id,)), timeout=0.5)
        with pytest.raises(ContextAttachmentError, match="active"):
            owner.acquire_text("late acquisition", conversation_id="chat", turn_id="turn")
        assert owner.diagnostics().item_count == 1
    finally:
        release.set()
        await running
        await backend.stop()
    assert len(llm.calls) == 1


def test_host_submit_turn_projects_explicit_conversation_snapshot_to_model(monkeypatch):
    from zara.runtime.commands import SubmitTurn
    from zara.runtime.host import RuntimeHost
    from test_runtime_host import RecordingPublisher, stop_host

    backend, manager, owner, memory, llm = setup(monkeypatch)
    item = owner.acquire_text("host snapshot", conversation_id="chat",
                              scope=ContextAttachmentScope.CONVERSATION)
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)
    try:
        host.start().result(timeout=5)
        receipt = host.submit(SubmitTurn(text="inspect", conversation_id="chat", context_ids=(item.id,))).result(timeout=5)
        assert receipt.turn_id and publisher.output_ready.wait(timeout=5)
        assert any("host snapshot" in str(message.content) for message in llm.calls[0])
        assert [str(m.content) for m in manager.context_manager.history] == ["inspect", "answer"]
        assert len(memory.messages) == 2
    finally:
        stop_host(host)
    assert owner.diagnostics().item_count == 0


@pytest.mark.asyncio
@pytest.mark.parametrize("cleanup_fails", [False, True])
async def test_rejected_binding_disposes_legacy_manager_and_preserves_primary_error(cleanup_fails):
    closed = []

    def exit_conversation():
        closed.append(True)
        if cleanup_fails:
            raise RuntimeError("cleanup failed")

    manager = SimpleNamespace(principal=PrincipalContext("owner"), exit_conversation=exit_conversation)
    owner = ContextAttachmentStore(principal=PrincipalContext("other"))
    backend = LangGraphRuntimeBackend(lambda: manager, attachment_store=owner)
    with pytest.raises(ValueError, match="principal"):
        await backend.start()
    assert closed == [True]


@pytest.mark.asyncio
async def test_attachment_still_obeys_full_provider_context_budget(monkeypatch):
    from dataclasses import replace
    from zara.agent.context import ContextError

    backend, manager, owner, memory, llm = setup(monkeypatch)
    await backend.start()
    manager._ensure_context_runtime()
    manager.context_manager.config = replace(manager.context_manager.config,
        max_tokens=64, summary_max_tokens=8, skill_max_tokens=8)
    item = owner.acquire_text("x" * 1000, conversation_id="chat", turn_id="turn")
    with pytest.raises(ContextError):
        await backend.submit_turn("inspect", turn_id="turn", conversation_id="chat", context_ids=(item.id,))
    assert llm.calls == [] and memory.messages == []
    assert not manager.context_manager.history
    assert owner.diagnostics().item_count == 0
    await backend.stop()


@pytest.mark.asyncio
async def test_store_cannot_be_owned_by_two_runtime_backends(monkeypatch):
    from zara.context import ContextAttachmentError

    first, manager, owner, _memory, _llm = setup(monkeypatch)
    await first.start()
    item = owner.acquire_text("private", conversation_id="chat", scope=ContextAttachmentScope.CONVERSATION)
    disposed = []

    async def shutdown():
        disposed.append(True)

    other_manager = SimpleNamespace(principal=manager.principal, shutdown_async=shutdown)
    second = LangGraphRuntimeBackend(lambda: other_manager, attachment_store=owner)
    try:
        with pytest.raises(ContextAttachmentError, match="bound"):
            await second.start()
        await second.stop()
        assert disposed == [True]
        assert "private" in owner.render([item.id], conversation_id="chat", turn_id="turn")
    finally:
        await first.stop()
    await second.start()
    await second.stop()
