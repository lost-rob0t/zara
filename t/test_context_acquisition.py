from __future__ import annotations

from types import SimpleNamespace

import pytest

from zara.context import ContextAttachmentError, ContextAttachmentScope, ContextAttachmentStore, ContextSensitivity
from zara.principals import PrincipalContext
from zara.runtime.backend import AgentRuntimeBackend, LangGraphRuntimeBackend, UnsupportedRuntimeCommand
from zara.runtime.commands import (
    AcquireTextContext, ContextAcquiredReceipt, ContextRevokedReceipt,
    RevokeTextContext, SubmitTurn, RestartRuntime,
)
from zara.runtime.host import RuntimeHost, RuntimeNotReady
from test_runtime_host import ImmediateBackend, RecordingPublisher, stop_host
from test_runtime_context_attachments import Memory
from test_task_turn_context import CapturingLLM, ContextConfig


@pytest.mark.parametrize("kwargs", [
    {"text": None}, {"text": ""}, {"text": "  "}, {"text": "x" * 65537},
    {"text": "\ud800"}, {"conversation_id": ""}, {"conversation_id": " chat"},
    {"conversation_id": "x" * 129}, {"conversation_id": "\ud800"},
    {"display_name": "x" * 257}, {"display_name": "bad\nname"},
    {"display_name": "\ud800"}, {"sensitivity": "private"},
])
def test_acquisition_command_rejects_invalid_payload_before_dispatch(kwargs):
    with pytest.raises((ValueError, TypeError)):
        AcquireTextContext(**{"text": "snapshot", "conversation_id": "chat", **kwargs})


def test_acquisition_repr_and_receipts_omit_private_payload():
    command = AcquireTextContext(text="private payload", conversation_id="chat", display_name="private name")
    assert "private payload" not in repr(command)
    assert "private name" not in repr(command)
    assert command.sensitivity is ContextSensitivity.PRIVATE
    assert "text" not in ContextAcquiredReceipt.__dataclass_fields__
    assert "text" not in ContextRevokedReceipt.__dataclass_fields__


@pytest.mark.parametrize("context_id", [None, "", " x", "bad\x00id", "x" * 129])
def test_revoke_command_rejects_malformed_references(context_id):
    with pytest.raises((ValueError, TypeError)):
        RevokeTextContext(context_id=context_id, conversation_id="chat")


@pytest.mark.parametrize("reference", ["ctx-bad\nref", "ctx-\ud800", "ctx-\x00private", None, 123, "bad-ref", "ctx-" + "x" * 129])
def test_invalid_backend_context_reference_never_reaches_receipt(reference):
    class Invalid(ImmediateBackend):
        def acquire_text_context(self, *_args, **_kwargs):
            return reference

    host = RuntimeHost(Invalid)
    try:
        host.start().result(timeout=5)
        with pytest.raises(Exception, match="context"):
            host.submit(AcquireTextContext(text="private", conversation_id="chat")).result(timeout=5)
    finally:
        stop_host(host)


@pytest.mark.parametrize("revoke", [False, True])
@pytest.mark.parametrize("exception_type", [RuntimeError, UnsupportedRuntimeCommand])
def test_private_command_backend_errors_are_sanitized_in_future_and_events(revoke, exception_type):
    class Failing(ImmediateBackend):
        def acquire_text_context(self, text, **_kwargs):
            raise exception_type(text)

        def revoke_text_context(self, *_args, **_kwargs):
            raise exception_type("PRIVATE-PAYLOAD")

    publisher = RecordingPublisher()
    host = RuntimeHost(Failing, publisher=publisher)
    try:
        host.start().result(timeout=5)
        command = (RevokeTextContext(context_id="ctx-ref", conversation_id="chat") if revoke
                   else AcquireTextContext(text="PRIVATE-PAYLOAD", conversation_id="chat", sensitivity=ContextSensitivity.SECRET))
        with pytest.raises(Exception) as caught:
            host.submit(command).result(timeout=5)
        assert "PRIVATE-PAYLOAD" not in str(caught.value)
        assert "PRIVATE-PAYLOAD" not in repr(publisher.events)
    finally:
        stop_host(host)


@pytest.mark.parametrize("default_factory", ["canonical", "direct", "facade"])
def test_default_local_factory_supplies_canonical_local_identity(monkeypatch, default_factory):
    import zara.agent as agent
    from zara.runtime.backend import create_runtime_backend

    captured = []

    def factory(**kwargs):
        captured.append(kwargs)
        return SimpleNamespace(**kwargs)

    monkeypatch.setattr(agent, "AgentManager", factory)
    config = SimpleNamespace(get=lambda *_args: "langgraph")
    backend = (create_runtime_backend(config) if default_factory == "canonical"
               else LangGraphRuntimeBackend() if default_factory == "direct"
               else AgentRuntimeBackend(config=config))
    import asyncio
    asyncio.run(backend.start())
    assert captured[0]["principal"] == PrincipalContext.local_owner()
    reference = backend.acquire_text_context("local snapshot", conversation_id="chat", display_name="Context", sensitivity=ContextSensitivity.PRIVATE)
    assert reference.startswith("ctx-")


@pytest.mark.parametrize("value", [None, 1, "true"])
def test_invalid_backend_revocation_cannot_be_presented_as_success(value):
    class Invalid(ImmediateBackend):
        def revoke_text_context(self, *_args, **_kwargs):
            return value

    host = RuntimeHost(Invalid)
    try:
        host.start().result(timeout=5)
        with pytest.raises(Exception, match="context"):
            host.submit(RevokeTextContext(context_id="ctx-ref", conversation_id="chat")).result(timeout=5)
    finally:
        stop_host(host)


@pytest.mark.parametrize("revoke", [False, True])
def test_remote_client_does_not_invent_a_wire_alias_for_local_acquisition(revoke):
    from zara.client import ZaraClientState
    from zara.zmq_transport import ZmqZaraClient

    client = ZmqZaraClient.__new__(ZmqZaraClient)
    import threading
    client._state_lock = threading.RLock()
    client._state = ZaraClientState.READY
    command = (RevokeTextContext(context_id="ctx-ref", conversation_id="chat") if revoke
               else AcquireTextContext(text="private", conversation_id="chat"))
    with pytest.raises(TypeError, match="does not support"):
        client.submit(command)


@pytest.mark.asyncio
async def test_direct_stopped_or_rebound_backend_cannot_acquire_or_revoke():
    manager = SimpleNamespace(principal=PrincipalContext("owner"))
    backend = LangGraphRuntimeBackend(lambda: manager)
    options = dict(conversation_id="chat", display_name="Context", sensitivity=ContextSensitivity.PRIVATE)
    with pytest.raises(RuntimeError, match="not started"):
        backend.acquire_text_context("private", **options)
    await backend.start()
    reference = backend.acquire_text_context("private", **options)
    manager.principal = PrincipalContext("other")
    with pytest.raises(ValueError, match="principal"):
        backend.acquire_text_context("other", **options)
    with pytest.raises(ValueError, match="principal"):
        backend.revoke_text_context(reference, conversation_id="chat")
    assert backend._attachment_store.diagnostics().item_count == 1


def build_backend(monkeypatch):
    from zara.agent import AgentManager

    llm, memory = CapturingLLM("answer"), Memory()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *_args: llm)
    manager = AgentManager(config=ContextConfig(), memory_manager=memory, principal=PrincipalContext("owner"))
    return AgentRuntimeBackend(lambda: manager, semantic_first=False), manager, memory, llm


def test_host_acquire_use_and_revoke_needs_no_direct_store_access(monkeypatch):
    backend, manager, memory, llm = build_backend(monkeypatch)
    publisher = RecordingPublisher()
    host = RuntimeHost(lambda: backend, publisher=publisher)
    try:
        host.start().result(timeout=5)
        command = AcquireTextContext(text="private snapshot", conversation_id="chat", display_name="reference")
        receipt = host.submit(command).result(timeout=5)
        assert isinstance(receipt, ContextAcquiredReceipt)
        assert receipt.request_id == command.request_id and receipt.conversation_id == "chat"
        assert receipt.context_id.startswith("ctx-") and receipt.turn_id is None
        assert llm.calls == [] and memory.messages == []
        host.submit(SubmitTurn(text="inspect", conversation_id="chat", context_ids=(receipt.context_id,))).result(timeout=5)
        assert publisher.output_ready.wait(timeout=5)
        assert any("private snapshot" in str(message.content) for message in llm.calls[0])
        assert [str(message.content) for message in manager.context_manager.history] == ["inspect", "answer"]
        assert len(memory.messages) == 2
        wrong = host.submit(RevokeTextContext(context_id=receipt.context_id, conversation_id="other")).result(timeout=5)
        assert isinstance(wrong, ContextRevokedReceipt) and wrong.revoked is False
        revoked = host.submit(RevokeTextContext(context_id=receipt.context_id, conversation_id="chat")).result(timeout=5)
        assert revoked.revoked is True
        assert host.submit(RevokeTextContext(context_id=receipt.context_id, conversation_id="chat")).result(timeout=5).revoked is False
        with pytest.raises(Exception, match="unavailable"):
            host.run_coroutine(backend.submit_turn("inspect", turn_id="later", conversation_id="chat", context_ids=(receipt.context_id,))).result(timeout=5)
    finally:
        stop_host(host)


def test_host_restart_invalidates_acquired_reference(monkeypatch):
    backend, _manager, _memory, _llm = build_backend(monkeypatch)
    host = RuntimeHost(lambda: backend)
    try:
        host.start().result(timeout=5)
        old = host.submit(AcquireTextContext(text="old", conversation_id="chat")).result(timeout=5)
        host.submit(RestartRuntime()).result(timeout=5)
        assert host.submit(RevokeTextContext(context_id=old.context_id, conversation_id="chat")).result(timeout=5).revoked is False
        fresh = host.submit(AcquireTextContext(text="fresh", conversation_id="chat")).result(timeout=5)
        assert fresh.context_id != old.context_id
    finally:
        stop_host(host)


@pytest.mark.parametrize("revoke", [False, True])
def test_lifecycle_and_unsupported_backend_fail_without_effects(revoke):
    host = RuntimeHost(ImmediateBackend)
    command = (RevokeTextContext(context_id="ctx-ref", conversation_id="chat") if revoke
               else AcquireTextContext(text="private", conversation_id="chat"))
    with pytest.raises(RuntimeNotReady):
        host.submit(command).result(timeout=5)
    try:
        host.start().result(timeout=5)
        with pytest.raises(UnsupportedRuntimeCommand):
            host.submit(command).result(timeout=5)
    finally:
        stop_host(host)
    with pytest.raises(RuntimeNotReady):
        host.submit(command).result(timeout=5)


@pytest.mark.asyncio
async def test_missing_canonical_principal_rejects_lazy_store_creation():
    backend = LangGraphRuntimeBackend(lambda: SimpleNamespace())
    await backend.start()
    with pytest.raises((ValueError, RuntimeError), match="principal"):
        backend.acquire_text_context("private", conversation_id="chat", display_name="reference", sensitivity=ContextSensitivity.PRIVATE)
    assert backend._attachment_store is None


@pytest.mark.asyncio
async def test_revoke_conversation_context_cannot_delete_turn_snapshot():
    principal = PrincipalContext("owner")
    store = ContextAttachmentStore(principal=principal)
    item = store.acquire_text("turn snapshot", conversation_id="chat", turn_id="context-revoke")
    backend = LangGraphRuntimeBackend(lambda: SimpleNamespace(principal=principal), attachment_store=store)
    await backend.start()
    assert backend.revoke_text_context(item.id, conversation_id="chat") is False
    assert store.diagnostics().item_count == 1


@pytest.mark.asyncio
async def test_acquisition_uses_existing_owner_limits_and_secret_withholding():
    principal = PrincipalContext("owner")
    store = ContextAttachmentStore(principal=principal, max_items=1)
    backend = LangGraphRuntimeBackend(lambda: SimpleNamespace(principal=principal), attachment_store=store)
    await backend.start()
    reference = backend.acquire_text_context("private", conversation_id="chat", display_name="reference", sensitivity=ContextSensitivity.SECRET)
    assert "private" not in store.render([reference], conversation_id="chat", turn_id="turn")
    with pytest.raises(ContextAttachmentError):
        backend.acquire_text_context("overflow", conversation_id="chat", display_name="reference", sensitivity=ContextSensitivity.PRIVATE)
    assert backend.revoke_text_context(reference, conversation_id="chat")
    assert backend.acquire_text_context("recovered", conversation_id="chat", display_name="reference", sensitivity=ContextSensitivity.PRIVATE)


def test_command_text_limit_counts_utf8_bytes_at_boundary():
    assert AcquireTextContext(text="é" * 32768, conversation_id="chat")
    with pytest.raises(ValueError, match="byte limit"):
        AcquireTextContext(text="é" * 32769, conversation_id="chat")


def test_admitted_private_command_cannot_mutate_during_restart():
    import asyncio
    import threading

    admitted, stopping, release = threading.Event(), threading.Event(), threading.Event()
    gate = asyncio.Event()

    class PausingBackend(ImmediateBackend):
        acquired = 0

        def acquire_text_context(self, *_args, **_kwargs):
            self.acquired += 1
            return "ctx-unexpected"

        async def stop(self):
            stopping.set()
            await asyncio.to_thread(release.wait, 3)
            await super().stop()

    backend = PausingBackend()
    host = RuntimeHost(lambda: backend)

    async def delayed_dispatch():
        admitted.set()
        await gate.wait()
        return await host._dispatch(AcquireTextContext(text="private", conversation_id="chat"))

    try:
        host.start().result(timeout=5)
        pending = host.run_coroutine(delayed_dispatch())
        assert admitted.wait(timeout=5)
        restarting = host.submit(RestartRuntime())
        assert stopping.wait(timeout=5)
        host._loop.call_soon_threadsafe(gate.set)
        with pytest.raises(Exception, match="context"):
            pending.result(timeout=5)
        assert backend.acquired == 0
        release.set()
        restarting.result(timeout=5)
    finally:
        release.set()
        stop_host(host)
