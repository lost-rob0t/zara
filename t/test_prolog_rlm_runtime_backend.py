from __future__ import annotations

import asyncio
from types import SimpleNamespace

import pytest

from zara.runtime.discovery import RuntimeDiscoveryError, runtime_descriptor_from_wire
from zara.runtime.prolog_rlm import (
    PrologRlmRuntimeBackend,
    PrologRlmRuntimeError,
)


def descriptor(**overrides):
    value = {
        "id": "prolog-rlm",
        "display_name": "Prolog-RLM",
        "protocol": "ZARA-RUNTIME/1",
        "runtime_version": "0.1.0-dev",
        "implementation_version": "0.1.0-dev",
        "installed": True,
        "available": True,
        "health": "ready",
        "locality": "local_sidecar",
        "transport": "loopback_http",
        "capabilities": ["direct", "rlm", "cancel"],
        "profiles": [],
        "provider_control": "runtime",
        "model_control": "runtime",
        "supports_streaming": False,
        "supports_cancel": True,
        "supports_context_handles": False,
        "supports_host_tools": False,
    }
    value.update(overrides)
    return runtime_descriptor_from_wire(value)


class FakeClient:
    def __init__(self, runtime=None, reply=None):
        self.runtime = runtime or descriptor()
        self.reply = reply or {
            "protocol": "ZARA-RUNTIME/1",
            "runtime_id": "prolog-rlm",
            "status": "completed",
            "text": "from Prolog",
            "result": {"trace": "bounded"},
        }
        self.generated = []
        self.cancelled = []

    def discover(self):
        return (self.runtime,)

    def generate(self, request):
        self.generated.append(request)
        return self.reply

    def cancel(self, request_id):
        self.cancelled.append(request_id)
        return {
            "protocol": "ZARA-RUNTIME/1",
            "runtime_id": "prolog-rlm",
            "request_id": request_id,
            "status": "cancel_requested",
        }


class RuntimeDeathClient(FakeClient):
    def generate(self, request):
        self.generated.append(request)
        raise RuntimeDiscoveryError(
            "provider payload Authorization: Bearer secret-runtime-token"
        )


def run(coro):
    return asyncio.run(coro)


def test_real_turn_uses_prolog_runtime_without_python_llm() -> None:
    client = FakeClient()
    backend = PrologRlmRuntimeBackend(client=client, principal_id="local-owner")

    async def scenario():
        await backend.start()
        return await backend.submit_turn(
            "solve this",
            turn_id="turn-1",
            conversation_id="conversation-1",
            conversation_history=[
                SimpleNamespace(type="system", content="system rules"),
                SimpleNamespace(type="human", content="earlier question"),
                SimpleNamespace(type="ai", content="earlier answer"),
                SimpleNamespace(type="tool", content="must not cross"),
            ],
            system_context="project context from Zara",
        )

    result = run(scenario())

    assert result.response == "from Prolog"
    request = client.generated[0]
    assert request["mode"] == "rlm"
    assert request["metadata"]["principal_id"] == "local-owner"
    assert request["inline_context"] == "project context from Zara"
    assert [message["role"] for message in request["messages"]] == [
        "system",
        "user",
        "assistant",
        "user",
    ]
    assert "provider" not in request
    assert "model" not in request
    assert "api_key" not in repr(request)


def test_context_handles_fail_closed_until_runtime_advertises_capability() -> None:
    backend = PrologRlmRuntimeBackend(client=FakeClient())

    async def scenario():
        await backend.start()
        await backend.submit_turn(
            "question",
            turn_id="turn-2",
            context_ids=("ctx-1",),
        )

    with pytest.raises(Exception, match="context handles"):
        run(scenario())


def test_cancel_maps_to_sidecar_request_id() -> None:
    client = FakeClient()
    backend = PrologRlmRuntimeBackend(client=client)

    async def scenario():
        await backend.start()
        await backend.cancel_turn("turn-3")

    run(scenario())
    assert client.cancelled == ["turn-3"]


def test_incompatible_runtime_identity_is_not_started() -> None:
    client = FakeClient(runtime=descriptor(id="different-runtime"))
    backend = PrologRlmRuntimeBackend(client=client)

    with pytest.raises(PrologRlmRuntimeError, match="not an available installed runtime"):
        run(backend.start())


def test_runtime_death_is_typed_and_redacts_sidecar_details() -> None:
    backend = PrologRlmRuntimeBackend(client=RuntimeDeathClient())

    async def scenario():
        await backend.start()
        await backend.submit_turn("question", turn_id="turn-death")

    with pytest.raises(PrologRlmRuntimeError) as raised:
        run(scenario())

    assert raised.value.kind == "transport_error"
    assert str(raised.value) == "transport_error: Prolog-RLM runtime transport failed"
    assert "secret-runtime-token" not in str(raised.value)


def test_host_tools_remain_host_owned_until_capability_is_advertised() -> None:
    backend = PrologRlmRuntimeBackend(client=FakeClient())
    tool = SimpleNamespace(name="plugin.lookup")
    backend.register_tools([tool])

    assert backend._registered_tools == {"plugin.lookup": tool}
