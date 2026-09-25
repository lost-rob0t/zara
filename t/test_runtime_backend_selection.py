from __future__ import annotations

import pytest

from zara.runtime.backend import (
    LangGraphRuntimeBackend,
    RuntimeBackend,
    RuntimeTurnResult,
    UnsupportedRuntimeCommand,
    create_runtime_backend,
)


class FakeConfig:
    def __init__(self, backend: str = "langgraph") -> None:
        self.backend = backend

    def get(self, section: str, key: str, default=None):
        if section == "agent" and key == "backend":
            return self.backend
        return default


def test_runtime_backend_defaults_to_langgraph() -> None:
    backend = create_runtime_backend(FakeConfig())
    assert isinstance(backend, LangGraphRuntimeBackend)


def test_removed_prolog_rlm_backend_fails_closed() -> None:
    with pytest.raises(ValueError, match="Unsupported agent backend"):
        create_runtime_backend(FakeConfig("prolog_rlm"))



@pytest.mark.asyncio
async def test_base_runtime_backend_optional_capabilities_fail_closed() -> None:
    backend = RuntimeBackend()

    backend.bind_event_publisher(lambda _event: None)
    await backend.start()
    backend.commit_turn_result(RuntimeTurnResult(), turn_id="turn-base")
    await backend.cancel_turn("turn-base")
    backend.unregister_tools(("fixture",))
    assert backend.unregister_agent_loop_advice(1) is False
    await backend.stop()

    with pytest.raises(NotImplementedError):
        await backend.submit_turn("hello", turn_id="turn-base")
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.register_tools(())
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.requires_composed_tool_approval("fixture")
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.invoke_composed_tool("user:alice", "fixture", {})
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.register_agent_loop_advice(
            "before_model",
            owner="fixture",
            priority=0,
            callback=lambda: None,
        )
    with pytest.raises(UnsupportedRuntimeCommand):
        backend.customization_diagnostics()
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.start_voice()
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.stop_voice()
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.mute_speech(True)
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.approve_tool("tool-1")
    with pytest.raises(UnsupportedRuntimeCommand):
        await backend.reject_tool("tool-1", "no")


class BareManager:
    def __init__(self) -> None:
        self.exit_calls = 0

    def exit_conversation(self) -> None:
        self.exit_calls += 1


@pytest.mark.asyncio
async def test_langgraph_runtime_lifecycle_fails_closed_without_manager_identity() -> None:
    backend = LangGraphRuntimeBackend(lambda: BareManager())

    with pytest.raises(RuntimeError, match="not started"):
        _ = backend.principal_id
    await backend.start()
    await backend.start()
    with pytest.raises(RuntimeError, match="no principal identity"):
        _ = backend.principal_id
    with pytest.raises(RuntimeError, match="not available"):
        await backend.approve_tool("tool-1")
    with pytest.raises(RuntimeError, match="not available"):
        await backend.reject_tool("tool-1")

    manager = backend._manager
    await backend.stop()
    assert manager.exit_calls == 1
    await backend.stop()
