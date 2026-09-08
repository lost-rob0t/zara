from __future__ import annotations

import pytest

from zara.actors import TurnCancelledReply
from zara.runtime.commands import CancelTurn
from zara.runtime.host import RuntimeHost
from zara.runtime.turn_context import TurnCapabilityLease


class _Backend:
    def __init__(self) -> None:
        self.cancelled: list[str] = []

    async def cancel_turn(self, turn_id: str) -> None:
        self.cancelled.append(turn_id)


class _PluginManager:
    def __init__(self) -> None:
        self.cancelled: list[str] = []

    def cancel_capability_turn(self, turn_id: str) -> None:
        self.cancelled.append(turn_id)


@pytest.mark.asyncio
async def test_cancel_turn_invalidates_plugin_composition_context() -> None:
    backend = _Backend()
    manager = _PluginManager()
    host = RuntimeHost(lambda: backend, publisher=lambda _event: None)
    host._backend = backend
    host._plugin_manager = manager
    lease = TurnCapabilityLease("turn-1")
    host._turn_capability_leases["turn-1"] = lease

    async def coordinator_ask(_message):
        return TurnCancelledReply(turn_id="turn-1", was_already_cancelled=False)

    host._coordinator_ask = coordinator_ask

    receipt = await host._cancel_turn(CancelTurn(turn_id="turn-1"))

    assert receipt.turn_id == "turn-1"
    assert not lease.active
    assert backend.cancelled == ["turn-1"]
    assert manager.cancelled == ["turn-1"]


@pytest.mark.asyncio
async def test_task_turn_cancellation_invalidates_plugin_composition_context() -> None:
    backend = _Backend()
    manager = _PluginManager()
    host = RuntimeHost(lambda: backend, publisher=lambda _event: None)
    host._backend = backend
    host._plugin_manager = manager
    lease = TurnCapabilityLease("task-turn-1")
    host._turn_capability_leases["task-turn-1"] = lease

    async def coordinator_ask(_message):
        return object()

    host._coordinator_ask = coordinator_ask

    await host._cancel_task_turn("task-turn-1")

    assert not lease.active
    assert backend.cancelled == ["task-turn-1"]
    assert manager.cancelled == ["task-turn-1"]
