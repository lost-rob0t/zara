from __future__ import annotations

import concurrent.futures
from types import SimpleNamespace

import pytest

from zara.runtime import events
from zara.server import PrincipalContext, PrincipalMismatch, RuntimeSupervisor


class FakeHost:
    def __init__(self) -> None:
        self.state = SimpleNamespace(value="new")

    def start(self):
        self.state = SimpleNamespace(value="running")
        future = concurrent.futures.Future()
        future.set_result(None)
        return future

    def shutdown(self):
        self.state = SimpleNamespace(value="stopped")
        future = concurrent.futures.Future()
        future.set_result(None)
        return future


def test_principal_event_buses_do_not_cross_deliver_and_reconnect_reuses_owner_slot():
    supervisor = RuntimeSupervisor(
        host_factory=lambda _principal, _bus: FakeHost(),
        max_active_principals=2,
    )
    alice = PrincipalContext("alice", "curve")
    bob = PrincipalContext("bob", "curve")
    alice_slot = supervisor.start(alice)
    bob_slot = supervisor.open_principal(bob)
    alice_subscription = alice_slot.bus.subscribe()
    bob_subscription = bob_slot.bus.subscribe()

    bob_slot.bus.publish(
        events.ResponseText(
            turn_id="bob-turn",
            conversation_id="bob-private-conversation",
            text="bob private event",
        )
    )

    assert alice_subscription.drain() == []
    bob_envelopes = bob_subscription.drain()
    assert len(bob_envelopes) == 1
    assert bob_envelopes[0].event.text == "bob private event"
    assert bob_envelopes[0].event.conversation_id == "bob-private-conversation"

    assert supervisor.open_principal(alice) is alice_slot
    assert supervisor.open_principal(bob) is bob_slot


def test_same_principal_id_cannot_reconnect_with_different_ownership_kind():
    supervisor = RuntimeSupervisor(
        host_factory=lambda _principal, _bus: FakeHost(),
        max_active_principals=2,
    )
    supervisor.start(PrincipalContext("alice", "curve"))

    with pytest.raises(PrincipalMismatch):
        supervisor.open_principal(PrincipalContext("alice", "guest"))
