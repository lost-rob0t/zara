from __future__ import annotations

import concurrent.futures
import queue

import pytest

from zara.runtime import bridge
from zara.runtime.commands import SubmitTurn
from zara.runtime.events import TurnStarted
from zara.runtime.host import RuntimeHostState
from zara.server import PrincipalContext, PrincipalMismatch, RuntimeSupervisor


def completed(value=None):
    future = concurrent.futures.Future()
    future.set_result(value)
    return future


class PublishingHost:
    def __init__(self, bus: bridge.RuntimeEventBus):
        self.bus = bus
        self.state = RuntimeHostState.NEW
        self.is_alive = False

    def start(self):
        self.state = RuntimeHostState.RUNNING
        self.is_alive = True
        return completed(None)

    def submit(self, command):
        event = TurnStarted(
            conversation_id=getattr(command, "conversation_id", None),
            label=getattr(command, "text", None),
        )
        self.bus.publish(event)
        return completed(command)

    def shutdown(self, reason=""):
        self.state = RuntimeHostState.STOPPED
        self.is_alive = False
        return completed(reason)

    def join(self, timeout=None):
        return None


def principal(name: str, kind: str = "authenticated") -> PrincipalContext:
    return PrincipalContext(principal_id=f"user:{name}", kind=kind)


def supervisor() -> RuntimeSupervisor:
    return RuntimeSupervisor(
        host_factory=lambda _principal, bus: PublishingHost(bus),
        max_active_principals=2,
        shutdown_timeout=0.2,
    )


def test_principal_event_subscriptions_do_not_cross_runtime_buses():
    runtime = supervisor()
    alice = principal("alice")
    bob = principal("bob")
    runtime.start(alice)
    runtime.open_principal(bob)
    alice_events = runtime.subscribe(alice)
    bob_events = runtime.subscribe(bob)

    try:
        alice_command = SubmitTurn(text="alice private", conversation_id="same-label")
        bob_command = SubmitTurn(text="bob private", conversation_id="same-label")
        runtime.submit(alice, alice_command).result(timeout=0.2)
        assert alice_events.get(timeout=0.2).event == TurnStarted(
            conversation_id="same-label",
            label="alice private",
        )
        with pytest.raises(queue.Empty):
            bob_events.get(timeout=0.01)

        runtime.submit(bob, bob_command).result(timeout=0.2)
        assert bob_events.get(timeout=0.2).event == TurnStarted(
            conversation_id="same-label",
            label="bob private",
        )
        with pytest.raises(queue.Empty):
            alice_events.get(timeout=0.01)
    finally:
        alice_events.close()
        bob_events.close()
        assert runtime.shutdown()


def test_same_principal_id_with_different_metadata_cannot_submit_or_subscribe():
    runtime = supervisor()
    alice = principal("alice")
    spoof = PrincipalContext(alice.principal_id, kind="guest")
    runtime.start(alice)

    try:
        with pytest.raises(PrincipalMismatch):
            runtime.submit(spoof, SubmitTurn(text="spoof"))
        with pytest.raises(PrincipalMismatch):
            runtime.subscribe(spoof)
    finally:
        assert runtime.shutdown()


def test_principal_runtime_lookup_cannot_resolve_foreign_principal():
    runtime = supervisor()
    alice = principal("alice")
    bob = principal("bob")
    alice_slot = runtime.start(alice)
    bob_slot = runtime.open_principal(bob)

    try:
        assert runtime.runtime(alice) is alice_slot
        assert runtime.runtime(bob) is bob_slot
        assert alice_slot.host is not bob_slot.host
        assert alice_slot.bus is not bob_slot.bus
        with pytest.raises(KeyError):
            runtime.runtime(principal("mallory"))
    finally:
        assert runtime.shutdown()
