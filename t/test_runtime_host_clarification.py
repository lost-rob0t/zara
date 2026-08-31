from __future__ import annotations

import threading

import pytest

from zara.runtime.clarification import (
    ClarificationCoordinator,
    TIMER_SET_TEMPLATE,
)
from zara.runtime.commands import RestartRuntime, SubmitTurn, ShutdownRuntime
from zara.runtime.host import RuntimeHost, RuntimeHostState


class ImmediateBackend:
    def __init__(self) -> None:
        self.turns: list[str] = []

    def bind_event_publisher(self, publisher) -> None:
        pass

    async def start(self) -> None:
        pass

    async def submit_turn(self, text, *, turn_id, conversation_id=None, context_ids=()):
        self.turns.append(text)
        from zara.runtime.backend import RuntimeTurnResult

        return RuntimeTurnResult(response="ok")

    async def cancel_turn(self, turn_id) -> None:
        pass

    async def stop(self) -> None:
        pass


def stop_host(host: RuntimeHost) -> None:
    from zara.runtime.host import RuntimeHostState

    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
        try:
            host.shutdown("test cleanup").result(timeout=5)
        except Exception:
            pass
    host.join(timeout=5)


def test_host_exposes_clarification_coordinator():
    host = RuntimeHost(lambda: ImmediateBackend())

    try:
        assert isinstance(host.clarifications, ClarificationCoordinator)
    finally:
        stop_host(host)


def test_restart_drops_open_clarification_sessions():
    host = RuntimeHost(lambda: ImmediateBackend())

    try:
        host.start().result(timeout=5)
        opened = host.clarifications.open(
            TIMER_SET_TEMPLATE, principal="alice", conversation_id="c1"
        )
        assert opened.kind == "opened"

        host.submit(RestartRuntime(reason="test restart")).result(timeout=10)

        outcome = host.clarifications.submit_follow_up(
            "twenty minutes", principal="alice", conversation_id="c1"
        )
        assert outcome.kind == "stale"
        assert outcome.message == ClarificationCoordinator.STALE_MESSAGE
    finally:
        stop_host(host)


def test_shutdown_drops_open_clarification_sessions():
    host = RuntimeHost(lambda: ImmediateBackend())
    host.start().result(timeout=5)
    opened = host.clarifications.open(
        TIMER_SET_TEMPLATE, principal="alice", conversation_id="c1"
    )
    assert opened.kind == "opened"

    host.shutdown("test shutdown").result(timeout=10)
    host.join(timeout=5)

    outcome = host.clarifications.submit_follow_up(
        "twenty minutes", principal="alice", conversation_id="c1"
    )
    assert outcome.kind == "stale"
    assert outcome.message == ClarificationCoordinator.STALE_MESSAGE
    closed = host.clarifications.session_for("alice", "c1")
    assert closed is not None
    assert closed.state == "closed"
    assert closed.frame.status.value == "superseded"
