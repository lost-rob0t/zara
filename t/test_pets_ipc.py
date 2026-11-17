"""Tests for the ZMQ PUB/SUB cross-process pet event bridge."""

from __future__ import annotations

import threading
import time

import pytest

pytest.importorskip("zmq", reason="pyzmq not installed")


def _drain(actor_ref) -> None:
    from zara.pets.actor import _GetState
    actor_ref.ask(_GetState(), timeout=5)


def test_subscriber_bind_and_publish_round_trip(monkeypatch):
    monkeypatch.setenv("ZARA_PET_ENDPOINT", "tcp://127.0.0.1:35699")
    from zara.pets.ipc import PetPublisher, PetSubscriber
    from zara.pets.actor import PetStateActor, _GetState
    from zara.pets import events
    from zara.pets.state import PetState

    received: list = []
    actor = PetStateActor.start(subscriber=lambda s, l, t: received.append(s))
    try:
        payloads: list = []

        sub = PetSubscriber(on_event=lambda p: payloads.append(p))
        assert sub.start() is True

        pub = PetPublisher()
        pub.start()
        time.sleep(0.8)  # ZMQ slow-joiner: give SUB time to connect

        # Send a few times — ZMQ PUB/SUB drops the first messages until
        # the subscription is fully established.
        for _ in range(5):
            pub.publish("ModelStarted", label="llm")
            time.sleep(0.1)
        for _ in range(40):
            sub.poll()
            if payloads:
                break
            time.sleep(0.05)

        assert payloads, "subscriber did not receive the event"
        assert payloads[0]["event"] == "ModelStarted"
        assert payloads[0]["label"] == "llm"

        sub.stop()
        pub.stop()
    finally:
        actor.stop()


def test_publisher_no_server_does_not_block():
    from zara.pets.ipc import PetPublisher

    pub = PetPublisher()
    pub.start()
    # Even with no subscriber, publish must return immediately.
    pub.publish("ModelStarted", label="llm")
    pub.stop()


def test_payload_map_covers_all_runtime_events():
    from zara.pets.qt_overlay import _PAYLOAD_MAP

    expected = {
        "ModelStarted", "ModelStreaming", "ModelCompleted", "ModelFailed",
        "ToolStarted", "ToolCompleted", "ToolFailed",
        "AgentStarted", "AgentCompleted", "AgentFailed",
        "UserInputRequired", "UserResponded",
        "OutputReady", "OutputSeen", "TaskCancelled",
        "RuntimeIdle", "ProviderUnavailable",
        "TaskStarted", "TaskStepCompleted", "TaskWaitingApproval",
        "TaskCompleted", "TaskFailed", "TaskAborted",
    }
    assert expected <= set(_PAYLOAD_MAP)


def test_payload_map_builds_long_horizon_task_events():
    from zara.pets.qt_overlay import _PAYLOAD_MAP
    from zara.pets import events

    started = _PAYLOAD_MAP["TaskStarted"]({"event": "TaskStarted", "label": "Deploy"})
    failed = _PAYLOAD_MAP["TaskFailed"](
        {"event": "TaskFailed", "label": "Deploy", "reason": "boom"}
    )
    aborted = _PAYLOAD_MAP["TaskAborted"]({"event": "TaskAborted", "label": "tasks"})

    assert started == events.TaskStarted(label="Deploy")
    assert failed == events.TaskFailed(reason="boom", label="Deploy")
    assert aborted == events.TaskAborted(label="tasks")


def test_dispatch_payload_transitions_actor():
    from zara.pets.qt_overlay import _PAYLOAD_MAP
    from zara.pets.actor import PetStateActor, _GetState
    from zara.pets.state import PetState

    actor = PetStateActor.start()
    try:
        factory = _PAYLOAD_MAP["ModelStarted"]
        actor.tell(factory({"event": "ModelStarted", "label": "llm"}))
        _GetState  # ensure import resolves
        state = actor.ask(_GetState(), timeout=5)
        assert state is PetState.RUNNING
    finally:
        actor.stop()


def test_emotion_labels_cover_all_states():
    from zara.pets.qt_overlay import _EMOTION_LABELS
    from zara.pets.state import PetState

    for state in PetState:
        assert state in _EMOTION_LABELS