"""Tests for the ZMQ PUB/SUB cross-process pet event bridge."""

from __future__ import annotations

import threading
import time

import pytest

pytest.importorskip("zmq", reason="pyzmq not installed")


def _drain(actor_ref) -> None:
    from zara.pets.actor import _GetState
    actor_ref.ask(_GetState(), timeout=5)


def test_subscriber_bind_and_publish_round_trip():
    from zara.pets.ipc import PetPublisher, PetSubscriber
    from zara.pets.actor import PetStateActor, _GetState
    from zara.pets import events
    from zara.pets.state import PetState

    received: list = []
    actor = PetStateActor.start(subscriber=lambda s, l: received.append(s))
    try:
        payloads: list = []

        sub = PetSubscriber(on_event=lambda p: payloads.append(p))
        assert sub.start() is True

        pub = PetPublisher()
        pub.start()
        time.sleep(0.4)  # let ZMQ connect

        pub.publish("ModelStarted", label="llm")
        # Drain: poll() is the non-blocking recv (no background thread).
        for _ in range(20):
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
    }
    assert expected <= set(_PAYLOAD_MAP)


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