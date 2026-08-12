from __future__ import annotations

import pathlib

import pytest

from zara.pets import events as pet_events
from zara.pets.runtime_adapter import adapt_runtime_event
from zara.runtime import bridge as runtime_bridge
from zara.runtime import events


def test_runtime_event_correlation_is_provider_neutral():
    event = events.AssistantStarted(
        turn_id="turn-7",
        conversation_id="conversation-3",
        label="llm",
    )

    assert event.turn_id == "turn-7"
    assert event.conversation_id == "conversation-3"
    assert event.label == "llm"


def test_event_bus_orders_events_for_each_subscriber():
    bus = runtime_bridge.RuntimeEventBus()
    subscription = bus.subscribe()

    first = bus.publish(events.AgentStarted(label="agent"))
    second = bus.publish(events.AssistantStarted(label="llm"))

    received = subscription.drain()
    assert [item.sequence for item in received] == [first.sequence, second.sequence]
    assert first.sequence == 1
    assert second.sequence == 2
    assert first.occurred_at <= second.occurred_at
    assert isinstance(received[0].event, events.AgentStarted)
    assert isinstance(received[1].event, events.AssistantStarted)


def test_default_subscription_is_lossless_for_control_plane_events():
    bus = runtime_bridge.RuntimeEventBus()
    subscription = bus.subscribe()

    for index in range(1000):
        bus.publish(events.AssistantDelta(text=str(index)))

    assert len(subscription.drain()) == 1000
    assert subscription.dropped_count == 0


def test_bounded_subscription_drops_oldest_and_counts_overflow():
    bus = runtime_bridge.RuntimeEventBus()
    subscription = bus.subscribe(maxsize=2)

    bus.publish(events.AgentStarted(label="one"))
    bus.publish(events.AgentStarted(label="two"))
    bus.publish(events.AgentStarted(label="three"))

    received = subscription.drain()
    assert [item.event.label for item in received] == ["two", "three"]
    assert subscription.dropped_count == 1


def test_subscription_close_removes_it_from_future_delivery():
    bus = runtime_bridge.RuntimeEventBus()
    subscription = bus.subscribe()
    subscription.close()

    bus.publish(events.RuntimeIdle())

    assert subscription.closed is True
    assert subscription.drain() == []


def test_bad_legacy_sink_cannot_fail_runtime_publish():
    bus = runtime_bridge.RuntimeEventBus()
    subscription = bus.subscribe()
    good_events = []

    def bad_sink(_event):
        raise RuntimeError("boom")

    def good_sink(event):
        good_events.append(event)

    bus.register_legacy_sink("bad", bad_sink)
    bus.register_legacy_sink("good", good_sink)

    envelope = bus.publish(events.RuntimeStarted())

    assert subscription.get_nowait() == envelope
    assert good_events == [envelope.event]


def test_named_legacy_sink_registration_is_idempotent():
    bus = runtime_bridge.RuntimeEventBus()
    seen = []

    bus.register_legacy_sink("pets", lambda _event: seen.append("old"))
    bus.register_legacy_sink("pets", lambda _event: seen.append("new"))
    bus.publish(events.RuntimeIdle())

    assert seen == ["new"]


def test_runtime_event_validation_rejects_invalid_progress_and_voice_state():
    with pytest.raises(ValueError):
        events.ToolProgress(progress=1.1)

    with pytest.raises(ValueError):
        events.VoiceStateChanged(state="telepathic")


def test_pet_adapter_maps_assistant_events_without_provider_objects():
    dispatch = adapt_runtime_event(
        events.AssistantComplete(success=True, label="llm", turn_id="turn-1")
    )

    assert dispatch is not None
    assert dispatch.event_name == "ModelCompleted"
    assert dispatch.payload == {"success": True, "label": "llm"}
    assert dispatch.event == pet_events.ModelCompleted(success=True, label="llm")


def test_pet_adapter_maps_tool_approval_and_cancellation():
    waiting = adapt_runtime_event(
        events.ToolWaitingForUser(kind="permission", label="shell")
    )
    cancelled = adapt_runtime_event(events.TurnCancelled(label="agent"))

    assert waiting is not None
    assert waiting.event == pet_events.UserInputRequired(
        kind="permission", label="shell"
    )
    assert cancelled is not None
    assert isinstance(cancelled.event, pet_events.TaskCancelled)


def test_pet_adapter_ignores_events_it_does_not_render_yet():
    assert adapt_runtime_event(events.VoiceStateChanged(state="listening")) is None


def test_response_text_compatibility_publisher_truncates_at_boundary():
    subscription = runtime_bridge.subscribe()
    try:
        runtime_bridge.response_text("x" * 300, label="Zara")
        event = subscription.get_nowait().event
    finally:
        subscription.close()

    assert isinstance(event, events.ResponseText)
    assert len(event.text) == 280
    assert event.truncated is True
    assert event.label == "Zara"


def test_agent_graph_no_longer_depends_on_pet_runtime_bridge():
    source = pathlib.Path(__file__).parents[1] / "zara" / "agent" / "graph.py"
    text = source.read_text(encoding="utf-8")

    assert "zara.pets" not in text
    assert "zara.runtime" in text
