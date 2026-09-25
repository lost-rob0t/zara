from __future__ import annotations

import pytest

from zara.runtime import bridge, events


def test_bus_rejects_invalid_subscription_sink_and_event_shapes():
    bus = bridge.RuntimeEventBus()
    with pytest.raises(ValueError, match="maxsize"):
        bus.subscribe(maxsize=-1)
    with pytest.raises(ValueError, match="sink name"):
        bus.register_legacy_sink("", lambda _event: None)
    with pytest.raises(TypeError, match="RuntimeEvent"):
        bus.publish(object())

    bus.unregister_legacy_sink("missing")
    subscription = bus.subscribe()
    subscription.close()
    subscription.close()
    bus.publish(events.RuntimeIdle())
    assert subscription.drain(limit=1) == []


def test_compatibility_publishers_emit_runtime_neutral_event_vocabulary():
    subscription = bridge.subscribe()
    try:
        bridge.model_started("model", turn_id="t", conversation_id="c")
        bridge.model_streaming("model", text="chunk", turn_id="t", conversation_id="c")
        bridge.model_completed(False, "model", text="done", turn_id="t", conversation_id="c")
        bridge.model_failed("boom", "model", turn_id="t", conversation_id="c")
        bridge.tool_started("tool", tool_run_id="tr", turn_id="t", conversation_id="c")
        bridge.tool_completed(False, "tool", tool_run_id="tr", turn_id="t", conversation_id="c")
        bridge.agent_started("agent", turn_id="t", conversation_id="c")
        bridge.agent_completed(False, "agent", turn_id="t", conversation_id="c")
        bridge.user_input_required("permission", "tool", turn_id="t", conversation_id="c")
        bridge.user_responded("tool", turn_id="t", conversation_id="c")
        bridge.output_ready("out", turn_id="t", conversation_id="c")
        bridge.response_text("short", "out", turn_id="t", conversation_id="c")
        bridge.output_seen("out", turn_id="t", conversation_id="c")
        bridge.task_cancelled("agent", reason="cancel", turn_id="t", conversation_id="c")
        bridge.runtime_idle("runtime")
        bridge.provider_unavailable("offline", "provider")
        emitted = [envelope.event for envelope in subscription.drain()]
    finally:
        subscription.close()

    assert [type(event) for event in emitted] == [
        events.AssistantStarted,
        events.AssistantDelta,
        events.AssistantComplete,
        events.AssistantFailed,
        events.ToolStarted,
        events.ToolCompleted,
        events.AgentStarted,
        events.AgentCompleted,
        events.UserInputRequired,
        events.UserResponded,
        events.OutputReady,
        events.ResponseText,
        events.OutputSeen,
        events.TurnCancelled,
        events.RuntimeIdle,
        events.ProviderUnavailable,
    ]
    assert emitted[2].success is False
    assert emitted[11].truncated is False
    assert emitted[13].reason == "cancel"
    assert emitted[15].reason == "offline"


def test_named_global_legacy_sink_can_be_removed_without_affecting_subscribers():
    seen = []
    subscription = bridge.subscribe()
    bridge.register_legacy_sink("edge-test", seen.append)
    try:
        bridge.runtime_idle("one")
        bridge.unregister_legacy_sink("edge-test")
        bridge.runtime_idle("two")
        events_seen = [item.event.label for item in subscription.drain()]
    finally:
        bridge.unregister_legacy_sink("edge-test")
        subscription.close()

    assert [event.label for event in seen] == ["one"]
    assert events_seen == ["one", "two"]
