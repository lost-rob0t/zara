from __future__ import annotations

from zara.desktop.state import (
    DesktopRuntimeState,
    DesktopStatus,
    INITIAL_STATUS,
    reduce_runtime_event,
)
from zara.runtime import events


def reduce(event, current=INITIAL_STATUS):
    return reduce_runtime_event(current, event)


def test_runtime_lifecycle_maps_to_reachable_statuses():
    assert reduce(events.RuntimeStarted()).state is DesktopRuntimeState.IDLE
    assert reduce(events.RuntimeIdle()).state is DesktopRuntimeState.IDLE
    assert reduce(events.RuntimeStopped(reason="bye")) == DesktopStatus(
        DesktopRuntimeState.DISCONNECTED,
        "bye",
    )
    assert reduce(events.ProviderUnavailable(reason="offline")) == DesktopStatus(
        DesktopRuntimeState.DISCONNECTED,
        "offline",
    )


def test_voice_states_map_without_widget_logic():
    assert reduce(events.VoiceStateChanged(state="listening")).state is DesktopRuntimeState.LISTENING
    assert reduce(events.VoiceStateChanged(state="transcribing")).state is DesktopRuntimeState.THINKING
    assert reduce(events.VoiceStateChanged(state="thinking")).state is DesktopRuntimeState.THINKING
    assert reduce(events.VoiceStateChanged(state="speaking")).state is DesktopRuntimeState.READY
    assert reduce(events.VoiceStateChanged(state="idle")).state is DesktopRuntimeState.IDLE
    assert reduce(events.VoiceStateChanged(state="error", detail="mic gone")) == DesktopStatus(
        DesktopRuntimeState.ERROR,
        "mic gone",
    )


def test_agent_tool_and_output_states_are_explicit():
    assert reduce(events.AgentStarted()).state is DesktopRuntimeState.THINKING
    assert reduce(events.AssistantStarted()).state is DesktopRuntimeState.THINKING
    assert reduce(events.ToolQueued(tool_name="search")).state is DesktopRuntimeState.TOOL_RUNNING
    assert reduce(events.ToolStarted(tool_name="search")).detail == "Running search…"
    assert reduce(events.ToolProgress(tool_name="search", progress=0.5)).state is DesktopRuntimeState.TOOL_RUNNING
    assert reduce(events.ToolWaitingForUser(prompt="Approve command?")).state is DesktopRuntimeState.NEEDS_INPUT
    assert reduce(events.UserInputRequired(prompt="Choose one")).detail == "Choose one"
    assert reduce(events.ToolCompleted()).state is DesktopRuntimeState.THINKING
    assert reduce(events.AgentCompleted()).state is DesktopRuntimeState.READY
    assert reduce(events.AssistantComplete()).state is DesktopRuntimeState.READY
    assert reduce(events.OutputReady()).state is DesktopRuntimeState.READY
    assert reduce(events.TurnCancelled()).state is DesktopRuntimeState.IDLE


def test_failures_are_not_collapsed_into_idle():
    failures = [
        events.RuntimeError(reason="runtime"),
        events.AgentFailed(reason="agent"),
        events.AssistantFailed(reason="model"),
        events.ToolFailed(reason="tool"),
    ]

    for event in failures:
        status = reduce(event)
        assert status.state is DesktopRuntimeState.ERROR
        assert status.detail


def test_unrelated_event_preserves_previous_status():
    current = DesktopStatus(DesktopRuntimeState.LISTENING, "Listening…")
    assert reduce_runtime_event(current, events.TranscriptReady(text="hello")) is current
