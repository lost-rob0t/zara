"""Pure desktop state reduction for Zara runtime events.

The tray and windows render this coarse state instead of duplicating event
interpretation or reaching into runtime internals.
"""

from __future__ import annotations

import enum
from dataclasses import dataclass

from zara.runtime import events


class DesktopRuntimeState(str, enum.Enum):
    STARTING = "starting"
    IDLE = "idle"
    LISTENING = "listening"
    THINKING = "thinking"
    TOOL_RUNNING = "tool-running"
    NEEDS_INPUT = "needs-input"
    READY = "ready"
    DISCONNECTED = "disconnected"
    ERROR = "error"


@dataclass(frozen=True)
class DesktopStatus:
    state: DesktopRuntimeState
    detail: str = ""


INITIAL_STATUS = DesktopStatus(
    state=DesktopRuntimeState.STARTING,
    detail="Starting Zara runtime…",
)


def reduce_runtime_event(
    current: DesktopStatus,
    event: events.RuntimeEvent,
) -> DesktopStatus:
    """Reduce one generic runtime event into user-facing desktop status."""

    if isinstance(event, events.RuntimeStarted):
        return DesktopStatus(DesktopRuntimeState.IDLE, "Zara is ready")
    if isinstance(event, events.RuntimeIdle):
        return DesktopStatus(DesktopRuntimeState.IDLE, "Idle")
    if isinstance(event, events.RuntimeStopped):
        detail = event.reason or "Runtime stopped"
        return DesktopStatus(DesktopRuntimeState.DISCONNECTED, detail)
    if isinstance(event, events.ProviderUnavailable):
        detail = event.reason or "Provider unavailable"
        return DesktopStatus(DesktopRuntimeState.DISCONNECTED, detail)
    if isinstance(
        event,
        (
            events.RuntimeError,
            events.AgentFailed,
            events.AssistantFailed,
            events.ToolFailed,
        ),
    ):
        detail = getattr(event, "reason", "") or "Runtime error"
        return DesktopStatus(DesktopRuntimeState.ERROR, detail)

    if isinstance(event, events.VoiceStateChanged):
        if event.state == "listening":
            return DesktopStatus(DesktopRuntimeState.LISTENING, event.detail or "Listening…")
        if event.state == "transcribing":
            return DesktopStatus(DesktopRuntimeState.THINKING, event.detail or "Transcribing…")
        if event.state == "thinking":
            return DesktopStatus(DesktopRuntimeState.THINKING, event.detail or "Thinking…")
        if event.state == "speaking":
            return DesktopStatus(DesktopRuntimeState.READY, event.detail or "Speaking…")
        if event.state == "error":
            return DesktopStatus(DesktopRuntimeState.ERROR, event.detail or "Voice error")
        return DesktopStatus(DesktopRuntimeState.IDLE, event.detail or "Idle")

    if isinstance(event, (events.AssistantStarted, events.AgentStarted)):
        return DesktopStatus(DesktopRuntimeState.THINKING, "Thinking…")
    if isinstance(event, (events.ToolQueued, events.ToolStarted, events.ToolProgress)):
        label = getattr(event, "tool_name", None) or event.label
        detail = f"Running {label}…" if label else "Running tool…"
        return DesktopStatus(DesktopRuntimeState.TOOL_RUNNING, detail)
    if isinstance(event, (events.ToolWaitingForUser, events.UserInputRequired)):
        prompt = getattr(event, "prompt", "")
        return DesktopStatus(
            DesktopRuntimeState.NEEDS_INPUT,
            prompt or "Waiting for your input",
        )
    if isinstance(event, events.ToolCompleted):
        return DesktopStatus(DesktopRuntimeState.THINKING, "Finishing response…")
    if isinstance(event, (events.AgentCompleted, events.AssistantComplete, events.OutputReady)):
        return DesktopStatus(DesktopRuntimeState.READY, "Response ready")
    if isinstance(event, events.TurnCancelled):
        return DesktopStatus(DesktopRuntimeState.IDLE, "Cancelled")

    return current
