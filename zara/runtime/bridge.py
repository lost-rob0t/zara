"""Thread-safe in-process runtime event fan-out.

The canonical runtime event stream is in-process and transport-neutral.  Each
subscriber receives ordered :class:`EventEnvelope` objects through its own
queue, so publishers never execute desktop/UI code directly.  Small legacy
adapters (currently Zarathushtra Pets) may register named sinks while their
transport is migrated; sink failures are isolated from runtime producers.
"""

from __future__ import annotations

import logging
import queue
import threading
import time
import uuid
import weakref
from dataclasses import dataclass
from typing import Callable, Optional

from . import events

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class EventEnvelope:
    sequence: int
    occurred_at: float
    event: events.RuntimeEvent


class RuntimeEventSubscription:
    """Queue-backed subscription owned and drained by a consumer."""

    def __init__(self, bus: "RuntimeEventBus", token: str, maxsize: int) -> None:
        self._bus_ref = weakref.ref(bus)
        self._token = token
        self._queue: queue.Queue[EventEnvelope] = queue.Queue(maxsize=maxsize)
        self._closed = False
        self._dropped = 0
        self._lock = threading.Lock()

    @property
    def dropped_count(self) -> int:
        with self._lock:
            return self._dropped

    @property
    def closed(self) -> bool:
        return self._closed

    def get(self, timeout: Optional[float] = None) -> EventEnvelope:
        return self._queue.get(timeout=timeout)

    def get_nowait(self) -> EventEnvelope:
        return self._queue.get_nowait()

    def drain(self, limit: Optional[int] = None) -> list[EventEnvelope]:
        items: list[EventEnvelope] = []
        while limit is None or len(items) < limit:
            try:
                items.append(self._queue.get_nowait())
            except queue.Empty:
                break
        return items

    def close(self) -> None:
        if self._closed:
            return
        self._closed = True
        bus = self._bus_ref()
        if bus is not None:
            bus.unsubscribe(self._token)

    def _put(self, envelope: EventEnvelope) -> None:
        if self._closed:
            return
        try:
            self._queue.put_nowait(envelope)
            return
        except queue.Full:
            pass

        # Bounded subscribers are explicitly lossy under overload. Drop the
        # oldest envelope so the consumer converges toward current state. The
        # default maxsize=0 is unbounded/lossless for control-plane consumers.
        try:
            self._queue.get_nowait()
        except queue.Empty:
            pass
        with self._lock:
            self._dropped += 1
        try:
            self._queue.put_nowait(envelope)
        except queue.Full:  # defensive: another producer may have raced us
            with self._lock:
                self._dropped += 1


LegacySink = Callable[[events.RuntimeEvent], None]


class RuntimeEventBus:
    """Ordered, thread-safe runtime event fan-out."""

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._sequence = 0
        self._subscriptions: dict[str, RuntimeEventSubscription] = {}
        self._legacy_sinks: dict[str, LegacySink] = {}

    def subscribe(self, *, maxsize: int = 0) -> RuntimeEventSubscription:
        if maxsize < 0:
            raise ValueError("subscription maxsize must be >= 0")
        token = uuid.uuid4().hex
        subscription = RuntimeEventSubscription(self, token, maxsize)
        with self._lock:
            self._subscriptions[token] = subscription
        return subscription

    def unsubscribe(self, token: str) -> None:
        with self._lock:
            self._subscriptions.pop(token, None)

    def register_legacy_sink(self, name: str, sink: LegacySink) -> None:
        if not name:
            raise ValueError("legacy sink name must not be empty")
        with self._lock:
            self._legacy_sinks[name] = sink

    def unregister_legacy_sink(self, name: str) -> None:
        with self._lock:
            self._legacy_sinks.pop(name, None)

    def publish(self, event: events.RuntimeEvent) -> EventEnvelope:
        if not isinstance(event, events.RuntimeEvent):
            raise TypeError("runtime event bus accepts RuntimeEvent instances only")

        with self._lock:
            self._sequence += 1
            envelope = EventEnvelope(
                sequence=self._sequence,
                occurred_at=time.time(),
                event=event,
            )
            subscriptions = tuple(self._subscriptions.values())
            sinks = tuple(self._legacy_sinks.items())

        for subscription in subscriptions:
            subscription._put(envelope)

        # Compatibility sinks must be tiny adapters only. A bad adapter is
        # never allowed to fail the assistant turn.
        for name, sink in sinks:
            try:
                sink(event)
            except Exception:
                logger.warning(
                    "Runtime legacy event sink %r failed for %r",
                    name,
                    event,
                    exc_info=True,
                )
        return envelope


_BUS = RuntimeEventBus()


def subscribe(*, maxsize: int = 0) -> RuntimeEventSubscription:
    return _BUS.subscribe(maxsize=maxsize)


def publish(event: events.RuntimeEvent) -> EventEnvelope:
    return _BUS.publish(event)


def register_legacy_sink(name: str, sink: LegacySink) -> None:
    _BUS.register_legacy_sink(name, sink)


def unregister_legacy_sink(name: str) -> None:
    _BUS.unregister_legacy_sink(name)


# Compatibility publisher vocabulary --------------------------------------
#
# Existing runtime call sites can migrate from zara.pets.runtime_bridge to
# this module without changing their behavior. The functions intentionally
# publish generic events; Pets is merely one downstream adapter.


def model_started(label: Optional[str] = None, *, turn_id: Optional[str] = None,
                  conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AssistantStarted(
        turn_id=turn_id, conversation_id=conversation_id, label=label,
    ))


def model_streaming(label: Optional[str] = None, *, text: str = "",
                    turn_id: Optional[str] = None,
                    conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AssistantDelta(
        turn_id=turn_id, conversation_id=conversation_id, label=label, text=text,
    ))


def model_completed(success: bool = True, label: Optional[str] = None, *,
                    text: str = "", turn_id: Optional[str] = None,
                    conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AssistantComplete(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        text=text,
        success=success,
    ))


def model_failed(reason: str = "", label: Optional[str] = None, *,
                 turn_id: Optional[str] = None,
                 conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AssistantFailed(
        turn_id=turn_id, conversation_id=conversation_id, label=label, reason=reason,
    ))


def tool_started(label: Optional[str] = None, *, tool_run_id: Optional[str] = None,
                 turn_id: Optional[str] = None,
                 conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.ToolStarted(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        tool_run_id=tool_run_id,
        tool_name=label,
    ))


def tool_completed(success: bool = True, label: Optional[str] = None, *,
                   tool_run_id: Optional[str] = None,
                   turn_id: Optional[str] = None,
                   conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.ToolCompleted(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        tool_run_id=tool_run_id,
        tool_name=label,
        success=success,
    ))


def agent_started(label: Optional[str] = None, *, turn_id: Optional[str] = None,
                  conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AgentStarted(
        turn_id=turn_id, conversation_id=conversation_id, label=label,
    ))


def agent_completed(success: bool = True, label: Optional[str] = None, *,
                    turn_id: Optional[str] = None,
                    conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.AgentCompleted(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        success=success,
    ))


def user_input_required(kind: str = "approval", label: Optional[str] = None, *,
                        turn_id: Optional[str] = None,
                        conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.UserInputRequired(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        kind=kind,
    ))


def user_responded(label: Optional[str] = None, *, turn_id: Optional[str] = None,
                   conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.UserResponded(
        turn_id=turn_id, conversation_id=conversation_id, label=label,
    ))


def output_ready(label: Optional[str] = None, *, turn_id: Optional[str] = None,
                 conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.OutputReady(
        turn_id=turn_id, conversation_id=conversation_id, label=label,
    ))


def response_text(text: str = "", label: Optional[str] = None, *,
                  turn_id: Optional[str] = None,
                  conversation_id: Optional[str] = None) -> EventEnvelope:
    truncated = len(text) > 280
    payload = text[:280] if truncated else text
    return publish(events.ResponseText(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        text=payload,
        truncated=truncated,
    ))


def output_seen(label: Optional[str] = None, *, turn_id: Optional[str] = None,
                conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.OutputSeen(
        turn_id=turn_id, conversation_id=conversation_id, label=label,
    ))


def task_cancelled(label: Optional[str] = None, *, reason: str = "",
                   turn_id: Optional[str] = None,
                   conversation_id: Optional[str] = None) -> EventEnvelope:
    return publish(events.TurnCancelled(
        turn_id=turn_id,
        conversation_id=conversation_id,
        label=label,
        reason=reason,
    ))


def runtime_idle(label: Optional[str] = None) -> EventEnvelope:
    return publish(events.RuntimeIdle(label=label))


def provider_unavailable(reason: str = "", label: Optional[str] = None) -> EventEnvelope:
    return publish(events.ProviderUnavailable(reason=reason, label=label))
