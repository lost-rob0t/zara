"""Zarathushtra Pets compatibility transport for generic runtime events.

The canonical event definition now lives in :mod:`zara.runtime.events`.  This
module keeps the existing in-process PetStateActor and cross-process ZMQ
transport working while legacy runtime call sites migrate to
``zara.runtime.bridge``.
"""

from __future__ import annotations

import logging
import weakref
from typing import Optional

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events as runtime_events

from .ipc import PetPublisher
from .runtime_adapter import adapt_runtime_event

logger = logging.getLogger(__name__)

_actor_ref: Optional[weakref.ReferenceType] = None
_publisher: Optional[PetPublisher] = None


def register_actor(actor) -> None:
    global _actor_ref
    _actor_ref = weakref.ref(actor)


def unregister_actor() -> None:
    global _actor_ref
    _actor_ref = None


def _ensure_publisher() -> Optional[PetPublisher]:
    global _publisher
    if _publisher is None:
        pub = PetPublisher()
        pub.start()
        if pub._socket is not None:
            _publisher = pub
        return pub if _publisher is not None else None
    return _publisher


def _publish(event_name: str, **kwargs) -> None:
    pub = _ensure_publisher()
    if pub is not None:
        pub.publish(event_name, **kwargs)


def _consume_runtime_event(event: runtime_events.RuntimeEvent) -> None:
    dispatch = adapt_runtime_event(event)
    if dispatch is None:
        return

    ref = _actor_ref() if _actor_ref is not None else None
    if ref is not None:
        try:
            ref.tell(dispatch.event)
        except Exception:
            logger.debug("[PetBridge] tell failed for %r", dispatch.event, exc_info=True)

    logger.debug("[PetBridge] publishing %s %s", dispatch.event_name, dispatch.payload)
    _publish(dispatch.event_name, **dispatch.payload)


# Named registration is idempotent: module reloads replace the same adapter
# instead of accumulating duplicate deliveries.
runtime_bridge.register_legacy_sink("zarathushtra-pets", _consume_runtime_event)


# Legacy compatibility publishers -----------------------------------------
#
# Keep the old call surface while wake.py and third-party code migrate. Every
# call now enters the canonical generic event stream first.


def model_started(label: Optional[str] = None) -> None:
    runtime_bridge.model_started(label=label)


def model_streaming(label: Optional[str] = None) -> None:
    runtime_bridge.model_streaming(label=label)


def model_completed(success: bool = True, label: Optional[str] = None) -> None:
    runtime_bridge.model_completed(success=success, label=label)


def model_failed(reason: str = "", label: Optional[str] = None) -> None:
    runtime_bridge.model_failed(reason=reason, label=label)


def tool_started(label: Optional[str] = None) -> None:
    runtime_bridge.tool_started(label=label)


def tool_completed(success: bool = True, label: Optional[str] = None) -> None:
    runtime_bridge.tool_completed(success=success, label=label)


def agent_started(label: Optional[str] = None) -> None:
    runtime_bridge.agent_started(label=label)


def agent_completed(success: bool = True, label: Optional[str] = None) -> None:
    runtime_bridge.agent_completed(success=success, label=label)


def user_input_required(kind: str = "approval", label: Optional[str] = None) -> None:
    runtime_bridge.user_input_required(kind=kind, label=label)


def user_responded(label: Optional[str] = None) -> None:
    runtime_bridge.user_responded(label=label)


def output_ready(label: Optional[str] = None) -> None:
    runtime_bridge.output_ready(label=label)


def response_text(text: str = "", label: Optional[str] = None) -> None:
    runtime_bridge.response_text(text=text, label=label)


def output_seen(label: Optional[str] = None) -> None:
    runtime_bridge.output_seen(label=label)


def task_cancelled(label: Optional[str] = None) -> None:
    runtime_bridge.task_cancelled(label=label)


def runtime_idle(label: Optional[str] = None) -> None:
    runtime_bridge.runtime_idle(label=label)


def provider_unavailable(reason: str = "", label: Optional[str] = None) -> None:
    runtime_bridge.provider_unavailable(reason=reason, label=label)
