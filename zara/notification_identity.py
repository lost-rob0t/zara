"""Stable notification identity helpers shared by platform adapters and router tests."""

from __future__ import annotations

import hashlib

_MAX_ID_BYTES = 1024


def _component(value: str, field_name: str) -> bytes:
    if not isinstance(value, str) or not value:
        raise ValueError(f"{field_name} must be a non-empty string")
    encoded = value.encode("utf-8")
    if len(encoded) > _MAX_ID_BYTES:
        raise ValueError(f"{field_name} exceeds {_MAX_ID_BYTES} UTF-8 bytes")
    return encoded


def derive_notification_id(*, source_peer: str, platform_identity: str, generation: int) -> str:
    """Derive one stable Zara identity for all generations of a platform notification.

    Platform adapters (#906 on Android, Linux notification ingress, Wear relay)
    should use this before publishing ``NotificationEvent``. The stable identity
    is owned by source peer plus platform identity; ``generation`` remains the
    monotonic replacement/staleness fence carried by the event itself.
    """

    if isinstance(generation, bool) or not isinstance(generation, int) or generation < 1:
        raise ValueError("generation must be an integer >= 1")
    parts = (
        _component(source_peer, "source_peer"),
        _component(platform_identity, "platform_identity"),
    )
    digest = hashlib.sha256()
    digest.update(b"ZARA-NOTIFICATION/1\0")
    for part in parts:
        digest.update(len(part).to_bytes(4, "big"))
        digest.update(part)
    return "notification:" + digest.hexdigest()


__all__ = ["derive_notification_id"]
