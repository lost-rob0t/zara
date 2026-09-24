from __future__ import annotations

from dataclasses import replace
from pathlib import Path

import pytest

from zara.database import DatabaseManager
from zara.notification_identity import derive_notification_id
from zara.notification_routing import (
    NotificationActionRequest,
    NotificationEvent,
    NotificationRouter,
    NotificationRouterStore,
    NotificationStale,
    PeerActivity,
)

NOW = 1_790_135_000_000


class Policy:
    def source_decision(self, app: str) -> str:
        return "allow"

    def content_mode(self, app: str) -> str:
        return "metadata_only"

    def route_policy(self, app: str) -> str:
        return "most_recently_active"

    def filter_decision(self, app: str, category: str, importance: str) -> tuple[None, None]:
        return None, None

    def spam_decision(self, app: str, count: int, duplicate: bool, feedback: str) -> tuple[str, str]:
        if duplicate:
            return "group", "duplicate"
        return "allow", "default_policy"

    def hooks(self, app: str, category: str, importance: str) -> tuple[()]:
        return ()


class NeverEffectPlane:
    def authorize(self, **kwargs):
        raise AssertionError("stale action reached authorization")

    def execute(self, *args, **kwargs):
        raise AssertionError("stale action reached execution")

    def verify(self, *args, **kwargs):
        raise AssertionError("stale action reached verification")


def _event(notification_id: str, generation: int) -> NotificationEvent:
    return NotificationEvent(
        notification_id=notification_id,
        principal_id="user:alice",
        workspace_id="ws:main",
        source_peer="phone",
        owner_peer="phone",
        platform="android",
        source_key="android:pkg:42",
        generation=generation,
        app="com.example.chat",
        created_at_ms=NOW - 1000,
        expires_at_ms=NOW + 60_000,
        title="hello",
        body="secret body",
        category="message",
        importance="normal",
        action_handles=("dismiss",),
        provenance={"collector": "android-notification-listener"},
    )


def _peers() -> list[PeerActivity]:
    capability = frozenset({"notification.present"})
    return [
        PeerActivity("phone", "user:alice", "ws:main", "android", NOW - 50, NOW + 10_000, True, True, capability),
        PeerActivity("desktop", "user:alice", "ws:main", "linux", NOW - 10, NOW + 10_000, True, True, capability),
    ]


def test_generation_replacement_uses_one_logical_identity_and_fences_old_generation_after_restart(tmp_path: Path) -> None:
    generation_one_id = derive_notification_id(
        source_peer="phone",
        platform_identity="android:pkg:42",
        generation=1,
    )
    generation_two_id = derive_notification_id(
        source_peer="phone",
        platform_identity="android:pkg:42",
        generation=2,
    )
    assert generation_one_id == generation_two_id

    db1 = DatabaseManager(tmp_path / "zara.db")
    router1 = NotificationRouter(
        local_peer_id="phone",
        policy=Policy(),
        store=NotificationRouterStore(db1),
    )
    first = _event(generation_one_id, 1)
    second = replace(first, generation=2)
    first_decision = router1.route(first, _peers(), now_ms=NOW)
    assert first_decision.sinks == ("desktop",)
    router1.route(second, _peers(), now_ms=NOW + 1)
    db1.close()

    db2 = DatabaseManager(tmp_path / "zara.db")
    router2 = NotificationRouter(
        local_peer_id="phone",
        policy=Policy(),
        store=NotificationRouterStore(db2),
    )
    with pytest.raises(NotificationStale):
        router2.route(first, _peers(), now_ms=NOW + 2)

    stale_action = NotificationActionRequest(
        request_id="req:old-generation",
        notification_id=generation_one_id,
        generation=1,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="desktop",
        action="dismiss",
    )
    with pytest.raises(NotificationStale):
        router2.perform_action(stale_action, NeverEffectPlane(), now_ms=NOW + 3)
