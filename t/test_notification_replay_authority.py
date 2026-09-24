from __future__ import annotations

from pathlib import Path

from zara.database import DatabaseManager
from zara.notification_routing import (
    NotificationEvent,
    NotificationRouter,
    NotificationRouterStore,
    PeerActivity,
)

NOW = 1_790_135_000_000


class AllowPolicy:
    def source_decision(self, app: str) -> str:
        return "allow"

    def content_mode(self, app: str) -> str:
        return "metadata_only"

    def route_policy(self, app: str) -> str:
        return "most_recently_active"

    def filter_decision(
        self,
        app: str,
        category: str,
        importance: str,
    ) -> tuple[str | None, str | None]:
        return None, None

    def spam_decision(
        self,
        app: str,
        count: int,
        duplicate: bool,
        feedback: str,
    ) -> tuple[str, str]:
        return "allow", "fixture"

    def hooks(self, app: str, category: str, importance: str) -> tuple[()]:
        return ()


def _event() -> NotificationEvent:
    return NotificationEvent(
        notification_id="n:replay-authority",
        principal_id="user:alice",
        workspace_id="ws:main",
        source_peer="phone",
        owner_peer="phone",
        platform="android",
        source_key="android:pkg:replay",
        generation=1,
        app="com.example.chat",
        created_at_ms=NOW - 1_000,
        expires_at_ms=NOW + 60_000,
        title="hello",
        body="secret",
        category="message",
        importance="normal",
        action_handles=("dismiss",),
        provenance={"collector": "android-notification-listener"},
    )


def _peers(*, desktop_seen: int, watch_seen: int) -> list[PeerActivity]:
    capabilities = frozenset({"notification.present"})
    return [
        PeerActivity(
            peer_id="desktop",
            principal_id="user:alice",
            workspace_id="ws:main",
            platform="linux",
            observed_at_ms=desktop_seen,
            expires_at_ms=NOW + 30_000,
            active=True,
            online=True,
            capabilities=capabilities,
        ),
        PeerActivity(
            peer_id="watch",
            principal_id="user:alice",
            workspace_id="ws:main",
            platform="wear",
            observed_at_ms=watch_seen,
            expires_at_ms=NOW + 30_000,
            active=True,
            online=True,
            capabilities=capabilities,
        ),
    ]


def test_exact_generation_replay_keeps_original_durable_sink_after_restart(
    tmp_path: Path,
) -> None:
    database_path = tmp_path / "zara.db"
    event = _event()

    first_db = DatabaseManager(database_path)
    first_store = NotificationRouterStore(first_db)
    first_router = NotificationRouter(
        local_peer_id="phone",
        policy=AllowPolicy(),
        store=first_store,
    )
    first = first_router.route(
        event,
        _peers(desktop_seen=NOW - 1, watch_seen=NOW - 10),
        now_ms=NOW,
    )
    assert first.sinks == ("desktop",)
    first_db.close()

    restarted_db = DatabaseManager(database_path)
    restarted_store = NotificationRouterStore(restarted_db)
    restarted_router = NotificationRouter(
        local_peer_id="phone",
        policy=AllowPolicy(),
        store=restarted_store,
    )
    replay = restarted_router.route(
        event,
        _peers(desktop_seen=NOW - 100, watch_seen=NOW + 1),
        now_ms=NOW + 2,
    )

    assert replay.duplicate is True
    assert replay.decision == first.decision
    assert replay.sinks == first.sinks
    assert replay.presentation == first.presentation
