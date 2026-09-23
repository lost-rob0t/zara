from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import pytest

from zara.database import DatabaseManager
from zara.notification_routing import (
    NotificationActionRequest,
    NotificationEvent,
    NotificationRouter,
    NotificationRouterStore,
    NotificationStale,
    PeerActivity,
    TypedHookAction,
)

NOW = 1_790_135_000_000


class HookPolicy:
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
        return "allow", "test"

    def hooks(
        self,
        app: str,
        category: str,
        importance: str,
    ) -> tuple[TypedHookAction, ...]:
        return (TypedHookAction("auto-dismiss", "dismiss"),)


class NoEffectPlane:
    def __init__(self) -> None:
        self.authorizations: list[tuple[str, str, str, str]] = []
        self.executed: list[str] = []
        self.verified: list[str] = []

    def authorize(
        self,
        *,
        owner_peer: str,
        capability: str,
        principal_id: str,
        workspace_id: str,
    ) -> bool:
        self.authorizations.append(
            (owner_peer, capability, principal_id, workspace_id)
        )
        return True

    def execute(
        self,
        request: NotificationActionRequest,
        *,
        owner_peer: str,
    ) -> Mapping[str, Any]:
        self.executed.append(request.request_id)
        return {"receipt_id": f"receipt:{request.request_id}"}

    def verify(
        self,
        request: NotificationActionRequest,
        receipt: Mapping[str, Any],
        *,
        owner_peer: str,
    ) -> Mapping[str, Any]:
        self.verified.append(request.request_id)
        return {
            "ok": True,
            "generation": request.generation,
            "observed_owner_peer": owner_peer,
            "receipt_id": receipt["receipt_id"],
        }


def _event(generation: int, title: str) -> NotificationEvent:
    return NotificationEvent(
        notification_id="n:hook-stale",
        principal_id="user:alice",
        workspace_id="ws:main",
        source_peer="phone",
        owner_peer="phone",
        platform="android",
        source_key="android:pkg:hook-stale",
        generation=generation,
        app="com.example.chat",
        created_at_ms=NOW - 1_000,
        expires_at_ms=NOW + 60_000,
        title=title,
        body="private",
        category="message",
        importance="normal",
        action_handles=("dismiss",),
    )


def _peers() -> list[PeerActivity]:
    return [
        PeerActivity(
            peer_id="desktop",
            principal_id="user:alice",
            workspace_id="ws:main",
            platform="linux",
            observed_at_ms=NOW - 1,
            expires_at_ms=NOW + 10_000,
            active=True,
            online=True,
            capabilities=frozenset({"notification.present"}),
        )
    ]


def test_stale_hook_decision_after_restart_fails_before_authorization_or_effect(
    tmp_path: Path,
) -> None:
    path = tmp_path / "zara.db"
    db1 = DatabaseManager(path)
    router1 = NotificationRouter(
        local_peer_id="phone",
        policy=HookPolicy(),
        store=NotificationRouterStore(db1),
    )
    first_event = _event(1, "generation one")
    first_decision = router1.route(first_event, _peers(), now_ms=NOW)
    router1.route(_event(2, "generation two"), _peers(), now_ms=NOW + 1)
    db1.close()

    db2 = DatabaseManager(path)
    router2 = NotificationRouter(
        local_peer_id="phone",
        policy=HookPolicy(),
        store=NotificationRouterStore(db2),
    )
    plane = NoEffectPlane()

    with pytest.raises(NotificationStale, match="generation"):
        router2.execute_hooks(
            first_event,
            first_decision,
            plane,
            now_ms=NOW + 2,
        )

    assert plane.authorizations == []
    assert plane.executed == []
    assert plane.verified == []
