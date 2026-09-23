from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import pytest

from zara.database import DatabaseManager
from zara.notification_routing import (
    NotificationActionRequest,
    NotificationDenied,
    NotificationEvent,
    NotificationRouter,
    NotificationRouterStore,
    NotificationStale,
    PeerActivity,
    TypedHookAction,
)

NOW = 1_790_135_000_000


class Policy:
    def __init__(self) -> None:
        self.route = "most_recently_active"
        self.content = "metadata_only"
        self.denied: set[str] = set()
        self.hook_actions: tuple[TypedHookAction, ...] = ()
        self.filter: tuple[str | None, str | None] = (None, None)

    def source_decision(self, app: str) -> str:
        return "deny" if app in self.denied else "allow"

    def content_mode(self, app: str) -> str:
        return self.content

    def route_policy(self, app: str) -> str:
        return self.route

    def filter_decision(self, app: str, category: str, importance: str) -> tuple[str | None, str | None]:
        return self.filter

    def spam_decision(self, app: str, count: int, duplicate: bool, feedback: str) -> tuple[str, str]:
        if feedback == "always_allow":
            return "allow", "explicit_always_allow"
        if feedback == "mute":
            return "suppress", "explicit_mute"
        if feedback == "digest":
            return "digest", "explicit_digest"
        if duplicate:
            return "group", "duplicate"
        if count >= 12:
            return "digest", "burst_digest"
        if count >= 6:
            return "group", "burst_group"
        return "allow", "default_policy"

    def hooks(self, app: str, category: str, importance: str) -> tuple[TypedHookAction, ...]:
        return self.hook_actions


class EffectPlane:
    def __init__(self) -> None:
        self.authorized = True
        self.executed: list[tuple[str, str]] = []
        self.verified: list[str] = []

    def authorize(self, *, owner_peer: str, capability: str, principal_id: str, workspace_id: str) -> bool:
        return self.authorized and owner_peer == "phone" and capability.startswith("notification.action.")

    def execute(self, request: NotificationActionRequest, *, owner_peer: str) -> Mapping[str, Any]:
        self.executed.append((request.request_id, owner_peer))
        return {"receipt_id": f"receipt:{request.request_id}", "owner_peer": owner_peer}

    def verify(self, request: NotificationActionRequest, receipt: Mapping[str, Any], *, owner_peer: str) -> Mapping[str, Any]:
        self.verified.append(request.request_id)
        return {"ok": True, "observed_owner_peer": owner_peer, "receipt_id": receipt["receipt_id"]}


def _store(tmp_path: Path) -> tuple[DatabaseManager, NotificationRouterStore]:
    db = DatabaseManager(tmp_path / "zara.db")
    return db, NotificationRouterStore(db)


def _event(*, notification_id: str = "n:1", generation: int = 1, body: str | None = "secret body", origin_chain: tuple[str, ...] = ()) -> NotificationEvent:
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
        body=body,
        category="message",
        importance="normal",
        origin_chain=origin_chain,
        action_handles=("reply", "dismiss"),
        provenance={"collector": "android-notification-listener"},
    )


def _peers(*, watch_time: int = NOW - 100, desktop_time: int = NOW - 10, watch_online: bool = True, desktop_ttl: int = NOW + 10_000) -> list[PeerActivity]:
    cap = frozenset({"notification.present"})
    return [
        PeerActivity("phone", "user:alice", "ws:main", "android", NOW - 50, NOW + 10_000, True, True, cap),
        PeerActivity("desktop", "user:alice", "ws:main", "linux", desktop_time, desktop_ttl, True, True, cap),
        PeerActivity("watch", "user:alice", "ws:main", "wear", watch_time, NOW + 10_000, True, watch_online, cap),
    ]


def test_routes_to_freshest_eligible_peer_and_switches_to_watch(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    policy = Policy()
    router = NotificationRouter(local_peer_id="phone", policy=policy, store=store)

    first = router.route(_event(notification_id="n:desktop"), _peers(), now_ms=NOW)
    assert first.decision == "allow"
    assert first.sinks == ("desktop",)

    second = router.route(
        _event(notification_id="n:watch"),
        _peers(watch_time=NOW - 1),
        now_ms=NOW,
    )
    assert second.sinks == ("watch",)


def test_stale_or_offline_peer_cannot_win_and_cross_principal_peer_is_ignored(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    peers = _peers(watch_time=NOW, watch_online=False, desktop_time=NOW + 100, desktop_ttl=NOW - 1)
    peers.append(
        PeerActivity(
            "foreign",
            "user:bob",
            "ws:main",
            "linux",
            NOW + 1000,
            NOW + 10_000,
            True,
            True,
            frozenset({"notification.present"}),
        )
    )

    decision = router.route(_event(notification_id="n:fallback"), peers, now_ms=NOW)
    assert decision.sinks == ("phone",)


def test_metadata_only_policy_never_forwards_body(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    policy = Policy()
    policy.content = "metadata_only"
    router = NotificationRouter(local_peer_id="phone", policy=policy, store=store)

    decision = router.route(_event(), _peers(), now_ms=NOW)
    assert decision.presentation["body"] is None
    assert decision.presentation["title"] == "hello"
    assert "secret body" not in repr(decision.presentation)


def test_duplicate_loop_and_storm_are_bounded_but_explicit_allow_wins(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    policy = Policy()
    router = NotificationRouter(local_peer_id="phone", policy=policy, store=store)

    first = router.route(_event(notification_id="n:dup"), _peers(), now_ms=NOW)
    duplicate = router.route(_event(notification_id="n:dup"), _peers(), now_ms=NOW + 1)
    loop = router.route(
        _event(notification_id="n:loop", origin_chain=("desktop", "phone")),
        _peers(),
        now_ms=NOW + 2,
    )

    assert first.decision == "allow"
    assert duplicate.decision == "group"
    assert duplicate.sinks == ()
    assert duplicate.duplicate is True
    assert loop.decision == "suppress"
    assert "loop-fence" in loop.evidence

    burst_decisions = []
    for index in range(10):
        burst_decisions.append(
            router.route(
                _event(notification_id=f"n:burst:{index}"),
                _peers(),
                now_ms=NOW + 10 + index,
            )
        )
    assert any(item.decision in {"group", "digest"} for item in burst_decisions)

    store.set_feedback(
        principal_id="user:alice",
        workspace_id="ws:main",
        app="com.example.chat",
        decision="always_allow",
        now_ms=NOW + 50,
    )
    allowed = router.route(_event(notification_id="n:feedback"), _peers(), now_ms=NOW + 51)
    assert allowed.decision == "allow"
    assert "spam:explicit_always_allow" in allowed.evidence


def test_restart_reuses_shared_database_for_dedupe_and_feedback(tmp_path: Path) -> None:
    db1, store1 = _store(tmp_path)
    router1 = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store1)
    router1.route(_event(notification_id="n:restart"), _peers(), now_ms=NOW)
    store1.set_feedback(
        principal_id="user:alice",
        workspace_id="ws:main",
        app="com.example.chat",
        decision="always_allow",
        now_ms=NOW,
    )
    db1.close()

    db2 = DatabaseManager(tmp_path / "zara.db")
    router2 = NotificationRouter(
        local_peer_id="phone",
        policy=Policy(),
        store=NotificationRouterStore(db2),
    )
    after_restart = router2.route(
        _event(notification_id="n:restart"),
        _peers(),
        now_ms=NOW + 1,
    )
    assert after_restart.duplicate is True
    assert after_restart.decision == "allow"
    assert "spam:explicit_always_allow" in after_restart.evidence


def test_hook_crosses_canonical_effect_plane_once_and_requires_fresh_verification(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    policy = Policy()
    policy.hook_actions = (TypedHookAction("auto-dismiss", "dismiss"),)
    router = NotificationRouter(local_peer_id="phone", policy=policy, store=store)
    event = _event(notification_id="n:hook")
    decision = router.route(event, _peers(), now_ms=NOW)
    plane = EffectPlane()

    results = router.execute_hooks(event, decision, plane, now_ms=NOW + 1)
    again = router.execute_hooks(event, decision, plane, now_ms=NOW + 2)

    assert len(results) == 1
    assert results[0].owner_peer == "phone"
    assert results[0].verification["ok"] is True
    assert plane.executed == [("hook:auto-dismiss:1", "phone")]
    assert plane.verified == ["hook:auto-dismiss:1"]
    assert again == ()


def test_action_from_remote_sink_routes_to_source_owner_and_stale_generation_is_fenced(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:action", generation=2)
    router.route(event, _peers(), now_ms=NOW)
    plane = EffectPlane()
    request = NotificationActionRequest(
        request_id="req:wear:reply",
        notification_id="n:action",
        generation=2,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="watch",
        action="inline_reply",
        argument="reply",
    )

    result = router.perform_action(request, plane, now_ms=NOW + 1)
    assert result.owner_peer == "phone"
    assert plane.executed[-1][1] == "phone"

    stale = NotificationActionRequest(
        request_id="req:wear:stale",
        notification_id="n:action",
        generation=1,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="watch",
        action="dismiss",
    )
    with pytest.raises(NotificationStale):
        router.perform_action(stale, plane, now_ms=NOW + 2)


def test_denied_capability_never_executes_effect(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:denied")
    router.route(event, _peers(), now_ms=NOW)
    plane = EffectPlane()
    plane.authorized = False
    request = NotificationActionRequest(
        request_id="req:denied",
        notification_id="n:denied",
        generation=1,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="desktop",
        action="dismiss",
    )
    with pytest.raises(NotificationDenied, match="capability denied"):
        router.perform_action(request, plane, now_ms=NOW + 1)
    assert plane.executed == []
