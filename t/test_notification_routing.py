from __future__ import annotations

import threading
import time
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
        self.authorizations: list[tuple[str, str, str, str]] = []
        self.executed: list[tuple[str, str]] = []
        self.verified: list[str] = []

    def authorize(self, *, owner_peer: str, capability: str, principal_id: str, workspace_id: str) -> bool:
        self.authorizations.append((owner_peer, capability, principal_id, workspace_id))
        return self.authorized and owner_peer == "phone" and capability.startswith("notification.action.")

    def execute(self, request: NotificationActionRequest, *, owner_peer: str) -> Mapping[str, Any]:
        self.executed.append((request.request_id, owner_peer))
        return {"receipt_id": f"receipt:{request.request_id}", "owner_peer": owner_peer}

    def verify(self, request: NotificationActionRequest, receipt: Mapping[str, Any], *, owner_peer: str) -> Mapping[str, Any]:
        self.verified.append(request.request_id)
        return {
            "ok": True,
            "observed_owner_peer": owner_peer,
            "receipt_id": receipt["receipt_id"],
            "generation": request.generation,
        }


class StaleVerificationPlane(EffectPlane):
    def verify(self, request: NotificationActionRequest, receipt: Mapping[str, Any], *, owner_peer: str) -> Mapping[str, Any]:
        self.verified.append(request.request_id)
        return {
            "ok": True,
            "observed_owner_peer": owner_peer,
            "receipt_id": receipt["receipt_id"],
            "generation": request.generation - 1,
        }


class RecoveringVerificationPlane(EffectPlane):
    def __init__(self) -> None:
        super().__init__()
        self._stale_once = True

    def verify(self, request: NotificationActionRequest, receipt: Mapping[str, Any], *, owner_peer: str) -> Mapping[str, Any]:
        self.verified.append(request.request_id)
        generation = request.generation
        if self._stale_once:
            self._stale_once = False
            generation -= 1
        return {
            "ok": True,
            "observed_owner_peer": owner_peer,
            "receipt_id": receipt["receipt_id"],
            "generation": generation,
        }


class SlowEffectPlane(EffectPlane):
    def __init__(self) -> None:
        super().__init__()
        self._lock = threading.Lock()

    def execute(self, request: NotificationActionRequest, *, owner_peer: str) -> Mapping[str, Any]:
        with self._lock:
            self.executed.append((request.request_id, owner_peer))
        time.sleep(0.05)
        return {"receipt_id": f"receipt:{request.request_id}", "owner_peer": owner_peer}


def _store(tmp_path: Path) -> tuple[DatabaseManager, NotificationRouterStore]:
    db = DatabaseManager(tmp_path / "zara.db")
    return db, NotificationRouterStore(db)


def _event(*, notification_id: str = "n:1", generation: int = 1, body: str | None = "secret body", origin_chain: tuple[str, ...] = (), expires_at_ms: int = NOW + 60_000, action_handles: tuple[str, ...] = ("reply", "dismiss")) -> NotificationEvent:
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
        expires_at_ms=expires_at_ms,
        title="hello",
        body=body,
        category="message",
        importance="normal",
        origin_chain=origin_chain,
        action_handles=action_handles,
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
        request_id="req:desktop:reply",
        notification_id="n:action",
        generation=2,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="desktop",
        action="inline_reply",
        argument="reply",
    )

    result = router.perform_action(request, plane, now_ms=NOW + 1)
    assert result.owner_peer == "phone"
    assert plane.executed[-1][1] == "phone"

    stale = NotificationActionRequest(
        request_id="req:desktop:stale",
        notification_id="n:action",
        generation=1,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="desktop",
        action="dismiss",
    )
    with pytest.raises(NotificationStale):
        router.perform_action(stale, plane, now_ms=NOW + 2)


def test_stale_postcondition_generation_is_rejected_after_effect_execution(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:verify", generation=3)
    router.route(event, _peers(), now_ms=NOW)
    plane = StaleVerificationPlane()
    request = NotificationActionRequest(
        request_id="req:verify",
        notification_id="n:verify",
        generation=3,
        principal_id="user:alice",
        workspace_id="ws:main",
        sink_peer="desktop",
        action="dismiss",
    )

    with pytest.raises(NotificationDenied, match="fresh verified postcondition"):
        router.perform_action(request, plane, now_ms=NOW + 1)

    assert plane.executed == [("req:verify", "phone")]
    assert plane.verified == ["req:verify"]
    assert store.effect_done("user:alice", "ws:main", "action:req:verify") is False


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


def test_expired_action_after_restart_is_denied_before_authorize_or_execute(tmp_path: Path) -> None:
    db1, store1 = _store(tmp_path)
    router1 = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store1)
    event = _event(notification_id="n:expired-action", expires_at_ms=NOW + 10)
    decision = router1.route(event, _peers(), now_ms=NOW)
    assert decision.sinks == ("desktop",)
    db1.close()

    db2 = DatabaseManager(tmp_path / "zara.db")
    router2 = NotificationRouter(
        local_peer_id="phone",
        policy=Policy(),
        store=NotificationRouterStore(db2),
    )
    plane = EffectPlane()
    request = NotificationActionRequest(
        request_id="req:expired",
        notification_id=event.notification_id,
        generation=event.generation,
        principal_id=event.principal_id,
        workspace_id=event.workspace_id,
        sink_peer="desktop",
        action="dismiss",
    )

    with pytest.raises(NotificationDenied, match="expired"):
        router2.perform_action(request, plane, now_ms=NOW + 11)

    assert plane.authorizations == []
    assert plane.executed == []
    assert plane.verified == []


def test_action_requires_routed_sink_and_exposed_source_handle(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:grant")
    decision = router.route(event, _peers(), now_ms=NOW)
    assert decision.sinks == ("desktop",)

    wrong_sink_plane = EffectPlane()
    wrong_sink = NotificationActionRequest(
        request_id="req:wrong-sink",
        notification_id=event.notification_id,
        generation=event.generation,
        principal_id=event.principal_id,
        workspace_id=event.workspace_id,
        sink_peer="watch",
        action="dismiss",
    )
    with pytest.raises(NotificationDenied, match="routed sink"):
        router.perform_action(wrong_sink, wrong_sink_plane, now_ms=NOW + 1)
    assert wrong_sink_plane.authorizations == []
    assert wrong_sink_plane.executed == []

    missing_handle_plane = EffectPlane()
    missing_handle = NotificationActionRequest(
        request_id="req:missing-handle",
        notification_id=event.notification_id,
        generation=event.generation,
        principal_id=event.principal_id,
        workspace_id=event.workspace_id,
        sink_peer="desktop",
        action="open",
    )
    with pytest.raises(NotificationDenied, match="action handle"):
        router.perform_action(missing_handle, missing_handle_plane, now_ms=NOW + 1)
    assert missing_handle_plane.authorizations == []
    assert missing_handle_plane.executed == []


def test_concurrent_duplicate_effect_requests_execute_exactly_once(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:race")
    router.route(event, _peers(), now_ms=NOW)
    plane = SlowEffectPlane()
    request = NotificationActionRequest(
        request_id="req:race",
        notification_id=event.notification_id,
        generation=event.generation,
        principal_id=event.principal_id,
        workspace_id=event.workspace_id,
        sink_peer="desktop",
        action="dismiss",
    )
    start = threading.Barrier(3)
    successes: list[object] = []
    failures: list[BaseException] = []

    def run() -> None:
        start.wait()
        try:
            successes.append(
                router.perform_action(
                    request,
                    plane,
                    effect_key="action:shared-race",
                    now_ms=NOW + 1,
                )
            )
        except BaseException as exc:
            failures.append(exc)

    threads = [threading.Thread(target=run), threading.Thread(target=run)]
    for thread in threads:
        thread.start()
    start.wait()
    for thread in threads:
        thread.join(timeout=5)
        assert not thread.is_alive()

    assert plane.executed == [("req:race", "phone")]
    assert len(successes) == 1
    assert len(failures) == 1
    assert isinstance(failures[0], NotificationDenied)


def test_failed_verification_retry_reuses_durable_receipt_without_reexecuting(tmp_path: Path) -> None:
    _, store = _store(tmp_path)
    router = NotificationRouter(local_peer_id="phone", policy=Policy(), store=store)
    event = _event(notification_id="n:recover")
    router.route(event, _peers(), now_ms=NOW)
    plane = RecoveringVerificationPlane()
    request = NotificationActionRequest(
        request_id="req:recover",
        notification_id=event.notification_id,
        generation=event.generation,
        principal_id=event.principal_id,
        workspace_id=event.workspace_id,
        sink_peer="desktop",
        action="dismiss",
    )

    with pytest.raises(NotificationDenied, match="fresh verified postcondition"):
        router.perform_action(request, plane, now_ms=NOW + 1)
    result = router.perform_action(request, plane, now_ms=NOW + 2)

    assert result.success is True
    assert plane.executed == [("req:recover", "phone")]
    assert plane.verified == ["req:recover", "req:recover"]
    assert store.effect_done("user:alice", "ws:main", "action:req:recover") is True
