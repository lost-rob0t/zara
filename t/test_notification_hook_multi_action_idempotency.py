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
    PeerActivity,
    PrologNotificationPolicy,
)
from zara.prolog_engine import PrologEngine

NOW = 1_790_135_000_000
REPO_ROOT = Path(__file__).resolve().parents[1]


class RecordingEffectPlane:
    def __init__(self) -> None:
        self.authorizations: list[tuple[str, str, str, str]] = []
        self.executed: list[tuple[str, str, str | None]] = []
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
        self.executed.append((request.request_id, request.action, request.argument))
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


@pytest.fixture
def prolog_policy(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> PrologNotificationPolicy:
    config_dir = tmp_path / "xdg" / "zarathushtra"
    config_dir.mkdir(parents=True)
    (config_dir / "config.pl").write_text(
        "kb_notification_policy:notification_match_action("
        "multi_rule, app('com.example.chat'), [dismiss, create_todo]).\n",
        encoding="utf-8",
    )
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "xdg"))
    engine = PrologEngine(REPO_ROOT / "main.pl")
    try:
        yield PrologNotificationPolicy(engine)
    finally:
        engine.query_once("config_loader:replace_user_config([])")


def _event() -> NotificationEvent:
    return NotificationEvent(
        notification_id="n:multi-hook",
        principal_id="user:alice",
        workspace_id="ws:main",
        source_peer="phone",
        owner_peer="phone",
        platform="android",
        source_key="android:pkg:multi-hook",
        generation=1,
        app="com.example.chat",
        created_at_ms=NOW - 1_000,
        expires_at_ms=NOW + 60_000,
        title="multi action",
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


def test_multi_action_prolog_hook_executes_each_typed_action_once_across_restart(
    tmp_path: Path,
    prolog_policy: PrologNotificationPolicy,
) -> None:
    db_path = tmp_path / "zara.db"
    db1 = DatabaseManager(db_path)
    router1 = NotificationRouter(
        local_peer_id="phone",
        policy=prolog_policy,
        store=NotificationRouterStore(db1),
    )
    event = _event()
    decision = router1.route(event, _peers(), now_ms=NOW)
    assert [(hook.hook_id, hook.kind, hook.argument) for hook in decision.hooks] == [
        ("multi_rule", "dismiss", None),
        ("multi_rule", "create_todo", None),
    ]

    plane = RecordingEffectPlane()
    first = router1.execute_hooks(event, decision, plane, now_ms=NOW + 1)

    assert len(first) == 2
    assert [action for _, action, _ in plane.executed] == ["dismiss", "create_todo"]
    assert len({request_id for request_id, _, _ in plane.executed}) == 2
    db1.close()

    db2 = DatabaseManager(db_path)
    router2 = NotificationRouter(
        local_peer_id="phone",
        policy=prolog_policy,
        store=NotificationRouterStore(db2),
    )
    replay = router2.route(event, _peers(), now_ms=NOW + 2)
    again = router2.execute_hooks(event, replay, plane, now_ms=NOW + 3)

    assert replay.duplicate is True
    assert again == ()
    assert [action for _, action, _ in plane.executed] == ["dismiss", "create_todo"]
    assert len(plane.verified) == 2
