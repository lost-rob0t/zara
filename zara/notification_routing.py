"""First-class symbolic notification routing for Zara.

The router is a policy/coordination layer over existing platform notification
adapters and the shared Zara SQLite database. It does not own a listener,
transport, scheduler, expert registry, provider runtime, or effect plane.
"""

from __future__ import annotations

import hashlib
import json
import re
import time
from dataclasses import asdict, dataclass, field
from typing import Any, Mapping, Optional, Protocol, Sequence

from .database import DatabaseManager, get_database
from .prolog_engine import PrologEngine

_MAX_TEXT = 8192
_MAX_ID = 192
_MAX_CHAIN = 8
_MAX_TTL_MS = 24 * 60 * 60 * 1000
_ALLOWED_PLATFORMS = frozenset({"android", "linux", "wear"})
_ALLOWED_DECISIONS = frozenset({"allow", "suppress", "group", "digest", "ask"})
_ALLOWED_CONTENT_MODES = frozenset({"metadata_only", "full_content"})
_ALLOWED_HOOKS = frozenset(
    {
        "dismiss",
        "open",
        "invoke_action",
        "inline_reply",
        "start_workflow",
        "open_link",
        "create_todo",
        "capture_note",
        "route_peer",
    }
)
_PORTABLE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/@+-]{0,191}$")


class NotificationRoutingError(RuntimeError):
    """Base notification routing failure."""


class NotificationDenied(NotificationRoutingError):
    """Notification or action was denied by policy/capability fences."""


class NotificationStale(NotificationRoutingError):
    """A stale generation tried to act after a newer route state existed."""


def _bounded_text(value: Optional[str], field_name: str, *, limit: int = _MAX_TEXT) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str):
        raise TypeError(f"{field_name} must be a string or None")
    if len(value.encode("utf-8")) > limit:
        raise ValueError(f"{field_name} exceeds {limit} UTF-8 bytes")
    return value


def _portable(value: str, field_name: str) -> str:
    if not isinstance(value, str) or not _PORTABLE.fullmatch(value):
        raise ValueError(f"invalid {field_name}: {value!r}")
    return value


def _exact_int(value: int, field_name: str, *, minimum: int = 0) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
        raise ValueError(f"{field_name} must be an integer >= {minimum}")
    return value


@dataclass(frozen=True)
class NotificationEvent:
    notification_id: str
    principal_id: str
    workspace_id: str
    source_peer: str
    owner_peer: str
    platform: str
    source_key: str
    generation: int
    app: str
    created_at_ms: int
    expires_at_ms: int
    title: Optional[str] = None
    body: Optional[str] = None
    category: str = "default"
    importance: str = "normal"
    origin_chain: tuple[str, ...] = ()
    action_handles: tuple[str, ...] = ()
    content_digest: Optional[str] = None
    provenance: Mapping[str, str] = field(default_factory=dict)

    def __post_init__(self) -> None:
        for name in (
            "notification_id",
            "principal_id",
            "workspace_id",
            "source_peer",
            "owner_peer",
            "source_key",
            "app",
            "category",
            "importance",
        ):
            _portable(getattr(self, name), name)
        if self.platform not in _ALLOWED_PLATFORMS:
            raise ValueError(f"unsupported notification platform: {self.platform!r}")
        _exact_int(self.generation, "generation", minimum=1)
        _exact_int(self.created_at_ms, "created_at_ms")
        _exact_int(self.expires_at_ms, "expires_at_ms")
        if self.expires_at_ms < self.created_at_ms:
            raise ValueError("notification expiry precedes creation")
        if self.expires_at_ms - self.created_at_ms > _MAX_TTL_MS:
            raise ValueError("notification TTL exceeds 24 hours")
        _bounded_text(self.title, "title", limit=2048)
        _bounded_text(self.body, "body")
        if len(self.origin_chain) > _MAX_CHAIN:
            raise ValueError("notification origin chain exceeds hop bound")
        for peer in self.origin_chain:
            _portable(peer, "origin_chain peer")
        if len(self.action_handles) > 32:
            raise ValueError("too many notification action handles")
        for handle in self.action_handles:
            _portable(handle, "action handle")
        if self.content_digest is not None:
            if not re.fullmatch(r"sha256:[0-9a-f]{64}", self.content_digest):
                raise ValueError("content_digest must be sha256:<64 lowercase hex>")
        if len(self.provenance) > 32:
            raise ValueError("notification provenance exceeds field bound")
        for key, value in self.provenance.items():
            _portable(key, "provenance key")
            _bounded_text(value, f"provenance[{key}]", limit=1024)


@dataclass(frozen=True)
class PeerActivity:
    peer_id: str
    principal_id: str
    workspace_id: str
    platform: str
    observed_at_ms: int
    expires_at_ms: int
    active: bool
    online: bool
    capabilities: frozenset[str] = frozenset()

    def __post_init__(self) -> None:
        _portable(self.peer_id, "peer_id")
        _portable(self.principal_id, "principal_id")
        _portable(self.workspace_id, "workspace_id")
        if self.platform not in _ALLOWED_PLATFORMS:
            raise ValueError(f"unsupported peer platform: {self.platform!r}")
        _exact_int(self.observed_at_ms, "observed_at_ms")
        _exact_int(self.expires_at_ms, "expires_at_ms")
        if not isinstance(self.active, bool) or not isinstance(self.online, bool):
            raise TypeError("peer active/online fields must be booleans")
        for capability in self.capabilities:
            _portable(capability, "capability")


@dataclass(frozen=True)
class TypedHookAction:
    hook_id: str
    kind: str
    argument: Optional[str] = None

    def __post_init__(self) -> None:
        _portable(self.hook_id, "hook_id")
        if self.kind not in _ALLOWED_HOOKS:
            raise ValueError(f"unsupported notification hook action: {self.kind!r}")
        if self.argument is not None:
            _portable(self.argument, "hook argument")


@dataclass(frozen=True)
class NotificationDecision:
    notification_id: str
    decision: str
    sinks: tuple[str, ...]
    presentation: Mapping[str, Any]
    evidence: tuple[str, ...]
    hooks: tuple[TypedHookAction, ...] = ()
    duplicate: bool = False
    coalesced_count: int = 1

    def __post_init__(self) -> None:
        _portable(self.notification_id, "notification_id")
        if self.decision not in _ALLOWED_DECISIONS:
            raise ValueError(f"unsupported notification decision: {self.decision!r}")
        if len(self.sinks) > 16:
            raise ValueError("too many notification sinks")
        if len(self.evidence) > 64:
            raise ValueError("too many notification evidence references")


@dataclass(frozen=True)
class NotificationActionRequest:
    request_id: str
    notification_id: str
    generation: int
    principal_id: str
    workspace_id: str
    sink_peer: str
    action: str
    argument: Optional[str] = None

    def __post_init__(self) -> None:
        for name in (
            "request_id",
            "notification_id",
            "principal_id",
            "workspace_id",
            "sink_peer",
        ):
            _portable(getattr(self, name), name)
        _exact_int(self.generation, "generation", minimum=1)
        if self.action not in _ALLOWED_HOOKS:
            raise ValueError(f"unsupported notification action: {self.action!r}")
        if self.argument is not None:
            _portable(self.argument, "action argument")


@dataclass(frozen=True)
class NotificationActionResult:
    request_id: str
    notification_id: str
    success: bool
    owner_peer: str
    receipt: Mapping[str, Any]
    verification: Mapping[str, Any]


class NotificationPolicy(Protocol):
    def source_decision(self, app: str) -> str: ...
    def content_mode(self, app: str) -> str: ...
    def route_policy(self, app: str) -> str: ...
    def filter_decision(self, app: str, category: str, importance: str) -> tuple[Optional[str], Optional[str]]: ...
    def spam_decision(self, app: str, count: int, duplicate: bool, feedback: str) -> tuple[str, str]: ...
    def hooks(self, app: str, category: str, importance: str) -> tuple[TypedHookAction, ...]: ...


class EffectPlane(Protocol):
    """Existing canonical capability/approval/action plane projected to this router."""

    def authorize(self, *, owner_peer: str, capability: str, principal_id: str, workspace_id: str) -> bool: ...
    def execute(self, request: NotificationActionRequest, *, owner_peer: str) -> Mapping[str, Any]: ...
    def verify(self, request: NotificationActionRequest, receipt: Mapping[str, Any], *, owner_peer: str) -> Mapping[str, Any]: ...


def _prolog_atom(value: str) -> str:
    _bounded_text(value, "Prolog atom", limit=1024)
    escaped = value.replace("\\", "\\\\").replace("'", "\\'")
    return f"'{escaped}'"


class PrologNotificationPolicy:
    """Read-only adapter over the canonical SWI-Prolog policy module."""

    def __init__(self, engine: PrologEngine) -> None:
        self._engine = engine

    def _one(self, goal: str, key: str) -> str:
        row = self._engine.query_once(goal)
        if row is None or key not in row:
            raise NotificationRoutingError(f"notification policy returned no {key}")
        return str(row[key])

    def source_decision(self, app: str) -> str:
        return self._one(
            f"kb_notification_policy:notification_source_decision({_prolog_atom(app)}, Decision)",
            "Decision",
        )

    def content_mode(self, app: str) -> str:
        mode = self._one(
            f"kb_notification_policy:notification_content_mode({_prolog_atom(app)}, Mode)",
            "Mode",
        )
        if mode not in _ALLOWED_CONTENT_MODES:
            raise NotificationRoutingError(f"invalid notification content mode: {mode!r}")
        return mode

    def route_policy(self, app: str) -> str:
        return self._one(
            f"kb_notification_policy:notification_route_target({_prolog_atom(app)}, Policy)",
            "Policy",
        )

    def filter_decision(self, app: str, category: str, importance: str) -> tuple[Optional[str], Optional[str]]:
        row = self._engine.query_once(
            "kb_notification_policy:notification_filter_decision("
            f"{_prolog_atom(app)}, {_prolog_atom(category)}, {_prolog_atom(importance)}, Decision, RuleId)"
        )
        if row is None:
            return None, None
        return str(row["Decision"]), str(row["RuleId"])

    def spam_decision(self, app: str, count: int, duplicate: bool, feedback: str) -> tuple[str, str]:
        row = self._engine.query_once(
            "kb_notification_policy:notification_spam_decision("
            f"{_prolog_atom(app)}, {int(count)}, {'true' if duplicate else 'false'}, "
            f"{_prolog_atom(feedback)}, Decision, Reason)"
        )
        if row is None:
            raise NotificationRoutingError("notification spam expert returned no decision")
        return str(row["Decision"]), str(row["Reason"])

    def hooks(self, app: str, category: str, importance: str) -> tuple[TypedHookAction, ...]:
        rows = self._engine.query_all(
            "kb_notification_policy:notification_hook_for("
            f"{_prolog_atom(app)}, {_prolog_atom(category)}, {_prolog_atom(importance)}, "
            "HookId, Kind, Arg)",
            max_solutions=32,
        )
        actions: list[TypedHookAction] = []
        for row in rows:
            argument = str(row["Arg"])
            actions.append(
                TypedHookAction(
                    hook_id=str(row["HookId"]),
                    kind=str(row["Kind"]),
                    argument=None if argument == "none" else argument,
                )
            )
        return tuple(actions)


class NotificationRouterStore:
    """Notification route state in Zara's existing shared SQLite database."""

    def __init__(self, db: Optional[DatabaseManager] = None) -> None:
        self._db = db or get_database()
        self._ensure_schema()

    def _ensure_schema(self) -> None:
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_seen (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                notification_id TEXT NOT NULL,
                generation INTEGER NOT NULL,
                app TEXT NOT NULL,
                owner_peer TEXT NOT NULL,
                content_digest TEXT NOT NULL,
                event_json TEXT NOT NULL,
                first_seen_ms INTEGER NOT NULL,
                last_seen_ms INTEGER NOT NULL,
                expires_at_ms INTEGER NOT NULL,
                count INTEGER NOT NULL,
                PRIMARY KEY (principal_id, workspace_id, notification_id)
            )
            """
        )
        self._db.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_notification_router_app_seen
            ON notification_router_seen(principal_id, workspace_id, app, last_seen_ms)
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_feedback (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                app TEXT NOT NULL,
                decision TEXT NOT NULL,
                updated_at_ms INTEGER NOT NULL,
                PRIMARY KEY (principal_id, workspace_id, app)
            )
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_presentations (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                notification_id TEXT NOT NULL,
                generation INTEGER NOT NULL,
                sink_peer TEXT NOT NULL,
                expires_at_ms INTEGER NOT NULL,
                PRIMARY KEY (
                    principal_id, workspace_id, notification_id, generation, sink_peer
                )
            )
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_decisions (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                notification_id TEXT NOT NULL,
                generation INTEGER NOT NULL,
                decision_json TEXT NOT NULL,
                expires_at_ms INTEGER NOT NULL,
                PRIMARY KEY (
                    principal_id, workspace_id, notification_id, generation
                )
            )
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_effect_claims (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                effect_key TEXT NOT NULL,
                receipt_json TEXT,
                claimed_at_ms INTEGER NOT NULL,
                PRIMARY KEY (principal_id, workspace_id, effect_key)
            )
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_effects (
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                effect_key TEXT NOT NULL,
                receipt_json TEXT NOT NULL,
                verification_json TEXT NOT NULL,
                completed_at_ms INTEGER NOT NULL,
                PRIMARY KEY (principal_id, workspace_id, effect_key)
            )
            """
        )
        self._db.execute(
            """
            CREATE TABLE IF NOT EXISTS notification_router_audit (
                seq INTEGER PRIMARY KEY AUTOINCREMENT,
                principal_id TEXT NOT NULL,
                workspace_id TEXT NOT NULL,
                notification_id TEXT NOT NULL,
                decision TEXT NOT NULL,
                evidence_json TEXT NOT NULL,
                created_at_ms INTEGER NOT NULL
            )
            """
        )

    def cleanup(self, now_ms: int) -> None:
        self._db.execute(
            "DELETE FROM notification_router_seen WHERE expires_at_ms < ?",
            (now_ms,),
        )
        self._db.execute(
            "DELETE FROM notification_router_presentations WHERE expires_at_ms < ?",
            (now_ms,),
        )
        self._db.execute(
            "DELETE FROM notification_router_decisions WHERE expires_at_ms < ?",
            (now_ms,),
        )

    def observe(self, event: NotificationEvent, digest: str, now_ms: int) -> tuple[bool, int, bool]:
        event_json = json.dumps(asdict(event), sort_keys=True, separators=(",", ":"))
        key = (event.principal_id, event.workspace_id, event.notification_id)
        with self._db.transaction(immediate=True) as conn:
            row = conn.execute(
                """
                SELECT generation, content_digest, event_json, count
                  FROM notification_router_seen
                 WHERE principal_id = ? AND workspace_id = ? AND notification_id = ?
                """,
                key,
            ).fetchone()
            if row is not None:
                stored_generation = int(row["generation"])
                if stored_generation > event.generation:
                    raise NotificationStale(
                        "notification generation is older than durable route state"
                    )
                if stored_generation == event.generation:
                    if (
                        str(row["content_digest"]) != digest
                        or str(row["event_json"]) != event_json
                    ):
                        raise NotificationStale(
                            "notification generation replay diverges from durable route state"
                        )
                    count = int(row["count"]) + 1
                    conn.execute(
                        """
                        UPDATE notification_router_seen
                           SET last_seen_ms=?, count=count+1
                         WHERE principal_id=? AND workspace_id=? AND notification_id=?
                           AND generation=?
                        """,
                        (now_ms, *key, event.generation),
                    )
                    return True, count, True

            duplicate = row is not None and str(row["content_digest"]) == digest
            count = int(row["count"]) + 1 if row is not None else 1
            if row is None:
                conn.execute(
                    """
                    INSERT INTO notification_router_seen (
                        principal_id, workspace_id, notification_id, generation, app,
                        owner_peer, content_digest, event_json, first_seen_ms,
                        last_seen_ms, expires_at_ms, count
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    (
                        event.principal_id,
                        event.workspace_id,
                        event.notification_id,
                        event.generation,
                        event.app,
                        event.owner_peer,
                        digest,
                        event_json,
                        now_ms,
                        now_ms,
                        event.expires_at_ms,
                        count,
                    ),
                )
            else:
                updated = conn.execute(
                    """
                    UPDATE notification_router_seen
                       SET generation=?,
                           app=?,
                           owner_peer=?,
                           content_digest=?,
                           event_json=?,
                           last_seen_ms=?,
                           expires_at_ms=?,
                           count=count+1
                     WHERE principal_id=? AND workspace_id=? AND notification_id=?
                       AND generation < ?
                    """,
                    (
                        event.generation,
                        event.app,
                        event.owner_peer,
                        digest,
                        event_json,
                        now_ms,
                        event.expires_at_ms,
                        event.principal_id,
                        event.workspace_id,
                        event.notification_id,
                        event.generation,
                    ),
                )
                if updated.rowcount != 1:
                    raise NotificationStale(
                        "notification generation lost durable admission authority"
                    )
            return duplicate, count, False

    def recent_app_count(self, event: NotificationEvent, now_ms: int, window_ms: int = 60_000) -> int:
        row = self._db.fetch_one(
            """
            SELECT COALESCE(SUM(count), 0) AS total
              FROM notification_router_seen
             WHERE principal_id = ? AND workspace_id = ? AND app = ? AND last_seen_ms >= ?
            """,
            (event.principal_id, event.workspace_id, event.app, now_ms - window_ms),
        )
        return int(row["total"]) if row is not None else 0

    def feedback(self, event: NotificationEvent) -> str:
        row = self._db.fetch_one(
            """
            SELECT decision FROM notification_router_feedback
             WHERE principal_id = ? AND workspace_id = ? AND app = ?
            """,
            (event.principal_id, event.workspace_id, event.app),
        )
        return str(row["decision"]) if row is not None else "none"

    def set_feedback(self, *, principal_id: str, workspace_id: str, app: str, decision: str, now_ms: Optional[int] = None) -> None:
        for value, name in ((principal_id, "principal_id"), (workspace_id, "workspace_id"), (app, "app")):
            _portable(value, name)
        if decision not in {"always_allow", "mute", "digest", "default"}:
            raise ValueError("unsupported notification feedback decision")
        stamp = int(time.time() * 1000) if now_ms is None else _exact_int(now_ms, "now_ms")
        if decision == "default":
            self._db.execute(
                "DELETE FROM notification_router_feedback WHERE principal_id=? AND workspace_id=? AND app=?",
                (principal_id, workspace_id, app),
            )
            return
        self._db.execute(
            """
            INSERT INTO notification_router_feedback(principal_id,workspace_id,app,decision,updated_at_ms)
            VALUES(?,?,?,?,?)
            ON CONFLICT(principal_id,workspace_id,app) DO UPDATE SET
                decision=excluded.decision, updated_at_ms=excluded.updated_at_ms
            """,
            (principal_id, workspace_id, app, decision, stamp),
        )

    def event(self, request: NotificationActionRequest) -> NotificationEvent:
        row = self._db.fetch_one(
            """
            SELECT event_json, generation FROM notification_router_seen
             WHERE principal_id=? AND workspace_id=? AND notification_id=?
            """,
            (request.principal_id, request.workspace_id, request.notification_id),
        )
        if row is None:
            raise NotificationDenied("notification action has no durable routed source state")
        if int(row["generation"]) != request.generation:
            raise NotificationStale("notification action generation no longer matches durable source state")
        raw = json.loads(str(row["event_json"]))
        raw["origin_chain"] = tuple(raw.get("origin_chain", ()))
        raw["action_handles"] = tuple(raw.get("action_handles", ()))
        return NotificationEvent(**raw)

    def record_route_state(self, event: NotificationEvent, decision: NotificationDecision) -> None:
        event_json = json.dumps(asdict(event), sort_keys=True, separators=(",", ":"))
        decision_json = json.dumps(asdict(decision), sort_keys=True, separators=(",", ":"))
        with self._db.transaction(immediate=True) as conn:
            admitted = conn.execute(
                """
                SELECT generation, event_json
                  FROM notification_router_seen
                 WHERE principal_id=? AND workspace_id=? AND notification_id=?
                """,
                (
                    event.principal_id,
                    event.workspace_id,
                    event.notification_id,
                ),
            ).fetchone()
            if (
                admitted is None
                or int(admitted["generation"]) != event.generation
                or str(admitted["event_json"]) != event_json
            ):
                raise NotificationStale(
                    "notification route state no longer matches durable source authority"
                )

            existing = conn.execute(
                """
                SELECT decision_json FROM notification_router_decisions
                 WHERE principal_id=? AND workspace_id=? AND notification_id=? AND generation=?
                """,
                (
                    event.principal_id,
                    event.workspace_id,
                    event.notification_id,
                    event.generation,
                ),
            ).fetchone()
            if existing is not None and str(existing["decision_json"]) != decision_json:
                raise NotificationStale("notification generation already has a different durable decision")
            if existing is None:
                conn.execute(
                    """
                    INSERT INTO notification_router_decisions(
                        principal_id,workspace_id,notification_id,generation,decision_json,expires_at_ms
                    ) VALUES(?,?,?,?,?,?)
                    """,
                    (
                        event.principal_id,
                        event.workspace_id,
                        event.notification_id,
                        event.generation,
                        decision_json,
                        event.expires_at_ms,
                    ),
                )
            conn.execute(
                """
                DELETE FROM notification_router_presentations
                 WHERE principal_id=? AND workspace_id=? AND notification_id=?
                """,
                (event.principal_id, event.workspace_id, event.notification_id),
            )
            for sink in decision.sinks:
                conn.execute(
                    """
                    INSERT INTO notification_router_presentations(
                        principal_id,workspace_id,notification_id,generation,sink_peer,expires_at_ms
                    ) VALUES(?,?,?,?,?,?)
                    """,
                    (
                        event.principal_id,
                        event.workspace_id,
                        event.notification_id,
                        event.generation,
                        sink,
                        event.expires_at_ms,
                    ),
                )

    def replay_decision(self, event: NotificationEvent, *, coalesced_count: int) -> NotificationDecision:
        row = self._db.fetch_one(
            """
            SELECT decision_json FROM notification_router_decisions
             WHERE principal_id=? AND workspace_id=? AND notification_id=? AND generation=?
            """,
            (
                event.principal_id,
                event.workspace_id,
                event.notification_id,
                event.generation,
            ),
        )
        if row is None:
            raise NotificationRoutingError("exact notification replay has no durable decision state")
        raw = json.loads(str(row["decision_json"]))
        presentation = dict(raw.get("presentation", {}))
        for key in ("origin_chain", "action_handles"):
            if key in presentation:
                presentation[key] = tuple(presentation[key])
        hooks = tuple(TypedHookAction(**item) for item in raw.get("hooks", ()))
        return NotificationDecision(
            notification_id=str(raw["notification_id"]),
            decision=str(raw["decision"]),
            sinks=tuple(raw.get("sinks", ())),
            presentation=presentation,
            evidence=tuple(raw.get("evidence", ())),
            hooks=hooks,
            duplicate=True,
            coalesced_count=coalesced_count,
        )

    def record_presentations(self, event: NotificationEvent, sinks: Sequence[str]) -> None:
        with self._db.transaction(immediate=True) as conn:
            conn.execute(
                """
                DELETE FROM notification_router_presentations
                 WHERE principal_id=? AND workspace_id=? AND notification_id=?
                """,
                (event.principal_id, event.workspace_id, event.notification_id),
            )
            for sink in sinks:
                conn.execute(
                    """
                    INSERT INTO notification_router_presentations(
                        principal_id,workspace_id,notification_id,generation,sink_peer,expires_at_ms
                    ) VALUES(?,?,?,?,?,?)
                    """,
                    (
                        event.principal_id,
                        event.workspace_id,
                        event.notification_id,
                        event.generation,
                        sink,
                        event.expires_at_ms,
                    ),
                )

    def require_presented_sink(self, request: NotificationActionRequest, now_ms: int) -> None:
        row = self._db.fetch_one(
            """
            SELECT 1 FROM notification_router_presentations
             WHERE principal_id=? AND workspace_id=? AND notification_id=?
               AND generation=? AND sink_peer=? AND expires_at_ms>=?
            """,
            (
                request.principal_id,
                request.workspace_id,
                request.notification_id,
                request.generation,
                request.sink_peer,
                now_ms,
            ),
        )
        if row is None:
            raise NotificationDenied("notification action sink was not a routed sink")

    def effect_done(self, principal_id: str, workspace_id: str, effect_key: str) -> bool:
        return self._db.fetch_one(
            "SELECT 1 FROM notification_router_effects WHERE principal_id=? AND workspace_id=? AND effect_key=?",
            (principal_id, workspace_id, effect_key),
        ) is not None

    def claim_effect(
        self,
        *,
        principal_id: str,
        workspace_id: str,
        effect_key: str,
        now_ms: int,
    ) -> tuple[str, Optional[Mapping[str, Any]]]:
        with self._db.transaction(immediate=True) as conn:
            completed = conn.execute(
                """
                SELECT 1 FROM notification_router_effects
                 WHERE principal_id=? AND workspace_id=? AND effect_key=?
                """,
                (principal_id, workspace_id, effect_key),
            ).fetchone()
            if completed is not None:
                return "done", None
            row = conn.execute(
                """
                SELECT receipt_json FROM notification_router_effect_claims
                 WHERE principal_id=? AND workspace_id=? AND effect_key=?
                """,
                (principal_id, workspace_id, effect_key),
            ).fetchone()
            if row is not None:
                raw_receipt = row["receipt_json"]
                if raw_receipt is None:
                    return "in_progress", None
                return "verify", dict(json.loads(str(raw_receipt)))
            conn.execute(
                """
                INSERT INTO notification_router_effect_claims(
                    principal_id,workspace_id,effect_key,receipt_json,claimed_at_ms
                ) VALUES(?,?,?,NULL,?)
                """,
                (principal_id, workspace_id, effect_key, now_ms),
            )
            return "execute", None

    def attach_effect_receipt(
        self,
        *,
        principal_id: str,
        workspace_id: str,
        effect_key: str,
        receipt: Mapping[str, Any],
    ) -> None:
        encoded = json.dumps(dict(receipt), sort_keys=True, separators=(",", ":"))
        with self._db.transaction(immediate=True) as conn:
            cursor = conn.execute(
                """
                UPDATE notification_router_effect_claims
                   SET receipt_json=?
                 WHERE principal_id=? AND workspace_id=? AND effect_key=?
                   AND receipt_json IS NULL
                """,
                (encoded, principal_id, workspace_id, effect_key),
            )
            if cursor.rowcount == 1:
                return
            row = conn.execute(
                """
                SELECT receipt_json FROM notification_router_effect_claims
                 WHERE principal_id=? AND workspace_id=? AND effect_key=?
                """,
                (principal_id, workspace_id, effect_key),
            ).fetchone()
            if row is None:
                raise NotificationRoutingError("notification effect claim disappeared before receipt admission")
            if str(row["receipt_json"]) != encoded:
                raise NotificationDenied("notification effect claim already carries a different receipt")

    def record_effect(self, *, principal_id: str, workspace_id: str, effect_key: str, receipt: Mapping[str, Any], verification: Mapping[str, Any], now_ms: int) -> None:
        receipt_json = json.dumps(dict(receipt), sort_keys=True, separators=(",", ":"))
        verification_json = json.dumps(dict(verification), sort_keys=True, separators=(",", ":"))
        with self._db.transaction(immediate=True) as conn:
            conn.execute(
                """
                INSERT OR IGNORE INTO notification_router_effects(
                    principal_id,workspace_id,effect_key,receipt_json,verification_json,completed_at_ms
                ) VALUES(?,?,?,?,?,?)
                """,
                (
                    principal_id,
                    workspace_id,
                    effect_key,
                    receipt_json,
                    verification_json,
                    now_ms,
                ),
            )
            conn.execute(
                """
                DELETE FROM notification_router_effect_claims
                 WHERE principal_id=? AND workspace_id=? AND effect_key=?
                """,
                (principal_id, workspace_id, effect_key),
            )

    def audit(self, event: NotificationEvent, decision: str, evidence: Sequence[str], now_ms: int) -> None:
        self._db.execute(
            """
            INSERT INTO notification_router_audit(
                principal_id,workspace_id,notification_id,decision,evidence_json,created_at_ms
            ) VALUES(?,?,?,?,?,?)
            """,
            (
                event.principal_id,
                event.workspace_id,
                event.notification_id,
                decision,
                json.dumps(list(evidence), separators=(",", ":")),
                now_ms,
            ),
        )


class NotificationRouter:
    """Deterministic cross-device router with Prolog policy and canonical effects."""

    def __init__(
        self,
        *,
        local_peer_id: str,
        policy: NotificationPolicy,
        store: Optional[NotificationRouterStore] = None,
    ) -> None:
        self.local_peer_id = _portable(local_peer_id, "local_peer_id")
        self.policy = policy
        self.store = store or NotificationRouterStore()

    def route(
        self,
        event: NotificationEvent,
        peers: Sequence[PeerActivity],
        *,
        now_ms: Optional[int] = None,
    ) -> NotificationDecision:
        now = int(time.time() * 1000) if now_ms is None else _exact_int(now_ms, "now_ms")
        self.store.cleanup(now)
        evidence = [f"source:{event.source_peer}", f"generation:{event.generation}"]
        if event.expires_at_ms < now:
            return self._terminal(event, "suppress", (), {}, (*evidence, "expired"), now)
        if self.local_peer_id in event.origin_chain or len(event.origin_chain) >= _MAX_CHAIN:
            return self._terminal(event, "suppress", (), {}, (*evidence, "loop-fence"), now)
        if self.policy.source_decision(event.app) == "deny":
            return self._terminal(event, "suppress", (), {}, (*evidence, "source-denied"), now)

        content_mode = self.policy.content_mode(event.app)
        digest = event.content_digest or self._content_digest(event, include_body=content_mode == "full_content")
        duplicate, seen_count, exact_replay = self.store.observe(event, digest, now)
        if exact_replay:
            return self.store.replay_decision(event, coalesced_count=seen_count)
        recent_count = self.store.recent_app_count(event, now)
        feedback = self.store.feedback(event)

        filter_decision, filter_rule = self.policy.filter_decision(
            event.app, event.category, event.importance
        )
        if filter_decision is not None:
            decision = filter_decision
            reason = f"filter:{filter_rule}"
        else:
            decision, spam_reason = self.policy.spam_decision(
                event.app, recent_count, duplicate, feedback
            )
            reason = f"spam:{spam_reason}"
        if decision not in _ALLOWED_DECISIONS:
            raise NotificationRoutingError(f"policy emitted invalid decision: {decision!r}")

        evidence.extend((reason, f"recent-count:{recent_count}", f"content:{content_mode}"))
        hooks = self.policy.hooks(event.app, event.category, event.importance)
        if decision in {"suppress", "group", "digest"}:
            return self._terminal(
                event,
                decision,
                (),
                self._presentation(event, content_mode, origin_chain=(*event.origin_chain, self.local_peer_id)),
                tuple(evidence),
                now,
                hooks=hooks,
                duplicate=duplicate,
                coalesced_count=seen_count,
            )

        route_policy = self.policy.route_policy(event.app)
        sinks = self._select_sinks(event, peers, route_policy, now)
        if not sinks:
            return self._terminal(
                event,
                "suppress",
                (),
                {},
                (*evidence, f"route:{route_policy}", "no-eligible-sink"),
                now,
                duplicate=duplicate,
                coalesced_count=seen_count,
            )
        presentation = self._presentation(
            event,
            content_mode,
            origin_chain=(*event.origin_chain, self.local_peer_id),
        )
        return self._terminal(
            event,
            decision,
            sinks,
            presentation,
            (*evidence, f"route:{route_policy}"),
            now,
            hooks=hooks,
            duplicate=duplicate,
            coalesced_count=seen_count,
        )

    def execute_hooks(
        self,
        event: NotificationEvent,
        decision: NotificationDecision,
        effect_plane: EffectPlane,
        *,
        now_ms: Optional[int] = None,
    ) -> tuple[NotificationActionResult, ...]:
        now = int(time.time() * 1000) if now_ms is None else _exact_int(now_ms, "now_ms")
        durable_decision = self.store.replay_decision(
            event,
            coalesced_count=decision.coalesced_count,
        )
        if (
            decision.notification_id != durable_decision.notification_id
            or decision.decision != durable_decision.decision
            or decision.sinks != durable_decision.sinks
            or dict(decision.presentation) != dict(durable_decision.presentation)
            or decision.evidence != durable_decision.evidence
            or decision.hooks != durable_decision.hooks
        ):
            raise NotificationDenied("notification hook decision does not match durable route decision")
        hook_id_counts: dict[str, int] = {}
        for hook in durable_decision.hooks:
            hook_id_counts[hook.hook_id] = hook_id_counts.get(hook.hook_id, 0) + 1
        results: list[NotificationActionResult] = []
        for hook in durable_decision.hooks:
            if hook_id_counts[hook.hook_id] > 1:
                effect_key = self._hook_effect_key(event, hook)
                request_id = effect_key
            else:
                effect_key = f"hook:{event.notification_id}:{event.generation}:{hook.hook_id}"
                request_id = f"hook:{hook.hook_id}:{event.generation}"
            if self.store.effect_done(event.principal_id, event.workspace_id, effect_key):
                continue
            request = NotificationActionRequest(
                request_id=request_id,
                notification_id=event.notification_id,
                generation=event.generation,
                principal_id=event.principal_id,
                workspace_id=event.workspace_id,
                sink_peer=durable_decision.sinks[0] if durable_decision.sinks else self.local_peer_id,
                action=hook.kind,
                argument=hook.argument,
            )
            results.append(
                self._perform_effect(
                    request,
                    effect_plane,
                    effect_key=effect_key,
                    now_ms=now,
                    enforce_route_authority=False,
                )
            )
        return tuple(results)

    def perform_action(
        self,
        request: NotificationActionRequest,
        effect_plane: EffectPlane,
        *,
        now_ms: Optional[int] = None,
    ) -> NotificationActionResult:
        return self._perform_effect(
            request,
            effect_plane,
            effect_key=self._action_effect_key(request),
            now_ms=now_ms,
            enforce_route_authority=True,
        )

    def _perform_effect(
        self,
        request: NotificationActionRequest,
        effect_plane: EffectPlane,
        *,
        effect_key: str,
        now_ms: Optional[int],
        enforce_route_authority: bool,
    ) -> NotificationActionResult:
        now = int(time.time() * 1000) if now_ms is None else _exact_int(now_ms, "now_ms")
        event = self.store.event(request)
        if event.expires_at_ms <= now:
            raise NotificationDenied("notification action source expired")
        if enforce_route_authority:
            self.store.require_presented_sink(request, now)
            requested_handle = request.argument if request.argument is not None else request.action
            if requested_handle not in event.action_handles:
                raise NotificationDenied("notification action handle was not exposed by source")
        capability = f"notification.action.{request.action}"
        if not effect_plane.authorize(
            owner_peer=event.owner_peer,
            capability=capability,
            principal_id=request.principal_id,
            workspace_id=request.workspace_id,
        ):
            raise NotificationDenied(f"capability denied: {capability}")
        claim_state, stored_receipt = self.store.claim_effect(
            principal_id=request.principal_id,
            workspace_id=request.workspace_id,
            effect_key=effect_key,
            now_ms=now,
        )
        if claim_state == "done":
            raise NotificationDenied("notification effect already completed")
        if claim_state == "in_progress":
            raise NotificationDenied("notification effect is already in progress")
        if claim_state == "execute":
            receipt = dict(effect_plane.execute(request, owner_peer=event.owner_peer))
            self.store.attach_effect_receipt(
                principal_id=request.principal_id,
                workspace_id=request.workspace_id,
                effect_key=effect_key,
                receipt=receipt,
            )
        elif claim_state == "verify" and stored_receipt is not None:
            receipt = dict(stored_receipt)
        else:
            raise NotificationRoutingError(f"invalid notification effect claim state: {claim_state!r}")
        verification = dict(effect_plane.verify(request, receipt, owner_peer=event.owner_peer))
        receipt_id = receipt.get("receipt_id")
        fresh_postcondition = (
            verification.get("ok") is True
            and verification.get("generation") == request.generation
            and verification.get("observed_owner_peer") == event.owner_peer
            and isinstance(receipt_id, str)
            and bool(receipt_id)
            and verification.get("receipt_id") == receipt_id
        )
        if not fresh_postcondition:
            raise NotificationDenied("notification effect did not produce fresh verified postcondition evidence")
        self.store.record_effect(
            principal_id=request.principal_id,
            workspace_id=request.workspace_id,
            effect_key=effect_key,
            receipt=receipt,
            verification=verification,
            now_ms=now,
        )
        return NotificationActionResult(
            request_id=request.request_id,
            notification_id=request.notification_id,
            success=True,
            owner_peer=event.owner_peer,
            receipt=receipt,
            verification=verification,
        )

    @staticmethod
    def _hook_effect_key(event: NotificationEvent, hook: TypedHookAction) -> str:
        payload = {
            "principal_id": event.principal_id,
            "workspace_id": event.workspace_id,
            "notification_id": event.notification_id,
            "generation": event.generation,
            "hook_id": hook.hook_id,
            "kind": hook.kind,
            "argument": hook.argument,
        }
        encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
        return "hook:" + hashlib.sha256(encoded).hexdigest()

    @staticmethod
    def _action_effect_key(request: NotificationActionRequest) -> str:
        payload = {
            "principal_id": request.principal_id,
            "workspace_id": request.workspace_id,
            "notification_id": request.notification_id,
            "generation": request.generation,
            "action": request.action,
            "argument": request.argument,
        }
        encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
        return "action:" + hashlib.sha256(encoded).hexdigest()

    def _terminal(
        self,
        event: NotificationEvent,
        decision: str,
        sinks: Sequence[str],
        presentation: Mapping[str, Any],
        evidence: Sequence[str],
        now_ms: int,
        *,
        hooks: Sequence[TypedHookAction] = (),
        duplicate: bool = False,
        coalesced_count: int = 1,
        preserve_presentations: bool = False,
    ) -> NotificationDecision:
        result = NotificationDecision(
            notification_id=event.notification_id,
            decision=decision,
            sinks=tuple(sinks),
            presentation=dict(presentation),
            evidence=tuple(evidence),
            hooks=tuple(hooks),
            duplicate=duplicate,
            coalesced_count=coalesced_count,
        )
        if preserve_presentations:
            raise NotificationRoutingError("replay must use the durable notification decision")
        self.store.record_route_state(event, result)
        self.store.audit(event, decision, result.evidence, now_ms)
        return result

    @staticmethod
    def _content_digest(event: NotificationEvent, *, include_body: bool) -> str:
        payload = {
            "app": event.app,
            "category": event.category,
            "importance": event.importance,
            "title": event.title,
            "body": event.body if include_body else None,
        }
        encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
        return "sha256:" + hashlib.sha256(encoded).hexdigest()

    @staticmethod
    def _presentation(event: NotificationEvent, content_mode: str, *, origin_chain: Sequence[str]) -> Mapping[str, Any]:
        return {
            "notification_id": event.notification_id,
            "generation": event.generation,
            "source_peer": event.source_peer,
            "owner_peer": event.owner_peer,
            "app": event.app,
            "category": event.category,
            "importance": event.importance,
            "title": event.title,
            "body": event.body if content_mode == "full_content" else None,
            "origin_chain": tuple(origin_chain),
            "action_handles": event.action_handles,
        }

    @staticmethod
    def _select_sinks(
        event: NotificationEvent,
        peers: Sequence[PeerActivity],
        route_policy: str,
        now_ms: int,
    ) -> tuple[str, ...]:
        eligible = [
            peer
            for peer in peers
            if peer.principal_id == event.principal_id
            and peer.workspace_id == event.workspace_id
            and peer.online
            and peer.active
            and peer.expires_at_ms >= now_ms
            and "notification.present" in peer.capabilities
            and peer.peer_id not in event.origin_chain
        ]
        eligible.sort(key=lambda peer: (-peer.observed_at_ms, peer.peer_id))
        if route_policy == "mirror":
            return tuple(peer.peer_id for peer in eligible[:16])
        platform = {
            "phone_only": "android",
            "watch_only": "wear",
            "desktop_only": "linux",
        }.get(route_policy)
        if platform is not None:
            matches = [peer for peer in eligible if peer.platform == platform]
            return (matches[0].peer_id,) if matches else ()
        if route_policy == "prefer_desktop":
            for wanted in ("linux", "wear", "android"):
                matches = [peer for peer in eligible if peer.platform == wanted]
                if matches:
                    return (matches[0].peer_id,)
            return ()
        if route_policy != "most_recently_active":
            raise NotificationRoutingError(f"unsupported notification route policy: {route_policy!r}")
        return (eligible[0].peer_id,) if eligible else ()


__all__ = [
    "EffectPlane",
    "NotificationActionRequest",
    "NotificationActionResult",
    "NotificationDecision",
    "NotificationDenied",
    "NotificationEvent",
    "NotificationPolicy",
    "NotificationRouter",
    "NotificationRouterStore",
    "NotificationRoutingError",
    "NotificationStale",
    "PeerActivity",
    "PrologNotificationPolicy",
    "TypedHookAction",
]
