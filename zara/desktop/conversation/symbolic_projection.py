"""Portable symbolic conversation-state projection over canonical history.

The projection lives in Zara's existing conversation SQLite database and is
keyed by the same conversation/principal identity. It is deliberately not a
second history store. Higher symbolic layers own the meaning of the JSON
payloads; this module owns persistence, monotonic usage accounting, stale
write fencing, and the bounded cross-platform dialogue/evidence references
required to rebuild natural symbolic conversation after restart.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field, replace
from typing import Any, Optional

_OUTCOMES = frozenset({"unknown", "pending", "success", "cancelled", "interrupted", "error"})
_TERMINAL_OUTCOMES = frozenset({"success", "cancelled", "interrupted", "error"})
_SYMBOLIC_RENDERER_ID = "symbolic-dcg/v1"
_DIALOGUE_ACT_RE = re.compile(r"^[a-z][a-z0-9_.-]{0,127}$")
_VERIFIED_OUTCOME_REF_RE = re.compile(
    r"^zara\.verified-outcome/v1:(?:effect|outcome):[A-Za-z0-9][A-Za-z0-9._:/#-]{0,383}$"
)


def _require_exact_integer(name: str, value: object, *, minimum: int = 0) -> int:
    """Reject bool/float coercions at persisted generation/accounting boundaries."""
    if type(value) is not int:
        raise TypeError(f"{name} must be an exact integer")
    if value < minimum:
        raise ValueError(f"{name} must be >= {minimum}")
    return value


def _require_exact_boolean(name: str, value: object) -> bool:
    if type(value) is not bool:
        raise TypeError(f"{name} must be an exact boolean")
    return value


def _decode_sqlite_integer(name: str, value: object, *, minimum: int = 0) -> int:
    """Require SQLite INTEGER storage instead of coercing REAL/TEXT numerics."""
    if type(value) is not int:
        raise ValueError(f"stored {name} must use SQLite integer storage")
    if value < minimum:
        raise ValueError(f"stored {name} must be >= {minimum}")
    return value


def _decode_sqlite_boolean(name: str, value: object) -> bool:
    if type(value) is not int or value not in (0, 1):
        raise ValueError(f"stored {name} must be SQLite integer 0 or 1")
    return bool(value)


def _canonical_object(value: dict[str, Any]) -> str:
    if not isinstance(value, dict):
        raise TypeError("symbolic object payload must be a dict")
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def _canonical_array(value: list[dict[str, Any]]) -> str:
    if not isinstance(value, list) or any(not isinstance(item, dict) for item in value):
        raise TypeError("symbolic array payload must be a list of dicts")
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def _decode_object(value: str) -> dict[str, Any]:
    decoded = json.loads(value)
    if not isinstance(decoded, dict):
        raise ValueError("stored symbolic object payload is not an object")
    return decoded


def _decode_array(value: str) -> list[dict[str, Any]]:
    decoded = json.loads(value)
    if not isinstance(decoded, list) or any(not isinstance(item, dict) for item in decoded):
        raise ValueError("stored symbolic array payload is not an array of objects")
    return decoded


def _validate_dialogue_act(value: str) -> str:
    if not isinstance(value, str) or _DIALOGUE_ACT_RE.fullmatch(value) is None:
        raise ValueError("dialogue_act must be a normalized symbolic act token")
    return value


def _validate_verified_outcome_refs(value: list[str]) -> list[str]:
    if not isinstance(value, list) or any(not isinstance(item, str) for item in value):
        raise TypeError("verified_outcome_refs must be a list of strings")
    if len(value) > 64:
        raise ValueError("verified_outcome_refs exceeds 64 entries")
    if len(set(value)) != len(value):
        raise ValueError("verified_outcome_refs must be unique")
    for item in value:
        if _VERIFIED_OUTCOME_REF_RE.fullmatch(item) is None:
            raise ValueError(f"invalid verified outcome reference: {item!r}")
    return value


def _encode_verified_outcome_refs(value: list[str]) -> str:
    _validate_verified_outcome_refs(value)
    return "\n".join(value)


def _decode_verified_outcome_refs(value: str) -> list[str]:
    if not isinstance(value, str):
        raise ValueError("stored verified outcome references are not text")
    refs = [] if value == "" else value.split("\n")
    try:
        return _validate_verified_outcome_refs(refs)
    except (TypeError, ValueError) as error:
        raise ValueError("stored verified outcome references are invalid") from error


@dataclass(frozen=True)
class SymbolicConversationProjection:
    conversation_id: str
    projection_generation: int
    runtime_generation: int
    turn_id: Optional[str] = None
    outcome: str = "unknown"
    project_id: Optional[str] = None
    project_generation: int = 0
    dialogue_act: str = "unknown"
    dialogue_state: dict[str, Any] = field(default_factory=dict)
    discourse_entities: list[dict[str, Any]] = field(default_factory=list)
    unresolved_questions: list[dict[str, Any]] = field(default_factory=list)
    expert_evidence: list[dict[str, Any]] = field(default_factory=list)
    verified_facts: list[dict[str, Any]] = field(default_factory=list)
    verified_outcome_refs: list[str] = field(default_factory=list)
    renderer_provenance: str = ""
    providers_enabled: bool = True
    max_model_calls: int = 1
    provider_calls: int = 0
    model_calls: int = 0
    updated_at: str = ""

    def validate(self) -> None:
        if not self.conversation_id:
            raise ValueError("conversation_id must not be empty")
        if self.turn_id is not None and (not self.turn_id or len(self.turn_id) > 512):
            raise ValueError("turn_id must be null or 1..512 characters")
        if self.outcome not in _OUTCOMES:
            raise ValueError(f"unsupported symbolic outcome: {self.outcome}")
        _require_exact_integer("projection_generation", self.projection_generation, minimum=1)
        _require_exact_integer("runtime_generation", self.runtime_generation)
        _require_exact_integer("project_generation", self.project_generation)
        _require_exact_boolean("providers_enabled", self.providers_enabled)
        _require_exact_integer("max_model_calls", self.max_model_calls)
        _require_exact_integer("provider_calls", self.provider_calls)
        _require_exact_integer("model_calls", self.model_calls)
        if self.project_id is not None and len(self.project_id) > 512:
            raise ValueError("project_id exceeds 512 characters")
        _validate_dialogue_act(self.dialogue_act)
        _validate_verified_outcome_refs(self.verified_outcome_refs)
        if self.renderer_provenance not in ("", _SYMBOLIC_RENDERER_ID):
            raise ValueError(
                "renderer_provenance must be empty or the canonical symbolic renderer"
            )
        if self.outcome == "success" and self.renderer_provenance != _SYMBOLIC_RENDERER_ID:
            raise ValueError("successful projection requires canonical symbolic renderer")
        _canonical_object(self.dialogue_state)
        _canonical_array(self.discourse_entities)
        _canonical_array(self.unresolved_questions)
        _canonical_array(self.expert_evidence)
        _canonical_array(self.verified_facts)

    def assert_pure_symbolic(self) -> None:
        providers_disabled = type(self.providers_enabled) is bool and not self.providers_enabled
        max_model_calls_exact_zero = type(self.max_model_calls) is int and self.max_model_calls == 0
        provider_exact_zero = type(self.provider_calls) is int and self.provider_calls == 0
        model_exact_zero = type(self.model_calls) is int and self.model_calls == 0
        if not providers_disabled:
            raise AssertionError("pure-symbolic conversation has providers enabled")
        if not max_model_calls_exact_zero:
            raise AssertionError(
                "pure-symbolic conversation recorded "
                f"max_model_calls={self.max_model_calls!r}"
            )
        if not provider_exact_zero or not model_exact_zero:
            raise AssertionError(
                "pure-symbolic conversation recorded "
                f"provider_calls={self.provider_calls!r}, model_calls={self.model_calls!r}"
            )
        if self.renderer_provenance not in ("", _SYMBOLIC_RENDERER_ID):
            raise AssertionError(
                "pure-symbolic conversation recorded non-symbolic renderer "
                f"{self.renderer_provenance!r}"
            )
        if self.outcome == "success" and self.renderer_provenance != _SYMBOLIC_RENDERER_ID:
            raise AssertionError("successful projection requires canonical symbolic renderer")


class SymbolicProjectionMixin:
    """Methods mixed into the canonical desktop ``ConversationStore``.

    The host store supplies ``database``, ``storage_principal_id``, and
    ``get_conversation``. Writes use compare-and-swap projection generations
    so cancelled or stale runtime completions cannot overwrite newer context.
    """

    def _ensure_symbolic_policy_columns(self) -> None:
        """Upgrade the legacy v3 projection in-place with fail-closed policy facts."""
        columns = {
            row["name"]
            for row in self.database.fetch_all("PRAGMA table_info(desktop_symbolic_projections)")
        }
        if not columns:
            return
        with self.database.transaction(immediate=True) as conn:
            if "providers_enabled" not in columns:
                conn.execute(
                    "ALTER TABLE desktop_symbolic_projections "
                    "ADD COLUMN providers_enabled INTEGER NOT NULL DEFAULT 1 "
                    "CHECK (typeof(providers_enabled) = 'integer' "
                    "AND providers_enabled IN (0, 1))"
                )
            if "max_model_calls" not in columns:
                conn.execute(
                    "ALTER TABLE desktop_symbolic_projections "
                    "ADD COLUMN max_model_calls INTEGER NOT NULL DEFAULT 1 "
                    "CHECK (typeof(max_model_calls) = 'integer' AND max_model_calls >= 0)"
                )

    def load_symbolic_projection(
        self,
        conversation_id: str,
    ) -> Optional[SymbolicConversationProjection]:
        if self.get_conversation(conversation_id) is None:
            return None
        self._ensure_symbolic_policy_columns()
        row = self.database.fetch_one(
            """
            SELECT * FROM desktop_symbolic_projections
            WHERE conversation_id = ? AND principal_id = ?
            """,
            (conversation_id, self.storage_principal_id),
        )
        if row is None:
            return None
        projection = SymbolicConversationProjection(
            conversation_id=row["conversation_id"],
            projection_generation=_decode_sqlite_integer(
                "projection_generation", row["projection_generation"], minimum=1
            ),
            runtime_generation=_decode_sqlite_integer(
                "runtime_generation", row["runtime_generation"]
            ),
            turn_id=row["turn_id"],
            outcome=row["outcome"],
            project_id=row["project_id"],
            project_generation=_decode_sqlite_integer(
                "project_generation", row["project_generation"]
            ),
            dialogue_act=row["dialogue_act"],
            dialogue_state=_decode_object(row["dialogue_state_json"]),
            discourse_entities=_decode_array(row["discourse_entities_json"]),
            unresolved_questions=_decode_array(row["unresolved_questions_json"]),
            expert_evidence=_decode_array(row["expert_evidence_json"]),
            verified_facts=_decode_array(row["verified_facts_json"]),
            verified_outcome_refs=_decode_verified_outcome_refs(row["verified_outcome_refs"]),
            renderer_provenance=row["renderer_provenance"],
            providers_enabled=_decode_sqlite_boolean("providers_enabled", row["providers_enabled"]),
            max_model_calls=_decode_sqlite_integer("max_model_calls", row["max_model_calls"]),
            provider_calls=_decode_sqlite_integer("provider_calls", row["provider_calls"]),
            model_calls=_decode_sqlite_integer("model_calls", row["model_calls"]),
            updated_at=row["updated_at"],
        )
        projection.validate()
        return projection

    def save_symbolic_projection(
        self,
        projection: SymbolicConversationProjection,
        *,
        expected_generation: int,
    ) -> SymbolicConversationProjection:
        projection.validate()
        _require_exact_integer("expected_generation", expected_generation)
        if projection.projection_generation != expected_generation + 1:
            raise ValueError(
                "projection_generation must equal expected_generation + 1"
            )
        if self.get_conversation(projection.conversation_id) is None:
            raise KeyError(projection.conversation_id)

        self._ensure_symbolic_policy_columns()
        owner = self.storage_principal_id
        with self.database.transaction(immediate=True) as conn:
            current = conn.execute(
                """
                SELECT projection_generation, runtime_generation, turn_id,
                       outcome, project_id, project_generation,
                       providers_enabled, max_model_calls,
                       provider_calls, model_calls
                FROM desktop_symbolic_projections
                WHERE conversation_id = ? AND principal_id = ?
                """,
                (projection.conversation_id, owner),
            ).fetchone()

            if current is None:
                if expected_generation != 0:
                    raise RuntimeError(
                        "stale symbolic projection write: projection does not exist"
                    )
            else:
                current_generation = _decode_sqlite_integer(
                    "projection_generation", current["projection_generation"], minimum=1
                )
                current_runtime_generation = _decode_sqlite_integer(
                    "runtime_generation", current["runtime_generation"]
                )
                current_turn_id = current["turn_id"]
                current_outcome = current["outcome"]
                if current_generation != expected_generation:
                    raise RuntimeError(
                        "stale symbolic projection write: "
                        f"expected generation {expected_generation}, current {current_generation}"
                    )
                if projection.runtime_generation < current_runtime_generation:
                    raise RuntimeError("runtime_generation regression rejected")
                if current_turn_id is not None and projection.turn_id is None:
                    raise RuntimeError("turn_id rewind rejected")
                if projection.turn_id == current_turn_id:
                    if current_turn_id is not None and projection.runtime_generation != current_runtime_generation:
                        raise RuntimeError("same turn must preserve runtime_generation")
                    if current_outcome in _TERMINAL_OUTCOMES:
                        raise RuntimeError("terminal turn projection is immutable")
                elif projection.runtime_generation <= current_runtime_generation:
                    raise RuntimeError("new turn must advance runtime_generation")
                current_providers_enabled = _decode_sqlite_boolean(
                    "providers_enabled", current["providers_enabled"]
                )
                current_max_model_calls = _decode_sqlite_integer(
                    "max_model_calls", current["max_model_calls"]
                )
                current_provider_calls = _decode_sqlite_integer(
                    "provider_calls", current["provider_calls"]
                )
                current_model_calls = _decode_sqlite_integer(
                    "model_calls", current["model_calls"]
                )
                if not current_providers_enabled and projection.providers_enabled:
                    raise RuntimeError("provider policy widening rejected")
                if projection.max_model_calls > current_max_model_calls:
                    raise RuntimeError("model-call budget widening rejected")
                if projection.provider_calls < current_provider_calls:
                    raise RuntimeError("provider-call ledger rewind rejected")
                if projection.model_calls < current_model_calls:
                    raise RuntimeError("model-call ledger rewind rejected")
                current_project_id = current["project_id"]
                current_project_generation = _decode_sqlite_integer(
                    "project_generation", current["project_generation"]
                )
                if projection.project_id == current_project_id:
                    if projection.project_generation < current_project_generation:
                        raise RuntimeError("project_generation regression rejected")
                elif projection.project_generation <= current_project_generation:
                    raise RuntimeError(
                        "project switch must advance project_generation"
                    )

            from .store import _now_iso

            stored = replace(projection, updated_at=_now_iso())
            parameters = (
                stored.conversation_id,
                owner,
                stored.turn_id,
                stored.outcome,
                stored.projection_generation,
                stored.runtime_generation,
                stored.project_id,
                stored.project_generation,
                stored.dialogue_act,
                _canonical_object(stored.dialogue_state),
                _canonical_array(stored.discourse_entities),
                _canonical_array(stored.unresolved_questions),
                _canonical_array(stored.expert_evidence),
                _canonical_array(stored.verified_facts),
                _encode_verified_outcome_refs(stored.verified_outcome_refs),
                stored.renderer_provenance,
                int(stored.providers_enabled),
                stored.max_model_calls,
                stored.provider_calls,
                stored.model_calls,
                stored.updated_at,
            )
            if current is None:
                conn.execute(
                    """
                    INSERT INTO desktop_symbolic_projections (
                        conversation_id, principal_id, turn_id, outcome,
                        projection_generation, runtime_generation, project_id,
                        project_generation, dialogue_act, dialogue_state_json,
                        discourse_entities_json, unresolved_questions_json,
                        expert_evidence_json, verified_facts_json,
                        verified_outcome_refs, renderer_provenance,
                        providers_enabled, max_model_calls,
                        provider_calls, model_calls, updated_at
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    parameters,
                )
            else:
                cursor = conn.execute(
                    """
                    UPDATE desktop_symbolic_projections
                    SET turn_id = ?, outcome = ?, projection_generation = ?,
                        runtime_generation = ?, project_id = ?,
                        project_generation = ?, dialogue_act = ?, dialogue_state_json = ?,
                        discourse_entities_json = ?, unresolved_questions_json = ?,
                        expert_evidence_json = ?, verified_facts_json = ?,
                        verified_outcome_refs = ?, renderer_provenance = ?,
                        providers_enabled = ?, max_model_calls = ?,
                        provider_calls = ?, model_calls = ?, updated_at = ?
                    WHERE conversation_id = ? AND principal_id = ?
                      AND projection_generation = ?
                    """,
                    (
                        stored.turn_id,
                        stored.outcome,
                        stored.projection_generation,
                        stored.runtime_generation,
                        stored.project_id,
                        stored.project_generation,
                        stored.dialogue_act,
                        _canonical_object(stored.dialogue_state),
                        _canonical_array(stored.discourse_entities),
                        _canonical_array(stored.unresolved_questions),
                        _canonical_array(stored.expert_evidence),
                        _canonical_array(stored.verified_facts),
                        _encode_verified_outcome_refs(stored.verified_outcome_refs),
                        stored.renderer_provenance,
                        int(stored.providers_enabled),
                        stored.max_model_calls,
                        stored.provider_calls,
                        stored.model_calls,
                        stored.updated_at,
                        stored.conversation_id,
                        owner,
                        expected_generation,
                    ),
                )
                if cursor.rowcount != 1:
                    raise RuntimeError("stale symbolic projection write rejected")
        return stored


__all__ = ["SymbolicConversationProjection", "SymbolicProjectionMixin"]
