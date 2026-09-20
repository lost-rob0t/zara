"""Portable symbolic conversation-state projection over canonical history.

The projection lives in Zara's existing conversation SQLite database and is
keyed by the same conversation/principal identity. It is deliberately not a
second history store. Higher symbolic layers own the meaning of the JSON
payloads; this module owns persistence, monotonic usage accounting, and stale
write fencing only.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field, replace
from typing import Any, Optional

_OUTCOMES = frozenset({"unknown", "pending", "success", "cancelled", "interrupted", "error"})
_TERMINAL_OUTCOMES = frozenset({"success", "cancelled", "interrupted", "error"})


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


@dataclass(frozen=True)
class SymbolicConversationProjection:
    conversation_id: str
    projection_generation: int
    runtime_generation: int
    turn_id: Optional[str] = None
    outcome: str = "unknown"
    project_id: Optional[str] = None
    project_generation: int = 0
    dialogue_state: dict[str, Any] = field(default_factory=dict)
    discourse_entities: list[dict[str, Any]] = field(default_factory=list)
    unresolved_questions: list[dict[str, Any]] = field(default_factory=list)
    expert_evidence: list[dict[str, Any]] = field(default_factory=list)
    verified_facts: list[dict[str, Any]] = field(default_factory=list)
    renderer_provenance: str = ""
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
        if self.projection_generation < 1:
            raise ValueError("projection_generation must be >= 1")
        if self.runtime_generation < 0:
            raise ValueError("runtime_generation must be >= 0")
        if self.project_generation < 0:
            raise ValueError("project_generation must be >= 0")
        if self.provider_calls < 0:
            raise ValueError("provider_calls must be >= 0")
        if self.model_calls < 0:
            raise ValueError("model_calls must be >= 0")
        if self.project_id is not None and len(self.project_id) > 512:
            raise ValueError("project_id exceeds 512 characters")
        if len(self.renderer_provenance) > 512:
            raise ValueError("renderer_provenance exceeds 512 characters")
        _canonical_object(self.dialogue_state)
        _canonical_array(self.discourse_entities)
        _canonical_array(self.unresolved_questions)
        _canonical_array(self.expert_evidence)
        _canonical_array(self.verified_facts)

    def assert_pure_symbolic(self) -> None:
        if self.provider_calls != 0 or self.model_calls != 0:
            raise AssertionError(
                "pure-symbolic conversation recorded "
                f"provider_calls={self.provider_calls}, model_calls={self.model_calls}"
            )


class SymbolicProjectionMixin:
    """Methods mixed into the canonical desktop ``ConversationStore``.

    The host store supplies ``database``, ``storage_principal_id``, and
    ``get_conversation``. Writes use compare-and-swap projection generations
    so cancelled or stale runtime completions cannot overwrite newer context.
    """

    def load_symbolic_projection(
        self,
        conversation_id: str,
    ) -> Optional[SymbolicConversationProjection]:
        if self.get_conversation(conversation_id) is None:
            return None
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
            projection_generation=int(row["projection_generation"]),
            runtime_generation=int(row["runtime_generation"]),
            turn_id=row["turn_id"],
            outcome=row["outcome"],
            project_id=row["project_id"],
            project_generation=int(row["project_generation"]),
            dialogue_state=_decode_object(row["dialogue_state_json"]),
            discourse_entities=_decode_array(row["discourse_entities_json"]),
            unresolved_questions=_decode_array(row["unresolved_questions_json"]),
            expert_evidence=_decode_array(row["expert_evidence_json"]),
            verified_facts=_decode_array(row["verified_facts_json"]),
            renderer_provenance=row["renderer_provenance"],
            provider_calls=int(row["provider_calls"]),
            model_calls=int(row["model_calls"]),
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
        if expected_generation < 0:
            raise ValueError("expected_generation must be >= 0")
        if projection.projection_generation != expected_generation + 1:
            raise ValueError(
                "projection_generation must equal expected_generation + 1"
            )
        if self.get_conversation(projection.conversation_id) is None:
            raise KeyError(projection.conversation_id)

        owner = self.storage_principal_id
        with self.database.transaction(immediate=True) as conn:
            current = conn.execute(
                """
                SELECT projection_generation, runtime_generation, turn_id,
                       outcome, project_id, project_generation,
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
                current_generation = int(current["projection_generation"])
                current_runtime_generation = int(current["runtime_generation"])
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
                    if current_outcome in _TERMINAL_OUTCOMES and projection.outcome != current_outcome:
                        raise RuntimeError(
                            "terminal turn outcome rewrite rejected: "
                            f"{current_outcome} -> {projection.outcome}"
                        )
                elif projection.runtime_generation <= current_runtime_generation:
                    raise RuntimeError("new turn must advance runtime_generation")
                if projection.provider_calls < int(current["provider_calls"]):
                    raise RuntimeError("provider-call ledger rewind rejected")
                if projection.model_calls < int(current["model_calls"]):
                    raise RuntimeError("model-call ledger rewind rejected")
                current_project_id = current["project_id"]
                current_project_generation = int(current["project_generation"])
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
                _canonical_object(stored.dialogue_state),
                _canonical_array(stored.discourse_entities),
                _canonical_array(stored.unresolved_questions),
                _canonical_array(stored.expert_evidence),
                _canonical_array(stored.verified_facts),
                stored.renderer_provenance,
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
                        project_generation, dialogue_state_json,
                        discourse_entities_json, unresolved_questions_json,
                        expert_evidence_json, verified_facts_json,
                        renderer_provenance, provider_calls, model_calls,
                        updated_at
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    parameters,
                )
            else:
                cursor = conn.execute(
                    """
                    UPDATE desktop_symbolic_projections
                    SET turn_id = ?, outcome = ?, projection_generation = ?,
                        runtime_generation = ?, project_id = ?,
                        project_generation = ?, dialogue_state_json = ?,
                        discourse_entities_json = ?, unresolved_questions_json = ?,
                        expert_evidence_json = ?, verified_facts_json = ?,
                        renderer_provenance = ?, provider_calls = ?, model_calls = ?,
                        updated_at = ?
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
                        _canonical_object(stored.dialogue_state),
                        _canonical_array(stored.discourse_entities),
                        _canonical_array(stored.unresolved_questions),
                        _canonical_array(stored.expert_evidence),
                        _canonical_array(stored.verified_facts),
                        stored.renderer_provenance,
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
