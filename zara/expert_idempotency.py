"""Durable ZARA-EXPERT/1 idempotency journal owned by the canonical registry.

This is not a second registry, scheduler, permission plane, or history store.  It is
an internal SQLite projection used exclusively by :class:`zara.experts.ExpertRegistry`
to keep one logical idempotency decision truthful across process recreation.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass, replace
from typing import Any, Optional

from ._experts_v1 import (
    ZARA_EXPERT_PROTOCOL,
    ActivationHandle,
    ExpertErrorCode,
    ExpertInvalidInputError,
    ExpertResult,
    ExpertVerdict,
    _PORTABLE,
    _bounded_error_message,
    _bounded_mapping,
    _bounded_pattern,
    _bounded_receipts,
    _bounded_refs,
)
from .database import DatabaseManager, get_database
from .protocol import _reject_constant, _strict_object

_TABLE = "expert_idempotency_v1"
_STATES = frozenset({"reserved", "dispatching", "completed", "interrupted", "corrupt"})


@dataclass(frozen=True)
class DurableIdempotencyClaim:
    principal: str
    workspace: str
    expert_id: str
    expert_operation: str
    idempotency_key: str
    input_digest: str

    @property
    def scope(self) -> tuple[str, str, str, str, str]:
        return (
            self.principal,
            self.workspace,
            self.expert_id,
            self.expert_operation,
            self.idempotency_key,
        )


@dataclass(frozen=True)
class ClaimDecision:
    claim: DurableIdempotencyClaim
    replay: Optional[ExpertResult]
    created: bool


class ExpertIdempotencyJournal:
    """Atomic durable claim/result projection for one canonical ExpertRegistry."""

    def __init__(self, database: Optional[DatabaseManager] = None) -> None:
        self._database = database
        self._schema_ready = False

    @property
    def database(self) -> DatabaseManager:
        if self._database is None:
            self._database = get_database()
        self._ensure_schema()
        return self._database

    def claim_or_replay(
        self,
        *,
        principal: str,
        workspace: str,
        expert_id: str,
        expert_operation: str,
        idempotency_key: str,
        input_digest: str,
        handle: ActivationHandle,
    ) -> ClaimDecision:
        claim = DurableIdempotencyClaim(
            principal=principal,
            workspace=workspace,
            expert_id=expert_id,
            expert_operation=expert_operation,
            idempotency_key=idempotency_key,
            input_digest=input_digest,
        )
        db = self.database
        with db.transaction(immediate=True) as conn:
            row = conn.execute(
                f"""
                SELECT * FROM {_TABLE}
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                """,
                claim.scope,
            ).fetchone()
            if row is None:
                conn.execute(
                    f"""
                    INSERT INTO {_TABLE} (
                        principal, workspace, expert_id, expert_operation,
                        idempotency_key, input_digest, expert_version,
                        manifest_digest, activation_id, registry_generation,
                        runtime_generation, state
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'reserved')
                    """,
                    (
                        *claim.scope,
                        input_digest,
                        handle.expert_version,
                        handle.manifest_digest,
                        handle.activation_id,
                        handle.registry_generation,
                        handle.runtime_generation,
                    ),
                )
                return ClaimDecision(claim=claim, replay=None, created=True)

            if row["input_digest"] != input_digest:
                raise ExpertInvalidInputError(
                    "idempotency conflict: idempotency key "
                    f"{idempotency_key!r} was reused with changed input"
                )

            build_changed = (
                row["expert_version"] != handle.expert_version
                or row["manifest_digest"] != handle.manifest_digest
            )
            generation_changed = (
                row["registry_generation"] != handle.registry_generation
                or row["runtime_generation"] != handle.runtime_generation
            )

            if row["state"] == "completed" and row["result_json"]:
                try:
                    result = self._decode_result(row["result_json"], row)
                except (TypeError, ValueError, KeyError, json.JSONDecodeError):
                    conn.execute(
                        f"""
                        UPDATE {_TABLE}
                        SET state = 'corrupt', updated_at = strftime('%s','now')
                        WHERE principal = ? AND workspace = ? AND expert_id = ?
                          AND expert_operation = ? AND idempotency_key = ?
                        """,
                        claim.scope,
                    )
                    replay = self._unknown_result(
                        row,
                        handle=handle,
                        message="durable idempotency result is corrupt or unverifiable",
                    )
                    return ClaimDecision(claim=claim, replay=replay, created=False)
                replay = replace(result, replayed=True)
                if build_changed and replay.verdict is ExpertVerdict.SUCCEEDED:
                    replay = self._stale_success(
                        replay,
                        message=(
                            "prior idempotent expert success belongs to a different "
                            "expert build"
                        ),
                    )
                    return ClaimDecision(claim=claim, replay=replay, created=False)
                if generation_changed and replay.verdict is ExpertVerdict.SUCCEEDED:
                    replay = self._stale_success(
                        replay,
                        message=(
                            "prior idempotent expert success belongs to a different "
                            "registry/runtime generation"
                        ),
                    )
                    return ClaimDecision(claim=claim, replay=replay, created=False)
                if (
                    replay.verdict is ExpertVerdict.SUCCEEDED
                    and replay.effect_receipts
                ):
                    replay = replace(
                        replay,
                        verdict=ExpertVerdict.UNKNOWN,
                        error_code=ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME,
                        error_message=(
                            "durable effectful expert result requires fresh postcondition "
                            "verification before replay success"
                        ),
                    )
                return ClaimDecision(
                    claim=claim,
                    replay=replay,
                    created=False,
                )

            replay = self._unknown_result(
                row,
                handle=handle,
                message=(
                    "prior idempotent expert execution belongs to a different expert build"
                    if build_changed
                    else (
                        "prior idempotent expert execution belongs to a different "
                        "registry/runtime generation"
                        if generation_changed
                        else "prior idempotent expert execution is not durably terminal"
                    )
                ),
            )
            return ClaimDecision(claim=claim, replay=replay, created=False)

    def mark_dispatching(
        self,
        claim: DurableIdempotencyClaim,
        *,
        invocation_id: str,
        request_id: str,
    ) -> None:
        db = self.database
        with db.transaction(immediate=True) as conn:
            cursor = conn.execute(
                f"""
                UPDATE {_TABLE}
                SET state = 'dispatching', invocation_id = ?, request_id = ?,
                    updated_at = strftime('%s','now')
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                  AND input_digest = ? AND state = 'reserved'
                """,
                (invocation_id, request_id, *claim.scope, claim.input_digest),
            )
            if cursor.rowcount != 1:
                raise ExpertInvalidInputError(
                    "durable idempotency claim lost before expert dispatch"
                )

    def commit(self, claim: DurableIdempotencyClaim, result: ExpertResult) -> None:
        serialization_error: Optional[ExpertInvalidInputError] = None
        db = self.database
        with db.transaction(immediate=True) as conn:
            row = conn.execute(
                f"""
                SELECT registry_generation, runtime_generation
                FROM {_TABLE}
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                  AND input_digest = ? AND state = 'dispatching'
                  AND invocation_id = ? AND request_id = ?
                """,
                (
                    *claim.scope,
                    claim.input_digest,
                    result.invocation_id,
                    result.request_id,
                ),
            ).fetchone()
            if row is None:
                raise ExpertInvalidInputError(
                    "durable idempotency terminal commit lost canonical dispatch identity"
                )

            # The durable row owns the generation identity admitted before dispatch.
            # A late completion may observe a newer live registry generation, but
            # replay must remain bound to the original reservation rather than
            # becoming corrupt (and thereby losing truthful usage/effect evidence).
            durable_result = replace(
                result,
                resolved_registry_generation=row["registry_generation"],
                resolved_runtime_generation=row["runtime_generation"],
            )
            try:
                payload = self._encode_result(durable_result)
            except ExpertInvalidInputError as error:
                # Handler work is already known to have completed.  Do not turn a
                # serialization rejection into an "interrupted" row that erases
                # known budget/effect accounting.  Persist only canonical fields
                # that are still trustworthy and fail closed to the live caller.
                model_calls = durable_result.usage.get("model_calls")
                if type(model_calls) is not int or model_calls < 0:
                    raise
                safe_receipts: list[dict[str, Any]] = []
                effect_identity_uncertain = False
                identity_fields = ("effect_id", "verified_outcome_ref")
                for receipt in durable_result.effect_receipts:
                    canonical_receipt = {
                        key: value
                        for key, value in receipt.items()
                        if type(key) is str
                        and self._is_canonical_json({key: value})
                    }
                    identity_is_preserved = all(
                        type(receipt.get(field)) is str
                        and bool(receipt.get(field))
                        and field in canonical_receipt
                        for field in identity_fields
                    )
                    if not identity_is_preserved:
                        effect_identity_uncertain = True
                        continue
                    try:
                        safe_receipts.extend(_bounded_receipts((canonical_receipt,)))
                    except ExpertInvalidInputError:
                        effect_identity_uncertain = True
                        continue
                durable_result = replace(
                    durable_result,
                    verdict=ExpertVerdict.UNKNOWN,
                    data={},
                    evidence_refs=(),
                    usage={"model_calls": model_calls},
                    effect_receipts=tuple(safe_receipts),
                    error_code=(
                        ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
                        if effect_identity_uncertain
                        else ExpertErrorCode.INVALID_INPUT
                    ),
                    error_message=(
                        "completed expert result has effect accounting without a "
                        "durably canonical identity tuple; prior external outcome is unknown"
                        if effect_identity_uncertain
                        else (
                            "completed expert result was not canonical JSON; "
                            "known accounting was preserved fail-closed"
                        )
                    ),
                )
                payload = self._encode_result(durable_result)
                serialization_error = error

            cursor = conn.execute(
                f"""
                UPDATE {_TABLE}
                SET state = 'completed', result_json = ?,
                    updated_at = strftime('%s','now')
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                  AND input_digest = ? AND state = 'dispatching'
                  AND invocation_id = ? AND request_id = ?
                """,
                (
                    payload,
                    *claim.scope,
                    claim.input_digest,
                    result.invocation_id,
                    result.request_id,
                ),
            )
            if cursor.rowcount != 1:
                raise ExpertInvalidInputError(
                    "durable idempotency terminal commit lost canonical dispatch identity"
                )

        if serialization_error is not None:
            raise ExpertInvalidInputError(
                "durable expert result is not canonical JSON data; "
                "stored fail-closed terminal with known accounting"
            ) from serialization_error

    def interrupt(self, claim: DurableIdempotencyClaim) -> None:
        db = self.database
        with db.transaction(immediate=True) as conn:
            conn.execute(
                f"""
                UPDATE {_TABLE}
                SET state = 'interrupted', result_json = NULL,
                    updated_at = strftime('%s','now')
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                  AND input_digest = ? AND state != 'completed'
                """,
                (*claim.scope, claim.input_digest),
            )

    def release_unstarted(self, claim: DurableIdempotencyClaim) -> None:
        db = self.database
        with db.transaction(immediate=True) as conn:
            conn.execute(
                f"""
                DELETE FROM {_TABLE}
                WHERE principal = ? AND workspace = ? AND expert_id = ?
                  AND expert_operation = ? AND idempotency_key = ?
                  AND input_digest = ? AND state = 'reserved'
                """,
                (*claim.scope, claim.input_digest),
            )

    def _ensure_schema(self) -> None:
        if self._schema_ready:
            return
        if self._database is None:
            return
        self._database.execute(
            f"""
            CREATE TABLE IF NOT EXISTS {_TABLE} (
                principal TEXT NOT NULL,
                workspace TEXT NOT NULL,
                expert_id TEXT NOT NULL,
                expert_operation TEXT NOT NULL,
                idempotency_key TEXT NOT NULL,
                input_digest TEXT NOT NULL,
                expert_version TEXT NOT NULL,
                manifest_digest TEXT NOT NULL,
                activation_id TEXT NOT NULL,
                registry_generation INTEGER NOT NULL,
                runtime_generation INTEGER NOT NULL,
                state TEXT NOT NULL,
                invocation_id TEXT,
                request_id TEXT,
                result_json TEXT,
                updated_at INTEGER NOT NULL DEFAULT (strftime('%s','now')),
                PRIMARY KEY (
                    principal, workspace, expert_id, expert_operation, idempotency_key
                )
            )
            """
        )
        self._schema_ready = True

    @staticmethod
    def _is_canonical_json(value: Any) -> bool:
        try:
            json.dumps(
                value,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            )
        except (TypeError, ValueError):
            return False
        return True

    @staticmethod
    def _encode_result(result: ExpertResult) -> str:
        payload = {
            "protocol": result.protocol,
            "request_id": result.request_id,
            "invocation_id": result.invocation_id,
            "activation_id": result.activation_id,
            "expert_id": result.expert_id,
            "expert_version": result.expert_version,
            "manifest_digest": result.manifest_digest,
            "expert_operation": result.expert_operation,
            "resolved_registry_generation": result.resolved_registry_generation,
            "resolved_runtime_generation": result.resolved_runtime_generation,
            "verdict": result.verdict.value,
            "data": result.data,
            "evidence_refs": list(result.evidence_refs),
            "usage": result.usage,
            "effect_receipts": list(result.effect_receipts),
            "error_code": result.error_code.value if result.error_code else None,
            "error_message": result.error_message,
        }
        try:
            return json.dumps(
                payload,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            )
        except (TypeError, ValueError) as error:
            raise ExpertInvalidInputError(
                f"durable expert result is not canonical JSON data: {error}"
            ) from error

    @staticmethod
    def _decode_result(payload: str, row: Any) -> ExpertResult:
        # Durable replay uses the same recursive strict-JSON policy as ZARA/1:
        # duplicate object members and non-finite constants are ambiguous and fail closed.
        wire = json.loads(
            payload,
            object_pairs_hook=_strict_object,
            parse_constant=_reject_constant,
        )
        if not isinstance(wire, dict):
            raise ValueError("durable expert result must be an object")
        required = {
            "protocol",
            "request_id",
            "invocation_id",
            "activation_id",
            "expert_id",
            "expert_version",
            "manifest_digest",
            "expert_operation",
            "resolved_registry_generation",
            "resolved_runtime_generation",
            "verdict",
            "data",
            "evidence_refs",
            "usage",
            "effect_receipts",
            "error_code",
            "error_message",
        }
        if set(wire) != required:
            raise ValueError("durable expert result has an unexpected shape")
        if wire["protocol"] != ZARA_EXPERT_PROTOCOL:
            raise ValueError("durable expert result protocol mismatch")
        for key in ("request_id", "invocation_id", "expert_id", "expert_operation"):
            _bounded_pattern(
                wire[key],
                field_name=key,
                pattern=_PORTABLE,
                limit=128,
            )
        historical_handle = ActivationHandle(
            activation_id=wire["activation_id"],
            principal=row["principal"],
            workspace=row["workspace"],
            expert_id=wire["expert_id"],
            expert_version=wire["expert_version"],
            manifest_digest=wire["manifest_digest"],
            registry_generation=wire["resolved_registry_generation"],
            runtime_generation=wire["resolved_runtime_generation"],
        )
        del historical_handle
        for key in ("resolved_registry_generation", "resolved_runtime_generation"):
            value = wire[key]
            if type(value) is not int or value < 0:
                raise ValueError(f"durable expert result {key} is invalid")
        for key, column in (
            ("request_id", "request_id"),
            ("invocation_id", "invocation_id"),
            ("activation_id", "activation_id"),
            ("expert_id", "expert_id"),
            ("expert_version", "expert_version"),
            ("manifest_digest", "manifest_digest"),
            ("expert_operation", "expert_operation"),
            ("resolved_registry_generation", "registry_generation"),
            ("resolved_runtime_generation", "runtime_generation"),
        ):
            if wire[key] != row[column]:
                raise ValueError(f"durable expert result {key} mismatch")
        data = _bounded_mapping(wire["data"], "data")
        usage = _bounded_mapping(wire["usage"], "usage")
        evidence_refs = _bounded_refs(wire["evidence_refs"])
        effect_receipts = _bounded_receipts(wire["effect_receipts"])
        model_calls = usage.get("model_calls")
        if type(model_calls) is not int or model_calls < 0:
            raise ValueError("durable expert result usage is invalid")
        error_code = wire["error_code"]
        error_message = _bounded_error_message(wire["error_message"])
        return ExpertResult(
            protocol=wire["protocol"],
            request_id=wire["request_id"],
            invocation_id=wire["invocation_id"],
            activation_id=wire["activation_id"],
            expert_id=wire["expert_id"],
            expert_version=wire["expert_version"],
            manifest_digest=wire["manifest_digest"],
            expert_operation=wire["expert_operation"],
            resolved_registry_generation=wire["resolved_registry_generation"],
            resolved_runtime_generation=wire["resolved_runtime_generation"],
            verdict=ExpertVerdict(wire["verdict"]),
            data=data,
            evidence_refs=evidence_refs,
            usage=usage,
            effect_receipts=effect_receipts,
            error_code=ExpertErrorCode(error_code) if error_code is not None else None,
            error_message=error_message,
            replayed=False,
        )

    @staticmethod
    def _stale_success(result: ExpertResult, *, message: str) -> ExpertResult:
        return replace(
            result,
            verdict=ExpertVerdict.UNKNOWN,
            error_code=ExpertErrorCode.INTERRUPTED,
            error_message=message,
            replayed=True,
        )

    @staticmethod
    def _unknown_result(row: Any, *, handle: ActivationHandle, message: str) -> ExpertResult:
        invocation_id = _safe_replay_id(
            "inv", "invocation_id", row["invocation_id"], row
        )
        request_id = _safe_replay_id("req", "request_id", row["request_id"], row)
        return ExpertResult(
            protocol=ZARA_EXPERT_PROTOCOL,
            request_id=request_id,
            invocation_id=invocation_id,
            activation_id=handle.activation_id,
            expert_id=handle.expert_id,
            expert_version=handle.expert_version,
            manifest_digest=handle.manifest_digest,
            expert_operation=row["expert_operation"],
            resolved_registry_generation=handle.registry_generation,
            resolved_runtime_generation=handle.runtime_generation,
            verdict=ExpertVerdict.UNKNOWN,
            data={},
            evidence_refs=(),
            usage={"model_calls": 0},
            effect_receipts=(),
            error_code=ExpertErrorCode.INTERRUPTED,
            error_message=message,
            replayed=True,
        )


def _safe_replay_id(prefix: str, field_name: str, value: Any, row: Any) -> str:
    try:
        return _bounded_pattern(
            value,
            field_name=field_name,
            pattern=_PORTABLE,
            limit=128,
        )
    except (TypeError, ValueError):
        return _synthetic_id(prefix, row)


def _synthetic_id(prefix: str, row: Any) -> str:
    payload = "\x1f".join(
        str(row[name])
        for name in (
            "principal",
            "workspace",
            "expert_id",
            "expert_operation",
            "idempotency_key",
            "input_digest",
        )
    )
    return f"{prefix}:{hashlib.sha256(payload.encode('utf-8')).hexdigest()[:32]}"
