"""Durable expert output-schema truth regressions."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import json

from zara.database import DatabaseManager
from zara.experts import (
    ZARA_EXPERT_PROTOCOL,
    ExpertDescriptor,
    ExpertLimits,
    ExpertRegistry,
    ExpertErrorCode,
    ExpertVerdict,
)

TARGET_ID = "zara:expert/output-schema-durable"
PRINCIPAL = "user:output-schema-durable"
WORKSPACE = "workspace:output-schema-durable"
IDEMPOTENCY_KEY = "idempotency:output-schema-durable"


@dataclass
class OutputHandler:
    summary: Any
    calls: int = 0

    def __call__(self, *, expert_operation: str, **payload: Any) -> dict[str, Any]:
        assert expert_operation == "route.diagnose"
        assert payload == {"subject": "fixture"}
        self.calls += 1
        return {
            "verdict": "succeeded",
            "data": {"summary": self.summary},
            "evidence_refs": ["evidence:observed-output"],
            "usage": {"model_calls": 0},
            "effect_receipts": [{"effect_id": "effect:observed-output"}],
        }


def _descriptor() -> ExpertDescriptor:
    return ExpertDescriptor.from_wire(
        {
            "protocol": ZARA_EXPERT_PROTOCOL,
            "expert_id": TARGET_ID,
            "expert_version": "1.0.0",
            "package_namespace": "zara",
            "manifest_digest": "sha256:output-schema-durable-v1",
            "name": "Output Schema Durable Fixture",
            "description": "Fail-closed durable output-schema fixture.",
            "source_reference": "t/test_expert_output_schema_durable.py",
            "reasoning_kind": "symbolic",
            "operations": [
                {
                    "operation_id": "route.diagnose",
                    "input_schema": {
                        "fields": [
                            {"name": "subject", "type": "string", "required": True}
                        ]
                    },
                    "output_schema": {
                        "fields": [
                            {"name": "summary", "type": "string", "required": True}
                        ]
                    },
                }
            ],
            "applicability": {"keywords": ["output", "schema"]},
            "required_capabilities": [],
            "possible_effects": ["none"],
            "supported_engines": [],
            "supported_platforms": ["linux"],
            "fallback_policy": "fail_closed",
            "delegation_policy": "never",
            "resource_limits": {
                "max_output_bytes": 4096,
                "max_model_calls": 0,
            },
            "registry_generation": 1,
            "availability": "ready",
        }
    )


def _runtime(
    database: DatabaseManager,
    handler: OutputHandler,
) -> tuple[ExpertRegistry, Any]:
    registry = ExpertRegistry(database=database)
    registry.reload([(_descriptor(), handler)])
    handle, _receipt = registry.activate(PRINCIPAL, WORKSPACE, TARGET_ID)
    return registry, handle


def _invoke(registry: ExpertRegistry, handle: Any):
    return registry.invoke(
        handle,
        "route.diagnose",
        {"subject": "fixture"},
        limits=ExpertLimits(max_output_bytes=4096, max_model_calls=0),
        idempotency_key=IDEMPOTENCY_KEY,
        request_id="request:output-schema-durable",
    )


def test_schema_invalid_success_becomes_terminal_unknown_and_never_redispatches(
    tmp_path: Path,
) -> None:
    path = tmp_path / "output-schema-durable.db"
    database = DatabaseManager(path)
    invalid_handler = OutputHandler(summary=7)
    registry, handle = _runtime(database, invalid_handler)

    first = _invoke(registry, handle)

    assert first.verdict is ExpertVerdict.UNKNOWN
    assert first.error_code is ExpertErrorCode.INVALID_INPUT
    assert "output schema violation" in first.error_message
    assert "summary" in first.error_message
    assert first.data == {}
    assert first.evidence_refs == ()
    assert first.usage == {"model_calls": 0}
    assert first.effect_receipts == ({"effect_id": "effect:observed-output"},)
    assert invalid_handler.calls == 1

    row = database.fetch_one(
        """
        SELECT state, result_json
        FROM expert_idempotency_v1
        WHERE idempotency_key = ?
        """,
        (IDEMPOTENCY_KEY,),
    )
    assert row is not None
    assert row["state"] == "completed"
    database.close()

    restarted_database = DatabaseManager(path)
    replacement_handler = OutputHandler(summary="replacement must never run")
    restarted_registry, restarted_handle = _runtime(
        restarted_database,
        replacement_handler,
    )

    replay = _invoke(restarted_registry, restarted_handle)

    assert replay.replayed is True
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INVALID_INPUT
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 0}
    assert replay.effect_receipts == ({"effect_id": "effect:observed-output"},)
    assert replacement_handler.calls == 0
    restarted_database.close()


def test_legacy_schema_invalid_success_replay_is_downgraded_without_redispatch(
    tmp_path: Path,
) -> None:
    path = tmp_path / "legacy-output-schema-durable.db"
    database = DatabaseManager(path)
    seed_handler = OutputHandler(summary="valid")
    registry, handle = _runtime(database, seed_handler)

    seeded = _invoke(registry, handle)
    assert seeded.verdict is ExpertVerdict.SUCCEEDED
    assert seed_handler.calls == 1

    row = database.fetch_one(
        """
        SELECT result_json
        FROM expert_idempotency_v1
        WHERE idempotency_key = ?
        """,
        (IDEMPOTENCY_KEY,),
    )
    assert row is not None
    legacy = json.loads(row["result_json"])
    legacy["data"] = {"summary": 7}
    database.execute(
        """
        UPDATE expert_idempotency_v1
        SET result_json = ?
        WHERE idempotency_key = ?
        """,
        (
            json.dumps(
                legacy,
                sort_keys=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
            IDEMPOTENCY_KEY,
        ),
    )
    database.close()

    restarted_database = DatabaseManager(path)
    replacement_handler = OutputHandler(summary="replacement must never run")
    restarted_registry, restarted_handle = _runtime(
        restarted_database,
        replacement_handler,
    )

    replay = _invoke(restarted_registry, restarted_handle)

    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.INVALID_INPUT
    assert "output schema violation" in replay.error_message
    assert replay.data == {}
    assert replay.evidence_refs == ()
    assert replay.usage == {"model_calls": 0}
    assert replay.effect_receipts == ({"effect_id": "effect:observed-output"},)
    assert replacement_handler.calls == 0

    persisted = restarted_database.fetch_one(
        """
        SELECT result_json
        FROM expert_idempotency_v1
        WHERE idempotency_key = ?
        """,
        (IDEMPOTENCY_KEY,),
    )
    assert persisted is not None
    assert json.loads(persisted["result_json"])["verdict"] == "succeeded"
    restarted_database.close()
