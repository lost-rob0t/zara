"""Correlated durable identity corruption must fail closed across restart (#1449)."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest

from t.test_expert_idempotency_restart_bounds import DispatchCounter, _request, _runtime
from zara.database import DatabaseManager
from zara.experts import ExpertErrorCode, ExpertVerdict


@pytest.mark.parametrize(
    ("column", "wire_key", "corrupt_value"),
    [
        ("request_id", "request_id", "r" * 129),
        ("invocation_id", "invocation_id", "i" * 129),
        ("activation_id", "activation_id", "invalid activation id"),
        ("expert_version", "expert_version", "v" * 65),
        ("manifest_digest", "manifest_digest", "m" * 193),
    ],
    ids=[
        "request-id",
        "invocation-id",
        "activation-id",
        "expert-version",
        "manifest-digest",
    ],
)
def test_correlated_row_and_result_identity_corruption_fails_closed_without_redispatch(
    tmp_path: Path,
    column: str,
    wire_key: str,
    corrupt_value: Any,
) -> None:
    counter = DispatchCounter()
    path = tmp_path / f"restart-correlated-{column}.db"
    first_db = DatabaseManager(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first = first_port.invoke(_request(first_registry, first_handle))
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert counter.calls == 1

    row = first_db.fetch_one(
        "SELECT result_json FROM expert_idempotency_v1 WHERE idempotency_key = ?",
        ("idempotency:restart-bounds",),
    )
    assert row is not None
    wire = json.loads(row["result_json"])
    wire[wire_key] = corrupt_value
    first_db.execute(
        f"UPDATE expert_idempotency_v1 SET {column} = ?, result_json = ? "
        "WHERE idempotency_key = ?",
        (
            corrupt_value,
            json.dumps(wire, sort_keys=True, separators=(",", ":")),
            "idempotency:restart-bounds",
        ),
    )
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry, restarted_port, restarted_handle = _runtime(counter, restarted_db)
    recovered = restarted_port.invoke(_request(restarted_registry, restarted_handle))

    assert counter.calls == 1, "corrupt durable identity must never redispatch"
    assert recovered.replayed is True
    assert recovered.verdict is ExpertVerdict.UNKNOWN
    assert recovered.error_code is ExpertErrorCode.INTERRUPTED
    assert recovered.activation_id == restarted_handle.activation_id
    assert recovered.expert_id == restarted_handle.expert_id
    assert recovered.expert_version == restarted_handle.expert_version
    assert recovered.manifest_digest == restarted_handle.manifest_digest
    assert recovered.resolved_registry_generation == restarted_handle.registry_generation
    assert recovered.resolved_runtime_generation == restarted_handle.runtime_generation
    assert recovered.request_id != corrupt_value
    assert recovered.invocation_id != corrupt_value
    assert recovered.data == {}
    assert recovered.evidence_refs == ()
    assert recovered.usage == {"model_calls": 0}
    assert recovered.effect_receipts == ()
    restarted_db.close()
