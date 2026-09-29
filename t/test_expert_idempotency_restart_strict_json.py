"""Strict JSON restart corruption contracts for durable expert idempotency (#1449)."""

from __future__ import annotations

from pathlib import Path
from typing import Callable

import pytest

from t.test_expert_idempotency_restart_bounds import DispatchCounter, _request, _runtime
from zara.database import DatabaseManager
from zara.experts import ExpertErrorCode, ExpertVerdict


def _duplicate_top_level_data(payload: str) -> str:
    needle = '"data":{"summary":"bounded durable fixture"}'
    assert payload.count(needle) == 1
    return payload.replace(
        needle,
        needle + ',"data":{"summary":"tampered duplicate"}',
        1,
    )


def _duplicate_nested_data_member(payload: str) -> str:
    needle = '"data":{"summary":"bounded durable fixture"}'
    assert payload.count(needle) == 1
    return payload.replace(
        needle,
        '"data":{"summary":"bounded durable fixture","summary":"tampered duplicate"}',
        1,
    )


@pytest.mark.parametrize(
    "corrupt",
    [_duplicate_top_level_data, _duplicate_nested_data_member],
    ids=["top-level-duplicate", "nested-duplicate"],
)
def test_duplicate_durable_json_members_fail_closed_without_redispatch(
    tmp_path: Path,
    corrupt: Callable[[str], str],
) -> None:
    counter = DispatchCounter()
    path = tmp_path / "restart-strict-json.db"
    first_db = DatabaseManager(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    first = first_port.invoke(_request(first_registry, first_handle))

    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.usage == {"model_calls": 0}
    assert counter.calls == 1

    row = first_db.fetch_one(
        "SELECT result_json FROM expert_idempotency_v1 WHERE invocation_id = ?",
        (first.invocation_id,),
    )
    assert row is not None
    raw_payload = row["result_json"]
    corrupt_payload = corrupt(raw_payload)
    assert corrupt_payload != raw_payload
    first_db.execute(
        "UPDATE expert_idempotency_v1 SET result_json = ? WHERE invocation_id = ?",
        (corrupt_payload, first.invocation_id),
    )
    first_db.close()

    restarted_db = DatabaseManager(path)
    restarted_registry, restarted_port, restarted_handle = _runtime(counter, restarted_db)
    recovered = restarted_port.invoke(_request(restarted_registry, restarted_handle))

    assert counter.calls == 1, "ambiguous durable JSON must never redispatch"
    assert recovered.replayed is True
    assert recovered.verdict is ExpertVerdict.UNKNOWN
    assert recovered.error_code is ExpertErrorCode.INTERRUPTED
    assert recovered.invocation_id == first.invocation_id
    assert recovered.request_id == first.request_id
    assert recovered.activation_id == restarted_handle.activation_id
    assert recovered.data == {}
    assert recovered.evidence_refs == ()
    assert recovered.usage == {"model_calls": 0}
    assert recovered.effect_receipts == ()
    restarted_db.close()
