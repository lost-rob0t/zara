"""Same-activation durable effect replay must fail closed without fresh evidence."""

from __future__ import annotations

from pathlib import Path

from t.test_expert_idempotency_restart_contract import (
    EffectDispatchCounter,
    _fresh_database,
    _request,
    _runtime,
)
from zara.experts import ExpertErrorCode, ExpertVerdict


def test_same_activation_effectful_replay_requires_fresh_postcondition(
    tmp_path: Path,
) -> None:
    counter = EffectDispatchCounter()
    database = _fresh_database(tmp_path / "same-activation-effect-replay.db")
    registry, port, handle = _runtime(counter, database)
    request = _request(registry, handle)

    first = port.invoke(request)
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert first.replayed is False
    assert first.effect_receipts
    assert counter.calls == 1

    replay = port.invoke(request)

    assert counter.calls == 1, "durable replay must never repeat the prior effect"
    assert replay.replayed is True
    assert replay.verdict is ExpertVerdict.UNKNOWN
    assert replay.error_code is ExpertErrorCode.UNKNOWN_EXTERNAL_OUTCOME
    assert replay.activation_id == first.activation_id == handle.activation_id
    assert replay.invocation_id == first.invocation_id
    assert replay.request_id == first.request_id
    assert replay.usage == first.usage
    assert replay.effect_receipts == first.effect_receipts
    assert "fresh postcondition" in (replay.error_message or "")
    database.close()
