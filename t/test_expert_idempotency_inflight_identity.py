"""In-flight durable replay must preserve the original dispatch authority identity."""

from __future__ import annotations

import threading
from pathlib import Path
from typing import Any

from t.test_expert_idempotency_restart_contract import (
    BlockingDispatchCounter,
    _descriptor,
    _fresh_database,
    _request,
    _runtime,
)
from zara.expert_port import CanonicalExpertInvocationPort
from zara.experts import ExpertErrorCode, ExpertVerdict


def test_inflight_retry_preserves_original_activation_build_and_generations(
    tmp_path: Path,
) -> None:
    counter = BlockingDispatchCounter()
    path = tmp_path / "inflight-identity.db"
    first_db = _fresh_database(path)
    first_registry, first_port, first_handle = _runtime(counter, first_db)
    outcome: dict[str, Any] = {}

    def invoke_first() -> None:
        try:
            outcome["result"] = first_port.invoke(_request(first_registry, first_handle))
        except BaseException as error:  # pragma: no cover - asserted below
            outcome["error"] = error

    thread = threading.Thread(target=invoke_first)
    thread.start()
    assert counter.entered.wait(timeout=10)

    durable = first_db.fetch_one(
        "SELECT activation_id, expert_version, manifest_digest, "
        "registry_generation, runtime_generation, invocation_id, request_id, state "
        "FROM expert_idempotency_v1 WHERE idempotency_key = ?",
        ("idempotency:restart-contract",),
    )
    assert durable is not None
    assert durable["state"] == "dispatching"
    assert durable["activation_id"] == first_handle.activation_id
    assert durable["manifest_digest"] == first_handle.manifest_digest

    second_db = _fresh_database(path)
    second_registry, _unused_port, _unused_handle = _runtime(counter, second_db)
    second_registry.reload(
        [
            (
                _descriptor(manifest_digest="sha256:restart-fixture-v2"),
                counter.handler,
            )
        ]
    )
    second_handle, _receipt = second_registry.activate(
        first_handle.principal,
        first_handle.workspace,
        first_handle.expert_id,
    )
    second_port = CanonicalExpertInvocationPort(second_registry)

    assert second_handle.activation_id != first_handle.activation_id
    assert second_handle.manifest_digest != first_handle.manifest_digest
    assert second_handle.registry_generation != first_handle.registry_generation
    assert second_handle.runtime_generation != first_handle.runtime_generation

    unresolved = second_port.invoke(_request(second_registry, second_handle))

    counter.release.set()
    thread.join(timeout=10)
    assert not thread.is_alive()
    assert "error" not in outcome
    first = outcome["result"]

    assert counter.calls == 1, "in-flight retry must never redispatch the original effect"
    assert unresolved.replayed is True
    assert unresolved.verdict is ExpertVerdict.UNKNOWN
    assert unresolved.error_code is ExpertErrorCode.INTERRUPTED
    assert unresolved.invocation_id == durable["invocation_id"] == first.invocation_id
    assert unresolved.request_id == durable["request_id"] == first.request_id
    assert unresolved.activation_id == durable["activation_id"] == first_handle.activation_id
    assert unresolved.expert_id == first_handle.expert_id
    assert unresolved.expert_version == durable["expert_version"] == first_handle.expert_version
    assert unresolved.manifest_digest == durable["manifest_digest"] == first_handle.manifest_digest
    assert (
        unresolved.resolved_registry_generation
        == durable["registry_generation"]
        == first_handle.registry_generation
    )
    assert (
        unresolved.resolved_runtime_generation
        == durable["runtime_generation"]
        == first_handle.runtime_generation
    )
    assert unresolved.usage == {"model_calls": 0}
    assert unresolved.effect_receipts == ()
    assert first.verdict is ExpertVerdict.SUCCEEDED
    assert counter.calls == 1

    first_db.close()
    second_db.close()
