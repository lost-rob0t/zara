from __future__ import annotations

from zara.runtime.prolog_rlm import _turn_result_from_reply


def test_runtime_result_payload_does_not_cross_into_zara_metadata() -> None:
    reply = {
        "protocol": "ZARA-RUNTIME/1",
        "runtime_id": "prolog-rlm",
        "request_id": "turn-secret",
        "status": "completed",
        "text": "safe answer",
        "result": {
            "provider_response": {
                "authorization": "Bearer secret-runtime-token",
                "raw": "provider payload",
            },
            "trace": "runtime-owned",
        },
    }

    result = _turn_result_from_reply(reply, request_id="turn-secret")

    assert result.response == "safe answer"
    assert result.metadata == {"runtime": "prolog-rlm"}
    assert "secret-runtime-token" not in repr(result.metadata)
    assert "provider payload" not in repr(result.metadata)
