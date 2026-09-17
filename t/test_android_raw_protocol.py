import json

import pytest

from zara import android_raw_protocol, protocol


@pytest.fixture(autouse=True)
def raw_protocol_extension():
    android_raw_protocol.install()
    try:
        yield
    finally:
        android_raw_protocol.uninstall_for_tests()


def frame(message):
    return [protocol.PROTOCOL_MARKER, json.dumps(message, separators=(",", ":")).encode()]


def base_message(message_type, body):
    return {
        "type": message_type,
        "id": "msg-1",
        "session_id": "session-1",
        "timestamp_ns": 1,
        "payload_count": 0,
        "body": body,
    }


def test_android_raw_is_a_real_device_capability():
    assert "android_raw" in protocol.DEVICE_CAPABILITIES

    message = base_message(
        "device.action.request",
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "android_raw",
            "args": {
                "backend": "accessibility",
                "operation": "accessibility.global_action",
                "arguments": {"action": "home"},
            },
            "deadline_ns": 10_000,
            "idempotency": "at_most_once",
        },
    )
    message["trace_id"] = "trace-1"

    decoded = protocol.decode_multipart(frame(message))
    assert decoded.message.body["capability"] == "android_raw"


def test_android_raw_rejects_non_string_argument_values_and_unknown_fields():
    body = {
        "action_id": "action-1",
        "action_seq": 1,
        "capability": "android_raw",
        "args": {
            "backend": "root",
            "operation": "shell.exec",
            "arguments": {"command": 7},
        },
        "deadline_ns": 10_000,
        "idempotency": "at_most_once",
    }
    message = base_message("device.action.request", body)

    with pytest.raises(protocol.ProtocolValidationError):
        protocol.decode_multipart(frame(message))

    body["args"] = {
        "backend": "root",
        "operation": "shell.exec",
        "arguments": {},
        "surprise": True,
    }
    with pytest.raises(protocol.ProtocolValidationError):
        protocol.decode_multipart(frame(message))


def test_device_action_result_can_return_backend_identity_and_output():
    message = base_message(
        "device.action.result",
        {
            "action_id": "action-1",
            "outcome": "completed",
            "backend": "root",
            "identity": "root:0",
            "output": "uid=0(root) gid=0(root)\n",
        },
    )

    decoded = protocol.decode_multipart(frame(message))
    assert decoded.message.body["backend"] == "root"
    assert decoded.message.body["output"].startswith("uid=0")


def test_device_action_result_keeps_legacy_minimal_body_valid():
    message = base_message(
        "device.action.result",
        {"action_id": "action-1", "outcome": "completed"},
    )

    decoded = protocol.decode_multipart(frame(message))
    assert decoded.message.body["outcome"] == "completed"
