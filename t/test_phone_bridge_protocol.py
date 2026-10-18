from __future__ import annotations

import pytest

from zara.protocol import (
    DEVICE_CAPABILITIES,
    ProtocolMessage,
    ProtocolValidationError,
    decode_message,
    encode_message,
)
from zara.protocol_runtime import runtime_event_to_message
from zara.runtime import events
from zara.runtime.bridge import EventEnvelope


def round_trip(message: ProtocolMessage) -> ProtocolMessage:
    return decode_message(encode_message(message)).message


def test_sms_send_is_a_closed_device_capability() -> None:
    assert "sms_send" in DEVICE_CAPABILITIES
    message = ProtocolMessage(
        type="device.action.request",
        id="request-1",
        session_id="session-1",
        timestamp_ns=1,
        payload_count=0,
        body={
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "sms_send",
            "args": {"to": "+15551234567", "text": "hello"},
            "deadline_ns": 2,
            "idempotency": "at_most_once",
        },
    )
    assert round_trip(message) == message


@pytest.mark.parametrize(
    ("args", "match"),
    [
        ({"to": "", "text": "hello"}, "to"),
        ({"to": "+15551234567", "text": ""}, "text"),
        ({"to": "+15551234567", "text": "x" * 4097}, "text"),
        ({"to": "+15551234567", "text": "hello", "shell": "id"}, "invalid fields"),
    ],
)
def test_sms_send_rejects_unbounded_or_executable_arguments(args, match: str) -> None:
    message = ProtocolMessage(
        type="device.action.request",
        id="request-1",
        session_id="session-1",
        timestamp_ns=1,
        payload_count=0,
        body={
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "sms_send",
            "args": args,
            "deadline_ns": 2,
            "idempotency": "at_most_once",
        },
    )
    with pytest.raises(ProtocolValidationError, match=match):
        encode_message(message)


def test_android_phone_events_are_closed_session_bound_messages() -> None:
    received = ProtocolMessage(
        type="device.event",
        id="event-request-1",
        session_id="session-1",
        timestamp_ns=1,
        payload_count=0,
        body={
            "event_id": "sms-1",
            "kind": "sms.received",
            "remote": "+15551234567",
            "text": "hello",
        },
    )
    assert round_trip(received) == received

    spam = ProtocolMessage(
        type="device.event",
        id="event-request-2",
        session_id="session-1",
        timestamp_ns=2,
        payload_count=0,
        body={
            "event_id": "call-1",
            "kind": "call.suspected_spam",
            "remote": "+15557654321",
        },
    )
    assert round_trip(spam) == spam


def test_phone_runtime_event_has_no_tool_or_prolog_authority() -> None:
    event = events.PhoneEventReceived(
        event_id="call-1",
        kind="call.suspected_spam",
        remote="+15557654321",
        text="",
        greeting="Hi, I'm a symbolic system.",
        actions=("alert", "voice_takeover"),
    )
    message = runtime_event_to_message(
        EventEnvelope(sequence=7, occurred_at=1.0, event=event),
        message_id="wire-1",
        timestamp_ns=2,
    )
    assert message.type == "phone.event"
    assert message.body == {
        "event_id": "call-1",
        "kind": "call.suspected_spam",
        "remote": "+15557654321",
        "text": "",
        "greeting": "Hi, I'm a symbolic system.",
        "actions": ["alert", "voice_takeover"],
    }
    assert "tool" not in repr(message.body).lower()
    assert "prolog" not in repr(message.body).lower()


def test_desktop_sms_command_is_session_bound_and_closed() -> None:
    request = ProtocolMessage(
        type="phone.sms.send",
        id="sms-request-1",
        session_id="desktop-session",
        timestamp_ns=3,
        payload_count=0,
        body={"to": "+15551234567", "text": "hello"},
    )
    assert round_trip(request) == request

    result = ProtocolMessage(
        type="phone.sms.result",
        id="sms-result-1",
        reply_to="sms-request-1",
        session_id="desktop-session",
        timestamp_ns=4,
        payload_count=0,
        body={"outcome": "completed"},
    )
    assert round_trip(result) == result


@pytest.mark.parametrize(
    "body",
    [
        {"to": "+15551234567"},
        {"text": "hello"},
        {"to": "", "text": "hello"},
        {"to": "+15551234567", "text": ""},
        {"to": "+15551234567", "text": "hello", "device_id": "pretend-authority"},
    ],
)
def test_desktop_sms_command_rejects_ambiguous_or_client_authoritative_shapes(body) -> None:
    with pytest.raises(ProtocolValidationError):
        encode_message(
            ProtocolMessage(
                type="phone.sms.send",
                id="sms-request-1",
                session_id="desktop-session",
                timestamp_ns=3,
                payload_count=0,
                body=body,
            )
        )
