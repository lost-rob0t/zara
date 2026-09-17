from __future__ import annotations

from zara.protocol import (
    DEVICE_CAPABILITIES,
    ProtocolMessage,
    decode_message,
    encode_message,
)


REQUESTED_ANDROID_TOOL_CAPABILITIES = frozenset(
    {
        "sms_compose",
        "sms_send",
        "sms_read",
        "contacts_list",
        "flashlight_set",
        "wifi_status",
        "wifi_panel",
        "wifi_set_enabled",
        "location_current",
        "maps_search",
        "maps_directions",
        "maps_show",
    }
)


def test_zara1_closed_capability_set_contains_requested_android_tools():
    missing = REQUESTED_ANDROID_TOOL_CAPABILITIES - DEVICE_CAPABILITIES
    assert not missing, f"missing Android tool capabilities: {sorted(missing)!r}"


def test_device_action_result_round_trips_bounded_structured_read_data():
    message = ProtocolMessage(
        type="device.action.result",
        id="result-contacts-1",
        session_id="session-1",
        timestamp_ns=5,
        payload_count=0,
        body={
            "action_id": "action-contacts-1",
            "outcome": "completed",
            "data": {
                "contacts": [
                    {
                        "display_name": "Alice Example",
                        "phones": ["+15551234567"],
                    }
                ],
                "truncated": False,
            },
        },
    )

    decoded = decode_message(encode_message(message)).message

    assert decoded.body == message.body
