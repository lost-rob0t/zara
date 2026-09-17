from zara.runtime import events
from zara.runtime.bridge import EventEnvelope
from zara.protocol_runtime import runtime_event_to_message


def test_prolog_question_event_has_typed_ui_metadata():
    event = events.UserInputRequired(
        turn_id="turn-1",
        conversation_id="conversation-1",
        kind="prolog_question",
        prompt="Which theme?",
        question_id="theme",
        choices=("outrun", "oled"),
    )

    assert event.question_id == "theme"
    assert event.choices == ("outrun", "oled")


def test_prolog_question_crosses_zara1_as_closed_input_event():
    event = events.UserInputRequired(
        turn_id="turn-1",
        conversation_id="conversation-1",
        kind="prolog_question",
        prompt="Which theme?",
        question_id="theme",
        choices=("outrun", "oled"),
    )
    envelope = EventEnvelope(sequence=7, occurred_at=0.0, event=event)

    message = runtime_event_to_message(
        envelope,
        message_id="message-1",
        timestamp_ns=1,
    )

    assert message.type == "input.required"
    assert message.turn_id == "turn-1"
    assert message.conversation_id == "conversation-1"
    assert message.body == {
        "kind": "prolog_question",
        "prompt": "Which theme?",
        "question_id": "theme",
        "choices": ["outrun", "oled"],
    }
