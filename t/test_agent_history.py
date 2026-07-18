import logging

from langchain_core.messages import AIMessage, HumanMessage, ToolMessage

from zara.agent.graph import add_messages, validate_and_clean_messages


def ai_with_tools(*tool_ids):
    return AIMessage(
        content="",
        tool_calls=[
            {"id": tool_id, "name": f"tool_{tool_id}", "args": {}}
            for tool_id in tool_ids
        ],
    )


def tool_result(tool_id, content=None):
    return ToolMessage(content=content or tool_id, tool_call_id=tool_id)


def test_zero_tool_calls_are_unchanged():
    messages = [HumanMessage(content="hello"), AIMessage(content="hi")]

    assert validate_and_clean_messages(messages) == messages


def test_single_tool_result_is_preserved():
    ai = ai_with_tools("one")
    result = tool_result("one")

    assert validate_and_clean_messages([ai, result]) == [ai, result]


def test_multiple_results_survive_in_call_order():
    ai = ai_with_tools("one", "two", "three")
    results = [tool_result("three"), tool_result("one"), tool_result("two")]

    cleaned = validate_and_clean_messages([ai, *results])

    assert [message.tool_call_id for message in cleaned[1:]] == ["one", "two", "three"]


def test_duplicate_and_unknown_ids_are_dropped(caplog):
    ai = ai_with_tools("one")

    with caplog.at_level(logging.WARNING):
        cleaned = validate_and_clean_messages([
            ai,
            tool_result("one", "kept"),
            tool_result("one", "duplicate"),
            tool_result("unknown"),
        ])

    assert cleaned == [ai, tool_result("one", "kept")]
    assert "duplicate ToolMessage" in caplog.text
    assert "unknown ToolMessage" in caplog.text


def test_orphan_and_cross_turn_results_are_dropped(caplog):
    ai = ai_with_tools("one", "two")
    human = HumanMessage(content="next turn")

    with caplog.at_level(logging.WARNING):
        cleaned = validate_and_clean_messages([
            tool_result("orphan"),
            ai,
            tool_result("one"),
            human,
            tool_result("two"),
        ])

    assert cleaned == [ai, tool_result("one"), human]
    assert caplog.text.count("orphan ToolMessage") == 2


def test_partial_results_preserve_the_matching_subset():
    ai = ai_with_tools("one", "two")
    result = tool_result("two")

    assert validate_and_clean_messages([ai, result]) == [ai, result]


def test_mixed_historical_turns_remain_provider_compatible():
    first_ai = ai_with_tools("first-a", "first-b")
    second_ai = ai_with_tools("second")
    next_user = HumanMessage(content="continue")
    final_ai = AIMessage(content="done")
    messages = [
        HumanMessage(content="start"),
        first_ai,
        tool_result("first-b"),
        tool_result("first-a"),
        next_user,
        second_ai,
        tool_result("second"),
        final_ai,
    ]

    cleaned = validate_and_clean_messages(messages)

    assert cleaned == [
        messages[0],
        first_ai,
        tool_result("first-a"),
        tool_result("first-b"),
        next_user,
        second_ai,
        tool_result("second"),
        final_ai,
    ]


def test_openai_compatible_tool_messages_follow_their_assistant_call():
    ai = ai_with_tools("call-a", "call-b")
    cleaned = validate_and_clean_messages([
        ai,
        tool_result("call-b"),
        tool_result("call-a"),
    ])

    openai_sequence = [
        {"role": "assistant", "tool_calls": [call["id"] for call in ai.tool_calls]},
        *[
            {"role": "tool", "tool_call_id": message.tool_call_id}
            for message in cleaned[1:]
        ],
    ]

    assert openai_sequence == [
        {"role": "assistant", "tool_calls": ["call-a", "call-b"]},
        {"role": "tool", "tool_call_id": "call-a"},
        {"role": "tool", "tool_call_id": "call-b"},
    ]


def test_anthropic_compatible_results_are_one_contiguous_group():
    ai = ai_with_tools("use-a", "use-b")
    human = HumanMessage(content="next")
    cleaned = validate_and_clean_messages([
        ai,
        tool_result("use-b"),
        tool_result("use-a"),
        human,
    ])

    assert isinstance(cleaned[0], AIMessage)
    assert all(isinstance(message, ToolMessage) for message in cleaned[1:3])
    assert isinstance(cleaned[3], HumanMessage)


def test_graph_reducer_still_appends_messages():
    initial = [HumanMessage(content="first")]
    appended = [AIMessage(content="second")]

    assert add_messages(initial, appended) == [*initial, *appended]
