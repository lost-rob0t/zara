from __future__ import annotations

from copy import deepcopy

import pytest
from langchain_core.messages import AIMessage, HumanMessage, SystemMessage, ToolMessage

from zara.agent.context import (
    ContextBudgetError,
    ContextConfig,
    ContextManager,
    StaleContextTurn,
    TransientContext,
)


def _message_counter(messages):
    return len(messages)


@pytest.mark.asyncio
async def test_one_base_prompt_and_transient_context_never_persists():
    manager = ContextManager(
        system_prompt="canonical base",
        config=ContextConfig(max_tokens=100, preserve_recent_turns=2),
        token_counter=_message_counter,
    )
    lease = manager.begin_turn("turn-1")
    build = await manager.build_messages(
        lease,
        "hello",
        transients=[
            TransientContext("memory", "Relevant memories:\n- likes Prolog"),
            TransientContext("mcp", "MCP server docs exposes tool search"),
        ],
        skill_context="<skill name=\"python-runtime\">rules</skill>",
    )

    base = [m for m in build.messages if getattr(m, "id", None) == "context:base"]
    assert len(base) == 1
    assert isinstance(base[0], SystemMessage)
    assert sum(str(getattr(m, "id", "")).startswith("context:transient:") for m in build.messages) == 2
    assert sum(str(getattr(m, "id", "")).startswith("context:skills:") for m in build.messages) == 1

    result_messages = [*build.messages, AIMessage(content="hi")]
    manager.commit_result(lease, result_messages)

    assert [type(message) for message in manager.history] == [HumanMessage, AIMessage]
    persisted_text = "\n".join(str(message.content) for message in manager.history)
    assert "Relevant memories" not in persisted_text
    assert "MCP server docs" not in persisted_text
    assert "python-runtime" not in persisted_text
    assert "canonical base" not in persisted_text


@pytest.mark.asyncio
async def test_newer_turn_invalidates_older_commit():
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(max_tokens=100),
        token_counter=_message_counter,
    )
    old_lease = manager.begin_turn("old")
    old_build = await manager.build_messages(old_lease, "old input")
    manager.begin_turn("new")

    with pytest.raises(StaleContextTurn, match="old"):
        manager.commit_result(old_lease, [*old_build.messages, AIMessage(content="late")])

    assert manager.history == ()


@pytest.mark.asyncio
async def test_cancelled_turn_cannot_commit():
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(max_tokens=100),
        token_counter=_message_counter,
    )
    lease = manager.begin_turn("cancel-me")
    build = await manager.build_messages(lease, "input")
    manager.cancel_turn("cancel-me")

    with pytest.raises(StaleContextTurn, match="cancel-me"):
        manager.commit_result(lease, [*build.messages, AIMessage(content="late")])


@pytest.mark.asyncio
async def test_truncation_drops_whole_old_tool_turn_group():
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(strategy="truncate", max_tokens=7, preserve_recent_turns=1),
        token_counter=_message_counter,
    )

    lease = manager.begin_turn("tool-turn")
    build = await manager.build_messages(lease, "use tools")
    manager.commit_result(
        lease,
        [
            *build.messages,
            AIMessage(
                content="",
                tool_calls=[
                    {"name": "a", "args": {}, "id": "call-a", "type": "tool_call"},
                    {"name": "b", "args": {}, "id": "call-b", "type": "tool_call"},
                ],
            ),
            ToolMessage(content="A", tool_call_id="call-a"),
            ToolMessage(content="B", tool_call_id="call-b"),
            AIMessage(content="tool work done"),
        ],
    )

    lease = manager.begin_turn("recent")
    build = await manager.build_messages(lease, "recent question")
    manager.commit_result(lease, [*build.messages, AIMessage(content="recent answer")])

    lease = manager.begin_turn("next")
    next_build = await manager.build_messages(lease, "next question")

    contents = [str(message.content) for message in next_build.messages]
    assert "use tools" not in contents
    assert "A" not in contents
    assert "B" not in contents
    assert "tool work done" not in contents
    assert "recent question" in contents
    assert "recent answer" in contents
    assert manager.audit.removed_groups


@pytest.mark.asyncio
async def test_truncate_raises_if_protected_context_cannot_fit():
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(strategy="truncate", max_tokens=2, preserve_recent_turns=1),
        token_counter=_message_counter,
    )
    lease = manager.begin_turn("turn")

    with pytest.raises(ContextBudgetError):
        await manager.build_messages(
            lease,
            "user",
            transients=[TransientContext("runtime", "required")],
        )


@pytest.mark.asyncio
async def test_compression_is_atomic_when_summarizer_fails():
    async def fail_summary(messages, max_tokens):
        raise RuntimeError("summary backend failed")

    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(strategy="compress", max_tokens=5, preserve_recent_turns=1),
        token_counter=_message_counter,
        summarizer=fail_summary,
    )
    for index in range(2):
        lease = manager.begin_turn(f"seed-{index}")
        build = await manager.build_messages(lease, f"question {index}")
        manager.commit_result(lease, [*build.messages, AIMessage(content=f"answer {index}")])

    before = deepcopy(manager.history)
    lease = manager.begin_turn("trigger")
    with pytest.raises(RuntimeError, match="summary backend failed"):
        await manager.build_messages(lease, "trigger compression")

    assert manager.history == tuple(before)


@pytest.mark.asyncio
async def test_repeated_compression_replaces_summary_instead_of_stacking():
    summaries = []

    async def summarize(messages, max_tokens):
        summaries.append(tuple(str(message.content) for message in messages))
        return f"summary-{len(summaries)}"

    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(
            strategy="compress",
            max_tokens=6,
            preserve_recent_turns=1,
            summary_max_tokens=2,
        ),
        token_counter=_message_counter,
        summarizer=summarize,
    )

    for index in range(5):
        lease = manager.begin_turn(f"turn-{index}")
        build = await manager.build_messages(lease, f"question {index}")
        manager.commit_result(lease, [*build.messages, AIMessage(content=f"answer {index}")])

    summary_messages = [
        message
        for message in manager.history
        if getattr(message, "id", None) == "context:summary"
    ]
    assert len(summary_messages) == 1
    assert len(summaries) >= 2
    assert any("summary-1" in part for part in summaries[1])


@pytest.mark.asyncio
async def test_clear_removes_active_history_and_invalidates_lease():
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(max_tokens=100),
        token_counter=_message_counter,
    )
    lease = manager.begin_turn("turn")
    build = await manager.build_messages(lease, "question")
    manager.commit_result(lease, [*build.messages, AIMessage(content="answer")])
    assert manager.history

    manager.clear()
    assert manager.history == ()

    with pytest.raises(StaleContextTurn):
        manager.commit_result(lease, [*build.messages, AIMessage(content="late")])
