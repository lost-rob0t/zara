from __future__ import annotations

import asyncio
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


def _config(
    *,
    strategy="truncate",
    max_tokens=100,
    preserve_recent_turns=2,
    summary_max_tokens=10,
):
    return ContextConfig(
        strategy=strategy,
        max_tokens=max_tokens,
        preserve_recent_turns=preserve_recent_turns,
        summary_max_tokens=summary_max_tokens,
        skill_max_tokens=min(20, max_tokens),
    )


async def _commit_turn(manager: ContextManager, index: int) -> None:
    lease = manager.begin_turn(f"turn-{index}")
    build = await manager.build_messages(lease, f"question {index}")
    manager.commit_result(
        lease,
        [*build.messages, AIMessage(content=f"answer {index}")],
    )


@pytest.mark.asyncio
async def test_one_base_prompt_and_transient_context_never_persists():
    manager = ContextManager(
        system_prompt="canonical base",
        config=_config(),
        token_counter=_message_counter,
    )
    lease = manager.begin_turn("turn-1")
    build = await manager.build_messages(
        lease,
        "hello",
        transients=[
            TransientContext("memory", "Relevant memories:\n- likes Prolog"),
            TransientContext("runtime", "Android capability snapshot"),
        ],
        skill_context="<skill name=\"python-prolog\">rules</skill>",
    )

    base = [message for message in build.messages if getattr(message, "id", None) == "context:base"]
    assert len(base) == 1
    assert isinstance(base[0], SystemMessage)

    manager.commit_result(lease, [*build.messages, AIMessage(content="hi")])

    assert [type(message) for message in manager.history] == [HumanMessage, AIMessage]
    persisted = "\n".join(str(message.content) for message in manager.history)
    assert "Relevant memories" not in persisted
    assert "Android capability snapshot" not in persisted
    assert "python-prolog" not in persisted
    assert "canonical base" not in persisted


@pytest.mark.asyncio
async def test_truncation_drops_a_whole_tool_turn_group():
    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="truncate", max_tokens=7, preserve_recent_turns=1),
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
    await _commit_turn(manager, 2)

    lease = manager.begin_turn("next")
    next_build = await manager.build_messages(lease, "next")
    contents = [str(message.content) for message in next_build.messages]

    assert "use tools" not in contents
    assert "A" not in contents
    assert "B" not in contents
    assert "tool work done" not in contents
    assert "question 2" in contents
    assert "answer 2" in contents
    assert manager.audit.removed_groups


@pytest.mark.asyncio
async def test_automatic_compression_failure_is_atomic():
    async def fail_summary(messages, max_tokens):
        raise RuntimeError("summary backend failed")

    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="compress", max_tokens=5, preserve_recent_turns=1),
        token_counter=_message_counter,
        summarizer=fail_summary,
    )
    await _commit_turn(manager, 0)
    await _commit_turn(manager, 1)

    before = deepcopy(manager.history)
    lease = manager.begin_turn("trigger")
    with pytest.raises(RuntimeError, match="summary backend failed"):
        await manager.build_messages(lease, "trigger compression")

    assert manager.history == tuple(before)


@pytest.mark.asyncio
async def test_manual_compact_works_before_budget_pressure_and_preserves_recent_tail():
    summaries = []

    async def summarize(messages, max_tokens):
        summaries.append(tuple(str(message.content) for message in messages))
        return "manual bounded summary"

    manager = ContextManager(
        system_prompt="base",
        config=_config(
            strategy="compress",
            max_tokens=100,
            preserve_recent_turns=1,
            summary_max_tokens=10,
        ),
        token_counter=_message_counter,
        summarizer=summarize,
    )
    for index in range(4):
        await _commit_turn(manager, index)

    before_count = len(manager.history)
    lease = manager.begin_turn("manual-compact")
    result = await manager.compact(lease, reason="manual")

    assert result.changed is True
    assert result.reason == "manual"
    assert result.before_tokens == before_count
    assert result.after_tokens < result.before_tokens
    assert result.removed_groups
    assert summaries

    summary_messages = [
        message
        for message in manager.history
        if getattr(message, "id", None) == "context:summary"
    ]
    assert len(summary_messages) == 1
    contents = [str(message.content) for message in manager.history]
    assert "question 0" not in contents
    assert "answer 0" not in contents
    assert "question 1" not in contents
    assert "answer 1" not in contents
    assert "question 2" not in contents
    assert "answer 2" not in contents
    assert "question 3" in contents
    assert "answer 3" in contents


@pytest.mark.asyncio
async def test_manual_compact_noops_when_no_prefix_is_eligible():
    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="compress", preserve_recent_turns=2),
        token_counter=_message_counter,
        summarizer=lambda messages, max_tokens: "unused",
    )
    await _commit_turn(manager, 0)
    before = deepcopy(manager.history)

    lease = manager.begin_turn("manual-compact")
    result = await manager.compact(lease, reason="manual")

    assert result.changed is False
    assert result.reason == "manual"
    assert result.removed_groups == ()
    assert manager.history == tuple(before)


@pytest.mark.asyncio
async def test_manual_compact_failure_leaves_history_unchanged():
    async def fail_summary(messages, max_tokens):
        raise RuntimeError("manual summary failed")

    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="compress", preserve_recent_turns=1),
        token_counter=_message_counter,
        summarizer=fail_summary,
    )
    await _commit_turn(manager, 0)
    await _commit_turn(manager, 1)
    before = deepcopy(manager.history)

    lease = manager.begin_turn("manual-compact")
    with pytest.raises(RuntimeError, match="manual summary failed"):
        await manager.compact(lease, reason="manual")

    assert manager.history == tuple(before)


@pytest.mark.asyncio
async def test_stale_manual_compact_cannot_mutate_after_summarizer_await():
    started = asyncio.Event()
    release = asyncio.Event()

    async def summarize(messages, max_tokens):
        started.set()
        await release.wait()
        return "stale summary"

    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="compress", preserve_recent_turns=1),
        token_counter=_message_counter,
        summarizer=summarize,
    )
    await _commit_turn(manager, 0)
    await _commit_turn(manager, 1)
    before = deepcopy(manager.history)

    stale_lease = manager.begin_turn("manual-compact-stale")
    compacting = asyncio.create_task(manager.compact(stale_lease, reason="manual"))
    await started.wait()
    manager.begin_turn("newer-turn")
    release.set()

    with pytest.raises(StaleContextTurn, match="manual-compact-stale"):
        await compacting

    assert manager.history == tuple(before)


@pytest.mark.asyncio
async def test_repeated_compaction_replaces_summary_instead_of_stacking():
    summaries = []

    async def summarize(messages, max_tokens):
        summaries.append(tuple(str(message.content) for message in messages))
        return f"summary-{len(summaries)}"

    manager = ContextManager(
        system_prompt="base",
        config=_config(strategy="compress", max_tokens=100, preserve_recent_turns=1),
        token_counter=_message_counter,
        summarizer=summarize,
    )

    for index in range(3):
        await _commit_turn(manager, index)
    first = manager.begin_turn("compact-one")
    assert (await manager.compact(first, reason="manual")).changed

    await _commit_turn(manager, 3)
    second = manager.begin_turn("compact-two")
    assert (await manager.compact(second, reason="manual")).changed

    summary_messages = [
        message
        for message in manager.history
        if getattr(message, "id", None) == "context:summary"
    ]
    assert len(summary_messages) == 1
    assert len(summaries) == 2
    assert any("summary-1" in part for part in summaries[1])


def test_context_config_rejects_unknown_strategy():
    with pytest.raises(ValueError, match="strategy"):
        ContextConfig(strategy="invent-another-history-policy")


def test_context_config_rejects_impossible_budget():
    with pytest.raises((ValueError, ContextBudgetError)):
        ContextConfig(max_tokens=0)
