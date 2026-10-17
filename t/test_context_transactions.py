from __future__ import annotations

import asyncio

import pytest
from langchain_core.messages import AIMessage

from zara.agent.context import ContextBudgetError, ContextConfig, ContextManager


async def seeded_manager(*, strategy="compress", summarizer=None):
    manager = ContextManager(
        system_prompt="base",
        config=ContextConfig(strategy=strategy, max_tokens=100, preserve_recent_turns=1,
                             summary_max_tokens=10, skill_max_tokens=10),
        token_counter=len,
        summarizer=summarizer,
    )
    for index in range(4):
        lease = manager.begin_turn(str(index))
        build = await manager.build_messages(lease, f"question {index}")
        manager.commit_result(lease, [*build.messages, AIMessage(content=f"answer {index}")])
    return manager


@pytest.mark.asyncio
async def test_later_summary_failure_rolls_back_entire_automatic_compression():
    calls = []

    async def summarize(messages, budget):
        calls.append(messages)
        if len(calls) == 2:
            raise RuntimeError("second summary failed")
        return "first temporary summary"

    manager = await seeded_manager(summarizer=summarize)
    before = manager.history
    manager.config = ContextConfig(strategy="compress", max_tokens=5, preserve_recent_turns=1,
                                   summary_max_tokens=2, skill_max_tokens=2)
    with pytest.raises(RuntimeError, match="second summary"):
        await manager.build_messages(manager.begin_turn("failing"), "next")
    assert len(calls) == 2
    assert manager.history == before


@pytest.mark.asyncio
async def test_impossible_truncation_keeps_original_history():
    manager = await seeded_manager(strategy="truncate")
    before = manager.history
    manager.config = ContextConfig(max_tokens=3, preserve_recent_turns=1,
                                   summary_max_tokens=1, skill_max_tokens=1)
    with pytest.raises(ContextBudgetError):
        await manager.build_messages(manager.begin_turn("failing"), "next")
    assert manager.history == before


@pytest.mark.asyncio
async def test_cancel_manual_compaction_retains_source_and_audit():
    started = asyncio.Event()

    async def summarize(messages, budget):
        started.set()
        await asyncio.Event().wait()

    manager = await seeded_manager(summarizer=summarize)
    before, audit = manager.history, manager.audit
    lease = manager.begin_turn("compact")
    pending = asyncio.create_task(manager.compact(lease, reason="manual"))
    await asyncio.wait_for(started.wait(), 1)
    pending.cancel()
    with pytest.raises(asyncio.CancelledError):
        await pending
    assert manager.history == before
    assert manager.audit == audit


def test_fallback_counter_includes_large_tool_arguments():
    from zara.agent.context import _default_token_counter

    short = AIMessage(content="", tool_calls=[{"id": "a", "name": "echo", "args": {}}])
    long = AIMessage(content="", tool_calls=[{"id": "a", "name": "echo", "args": {"text": "x" * 10000}}])
    assert _default_token_counter([long]) >= _default_token_counter([short]) + 10000


@pytest.mark.asyncio
async def test_compaction_deadline_leaves_history_unchanged():
    async def never_finishes(messages, budget):
        await asyncio.Event().wait()

    manager = ContextManager(system_prompt="base", summarizer=never_finishes,
                             summary_timeout=0.01, token_counter=len)
    for index in range(10):
        lease = manager.begin_turn(str(index))
        build = await manager.build_messages(lease, "question")
        manager.commit_result(lease, [*build.messages, AIMessage(content="answer")])
    before = manager.history
    with pytest.raises(TimeoutError):
        await manager.compact(manager.begin_turn("compact"))
    assert manager.history == before


@pytest.mark.asyncio
async def test_oversized_final_output_does_not_expand_active_history_without_bound():
    manager = ContextManager(system_prompt="base", config=ContextConfig(
        max_tokens=1000, summary_max_tokens=100, skill_max_tokens=100,
    ))
    lease = manager.begin_turn("turn")
    build = await manager.build_messages(lease, "question")
    with pytest.raises(ContextBudgetError):
        manager.commit_result(lease, [*build.messages, AIMessage(content="x" * 3000)])
    assert manager.history == ()


@pytest.mark.asyncio
async def test_automatic_then_manual_summary_revisions_and_audit_reasons():
    manager = await seeded_manager(summarizer=lambda messages, budget: "summary")
    manager.config = ContextConfig(strategy="compress", max_tokens=7, preserve_recent_turns=1,
                                   summary_max_tokens=2, skill_max_tokens=2)
    lease = manager.begin_turn("automatic")
    build = await manager.build_messages(lease, "next")
    manager.commit_result(lease, [*build.messages, AIMessage(content="answer")])
    assert manager.audit.reason == "compress"
    assert manager.audit.summary_revision == 1
    lease = manager.begin_turn("automatic-again")
    build = await manager.build_messages(lease, "again")
    manager.commit_result(lease, [*build.messages, AIMessage(content="answer")])
    assert manager.audit.summary_revision == 2
    result = await manager.compact(manager.begin_turn("manual"))
    assert result.changed
    assert result.summary_revision == 3
    assert manager.audit.reason == "manual"
    assert manager.audit.summary_revision == 3


@pytest.mark.parametrize("timeout", [0, -1, True, float("nan"), float("inf"), "60", 301])
def test_summary_deadline_rejects_unbounded_or_invalid_values(timeout):
    with pytest.raises(ValueError, match="timeout"):
        ContextManager(system_prompt="base", summary_timeout=timeout)


@pytest.mark.asyncio
async def test_summary_that_suppresses_cancellation_cannot_commit_after_deadline():
    async def late(messages, budget):
        try:
            await asyncio.sleep(1)
        except asyncio.CancelledError:
            return "late summary"

    manager = await seeded_manager(summarizer=late)
    manager._summary_timeout = 0.01
    before = manager.history
    with pytest.raises(TimeoutError):
        await manager.compact(manager.begin_turn("late"))
    assert manager.history == before


def test_duplicate_tool_call_ids_cannot_enter_context_history():
    from langchain_core.messages import HumanMessage, ToolMessage
    from zara.agent.context import ContextError

    manager = ContextManager(system_prompt="base")
    with pytest.raises(ContextError, match="tool-call"):
        manager.replace_history([
            HumanMessage(content="use tools"),
            AIMessage(content="", tool_calls=[
                {"id": "same", "name": "a", "args": {}},
                {"id": "same", "name": "b", "args": {}},
            ]),
            ToolMessage(content="result", tool_call_id="same"),
        ])
    assert manager.history == ()


@pytest.mark.asyncio
async def test_skill_sub_budget_is_enforced_before_model_context_is_built():
    manager = ContextManager(system_prompt="base", config=ContextConfig(
        max_tokens=5000, summary_max_tokens=1000, skill_max_tokens=500,
    ))
    with pytest.raises(ContextBudgetError, match="skill"):
        await manager.build_messages(manager.begin_turn("skill"), "question", skill_context="x" * 1000)
    assert manager.history == ()
