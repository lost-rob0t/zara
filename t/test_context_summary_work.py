from __future__ import annotations

import asyncio
import threading
import time
import subprocess
import sys
import textwrap

import pytest
from langchain_core.messages import AIMessage, HumanMessage

from zara.agent.context import ContextConfig, ContextError, ContextManager


def manager_for(callback, **kwargs):
    manager = ContextManager(system_prompt="base", token_counter=len, summarizer=callback,
                             summary_timeout=0.02, config=ContextConfig(max_tokens=100,
                             preserve_recent_turns=0, summary_max_tokens=10, skill_max_tokens=10), **kwargs)
    manager.replace_history([HumanMessage(content="old"), AIMessage(content="answer")])
    return manager


@pytest.mark.asyncio
async def test_sync_summary_cannot_block_loop_or_commit_after_deadline():
    release = threading.Event()

    def blocked(messages, budget):
        release.wait(1)
        return "late"

    manager = manager_for(blocked)
    before = manager.history
    start = time.monotonic()
    try:
        with pytest.raises(TimeoutError):
            await manager.compact(manager.begin_turn("sync"))
        assert time.monotonic() - start < 0.3
        assert manager.history == before
    finally:
        release.set()


@pytest.mark.asyncio
async def test_resistant_async_summary_times_out_without_waiting_for_cancel_ack():
    release = asyncio.Event()

    async def resistant(messages, budget):
        while not release.is_set():
            try:
                await release.wait()
            except asyncio.CancelledError:
                continue
        return "late"

    manager = manager_for(resistant)
    before = manager.history
    pending = asyncio.create_task(manager.compact(manager.begin_turn("async")))
    try:
        done, _ = await asyncio.wait({pending}, timeout=0.3)
        assert pending in done, "summary deadline waited for resistant provider cancellation"
        with pytest.raises(TimeoutError):
            await pending
        assert manager.history == before
    finally:
        release.set()
        await asyncio.gather(pending, return_exceptions=True)


@pytest.mark.asyncio
async def test_shared_summary_slot_refuses_repeated_and_cross_context_work_until_real_completion():
    from zara.agent.context import SummaryWork

    release = threading.Event()
    finished = threading.Event()
    calls = []

    def blocked(messages, budget):
        calls.append(1)
        try:
            release.wait(1)
            return "late"
        finally:
            finished.set()

    work = SummaryWork()
    first = manager_for(blocked, summary_work=work)
    second = manager_for(blocked, summary_work=work)
    try:
        with pytest.raises(TimeoutError):
            await first.compact(first.begin_turn("first"))
        assert work.state == "draining"
        for owner in (first, second, first):
            with pytest.raises(ContextError, match="busy"):
                await owner.compact(owner.begin_turn("retry"))
        assert calls == [1]
    finally:
        release.set()
        assert await asyncio.to_thread(finished.wait, 1)
        await asyncio.sleep(0)
    assert first.history == second.history


def test_runtime_restart_retains_occupied_slot_and_does_not_queue_another_summary(monkeypatch):
    from t.test_task_turn_context import CapturingLLM, build_manager
    from zara.agent import AgentManager
    from zara.runtime.backend import LangGraphRuntimeBackend
    from zara.runtime.commands import RestartRuntime
    from zara.runtime.host import RuntimeHost

    llm = CapturingLLM()
    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda *args: llm)
    managers = []
    release, finished = threading.Event(), threading.Event()
    calls = []

    def callback(messages, budget):
        calls.append(1)
        try:
            release.wait(2)
            return "late"
        finally:
            finished.set()

    def factory():
        manager = build_manager(llm)
        managers.append(manager)
        return LangGraphRuntimeBackend(lambda: manager)

    host = RuntimeHost(factory)

    async def compact(manager):
        owner = manager.context_manager
        owner.config = ContextConfig(max_tokens=100, preserve_recent_turns=0, summary_max_tokens=10, skill_max_tokens=10)
        owner._token_counter = len
        owner._summary_timeout = 0.02
        owner._summarizer = callback
        owner.replace_history([HumanMessage(content="old"), AIMessage(content="answer")])
        return await manager.compact_context()

    try:
        host.start().result(timeout=2)
        first = asyncio.run_coroutine_threadsafe(compact(managers[0]), host._loop)
        with pytest.raises(TimeoutError):
            first.result(timeout=0.5)
        assert first.done(), "caller remained blocked instead of reaching its own deadline"
        work = managers[0].context_manager._summary_work
        assert work.state == "draining"
        host.submit(RestartRuntime()).result(timeout=2)
        assert managers[1].context_manager._summary_work is work
        assert managers[1]._new_context_manager()._summary_work is work
        retry = asyncio.run_coroutine_threadsafe(compact(managers[1]), host._loop)
        with pytest.raises(ContextError, match="busy"):
            retry.result(timeout=1)
        assert calls == [1]
    finally:
        release.set()
        assert finished.wait(1)
        if host.is_alive:
            host.shutdown("test cleanup").result(timeout=2)
            host.join(timeout=2)
    assert not host.is_alive


def test_runtime_shutdown_bounds_resistant_background_drain_in_isolated_process():
    script = textwrap.dedent('''
        import asyncio
        from zara.runtime.backend import RuntimeBackend
        from zara.runtime.host import RuntimeHost

        async def resistant():
            while True:
                try:
                    await asyncio.sleep(10)
                except asyncio.CancelledError:
                    continue

        class Backend(RuntimeBackend):
            async def start(self):
                self.work = asyncio.create_task(resistant(), name="resistant-summary-fixture")

        host = RuntimeHost(Backend, shutdown_timeout=0.1)
        host.start().result(timeout=2)
        host.shutdown("test cleanup").result(timeout=2)
        host.join(timeout=1)
        assert not host.is_alive, "runtime final drain waited indefinitely for cancellation"
        print("bounded shutdown complete")
    ''')
    result = subprocess.run([sys.executable, "-c", script], capture_output=True, text=True, timeout=5)
    assert result.returncode == 0, result.stderr
    assert "bounded shutdown complete" in result.stdout
    assert "remain quarantined" in result.stderr


@pytest.mark.asyncio
async def test_caller_cancellation_keeps_running_sync_work_reserved():
    from zara.agent.context import SummaryWork

    entered, release = threading.Event(), threading.Event()

    def callback(messages, budget):
        entered.set()
        release.wait(1)
        return "late"

    work = SummaryWork()
    manager = manager_for(callback, summary_work=work)
    manager._summary_timeout = 1
    before = manager.history
    pending = asyncio.create_task(manager.compact(manager.begin_turn("cancel")))
    try:
        assert await asyncio.to_thread(entered.wait, 0.5)
        pending.cancel()
        with pytest.raises(asyncio.CancelledError):
            await pending
        assert work.state == "draining"
        with pytest.raises(ContextError, match="busy"):
            await manager.compact(manager.begin_turn("retry"))
        assert manager.history == before
    finally:
        running = work._task
        release.set()
        if running is not None:
            await asyncio.gather(running, return_exceptions=True)


@pytest.mark.asyncio
async def test_sync_work_queued_past_deadline_never_invokes_callback():
    from concurrent.futures import ThreadPoolExecutor
    from zara.agent.context import SummaryWork

    loop = asyncio.get_running_loop()
    loop.set_default_executor(ThreadPoolExecutor(max_workers=1))
    release = threading.Event()
    occupied = loop.run_in_executor(None, release.wait)
    calls = []
    work = SummaryWork()
    manager = manager_for(lambda messages, budget: calls.append(1) or "late", summary_work=work)
    try:
        with pytest.raises(TimeoutError):
            await manager.compact(manager.begin_turn("queued"))
        running = work._task
    finally:
        release.set()
        await occupied
    if running is not None:
        await asyncio.gather(running, return_exceptions=True)
    assert calls == []


@pytest.mark.asyncio
async def test_shutdown_cancelled_queued_wrapper_releases_slot_after_executor_drains():
    from concurrent.futures import ThreadPoolExecutor
    from zara.agent.context import SummaryWork

    loop = asyncio.get_running_loop()
    loop.set_default_executor(ThreadPoolExecutor(max_workers=1))
    release = threading.Event()
    occupied = loop.run_in_executor(None, release.wait)
    calls = []
    work = SummaryWork()
    caller = asyncio.create_task(work.run(lambda *args: calls.append(1) or "value", (), 1, 1))
    try:
        while work._task is None:
            await asyncio.sleep(0)
        await asyncio.sleep(0)
        wrapper = work._task
        wrapper.cancel()
        await asyncio.gather(caller, return_exceptions=True)
        release.set()
        await occupied

        async def drain():
            while work.state != "idle":
                await asyncio.sleep(0.001)

        await asyncio.wait_for(drain(), timeout=0.5)
        assert calls == []
        assert await work.run(lambda *args: "recovered", (), 1, 1) == "recovered"
    finally:
        release.set()
        await occupied


@pytest.mark.asyncio
async def test_wrapper_cancelled_before_submission_releases_slot():
    from zara.agent.context import SummaryWork

    work = SummaryWork()
    calls = []
    caller = asyncio.create_task(work.run(lambda *args: calls.append(1), (), 1, 1))
    # run() creates the wrapper; this continuation cancels it before invoke().
    await asyncio.sleep(0)
    work._task.cancel()
    await asyncio.gather(caller, return_exceptions=True)
    assert work.state == "idle"
    assert calls == []
    assert await work.run(lambda *args: "recovered", (), 1, 1) == "recovered"


@pytest.mark.asyncio
async def test_sync_summary_preserves_provider_context():
    from contextvars import ContextVar
    from zara.agent.context import SummaryWork

    marker = ContextVar("summary-test", default=None)
    token = marker.set("request-owner")
    try:
        assert await SummaryWork().run(lambda *args: marker.get(), (), 1, 1) == "request-owner"
    finally:
        marker.reset(token)


@pytest.mark.asyncio
async def test_shutdown_cancelled_running_wrapper_keeps_slot_reserved():
    from zara.agent.context import SummaryWork

    entered, release = threading.Event(), threading.Event()
    work = SummaryWork()

    def blocked(*args):
        entered.set()
        release.wait(1)
        return "late"

    caller = asyncio.create_task(work.run(blocked, (), 1, 1))
    try:
        while not entered.is_set():
            await asyncio.sleep(0.001)
        work._task.cancel()
        await asyncio.gather(caller, return_exceptions=True)
        assert work.state == "draining"
        with pytest.raises(ContextError, match="busy"):
            await work.run(lambda *args: "overlap", (), 1, 1)
    finally:
        release.set()

    async def drain():
        while work.state != "idle":
            await asyncio.sleep(0.001)

    await asyncio.wait_for(drain(), timeout=0.5)
