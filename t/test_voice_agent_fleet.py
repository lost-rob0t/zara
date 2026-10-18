"""Voice fleet contracts: conversation must remain independent of durable work."""

import asyncio

from zara.database import DatabaseManager
from zara.tasks.runner import TaskRunner
from zara.tasks.store import TaskStatus, TaskStore


def test_fleet_queues_new_requests_and_stops_queued_and_running_work(tmp_path):
    async def scenario():
        store = TaskStore(DatabaseManager(tmp_path / "fleet.db"))
        started = asyncio.Event()
        release = asyncio.Event()
        calls = []
        cancelled = []
        counter = 0

        async def allocate():
            nonlocal counter
            counter += 1
            return f"turn-{counter}"

        async def submit(text, **kwargs):
            calls.append(kwargs["turn_id"])
            started.set()
            await release.wait()
            return type("Result", (), {"response": "TASK_COMPLETE: done"})()

        async def cancel(turn_id):
            cancelled.append(turn_id)

        runner = TaskRunner(
            store=store,
            submit_turn=submit,
            allocate_turn_id=allocate,
            cancel_turn=cancel,
            publisher=lambda event: event,
            principal_id="owner",
            max_concurrent=1,
        )
        await runner.start()
        try:
            first = await runner.enqueue_task(goal="write a report")
            await asyncio.wait_for(started.wait(), 1)
            second = await runner.enqueue_task(goal="review another project")
            assert runner.get_task(first.task_id).status is TaskStatus.RUNNING
            assert runner.get_task(second.task_id).status is TaskStatus.PENDING
            await runner.cancel_all(reason="user_stop_all")
            assert runner.get_task(first.task_id).status is TaskStatus.CANCELLED
            assert runner.get_task(second.task_id).status is TaskStatus.CANCELLED
            release.set()
            await asyncio.sleep(0)
            assert calls == ["turn-1"]
            assert cancelled == ["turn-1"]
        finally:
            await runner.stop()

    asyncio.run(scenario())


def make_runner(tmp_path, submit, *, cancel=None, principal="owner", **limits):
    store = TaskStore(DatabaseManager(tmp_path / "fleet.db"))
    counter = 0
    cancelled = []

    async def allocate():
        nonlocal counter
        counter += 1
        return f"turn-{counter}"

    async def cancel_turn(turn_id):
        cancelled.append(turn_id)
        if cancel is not None:
            await cancel(turn_id)

    runner = TaskRunner(
        store=store, submit_turn=submit, allocate_turn_id=allocate,
        cancel_turn=cancel_turn, publisher=lambda event: event,
        principal_id=principal, **limits,
    )
    return runner, store, cancelled


def completed():
    return type("Result", (), {"response": "TASK_COMPLETE: done"})()


def test_queued_work_drains_fifo_without_blocking_conversation(tmp_path):
    async def scenario():
        release = asyncio.Event()
        calls = []

        async def submit(text, **kwargs):
            calls.append(kwargs["system_context"])
            await release.wait()
            return completed()

        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=1)
        await runner.start()
        try:
            tasks = [await runner.enqueue_task(goal=goal) for goal in ("first", "second", "third")]
            await asyncio.sleep(0)
            assert len(calls) == 1
            assert [runner.get_task(t.task_id).status for t in tasks] == [
                TaskStatus.RUNNING, TaskStatus.PENDING, TaskStatus.PENDING,
            ]
            release.set()
            for task in tasks:
                await runner.wait_for_task(task.task_id, timeout=1)
            assert len(calls) == 3
            assert all(runner.get_task(t.task_id).status is TaskStatus.COMPLETED for t in tasks)
            assert all(f"Goal: {goal}" in call for goal, call in zip(("first", "second", "third"), calls))
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_stop_all_fences_admission_during_slow_cancel(tmp_path):
    import pytest
    from zara.tasks.runner import TaskRunnerError

    async def scenario():
        entered = asyncio.Event()
        release = asyncio.Event()
        running = asyncio.Event()

        async def submit(text, **kwargs):
            running.set()
            await asyncio.Event().wait()

        async def cancel(turn_id):
            entered.set()
            await release.wait()

        runner, _, _ = make_runner(tmp_path, submit, cancel=cancel)
        await runner.start()
        try:
            await runner.enqueue_task(goal="first")
            await asyncio.wait_for(running.wait(), 1)
            stopping = asyncio.create_task(runner.cancel_all(reason="user_stop_all"))
            await asyncio.wait_for(entered.wait(), 1)
            with pytest.raises(TaskRunnerError):
                await runner.enqueue_task(goal="must not escape the stop")
            release.set()
            await asyncio.wait_for(stopping, 1)
            assert all(task.status is TaskStatus.CANCELLED for task in runner.list_tasks())
        finally:
            release.set()
            await runner.stop()
    asyncio.run(scenario())


def test_pause_and_explicit_resume_keep_identity(tmp_path):
    async def scenario():
        entered = asyncio.Event()
        calls = []

        async def submit(text, **kwargs):
            calls.append(kwargs["turn_id"])
            entered.set()
            if len(calls) == 1:
                await asyncio.Event().wait()
            return completed()

        runner, _, cancelled = make_runner(tmp_path, submit)
        await runner.start()
        try:
            task = await runner.enqueue_task(goal="keep my work")
            await asyncio.wait_for(entered.wait(), 1)
            paused = await runner.pause_task(task_id=task.task_id)
            assert paused.status is TaskStatus.INTERRUPTED
            assert paused.reason == "user_pause"
            await asyncio.sleep(0)
            assert len(calls) == 1
            resumed = await runner.resume_task(task_id=task.task_id)
            await runner.wait_for_task(task.task_id, timeout=1)
            assert resumed.task_id == task.task_id
            assert runner.get_task(task.task_id).status is TaskStatus.COMPLETED
            assert cancelled == ["turn-1"]
            assert calls == ["turn-1", "turn-2"]
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_cancel_is_idempotent_and_does_not_resurrect(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, cancelled = make_runner(tmp_path, submit)
        await runner.start()
        try:
            task = await runner.enqueue_task(goal="cancel once")
            await asyncio.sleep(0)
            await runner.cancel_task(task_id=task.task_id)
            again = await runner.cancel_task(task_id=task.task_id)
            assert again.status is TaskStatus.CANCELLED
            assert len(cancelled) == 1
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_queue_is_bounded_without_creating_orphan_rows(tmp_path):
    import pytest
    from zara.tasks.runner import TaskLimitError

    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=1, max_queued=1)
        await runner.start()
        try:
            await runner.enqueue_task(goal="running")
            await runner.enqueue_task(goal="queued")
            with pytest.raises(TaskLimitError):
                await runner.enqueue_task(goal="overflow")
            assert len(runner.list_tasks()) == 2
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_shutdown_invalidates_active_turns_and_retains_queued_work(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, cancelled = make_runner(tmp_path, submit, max_concurrent=1)
        await runner.start()
        first = await runner.enqueue_task(goal="running")
        second = await runner.enqueue_task(goal="queued")
        await asyncio.sleep(0)
        await runner.stop()
        assert cancelled == ["turn-1"]
        assert runner.get_task(first.task_id).status is TaskStatus.INTERRUPTED
        assert runner.get_task(second.task_id).status is TaskStatus.INTERRUPTED
    asyncio.run(scenario())


def test_recovery_does_not_interrupt_another_principal(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            return completed()
        runner, store, _ = make_runner(tmp_path, submit)
        other = store.create_task(principal_id="other", goal="other user's job", max_task_steps=2)
        store.transition(other.task_id, principal_id="other", status=TaskStatus.RUNNING)
        await runner.start()
        try:
            assert store.get_task(other.task_id, principal_id="other").status is TaskStatus.RUNNING
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_subagents_are_joined_with_one_slot_and_results_reach_parent(tmp_path):
    async def scenario():
        calls = []
        child_ids = []

        async def submit(text, **kwargs):
            calls.append(kwargs["system_context"])
            if len(calls) == 1:
                child = await runner.delegate_task(goal="verify the report")
                child_ids.append(child.task_id)
                return completed()
            return completed()

        runner, store, _ = make_runner(tmp_path, submit, max_concurrent=1)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="write the report")
            await runner.wait_for_task(root.task_id, timeout=1)
            assert len(calls) == 3
            child = runner.get_task(child_ids[0])
            assert child.parent_task_id == root.task_id
            assert child.root_task_id == root.task_id
            assert child.status is TaskStatus.COMPLETED
            assert child.task_id in calls[-1]
            assert runner.get_task(root.task_id).status is TaskStatus.COMPLETED
            restored = TaskStore(DatabaseManager(tmp_path / "fleet.db"))
            assert restored.get_task(child.task_id, principal_id="owner").parent_task_id == root.task_id
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_parent_cancel_stops_descendants_without_cancelling_other_roots(tmp_path):
    async def scenario():
        child_started = asyncio.Event()
        child_ids = []
        parent_id = None

        async def submit(text, **kwargs):
            if parent_id in text:
                child = await runner.delegate_task(goal="child work")
                child_ids.append(child.task_id)
                return completed()
            child_started.set()
            await asyncio.Event().wait()

        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=2)
        await runner.start()
        try:
            parent = await runner.enqueue_task(goal="parent work")
            parent_id = parent.task_id
            await asyncio.wait_for(child_started.wait(), 1)
            other = await runner.enqueue_task(goal="independent work")
            await runner.cancel_task(task_id=parent.task_id)
            assert runner.get_task(parent.task_id).status is TaskStatus.CANCELLED
            assert runner.get_task(child_ids[0]).status is TaskStatus.CANCELLED
            assert runner.get_task(other.task_id).status in {TaskStatus.PENDING, TaskStatus.RUNNING}
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_agent_cannot_escape_parent_by_creating_a_root_or_stopping_fleet(tmp_path):
    import pytest
    from zara.tasks.runner import TaskRunnerError

    async def scenario():
        child_ids = []
        calls = 0

        async def submit(text, **kwargs):
            nonlocal calls
            calls += 1
            if calls == 1:
                child = await runner.create_task(goal="requested as root by a model")
                child_ids.append(child.task_id)
                with pytest.raises(TaskRunnerError):
                    await runner.cancel_all()
            return completed()

        runner, _, _ = make_runner(tmp_path, submit)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="root")
            await runner.wait_for_task(root.task_id, timeout=1)
            assert runner.get_task(child_ids[0]).parent_task_id == root.task_id
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_subagents_share_root_step_budget(tmp_path):
    async def scenario():
        calls = []

        async def submit(text, **kwargs):
            calls.append(text)
            if len(calls) == 1:
                await runner.delegate_task(goal="child", max_task_steps=20)
                return completed()
            return type("Result", (), {"response": "working"})()

        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=1)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="root", max_task_steps=2)
            await runner.wait_for_task(root.task_id, timeout=1)
            assert len(calls) == 2
            assert runner.get_task(root.task_id).status is TaskStatus.FAILED
            assert all(task.status not in {TaskStatus.RUNNING, TaskStatus.PENDING} for task in runner.list_tasks())
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_repeating_no_progress_stops_without_exhausting_full_budget(tmp_path):
    async def scenario():
        calls = []
        async def submit(text, **kwargs):
            calls.append(text)
            return type("Result", (), {"response": "still thinking"})()
        runner, _, _ = make_runner(tmp_path, submit)
        await runner.start()
        try:
            task = await runner.enqueue_task(goal="make actual progress", max_task_steps=20)
            await runner.wait_for_task(task.task_id, timeout=1)
            assert runner.get_task(task.task_id).reason == "no_progress"
            assert len(calls) == 3
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_deadline_revokes_active_turn(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, cancelled = make_runner(tmp_path, submit, wall_clock_seconds=0.02)
        await runner.start()
        try:
            task = await runner.enqueue_task(goal="bounded work")
            await runner.wait_for_task(task.task_id, timeout=1)
            assert runner.get_task(task.task_id).reason == "wall_clock_exceeded"
            assert cancelled == ["turn-1"]
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_paused_subtree_resumes_without_orphaning_children(tmp_path):
    async def scenario():
        child_entered = asyncio.Event()
        child_calls = 0
        root_id = None
        delegated = False
        async def submit(text, **kwargs):
            nonlocal child_calls, delegated
            if root_id in text:
                if not delegated:
                    delegated = True
                    await runner.delegate_task(goal="child")
                return completed()
            child_calls += 1
            child_entered.set()
            if child_calls == 1:
                await asyncio.Event().wait()
            return completed()
        runner, _, _ = make_runner(tmp_path, submit, max_concurrent=1)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="root")
            root_id = root.task_id
            await asyncio.wait_for(child_entered.wait(), 1)
            await runner.pause_task(task_id=root_id)
            assert all(task.status is TaskStatus.INTERRUPTED for task in runner.list_tasks())
            await runner.resume_task(task_id=root_id)
            await runner.wait_for_task(root_id, timeout=1)
            assert all(task.status is TaskStatus.COMPLETED for task in runner.list_tasks())
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_resume_does_not_reset_deadline_or_attempt_budget(tmp_path, monkeypatch):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, _ = make_runner(tmp_path, submit, wall_clock_seconds=60)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="bounded", max_task_steps=1)
            await asyncio.sleep(0)
            await runner.pause_task(task_id=root.task_id)
            paused = runner.get_task(root.task_id)
            assert paused.attempts_started == 1
            assert paused.deadline_at is not None
            await runner.resume_task(task_id=root.task_id)
            await runner.wait_for_task(root.task_id, timeout=1)
            assert runner.get_task(root.task_id).reason == "step_budget_exhausted"
            assert runner.get_task(root.task_id).deadline_at == paused.deadline_at
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_completion_requires_exact_sentinel_not_a_prefix(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            return type("Result", (), {"response": "TASK_COMPLETELY_BROKEN"})()
        runner, _, _ = make_runner(tmp_path, submit)
        await runner.start()
        try:
            task = await runner.enqueue_task(goal="real completion", max_task_steps=1)
            await runner.wait_for_task(task.task_id, timeout=1)
            assert runner.get_task(task.task_id).status is TaskStatus.FAILED
        finally:
            await runner.stop()
    asyncio.run(scenario())


def test_stale_store_transition_cannot_overwrite_a_stop(tmp_path, monkeypatch):
    import pytest
    from zara.tasks.store import TaskTransitionError
    store = TaskStore(DatabaseManager(tmp_path / "race.db"))
    task = store.create_task(principal_id="owner", goal="work", max_task_steps=2)
    store.transition(task.task_id, principal_id="owner", status=TaskStatus.RUNNING)
    original = store.get_task
    injected = False
    def racing_get(task_id, *, principal_id):
        nonlocal injected
        row = original(task_id, principal_id=principal_id)
        if not injected:
            injected = True
            store._db.execute("UPDATE agent_tasks SET status='cancelled' WHERE task_id=?", (task_id,))
        return row
    monkeypatch.setattr(store, "get_task", racing_get)
    with pytest.raises(TaskTransitionError):
        store.transition(task.task_id, principal_id="owner", status=TaskStatus.COMPLETED)
    assert original(task.task_id, principal_id="owner").status is TaskStatus.CANCELLED


def test_stop_tree_includes_persisted_descendants_after_depth_limit_is_lowered(tmp_path):
    async def scenario():
        async def submit(text, **kwargs):
            await asyncio.Event().wait()
        runner, _, _ = make_runner(tmp_path, submit, max_depth=3)
        await runner.start()
        try:
            root = await runner.enqueue_task(goal="root")
            parent = root
            family_ids = [root.task_id]
            for index in range(3):
                child = runner._store.create_task(
                    principal_id="owner", goal=f"child-{index}", max_task_steps=20,
                    parent_task_id=parent.task_id,
                )
                parent = child
                family_ids.append(child.task_id)
            original_list = runner.list_tasks
            runner.list_tasks = lambda statuses=None: sorted(
                original_list(statuses), key=lambda task: family_ids.index(task.task_id), reverse=True,
            )
            runner._max_depth = 1
            await runner.cancel_task(task_id=root.task_id)
            assert all(task.status is TaskStatus.CANCELLED for task in runner.list_tasks())
        finally:
            await runner.stop()
    asyncio.run(scenario())
