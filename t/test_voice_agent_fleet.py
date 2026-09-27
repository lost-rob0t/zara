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
