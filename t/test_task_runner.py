import asyncio

import pytest

from zara.database import DatabaseManager
from zara.runtime import events
from zara.tasks.runner import TaskLimitError, TaskRunner
from zara.tasks.store import TaskStatus, TaskStore


GOAL = "write the quarterly report about rocket engines"


class Recorder:
    def __init__(self) -> None:
        self.events: list[events.RuntimeEvent] = []

    def __call__(self, event: events.RuntimeEvent):
        self.events.append(event)
        return event


class ScriptedSubmit:
    """Deterministic stub for the backend submit path."""

    def __init__(self, responses, *, delay=0.0, error=None, gate=None):
        self.responses = list(responses)
        self.calls: list[dict] = []
        self.delay = delay
        self.error = error
        self.gate = gate

    async def __call__(
        self, text, *, turn_id, conversation_id, system_context, latency_trace
    ):
        self.calls.append(
            {
                "text": text,
                "turn_id": turn_id,
                "conversation_id": conversation_id,
                "system_context": system_context,
                "latency_trace": latency_trace,
            }
        )
        if self.gate is not None:
            await self.gate.wait()
        if self.delay:
            await asyncio.sleep(self.delay)
        if self.error is not None:
            raise self.error
        if self.responses:
            response = self.responses.pop(0)
        else:
            response = "TASK_COMPLETE: nothing left to do"
        if isinstance(response, Exception):
            raise response
        return type("TurnResult", (), {"response": response, "tool_results": []})()


class TaskHarness:
    def __init__(self, store, submit, runner, recorder) -> None:
        self.store = store
        self.submit = submit
        self.runner = runner
        self.recorder = recorder

    async def stop(self):
        await self.runner.stop()


def build_harness(
    responses,
    *,
    store,
    principal="principal-1",
    max_concurrent=2,
    max_task_steps=4,
    wall_clock_seconds=None,
    step_log_chars=256,
    gate=None,
    delay=0.0,
    error=None,
):
    submit = ScriptedSubmit(responses, delay=delay, error=error, gate=gate)
    recorder = Recorder()
    counter = {"count": 0}

    async def allocate_turn_id():
        counter["count"] += 1
        return f"turn-{counter['count']:04d}"

    async def cancel_turn(turn_id):
        runner.test_cancelled_turns.append(turn_id)

    runner = TaskRunner(
        store=store,
        submit_turn=submit,
        allocate_turn_id=allocate_turn_id,
        cancel_turn=cancel_turn,
        publisher=recorder,
        principal_id=principal,
        max_concurrent=max_concurrent,
        default_max_task_steps=max_task_steps,
        wall_clock_seconds=wall_clock_seconds,
        step_log_chars=step_log_chars,
    )
    runner.test_cancelled_turns = []
    return TaskHarness(store, submit, runner, recorder)


def make_store(tmp_path, step_log_chars=256):
    return TaskStore(DatabaseManager(tmp_path / "zara.db"), step_log_chars=step_log_chars)


class TestStepExecution:
    def test_single_step_task_completes_on_sentinel(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(["TASK_COMPLETE: report ready"], store=store)

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await harness.runner.wait_for_task(task.task_id)
                return task
            finally:
                await harness.runner.stop()

        task = asyncio.run(scenario())

        final = store.get_task(task.task_id, principal_id="principal-1")
        assert final.status is TaskStatus.COMPLETED
        assert final.steps_completed == 1
        steps = store.list_steps(task.task_id, principal_id="principal-1")
        assert [step.summary for step in steps] == ["report ready"]

        started = [
            event
            for event in harness.recorder.events
            if isinstance(event, events.TaskStarted)
        ]
        completed = [
            event
            for event in harness.recorder.events
            if isinstance(event, events.TaskCompleted)
        ]
        assert [event.task_id for event in started] == [task.task_id]
        assert [event.task_id for event in completed] == [task.task_id]

    def test_multi_step_task_carries_compacted_context(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(
            ["partial draft finished", "TASK_COMPLETE: all sections done"],
            store=store,
        )

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        task = asyncio.run(scenario())

        assert len(harness.submit.calls) == 2
        first, second = harness.submit.calls
        assert "(no completed steps yet)" in first["system_context"]
        assert GOAL in first["system_context"]
        assert "step 1: partial draft finished" in second["system_context"]
        assert GOAL in second["system_context"]
        assert first["conversation_id"] is None
        for call in harness.submit.calls:
            assert GOAL not in call["text"]

        final = store.get_task(task.task_id, principal_id="principal-1")
        assert final.status is TaskStatus.COMPLETED
        assert final.steps_completed == 2

    def test_step_turn_uses_allocated_turn_id_and_trace(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(["TASK_COMPLETE: done"], store=store)

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return harness.submit.calls[0]

        call = asyncio.run(scenario())
        assert call["turn_id"] == "turn-0001"
        assert call["latency_trace"].trace_id == call["turn_id"]

    def test_step_prompt_asks_for_sentinel(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(["nope"], store=store)

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL, max_task_steps=1)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return harness.submit.calls[0]["text"]

        prompt = asyncio.run(scenario())
        assert "TASK_COMPLETE" in prompt

    def test_step_budget_exhaustion_fails_task(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(["step one", "step two"], store=store)

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL, max_task_steps=2)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        task = asyncio.run(scenario())

        final = store.get_task(task.task_id, principal_id="principal-1")
        assert final.status is TaskStatus.FAILED
        assert final.reason == "step_budget_exhausted"
        assert final.steps_completed == 2
        failures = [
            event
            for event in harness.recorder.events
            if isinstance(event, events.TaskFailed)
        ]
        assert [event.task_id for event in failures] == [task.task_id]
        assert failures[0].reason == "step_budget_exhausted"

    def test_wall_clock_budget_fails_task(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness(
            ["slow"] * 50, store=store, delay=0.05, wall_clock_seconds=0.12
        )

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL, max_task_steps=50)
                await harness.runner.wait_for_task(task.task_id, timeout=10)
            finally:
                await harness.runner.stop()
            return task

        task = asyncio.run(scenario())
        final = store.get_task(task.task_id, principal_id="principal-1")
        assert final.status is TaskStatus.FAILED
        assert final.reason == "wall_clock_exceeded"

    def test_crashing_step_fails_task_and_runner_survives(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness([], store=store, error=RuntimeError("boom"))

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL, max_task_steps=1)
                await harness.runner.wait_for_task(task.task_id)
                final = store.get_task(task.task_id, principal_id="principal-1")

                recovery = ScriptedSubmit(["TASK_COMPLETE: recovered"])
                harness.runner._submit_turn = recovery
                second = await harness.runner.create_task(goal="another goal")
                await harness.runner.wait_for_task(second.task_id)
                return (
                    final,
                    store.get_task(second.task_id, principal_id="principal-1"),
                )
            finally:
                await harness.runner.stop()

        final, second = asyncio.run(scenario())
        assert final.status is TaskStatus.FAILED
        assert final.reason == "step_error"
        assert second.status is TaskStatus.COMPLETED

    def test_step_summary_truncated_to_step_log_bound(self, tmp_path):
        store = make_store(tmp_path, step_log_chars=64)
        harness = build_harness(
            ["TASK_COMPLETE: " + "y" * 900], store=store, step_log_chars=64
        )

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        task = asyncio.run(scenario())
        steps = store.list_steps(task.task_id, principal_id="principal-1")
        assert len(steps) == 1
        assert 0 < len(steps[0].summary) <= 64


class TestConcurrency:
    def test_overflow_rejected_not_queued(self, tmp_path):
        store = make_store(tmp_path)
        gate = asyncio.Event()
        harness = build_harness([], store=store, gate=gate, max_concurrent=1)

        async def scenario():
            await harness.runner.start()
            try:
                first = await harness.runner.create_task(goal=GOAL)
                with pytest.raises(TaskLimitError):
                    await harness.runner.create_task(goal="second goal")
                assert (
                    store.list_tasks(
                        principal_id="principal-1", statuses=[TaskStatus.PENDING]
                    )
                    == []
                )
                gate.set()
                await harness.runner.wait_for_task(first.task_id)
                third = await harness.runner.create_task(goal="third goal")
                await harness.runner.wait_for_task(third.task_id)
            finally:
                await harness.runner.stop()

        asyncio.run(scenario())
        rows = store.list_tasks(principal_id="principal-1")
        assert {row.status for row in rows} == {TaskStatus.COMPLETED}
