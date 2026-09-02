import asyncio
import time

import pytest

from zara.runtime import events
from zara.tasks.store import TaskStatus

from test_task_runner import GOAL, build_harness, make_store


class ApprovalFlowSubmit:
    """Stub submit that emits controller-shaped approval events, then parks."""

    def __init__(self, responses, *, effective_publisher, gate=None) -> None:
        self.responses = list(responses)
        self.effective_publisher = effective_publisher
        self.gate = gate
        self.calls: list[dict] = []

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
        self.effective_publisher(
            events.ToolWaitingForUser(
                turn_id=turn_id,
                tool_run_id="approval-call-1",
                tool_name="approval_effect",
                label="approval_effect",
            )
        )
        if self.gate is not None:
            await self.gate.wait()
        if self.responses:
            response = self.responses.pop(0)
        else:
            response = "TASK_COMPLETE: nothing left to do"
        return type("TurnResult", (), {"response": response, "tool_results": []})()


async def wait_for_status(store, task_id, status, timeout=3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        task = store.get_task(task_id, principal_id="principal-1")
        if task is not None and task.status is status:
            return task
        await asyncio.sleep(0.005)
    raise AssertionError(f"task {task_id} did not reach {status} in time")


class TestApprovalPauseResume:
    def test_step_pauses_on_approval_and_resumes(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness([], store=store)
        effective = harness.runner.observing_publisher(harness.recorder)
        gate = asyncio.Event()
        stub = ApprovalFlowSubmit(
            ["TASK_COMPLETE: effect done"], effective_publisher=effective, gate=gate
        )
        harness.runner._submit_turn = stub

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await wait_for_status(store, task.task_id, TaskStatus.WAITING_APPROVAL)
                waiting_events = [
                    event
                    for event in harness.recorder.events
                    if isinstance(event, events.TaskWaitingApproval)
                ]
                assert [event.task_id for event in waiting_events] == [task.task_id]

                effective(
                    events.UserResponded(
                        turn_id=stub.calls[0]["turn_id"], label="approval_effect"
                    )
                )
                after_responded = store.get_task(
                    task.task_id, principal_id="principal-1"
                )
                gate.set()
                await harness.runner.wait_for_task(task.task_id)
                return (
                    after_responded,
                    store.get_task(task.task_id, principal_id="principal-1"),
                )
            finally:
                await harness.runner.stop()

        after_responded, final = asyncio.run(scenario())
        assert after_responded.status is TaskStatus.RUNNING
        assert final.status is TaskStatus.COMPLETED
        assert final.steps_completed == 1

    def test_waiting_approval_does_not_poll(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness([], store=store)
        effective = harness.runner.observing_publisher(harness.recorder)
        gate = asyncio.Event()
        stub = ApprovalFlowSubmit([], effective_publisher=effective, gate=gate)
        harness.runner._submit_turn = stub

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await wait_for_status(store, task.task_id, TaskStatus.WAITING_APPROVAL)
                calls_while_waiting = len(stub.calls)
                await asyncio.sleep(0.15)
                assert len(stub.calls) == calls_while_waiting
                current = store.get_task(task.task_id, principal_id="principal-1")
                assert current.status is TaskStatus.WAITING_APPROVAL
                gate.set()
                await harness.runner.wait_for_task(task.task_id)
                return store.get_task(task.task_id, principal_id="principal-1")
            finally:
                await harness.runner.stop()

        final = asyncio.run(scenario())
        assert final.status is TaskStatus.COMPLETED

    def test_approval_timeout_fails_task(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness([], store=store)
        effective = harness.runner.observing_publisher(harness.recorder)
        gate = asyncio.Event()
        stub = ApprovalFlowSubmit([], effective_publisher=effective, gate=gate)
        harness.runner._submit_turn = stub

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await wait_for_status(store, task.task_id, TaskStatus.WAITING_APPROVAL)
                turn_id = stub.calls[0]["turn_id"]
                effective(
                    events.ToolCancelled(
                        turn_id=turn_id,
                        tool_run_id="approval-call-1",
                        reason="approval timeout",
                    )
                )
                await harness.runner.wait_for_task(task.task_id, timeout=5)
                return store.get_task(task.task_id, principal_id="principal-1")
            finally:
                await harness.runner.stop()

        final = asyncio.run(scenario())
        assert final.status is TaskStatus.FAILED
        assert final.reason == "approval_timeout"
        assert harness.runner.test_cancelled_turns == [stub.calls[0]["turn_id"]]

    def test_rejected_approval_fails_task(self, tmp_path):
        store = make_store(tmp_path)
        harness = build_harness([], store=store)
        effective = harness.runner.observing_publisher(harness.recorder)
        gate = asyncio.Event()
        stub = ApprovalFlowSubmit([], effective_publisher=effective, gate=gate)
        harness.runner._submit_turn = stub

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await wait_for_status(store, task.task_id, TaskStatus.WAITING_APPROVAL)
                turn_id = stub.calls[0]["turn_id"]
                effective(
                    events.UserResponded(turn_id=turn_id, label="approval_effect")
                )
                effective(
                    events.ToolCancelled(
                        turn_id=turn_id,
                        tool_run_id="approval-call-1",
                        reason="tool rejected",
                    )
                )
                await harness.runner.wait_for_task(task.task_id, timeout=5)
                return store.get_task(task.task_id, principal_id="principal-1")
            finally:
                await harness.runner.stop()

        final = asyncio.run(scenario())
        assert final.status is TaskStatus.FAILED
        assert final.reason == "approval_rejected"
