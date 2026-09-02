import asyncio
import logging

import pytest

from zara.tasks.runner import REASONS
from zara.tasks.store import TaskStatus

from test_task_runner import GOAL, build_harness, make_store


SECRET_GOAL = "classified goal ZQX-SECRET-PAYLOAD-734: build the orbital calculator"


class TestObservability:
    def test_logs_never_contain_goal_text(self, tmp_path, caplog):
        store = make_store(tmp_path)
        harness = build_harness(
            ["drafted section one", "TASK_COMPLETE: report ready"],
            store=store,
        )

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=SECRET_GOAL)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        with caplog.at_level(logging.DEBUG, logger="zara.agent"):
            with caplog.at_level(logging.DEBUG, logger="zara.agent.graph"):
                with caplog.at_level(logging.DEBUG, logger="zara.tasks"):
                    asyncio.run(scenario())

        assert "ZQX-SECRET-PAYLOAD-734" not in caplog.text
        assert "orbital calculator" not in caplog.text

    def test_step_boundary_logs_carry_ids_status_and_lengths(self, tmp_path, caplog):
        store = make_store(tmp_path)
        harness = build_harness(["TASK_COMPLETE: done"], store=store)

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(goal=GOAL)
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        with caplog.at_level(logging.INFO, logger="zara.tasks"):
            task = asyncio.run(scenario())

        task_records = [
            record.getMessage()
            for record in caplog.records
            if record.name == "zara.tasks.runner"
        ]
        assert any(
            task.task_id in message and "started" in message
            for message in task_records
        )
        assert any(
            task.task_id in message
            and "step=0" in message
            and "completed" in message
            and "turn=" in message
            and "response_len=" in message
            for message in task_records
        )

    def test_failure_reasons_come_from_bounded_vocabulary(self):
        assert REASONS == frozenset(
            {
                "step_budget_exhausted",
                "wall_clock_exceeded",
                "step_error",
                "approval_timeout",
                "approval_rejected",
                "runtime_shutdown",
            }
        )

    def test_failure_log_records_reason_class_only(self, tmp_path, caplog):
        store = make_store(tmp_path)
        harness = build_harness([], store=store, error=RuntimeError("boom"))

        async def scenario():
            await harness.runner.start()
            try:
                task = await harness.runner.create_task(
                    goal=SECRET_GOAL, max_task_steps=1
                )
                await harness.runner.wait_for_task(task.task_id)
            finally:
                await harness.runner.stop()
            return task

        with caplog.at_level(logging.INFO, logger="zara.tasks"):
            asyncio.run(scenario())

        task_records = [
            record.getMessage()
            for record in caplog.records
            if record.name == "zara.tasks.runner"
        ]
        assert any("reason=step_error" in message for message in task_records)
        assert "ZQX-SECRET-PAYLOAD-734" not in caplog.text
