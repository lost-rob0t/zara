from datetime import datetime, timezone

import pytest

from zara.database import DatabaseManager
from zara.tasks.schedule import (
    CronExpression,
    ScheduleMode,
    ScheduleState,
    ScheduledTaskService,
    ScheduledTaskStore,
)


def utc(year, month, day, hour, minute):
    return datetime(year, month, day, hour, minute, tzinfo=timezone.utc)


def build_store(tmp_path):
    return ScheduledTaskStore(DatabaseManager(tmp_path / "zara.db"))


class FakeProlog:
    def __init__(self, *, route="prolog", command_ok=True, definitions=()):
        self.route = route
        self.command_ok = command_ok
        self.definitions = list(definitions)
        self.commands = []

    def scheduled_execution_route(self, goal):
        return self.route

    def execute_command(self, goal):
        self.commands.append(goal)
        return self.command_ok

    def scheduled_task_definitions(self):
        return list(self.definitions)


class FakeTaskRunner:
    def __init__(self):
        self.goals = []

    async def create_task(self, *, goal, max_task_steps=None):
        self.goals.append(goal)
        return type("Task", (), {"task_id": f"task-{len(self.goals)}"})()


class TestCron:
    def test_parse_and_match_standard_five_field_expression(self):
        cron = CronExpression.parse("*/15 9-17 * * 1-5")
        assert cron.matches(utc(2026, 9, 17, 9, 30))
        assert not cron.matches(utc(2026, 9, 19, 9, 30))
        assert not cron.matches(utc(2026, 9, 17, 9, 31))

    def test_next_after_is_strictly_future_and_preserves_timezone(self):
        cron = CronExpression.parse("0 * * * *")
        next_run = cron.next_after(utc(2026, 9, 17, 9, 0))
        assert next_run == utc(2026, 9, 17, 10, 0)
        assert next_run.tzinfo is timezone.utc

    @pytest.mark.parametrize("expression", ["", "* * * *", "61 * * * *", "*/0 * * * *"])
    def test_invalid_cron_is_rejected(self, expression):
        with pytest.raises(ValueError, match="cron"):
            CronExpression.parse(expression)


class TestStore:
    def test_schedule_identity_roundtrips_and_is_principal_scoped(self, tmp_path):
        store = build_store(tmp_path)
        created = store.create_schedule(
            principal_id="principal-1",
            cron="0 9 * * 1-5",
            goal="open firefox",
            mode="auto",
            label="Morning browser",
            schedule_id="schedule-fixed",
            now=utc(2026, 9, 17, 1, 0),
        )
        assert created.schedule_id == "schedule-fixed"
        assert created.state is ScheduleState.ACTIVE
        assert created.mode is ScheduleMode.AUTO
        assert store.get_schedule("schedule-fixed", principal_id="principal-1") == created
        assert store.get_schedule("schedule-fixed", principal_id="principal-2") is None

    def test_claim_due_is_idempotent_for_same_cron_slot(self, tmp_path):
        store = build_store(tmp_path)
        schedule = store.create_schedule(
            principal_id="p",
            cron="* * * * *",
            goal="hello",
            mode="auto",
            now=utc(2026, 9, 17, 1, 0),
        )
        due = utc(2026, 9, 17, 1, 1)
        next_run = utc(2026, 9, 17, 1, 2)
        assert store.claim_run(schedule, due_at=due, next_run_at=next_run)
        assert not store.claim_run(schedule, due_at=due, next_run_at=next_run)

    def test_pause_resume_cancel_are_explicit_states(self, tmp_path):
        store = build_store(tmp_path)
        schedule = store.create_schedule(
            principal_id="p",
            cron="0 * * * *",
            goal="hello",
            mode="auto",
            now=utc(2026, 9, 17, 1, 0),
        )
        assert store.pause(schedule.schedule_id, principal_id="p").state is ScheduleState.PAUSED
        resumed = store.resume(
            schedule.schedule_id,
            principal_id="p",
            now=utc(2026, 9, 17, 2, 0),
        )
        assert resumed.state is ScheduleState.ACTIVE
        assert store.cancel(schedule.schedule_id, principal_id="p").state is ScheduleState.CANCELLED


@pytest.mark.asyncio
async def test_short_auto_task_runs_prolog_without_llm(tmp_path):
    prolog = FakeProlog(route="prolog", command_ok=True)
    runner = FakeTaskRunner()
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=runner,
        prolog_engine=prolog,
        principal_id="p",
        now=lambda: utc(2026, 9, 17, 1, 1),
    )
    row = await service.create_schedule(cron="* * * * *", goal="open firefox")

    await service.run_due(now=utc(2026, 9, 17, 1, 2))

    assert prolog.commands == ["open firefox"]
    assert runner.goals == []
    current = service.get_schedule(row.schedule_id)
    assert current.last_status == "completed"
    assert current.last_route == "prolog"


@pytest.mark.asyncio
async def test_long_task_uses_llm_without_running_prolog_command(tmp_path):
    prolog = FakeProlog(route="llm")
    runner = FakeTaskRunner()
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=runner,
        prolog_engine=prolog,
        principal_id="p",
        now=lambda: utc(2026, 9, 17, 1, 1),
    )
    row = await service.create_schedule(cron="* * * * *", goal="research the release")

    await service.run_due(now=utc(2026, 9, 17, 1, 2))

    assert prolog.commands == []
    assert runner.goals == ["research the release"]
    current = service.get_schedule(row.schedule_id)
    assert current.last_status == "delegated"
    assert current.last_route == "llm"
    assert current.last_task_id == "task-1"


@pytest.mark.asyncio
async def test_failed_prolog_resolution_escalates_to_llm(tmp_path):
    prolog = FakeProlog(route="prolog", command_ok=False)
    runner = FakeTaskRunner()
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=runner,
        prolog_engine=prolog,
        principal_id="p",
        now=lambda: utc(2026, 9, 17, 1, 1),
    )
    await service.create_schedule(cron="* * * * *", goal="do the thing")

    await service.run_due(now=utc(2026, 9, 17, 1, 2))

    assert prolog.commands == ["do the thing"]
    assert runner.goals == ["do the thing"]


@pytest.mark.asyncio
async def test_prolog_definitions_are_materialized_with_stable_identity(tmp_path):
    prolog = FakeProlog(
        definitions=[
            {
                "id": "morning",
                "cron": "0 9 * * *",
                "goal": "open firefox",
                "mode": "auto",
            }
        ]
    )
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=FakeTaskRunner(),
        prolog_engine=prolog,
        principal_id="p",
        now=lambda: utc(2026, 9, 17, 1, 0),
    )

    await service.sync_prolog_definitions()
    first = service.get_schedule("prolog:morning")
    await service.sync_prolog_definitions()
    second = service.get_schedule("prolog:morning")

    assert first.schedule_id == second.schedule_id == "prolog:morning"
    assert len(service.list_schedules()) == 1
