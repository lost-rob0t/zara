from datetime import datetime, timezone
from zoneinfo import ZoneInfo

import pytest

from zara.database import DatabaseManager
from zara.tasks.schedule import (
    IntervalExpression,
    ScheduleMode,
    ScheduledTaskService,
    ScheduledTaskStore,
    parse_schedule_expression,
)


def utc(year, month, day, hour, minute):
    return datetime(year, month, day, hour, minute, tzinfo=timezone.utc)


class FakeProlog:
    def scheduled_execution_route(self, goal):
        return "llm"

    def scheduled_task_definitions(self):
        return []


class FakeTaskRunner:
    def __init__(self):
        self.goals = []

    async def create_task(self, *, goal, max_task_steps=None):
        self.goals.append(goal)
        return type("Task", (), {"task_id": f"task-{len(self.goals)}"})()


def build_store(tmp_path):
    return ScheduledTaskStore(DatabaseManager(tmp_path / "zara.db"))


@pytest.mark.parametrize(
    ("expression", "minutes"),
    [
        ("@every 1h", 60),
        ("@every 6h", 360),
        ("@every 90m", 90),
        ("@every 2d", 2880),
    ],
)
def test_interval_expression_parses_bounded_human_duration(expression, minutes):
    parsed = IntervalExpression.parse(expression)

    assert parsed.expression == expression
    assert parsed.interval.total_seconds() == minutes * 60


@pytest.mark.parametrize(
    "expression",
    [
        "@every",
        "@every 0h",
        "@every -1h",
        "@every 1s",
        "@every 1.5h",
        "@every 999999d",
    ],
)
def test_invalid_interval_expression_fails_closed(expression):
    with pytest.raises(ValueError, match="interval"):
        parse_schedule_expression(expression)


def test_interval_next_after_is_elapsed_time_not_calendar_cron():
    parsed = parse_schedule_expression("@every 6h")
    start = utc(2026, 9, 18, 1, 15)

    assert parsed.next_after(start) == utc(2026, 9, 18, 7, 15)


@pytest.mark.parametrize(
    ("start", "expected"),
    [
        (
            datetime(2026, 3, 8, 1, 30, tzinfo=ZoneInfo("America/New_York")),
            datetime(2026, 3, 8, 4, 30, tzinfo=ZoneInfo("America/New_York")),
        ),
        (
            datetime(2026, 11, 1, 0, 30, tzinfo=ZoneInfo("America/New_York")),
            datetime(
                2026,
                11,
                1,
                1,
                30,
                tzinfo=ZoneInfo("America/New_York"),
                fold=1,
            ),
        ),
    ],
)
def test_interval_next_after_preserves_elapsed_time_across_dst(start, expected):
    parsed = parse_schedule_expression("@every 2h")

    actual = parsed.next_after(start)

    assert actual == expected
    assert (
        actual.astimezone(timezone.utc) - start.astimezone(timezone.utc)
        == parsed.interval
    )


@pytest.mark.asyncio
async def test_interval_schedule_skips_missed_slots_without_drift_or_catchup_storm(tmp_path):
    runner = FakeTaskRunner()
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=runner,
        prolog_engine=FakeProlog(),
        principal_id="p",
        now=lambda: utc(2026, 9, 18, 1, 0),
    )
    row = await service.create_schedule(
        cron="@every 6h",
        goal="generate my autonomous music playlist",
        mode=ScheduleMode.LLM,
    )

    assert row.next_run_at == "2026-09-18T07:00:00+00:00"

    started = await service.run_due(now=utc(2026, 9, 18, 20, 30))

    assert started == 1
    assert runner.goals == ["generate my autonomous music playlist"]
    current = service.get_schedule(row.schedule_id)
    assert current.next_run_at == "2026-09-19T01:00:00+00:00"
    assert current.last_run_at == "2026-09-18T07:00:00+00:00"


@pytest.mark.asyncio
async def test_interval_misfire_phase_uses_elapsed_time_across_dst(tmp_path):
    eastern = ZoneInfo("America/New_York")
    created = datetime(2026, 3, 8, 0, 30, tzinfo=eastern)
    runner = FakeTaskRunner()
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=runner,
        prolog_engine=FakeProlog(),
        principal_id="p",
        now=lambda: created,
    )
    row = await service.create_schedule(
        cron="@every 2h",
        goal="elapsed interval fixture",
        mode=ScheduleMode.LLM,
    )

    assert row.next_run_at == "2026-03-08T03:30:00-04:00"

    started = await service.run_due(
        now=datetime(2026, 3, 8, 6, 45, tzinfo=eastern)
    )

    assert started == 1
    assert runner.goals == ["elapsed interval fixture"]
    current = service.get_schedule(row.schedule_id)
    assert current.last_run_at == "2026-03-08T03:30:00-04:00"
    assert current.next_run_at == "2026-03-08T07:30:00-04:00"


@pytest.mark.asyncio
async def test_interval_schedule_keeps_existing_cron_api_compatible(tmp_path):
    service = ScheduledTaskService(
        store=build_store(tmp_path),
        task_runner=FakeTaskRunner(),
        prolog_engine=FakeProlog(),
        principal_id="p",
        now=lambda: utc(2026, 9, 18, 1, 0),
    )

    row = await service.create_schedule(
        cron="0 */6 * * *",
        goal="cron playlist",
        mode=ScheduleMode.LLM,
    )

    assert row.cron == "0 */6 * * *"
    assert row.next_run_at == "2026-09-18T06:00:00+00:00"
