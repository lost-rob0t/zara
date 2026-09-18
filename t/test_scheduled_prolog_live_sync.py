from datetime import datetime, timezone

import pytest

from zara.database import DatabaseManager
from zara.tasks.schedule import ScheduledTaskService, ScheduledTaskStore


def utc(year, month, day, hour, minute):
    return datetime(year, month, day, hour, minute, tzinfo=timezone.utc)


def build_service(tmp_path, prolog, clock):
    store = ScheduledTaskStore(DatabaseManager(tmp_path / "zara.db"))
    return ScheduledTaskService(
        store=store,
        task_runner=None,
        prolog_engine=prolog,
        principal_id="principal-live-sync",
        now=lambda: clock[0],
    )


class FakeProlog:
    def __init__(self):
        self.definitions = []
        self.commands = []

    def scheduled_task_definitions(self):
        return list(self.definitions)

    def scheduled_execution_route(self, goal):
        return "prolog"

    def execute_command(self, goal):
        self.commands.append(goal)
        return True


@pytest.mark.asyncio
async def test_unchanged_definition_does_not_rebase_next_run(tmp_path):
    clock = [utc(2026, 9, 17, 1, 0)]
    prolog = FakeProlog()
    prolog.definitions = [
        {
            "id": "live",
            "cron": "* * * * *",
            "goal": "open firefox",
            "mode": "auto",
        }
    ]
    service = build_service(tmp_path, prolog, clock)

    await service.sync_prolog_definitions()
    first = service.get_schedule("prolog:live")
    assert first is not None
    assert first.next_run_at == "2026-09-17T01:01:00+00:00"

    clock[0] = utc(2026, 9, 17, 2, 0)
    await service.sync_prolog_definitions()
    second = service.get_schedule("prolog:live")
    assert second is not None
    assert second.next_run_at == first.next_run_at
    assert second.updated_at == first.updated_at


@pytest.mark.asyncio
async def test_run_due_discovers_definition_added_after_startup_sync(tmp_path):
    clock = [utc(2026, 9, 17, 1, 0)]
    prolog = FakeProlog()
    service = build_service(tmp_path, prolog, clock)

    await service.sync_prolog_definitions()
    assert service.list_schedules() == []

    prolog.definitions = [
        {
            "id": "live",
            "cron": "* * * * *",
            "goal": "open firefox",
            "mode": "auto",
        }
    ]
    started = await service.run_due(now=utc(2026, 9, 17, 1, 1))

    assert started == 1
    assert prolog.commands == ["open firefox"]
    schedule = service.get_schedule("prolog:live")
    assert schedule is not None
    assert schedule.last_status == "completed"
    assert schedule.last_route == "prolog"
