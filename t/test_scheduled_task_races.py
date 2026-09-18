from datetime import datetime, timezone

import pytest

from zara.database import DatabaseManager
from zara.tasks.schedule import ScheduledTaskStore


def utc(hour, minute):
    return datetime(2026, 9, 17, hour, minute, tzinfo=timezone.utc)


def store(tmp_path):
    return ScheduledTaskStore(DatabaseManager(tmp_path / "zara.db"))


def test_cancelled_schedule_cannot_be_paused_or_resumed(tmp_path):
    scheduled = store(tmp_path)
    row = scheduled.create_schedule(
        principal_id="p",
        cron="* * * * *",
        goal="noop",
        now=utc(1, 0),
    )
    scheduled.cancel(row.schedule_id, principal_id="p")

    with pytest.raises(ValueError, match="cancelled"):
        scheduled.pause(row.schedule_id, principal_id="p")
    with pytest.raises(ValueError, match="cancelled"):
        scheduled.resume(row.schedule_id, principal_id="p", now=utc(1, 1))


def test_pause_between_due_scan_and_claim_prevents_execution(tmp_path):
    scheduled = store(tmp_path)
    row = scheduled.create_schedule(
        principal_id="p",
        cron="* * * * *",
        goal="noop",
        now=utc(1, 0),
    )
    due = utc(1, 1)
    scheduled.pause(row.schedule_id, principal_id="p")

    assert not scheduled.claim_run(row, due_at=due, next_run_at=utc(1, 2))


def test_stale_due_snapshot_cannot_claim_newer_slot(tmp_path):
    scheduled = store(tmp_path)
    row = scheduled.create_schedule(
        principal_id="p",
        cron="* * * * *",
        goal="noop",
        now=utc(1, 0),
    )
    assert scheduled.claim_run(row, due_at=utc(1, 1), next_run_at=utc(1, 2))

    stale = row
    assert not scheduled.claim_run(stale, due_at=utc(1, 1), next_run_at=utc(1, 2))
