from dataclasses import dataclass

import pytest

from zara.agent.tools.schedule_tools import SCHEDULE_TOOL_NAMES, build_schedule_tools


@dataclass
class Row:
    schedule_id: str = "schedule-1"
    label: str = "Report review"
    cron: str = "0 9 * * 1-5"
    goal: str = "review inbox"
    mode: object = type("Mode", (), {"value": "auto"})()
    state: object = type("State", (), {"value": "active"})()
    next_run_at: str = "2026-09-17T13:00:00+00:00"
    last_run_at: str | None = None
    last_status: str | None = None
    last_route: str | None = None
    last_task_id: str | None = None


class FakeScheduleService:
    def __init__(self):
        self.row = Row()
        self.created = []
        self.actions = []

    async def create_schedule(self, *, cron, goal, mode="auto", label=None):
        self.created.append((cron, goal, mode, label))
        self.row.cron = cron
        self.row.goal = goal
        self.row.label = label or goal[:80]
        return self.row

    def list_schedules(self):
        return [self.row]

    def get_schedule(self, schedule_id):
        return self.row if schedule_id == self.row.schedule_id else None

    async def pause_schedule(self, schedule_id):
        self.actions.append(("pause", schedule_id))
        return self.row

    async def resume_schedule(self, schedule_id):
        self.actions.append(("resume", schedule_id))
        return self.row

    async def cancel_schedule(self, schedule_id):
        self.actions.append(("cancel", schedule_id))
        return self.row


@pytest.mark.asyncio
async def test_schedule_tools_create_list_and_control():
    service = FakeScheduleService()
    tools = {tool.name: tool for tool in build_schedule_tools(service)}

    assert tuple(tools) == SCHEDULE_TOOL_NAMES

    created = await tools["schedule_create"].ainvoke(
        {
            "cron": "*/30 8-18 * * 1-5",
            "goal": "review the queued reports",
            "mode": "auto",
            "label": "Report review",
        }
    )
    assert "schedule-1" in created
    assert service.created == [
        ("*/30 8-18 * * 1-5", "review the queued reports", "auto", "Report review")
    ]

    listed = await tools["schedule_list"].ainvoke({})
    assert "schedule-1" in listed
    assert "*/30 8-18 * * 1-5" in listed
    assert "review the queued reports" not in listed

    for name, action in (
        ("schedule_pause", "pause"),
        ("schedule_resume", "resume"),
        ("schedule_cancel", "cancel"),
    ):
        result = await tools[name].ainvoke({"schedule_id": "schedule-1"})
        assert "schedule-1" in result
        assert service.actions[-1] == (action, "schedule-1")
