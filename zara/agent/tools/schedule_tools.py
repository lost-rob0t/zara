"""LangChain tools for the canonical scheduled-task service."""

from __future__ import annotations

from typing import Literal, Optional

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field


SCHEDULE_TOOL_NAMES = (
    "schedule_create",
    "schedule_list",
    "schedule_pause",
    "schedule_resume",
    "schedule_cancel",
)


class ScheduleCreateArgs(BaseModel):
    cron: str = Field(..., description="Five-field cron expression, for example '0 9 * * 1-5'.")
    goal: str = Field(..., description="Task Zara should run when the cron expression is due.")
    mode: Literal["auto", "prolog", "llm"] = Field(
        default="auto",
        description="auto asks Prolog whether to execute directly or escalate to the LLM.",
    )
    label: Optional[str] = Field(default=None, description="Optional short display label.")


class ScheduleIdArgs(BaseModel):
    schedule_id: str = Field(..., description="Stable schedule id returned by schedule_create/list.")


class ScheduleListArgs(BaseModel):
    pass


def build_schedule_tools(schedule_service):
    async def create(cron: str, goal: str, mode: str = "auto", label: Optional[str] = None) -> str:
        row = await schedule_service.create_schedule(
            cron=cron,
            goal=goal,
            mode=mode,
            label=label,
        )
        return _format_row(row)

    async def list_schedules() -> str:
        rows = schedule_service.list_schedules()
        if not rows:
            return "No scheduled tasks."
        return "\n".join(_format_row(row) for row in rows)

    async def pause(schedule_id: str) -> str:
        return _format_row(await schedule_service.pause_schedule(schedule_id))

    async def resume(schedule_id: str) -> str:
        return _format_row(await schedule_service.resume_schedule(schedule_id))

    async def cancel(schedule_id: str) -> str:
        return _format_row(await schedule_service.cancel_schedule(schedule_id))

    return [
        StructuredTool.from_function(
            coroutine=create,
            name="schedule_create",
            description=(
                "Create a persistent cron scheduled task. Use five-field cron syntax. "
                "auto mode runs Prolog first and escalates long or unresolved work to the LLM."
            ),
            args_schema=ScheduleCreateArgs,
        ),
        StructuredTool.from_function(
            coroutine=list_schedules,
            name="schedule_list",
            description="List this user's scheduled tasks with state, next run, and last outcome.",
            args_schema=ScheduleListArgs,
        ),
        StructuredTool.from_function(
            coroutine=pause,
            name="schedule_pause",
            description="Pause a scheduled task without deleting its identity.",
            args_schema=ScheduleIdArgs,
        ),
        StructuredTool.from_function(
            coroutine=resume,
            name="schedule_resume",
            description="Resume a paused scheduled task and compute its next cron run.",
            args_schema=ScheduleIdArgs,
        ),
        StructuredTool.from_function(
            coroutine=cancel,
            name="schedule_cancel",
            description="Cancel a scheduled task so it cannot run again.",
            args_schema=ScheduleIdArgs,
        ),
    ]


def _format_row(row) -> str:
    fields = [
        row.schedule_id,
        f"state={row.state.value}",
        f"cron={row.cron}",
        f"mode={row.mode.value}",
        f"goal={row.goal}",
        f"next={row.next_run_at or '-'}",
    ]
    if row.last_status:
        fields.append(f"last={row.last_status}")
    if row.last_route:
        fields.append(f"route={row.last_route}")
    if row.last_task_id:
        fields.append(f"task={row.last_task_id}")
    return " | ".join(fields)


__all__ = ["SCHEDULE_TOOL_NAMES", "build_schedule_tools"]
