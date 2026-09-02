import asyncio

import pytest

from zara.agent.tools.builtin_tools import TASK_TOOL_NAMES, get_builtin_tools
from zara.tasks.runner import TaskLimitError
from zara.tasks.store import AgentTask, TaskStatus


class FakeTaskService:
    def __init__(self) -> None:
        self.created: list[dict] = []
        self.cancelled: list[str] = []
        self.resumed: list[str] = []
        self.tasks: dict[str, AgentTask] = {}
        self.create_error: Exception = None

    async def create_task(self, *, goal, max_task_steps=None):
        if self.create_error is not None:
            raise self.create_error
        self.created.append({"goal": goal, "max_task_steps": max_task_steps})
        task = AgentTask(
            task_id=f"task-{len(self.created):04d}",
            principal_id="principal-1",
            goal=goal,
            status=TaskStatus.RUNNING,
            max_task_steps=max_task_steps or 20,
            steps_completed=0,
            created_at="now",
            updated_at="now",
        )
        self.tasks[task.task_id] = task
        return task

    async def cancel_task(self, *, task_id, reason="cancelled"):
        self.cancelled.append(task_id)
        return self.tasks[task_id]

    async def resume_task(self, *, task_id):
        self.resumed.append(task_id)
        return self.tasks[task_id]

    def get_task(self, task_id):
        return self.tasks.get(task_id)

    def list_tasks(self, statuses=None):
        rows = list(self.tasks.values())
        if statuses is None:
            return rows
        return [task for task in rows if task.status in statuses]


@pytest.fixture
def service():
    return FakeTaskService()


@pytest.fixture
def tools(service):
    return get_builtin_tools(task_service=service)


def by_name(tools, name):
    return {tool.name: tool for tool in tools}[name]


def test_task_tools_appended_when_service_present(tools):
    names = {tool.name for tool in tools}
    assert set(TASK_TOOL_NAMES) == {
        "task_create",
        "task_list",
        "task_status",
        "task_cancel",
        "task_resume",
    }
    assert set(TASK_TOOL_NAMES) <= names


def test_task_tools_omitted_without_service():
    tools = get_builtin_tools()
    names = {tool.name for tool in tools}
    assert not (set(TASK_TOOL_NAMES) & names)


def test_task_create_reports_started_task(service, tools):
    result = asyncio.run(by_name(tools, "task_create").ainvoke({"goal": "write it"}))
    assert "task-0001" in result
    assert "running" in result
    assert service.created[0]["goal"] == "write it"


def test_task_create_reports_limit_without_raising(service, tools):
    service.create_error = TaskLimitError("task concurrency limit reached (2 running)")
    result = asyncio.run(by_name(tools, "task_create").ainvoke({"goal": "write it"}))
    assert "limit" in result.lower()
    assert service.created == []


def test_task_create_reports_invalid_goal(service, tools):
    service.create_error = ValueError("goal must not exceed 2000 characters")
    result = asyncio.run(
        by_name(tools, "task_create").ainvoke({"goal": "x" * 3000})
    )
    assert "goal" in result.lower()


def test_task_list_renders_rows(service, tools):
    asyncio.run(by_name(tools, "task_create").ainvoke({"goal": "first goal"}))
    result = asyncio.run(by_name(tools, "task_list").ainvoke({}))
    assert "task-0001" in result
    assert "first goal" in result
    assert "running" in result


def test_task_list_empty(service, tools):
    result = asyncio.run(by_name(tools, "task_list").ainvoke({}))
    assert "no long-horizon tasks" in result.lower()


def test_task_status_reports_reason_and_steps(service, tools):
    task = asyncio.run(service.create_task(goal="goal one"))
    failed = AgentTask(
        task_id=task.task_id,
        principal_id=task.principal_id,
        goal=task.goal,
        status=TaskStatus.FAILED,
        max_task_steps=task.max_task_steps,
        steps_completed=2,
        created_at=task.created_at,
        updated_at=task.updated_at,
        reason="step_budget_exhausted",
    )
    service.tasks[task.task_id] = failed
    result = asyncio.run(
        by_name(tools, "task_status").ainvoke({"task_id": task.task_id})
    )
    assert "failed" in result
    assert "step_budget_exhausted" in result
    assert "2" in result


def test_task_status_unknown_task(service, tools):
    result = asyncio.run(
        by_name(tools, "task_status").ainvoke({"task_id": "task-nope"})
    )
    assert "no task found" in result.lower()


def test_task_cancel(service, tools):
    task = asyncio.run(service.create_task(goal="goal"))
    result = asyncio.run(
        by_name(tools, "task_cancel").ainvoke({"task_id": task.task_id})
    )
    assert service.cancelled == [task.task_id]
    assert "cancelled" in result


def test_task_resume(service, tools):
    task = asyncio.run(service.create_task(goal="goal"))
    result = asyncio.run(
        by_name(tools, "task_resume").ainvoke({"task_id": task.task_id})
    )
    assert service.resumed == [task.task_id]
    assert "resumed" in result
