import threading
import time

import pytest

from langchain_core.messages import AIMessage
from langchain_core.tools import tool

from zara.agent import AgentManager
from zara.database import DatabaseManager
from zara.runtime import events
from zara.runtime.backend import LangGraphRuntimeBackend
from zara.runtime.commands import ApproveTool
from zara.runtime.host import RuntimeHost
from zara.server import PrincipalContext
from zara.tasks.store import TaskStatus, TaskStore


GOAL = "assemble the launch readiness report from the scripted effects"


class ScriptedTaskLLM:
    """Queue-driven stub LLM: strings are final answers, tuples are tool calls."""

    def __init__(self) -> None:
        self.script: list = []
        self.lock = threading.Lock()
        self.counter = 0

    def bind_tools(self, _tools):
        return self

    async def ainvoke(self, messages):
        with self.lock:
            item = self.script.pop(0)
            self.counter += 1
            call_id = f"call-{self.counter}"
        if isinstance(item, str):
            return AIMessage(content=item)
        name, args = item
        return AIMessage(
            content="",
            tool_calls=[
                {"name": name, "args": args, "id": call_id, "type": "tool_call"}
            ],
        )


class EmptyMemory:
    def retrieve(self, _query, k=5):
        return []


class TasksHostConfig:
    config_dir = "/nonexistent-zara-task-host-config"

    def __init__(self, *, tasks_enabled=True, required_tools=("scripted_effect",)) -> None:
        self.tasks_enabled = tasks_enabled
        self.required_tools = list(required_tools)

    def get_llm_config(self):
        return {}

    def get_section(self, name: str):
        sections = {
            "agent": {"conversation_timeout": 60, "max_steps": 6},
            "memory": {"max_chars": 1200, "top_k": 5},
            "tool_approval": {
                "required_tools": self.required_tools,
                "timeout_seconds": 5.0,
                "max_pending": 4,
            },
            "tasks": {
                "enabled": self.tasks_enabled,
                "max_concurrent": 2,
                "max_task_steps": 6,
                "wall_clock_minutes": 5.0,
                "step_log_chars": 512,
            },
        }
        return sections.get(name, {})

    def get_tasks_config(self):
        return {
            "enabled": self.tasks_enabled,
            "max_concurrent": 2,
            "max_task_steps": 6,
            "wall_clock_minutes": 5.0,
            "step_log_chars": 512,
        }

    def get_api_service_config(self):
        return {"enabled": False, "disabled_providers": ()}

    def get_module_search_paths(self):
        return []

    def get_tool_config(self):
        return {"file_tools": False}

    def get_agent_system_prompt(self):
        return "Test assistant."


class Recorder:
    def __init__(self) -> None:
        self.events: list[events.RuntimeEvent] = []
        self._lock = threading.Lock()

    def __call__(self, event: events.RuntimeEvent):
        with self._lock:
            self.events.append(event)
        return event

    def wait_for_event(self, event_type, timeout=5.0):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            with self._lock:
                if any(isinstance(event, event_type) for event in self.events):
                    return True
            time.sleep(0.01)
        return False

    def of_type(self, event_type):
        with self._lock:
            return [event for event in self.events if isinstance(event, event_type)]


def wait_until(predicate, timeout=5.0, message="condition not met"):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.01)
    raise AssertionError(message)


def build_environment(
    monkeypatch,
    tmp_path,
    *,
    tasks_enabled=True,
    store=None,
    llm=None,
    required_tools=("scripted_effect",),
):
    monkeypatch.setattr(
        "zara.agent.tools.builtin_tools.get_builtin_tools",
        lambda *_args, **_kwargs: [],
    )
    llm = llm or ScriptedTaskLLM()
    monkeypatch.setattr(
        AgentManager,
        "_create_llm_client",
        lambda _self, _config: llm,
    )
    effects: list[str] = []

    @tool("scripted_effect")
    def scripted_effect(value: str) -> str:
        """Record one deterministic scripted effect."""
        effects.append(value)
        return f"effect {value} complete"

    principal = PrincipalContext("test:task-owner", kind="test")
    config = TasksHostConfig(
        tasks_enabled=tasks_enabled, required_tools=required_tools
    )

    def make_manager():
        manager = AgentManager(
            config=config,
            memory_manager=EmptyMemory(),
            principal=principal,
        )
        manager.tool_registry.register_tool(scripted_effect)
        return manager

    if store is None:
        store = TaskStore(DatabaseManager(tmp_path / "zara.db"), step_log_chars=512)
    recorder = Recorder()
    host = RuntimeHost(
        lambda: LangGraphRuntimeBackend(make_manager),
        publisher=recorder,
        task_store=store,
        config=config,
    )
    return host, store, recorder, llm, effects


def stop_host(host):
    if host.state not in {"new", "stopped", "failed"}:
        try:
            host.shutdown("test cleanup").result(timeout=10)
        except Exception:
            pass
    host.join(timeout=10)


class TestWiring:
    def test_host_starts_task_runner_and_registers_tools(self, monkeypatch, tmp_path):
        host, store, recorder, llm, effects = build_environment(monkeypatch, tmp_path)
        try:
            host.start().result(timeout=5)
            assert host.task_runner is not None
            assert host.task_runner.get_task("task-none") is None
        finally:
            stop_host(host)

        assert host.task_runner is None

    def test_host_skips_task_runner_when_disabled(self, monkeypatch, tmp_path):
        host, store, recorder, llm, effects = build_environment(
            monkeypatch, tmp_path, tasks_enabled=False
        )
        try:
            host.start().result(timeout=5)
            assert host.task_runner is None
        finally:
            stop_host(host)


class TestEndToEnd:
    def test_three_step_task_completes_through_real_registry(
        self, monkeypatch, tmp_path
    ):
        host, store, recorder, llm, effects = build_environment(
            monkeypatch, tmp_path, required_tools=()
        )
        llm.script = [
            ("scripted_effect", {"value": "research"}),
            "step 1: research complete",
            ("scripted_effect", {"value": "draft"}),
            "step 2: draft complete",
            ("scripted_effect", {"value": "polish"}),
            "TASK_COMPLETE: report delivered",
        ]
        try:
            host.start().result(timeout=5)
            task = host.run_coroutine(
                host.task_runner.create_task(goal=GOAL)
            ).result(timeout=5)

            try:
                wait_until(
                    lambda: (
                        store.get_task(task.task_id, principal_id="test:task-owner").status
                        is TaskStatus.COMPLETED
                    ),
                    message="task did not complete",
                )
            except AssertionError:
                print("DIAG EVENTS:", [type(event).__name__ for event in recorder.events])
                print("DIAG TASK:", store.get_task(task.task_id, principal_id="test:task-owner"))
                print("DIAG RUNNER PRINCIPAL:", host.task_runner._principal_id if host.task_runner else None)
                print("DIAG STEPS:", store.list_steps(task.task_id, principal_id="test:task-owner"))
                raise
            final = store.get_task(task.task_id, principal_id="test:task-owner")
            assert final.steps_completed == 3
            steps = store.list_steps(task.task_id, principal_id="test:task-owner")
            assert [step.summary for step in steps] == [
                "step 1: research complete",
                "step 2: draft complete",
                "report delivered",
            ]

            started = recorder.of_type(events.TaskStarted)
            step_events = recorder.of_type(events.TaskStepCompleted)
            completed = recorder.of_type(events.TaskCompleted)
            assert [event.task_id for event in started] == [task.task_id]
            assert [event.task_id for event in step_events] == [task.task_id] * 3
            assert [event.task_id for event in completed] == [task.task_id]
        finally:
            stop_host(host)

    def test_task_step_approval_pause_and_resume_end_to_end(
        self, monkeypatch, tmp_path
    ):
        host, store, recorder, llm, effects = build_environment(monkeypatch, tmp_path)
        llm.script = [
            ("scripted_effect", {"value": "guarded"}),
            "TASK_COMPLETE: approved path done",
        ]
        try:
            host.start().result(timeout=5)
            task = host.run_coroutine(
                host.task_runner.create_task(goal=GOAL)
            ).result(timeout=5)

            assert recorder.wait_for_event(events.ToolWaitingForUser), recorder.events
            wait_until(
                lambda: (
                    store.get_task(task.task_id, principal_id="test:task-owner").status
                    is TaskStatus.WAITING_APPROVAL
                ),
                message="task did not pause for approval",
            )
            assert effects == []

            host.submit(ApproveTool(tool_run_id="call-1")).result(timeout=5)

            wait_until(
                lambda: (
                    store.get_task(task.task_id, principal_id="test:task-owner").status
                    is TaskStatus.COMPLETED
                ),
                message="task did not complete after approval",
            )
            assert effects == ["guarded"]
            waiting_events = recorder.of_type(events.TaskWaitingApproval)
            assert [event.task_id for event in waiting_events] == [task.task_id]
        finally:
            stop_host(host)

    def test_shutdown_interrupts_and_resume_completes_same_task(
        self, monkeypatch, tmp_path
    ):
        db_path = tmp_path / "zara.db"
        shared_store = TaskStore(DatabaseManager(db_path), step_log_chars=512)

        host, store, recorder, llm, effects = build_environment(
            monkeypatch, tmp_path, store=shared_store
        )
        llm.script = [("scripted_effect", {"value": "parked"})]
        try:
            host.start().result(timeout=5)
            task = host.run_coroutine(
                host.task_runner.create_task(goal=GOAL)
            ).result(timeout=5)
            task_id = task.task_id

            wait_until(
                lambda: (
                    store.get_task(task_id, principal_id="test:task-owner").status
                    is TaskStatus.WAITING_APPROVAL
                ),
                message="task did not reach waiting_approval",
            )
        finally:
            stop_host(host)

        final = store.get_task(task_id, principal_id="test:task-owner")
        assert final.status is TaskStatus.INTERRUPTED
        assert final.reason == "runtime_shutdown"

        resumed_llm = ScriptedTaskLLM()
        resumed_llm.script = ["TASK_COMPLETE: finished after restart"]
        host2, store2, recorder2, _llm2, _effects2 = build_environment(
            monkeypatch, tmp_path, store=shared_store, llm=resumed_llm
        )
        try:
            host2.start().result(timeout=5)
            host2.run_coroutine(
                host2.task_runner.resume_task(task_id=task_id)
            ).result(timeout=5)
            wait_until(
                lambda: (
                    store2.get_task(task_id, principal_id="test:task-owner").status
                    is TaskStatus.COMPLETED
                ),
                message="resumed task did not complete",
            )
            resumed = store2.get_task(task_id, principal_id="test:task-owner")
            assert resumed.task_id == task_id
            assert resumed.steps_completed == 1
            steps = store2.list_steps(task_id, principal_id="test:task-owner")
            assert [step.summary for step in steps] == ["finished after restart"]
        finally:
            stop_host(host2)
