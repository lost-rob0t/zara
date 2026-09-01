"""Execution of long-horizon agent tasks as bounded conversation turns.

The runner is owned by :class:`zara.runtime.host.RuntimeHost`. Every step is
ONE turn through the existing backend submit path (``AgentManager.process_async``
via ``LangGraphRuntimeBackend.submit_turn``) with a coordinator-allocated turn
id, a fresh per-step history, and a task-context system message carrying the
goal plus bounded step summaries. All seams are injected so the runner is
independently testable without a runtime host.
"""

from __future__ import annotations

import asyncio
import logging
import threading
from dataclasses import dataclass
from typing import Awaitable, Callable, Iterable, Optional

from ..latency import LatencyTrace
from ..runtime import events
from .store import AgentTask, TaskStatus, TaskStore, TaskStoreError, DEFAULT_STEP_LOG_CHARS

logger = logging.getLogger(__name__)

REASON_STEP_BUDGET = "step_budget_exhausted"
REASON_WALL_CLOCK = "wall_clock_exceeded"
REASON_STEP_ERROR = "step_error"
REASON_INTERRUPTED = "runtime_shutdown"
REASON_TIMEOUT_KW = "wall_clock_exceeded"

COMPLETION_SENTINEL = "TASK_COMPLETE"
_CONTEXT_STEP_WINDOW = 5


class TaskRunnerError(RuntimeError):
    """Raised when a task operation cannot be applied."""


class TaskLimitError(TaskRunnerError):
    """Raised when the configured task concurrency limit is reached."""


SubmitTurn = Callable[..., Awaitable[object]]


@dataclass
class _ActiveStep:
    task_id: str
    step_index: int
    turn_id: str


def _positive_number(value, name: str) -> Optional[float]:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a positive number")
    number = float(value)
    if not number > 0 or number != number or number in (float("inf"), float("-inf")):
        raise ValueError(f"{name} must be a positive finite number")
    return number


class TaskRunner:
    """Drives persistent agent tasks step by step on the runtime loop."""

    def __init__(
        self,
        *,
        store: TaskStore,
        submit_turn: SubmitTurn,
        allocate_turn_id: Callable[[], Awaitable[str]],
        cancel_turn: Callable[[str], Awaitable[None]],
        publisher: Callable[[events.RuntimeEvent], object],
        principal_id: str,
        max_concurrent: int = 2,
        default_max_task_steps: int = 20,
        wall_clock_seconds: Optional[float] = None,
        step_log_chars: int = DEFAULT_STEP_LOG_CHARS,
    ) -> None:
        if isinstance(max_concurrent, bool) or not isinstance(max_concurrent, int):
            raise ValueError("max_concurrent must be an integer")
        if max_concurrent < 1:
            raise ValueError("max_concurrent must be at least 1")
        if isinstance(default_max_task_steps, bool) or not isinstance(
            default_max_task_steps, int
        ):
            raise ValueError("default_max_task_steps must be an integer")
        if default_max_task_steps < 1:
            raise ValueError("default_max_task_steps must be at least 1")
        if isinstance(step_log_chars, bool) or not isinstance(step_log_chars, int):
            raise ValueError("step_log_chars must be an integer")
        if step_log_chars < 1:
            raise ValueError("step_log_chars must be at least 1")
        self._wall_clock_seconds = _positive_number(wall_clock_seconds, "wall_clock_seconds")

        self._store = store
        self._submit_turn = submit_turn
        self._allocate_turn_id = allocate_turn_id
        self._cancel_turn = cancel_turn
        self._publisher = publisher
        self._principal_id = principal_id
        self._max_concurrent = max_concurrent
        self._default_max_task_steps = default_max_task_steps
        self._step_log_chars = step_log_chars

        self._lock = threading.RLock()
        self._runs: dict[str, asyncio.Task] = {}
        self._active_steps: dict[str, _ActiveStep] = {}
        self._task_turn: dict[str, str] = {}
        self._stopping = False

    # ------------------------------------------------------------------
    # Lifecycle

    async def start(self) -> None:
        """Adopt persisted tasks left active by a dead runtime."""
        recovered = self._store.recover_interrupted()
        if recovered:
            logger.info("[TaskRunner] recovered %d interrupted task(s)", recovered)

    async def stop(self) -> None:
        """Interrupt all live tasks (persisted) and release resources."""
        self._stopping = True
        with self._lock:
            runs = list(self._runs.values())
        for run in runs:
            run.cancel()
        if runs:
            await asyncio.gather(*runs, return_exceptions=True)
        with self._lock:
            self._runs.clear()
            self._active_steps.clear()
            self._task_turn.clear()
        self._stopping = False

    # ------------------------------------------------------------------
    # Task operations

    async def create_task(
        self,
        *,
        goal: str,
        max_task_steps: Optional[int] = None,
    ) -> AgentTask:
        active = self._store.list_tasks(
            principal_id=self._principal_id,
            statuses=[TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL],
        )
        if len(active) >= self._max_concurrent:
            raise TaskLimitError(
                f"task concurrency limit reached ({self._max_concurrent} running)"
            )
        budget = (
            self._default_max_task_steps
            if max_task_steps is None
            else max_task_steps
        )
        task = self._store.create_task(
            principal_id=self._principal_id,
            goal=goal,
            max_task_steps=budget,
        )
        task = self._store.transition(
            task.task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING
        )
        self._publish(events.TaskStarted(task_id=task.task_id, label="tasks"))
        self._spawn_run(task.task_id)
        return task

    async def resume_task(self, *, task_id: str) -> AgentTask:
        task = self._store.get_task(task_id, principal_id=self._principal_id)
        if task is None:
            raise TaskRunnerError(f"task not found: {task_id!r}")
        active = self._store.list_tasks(
            principal_id=self._principal_id,
            statuses=[TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL],
        )
        if len(active) >= self._max_concurrent:
            raise TaskLimitError(
                f"task concurrency limit reached ({self._max_concurrent} running)"
            )
        task = self._store.transition(
            task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING
        )
        self._publish(events.TaskStarted(task_id=task_id, label="tasks"))
        self._spawn_run(task_id)
        return task

    async def cancel_task(self, *, task_id: str, reason: str = "cancelled") -> AgentTask:
        task = self._store.get_task(task_id, principal_id=self._principal_id)
        if task is None:
            raise TaskRunnerError(f"task not found: {task_id!r}")
        turn_id: Optional[str]
        with self._lock:
            turn_id = self._task_turn.get(task_id)
        task = self._store.transition(
            task_id,
            principal_id=self._principal_id,
            status=TaskStatus.CANCELLED,
            reason=reason,
        )
        self._publish(
            events.TaskCancelled(task_id=task_id, label="tasks", reason=reason)
        )
        if turn_id is not None:
            try:
                await self._cancel_turn(turn_id)
            except Exception:
                logger.warning(
                    "[TaskRunner] turn cancel failed for task %s", task_id, exc_info=True
                )
        run = self._runs.get(task_id)
        if run is not None and not run.done():
            run.cancel()
        return task

    def get_task(self, task_id: str) -> Optional[AgentTask]:
        return self._store.get_task(task_id, principal_id=self._principal_id)

    def list_tasks(
        self, statuses: Optional[Iterable[TaskStatus]] = None
    ) -> list[AgentTask]:
        return self._store.list_tasks(
            principal_id=self._principal_id, statuses=statuses
        )

    async def wait_for_task(self, task_id: str, timeout: Optional[float] = None) -> None:
        run = self._runs.get(task_id)
        if run is None:
            return
        done, pending = await asyncio.wait({run}, timeout=timeout)
        if pending:
            raise TimeoutError(f"task {task_id!r} did not finish in time")

    # ------------------------------------------------------------------
    # Step loop

    def _spawn_run(self, task_id: str) -> None:
        run = asyncio.create_task(
            self._run_task(task_id), name=f"zara-task-{task_id}"
        )
        self._runs[task_id] = run
        run.add_done_callback(lambda _task, tid=task_id: self._runs.pop(tid, None))

    async def _run_task(self, task_id: str) -> None:
        try:
            if self._wall_clock_seconds is not None:
                async with asyncio.timeout(self._wall_clock_seconds):
                    await self._step_loop(task_id)
            else:
                await self._step_loop(task_id)
        except asyncio.CancelledError:
            self._interrupt(task_id)
            raise
        except TimeoutError:
            logger.info(
                "[TaskRunner] task %s exceeded wall-clock budget", task_id
            )
            self._finish(task_id, TaskStatus.FAILED, REASON_WALL_CLOCK)
        except Exception as error:
            logger.error(
                "[TaskRunner] task %s step failed: %s", task_id, type(error).__name__
            )
            self._finish(task_id, TaskStatus.FAILED, REASON_STEP_ERROR)

    async def _step_loop(self, task_id: str) -> None:
        while True:
            task = self._store.get_task(task_id, principal_id=self._principal_id)
            if task is None or task.status is not TaskStatus.RUNNING:
                return
            if task.steps_completed >= task.max_task_steps:
                self._finish(task_id, TaskStatus.FAILED, REASON_STEP_BUDGET)
                return
            step_index = task.steps_completed
            turn_id, completed, summary = await self._run_step(task, step_index)
            current = self._store.get_task(task_id, principal_id=self._principal_id)
            if current is None or current.status is not TaskStatus.RUNNING:
                return
            self._store.record_step(
                task_id,
                principal_id=self._principal_id,
                step_index=step_index,
                status="completed",
                summary=summary,
            )
            self._publish(
                events.TaskStepCompleted(
                    task_id=task_id,
                    turn_id=turn_id,
                    label="tasks",
                    step_index=step_index,
                )
            )
            if completed:
                self._finish(task_id, TaskStatus.COMPLETED, None)
                return

    async def _run_step(self, task: AgentTask, step_index: int):
        turn_id = await self._allocate_turn_id()
        active = _ActiveStep(
            task_id=task.task_id, step_index=step_index, turn_id=turn_id
        )
        with self._lock:
            self._active_steps[turn_id] = active
            self._task_turn[task.task_id] = turn_id
        trace = LatencyTrace(trace_id=turn_id)
        try:
            result = await self._submit_turn(
                self._step_prompt(task, step_index),
                turn_id=turn_id,
                conversation_id=None,
                system_context=self._task_context(task),
                latency_trace=trace,
            )
        except asyncio.CancelledError:
            self._record_incomplete_step(task.task_id, step_index, "cancelled")
            raise
        except Exception as error:
            self._record_incomplete_step(
                task.task_id, step_index, "failed", type(error).__name__
            )
            raise
        finally:
            with self._lock:
                self._active_steps.pop(turn_id, None)
                if self._task_turn.get(task.task_id) == turn_id:
                    self._task_turn.pop(task.task_id, None)
        response = str(getattr(result, "response", "") or "")
        completed = response.strip().upper().startswith(COMPLETION_SENTINEL)
        return turn_id, completed, self._bounded_summary(response)

    def _record_incomplete_step(
        self, task_id: str, step_index: int, status: str, summary: str = ""
    ) -> None:
        try:
            self._store.record_step(
                task_id,
                principal_id=self._principal_id,
                step_index=step_index,
                status=status,
                summary=summary[: self._step_log_chars],
            )
        except TaskStoreError:
            return

    # ------------------------------------------------------------------
    # State helpers

    def _finish(
        self, task_id: str, status: TaskStatus, reason: Optional[str]
    ) -> None:
        try:
            self._store.transition(
                task_id,
                principal_id=self._principal_id,
                status=status,
                reason=reason,
            )
        except TaskStoreError:
            return
        if status is TaskStatus.COMPLETED:
            self._publish(events.TaskCompleted(task_id=task_id, label="tasks"))
        elif status is TaskStatus.FAILED:
            self._publish(
                events.TaskFailed(task_id=task_id, label="tasks", reason=reason or "")
            )

    def _interrupt(self, task_id: str) -> None:
        try:
            self._store.transition(
                task_id,
                principal_id=self._principal_id,
                status=TaskStatus.INTERRUPTED,
                reason=REASON_INTERRUPTED,
            )
        except TaskStoreError:
            return

    def _publish(self, event: events.RuntimeEvent) -> None:
        try:
            self._publisher(event)
        except Exception:
            logger.warning("[TaskRunner] event sink failed", exc_info=True)

    # ------------------------------------------------------------------
    # Prompt construction

    def _step_prompt(self, task: AgentTask, step_index: int) -> str:
        return (
            f"[long-horizon task {task.task_id} step {step_index + 1}/{task.max_task_steps}] "
            "Work toward the goal described in your task context. "
            "Use tools only when the goal requires them. "
            "When the goal is fully achieved, reply with a final answer starting with "
            "TASK_COMPLETE: followed by a short result summary. "
            "Otherwise report concrete progress made in this step."
        )

    def _task_context(self, task: AgentTask) -> str:
        steps = self._store.list_steps(
            task.task_id, principal_id=self._principal_id
        )
        lines = [
            f"Long-horizon task {task.task_id}",
            f"Goal: {task.goal}",
            "",
            "Progress from earlier steps:",
        ]
        if not steps:
            lines.append("(no completed steps yet)")
        else:
            for step in steps[-_CONTEXT_STEP_WINDOW:]:
                marker = step.summary or "(no summary)"
                lines.append(f"- step {step.step_index + 1}: {marker}")
        context = "\n".join(lines)
        if len(context) > self._step_log_chars:
            context = context[: self._step_log_chars]
        return context

    def _bounded_summary(self, response: str) -> str:
        stripped = response.strip()
        if stripped.upper().startswith(COMPLETION_SENTINEL):
            stripped = stripped[len(COMPLETION_SENTINEL):].lstrip(":").strip()
        if not stripped:
            stripped = "completed"
        return stripped[: self._step_log_chars]
