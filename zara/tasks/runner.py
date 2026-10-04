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
import contextvars
import logging
import threading
import time
from collections import deque
from dataclasses import dataclass
from typing import Awaitable, Callable, Iterable, Optional

from ..latency import LatencyTrace
from ..runtime import events
from .store import AgentTask, TaskStatus, TaskStore, TaskStoreError, DEFAULT_STEP_LOG_CHARS

logger = logging.getLogger(__name__)

REASON_STEP_BUDGET = "step_budget_exhausted"
REASON_WALL_CLOCK = "wall_clock_exceeded"
REASON_STEP_ERROR = "step_error"
REASON_APPROVAL_TIMEOUT = "approval_timeout"
REASON_APPROVAL_REJECTED = "approval_rejected"
REASON_INTERRUPTED = "runtime_shutdown"
REASON_NO_PROGRESS = "no_progress"
REASON_CHILD_FAILED = "child_failed"

REASONS = frozenset(
    {
        REASON_STEP_BUDGET,
        REASON_WALL_CLOCK,
        REASON_STEP_ERROR,
        REASON_APPROVAL_TIMEOUT,
        REASON_APPROVAL_REJECTED,
        REASON_INTERRUPTED,
        REASON_NO_PROGRESS,
        REASON_CHILD_FAILED,
    }
)

COMPLETION_SENTINEL = "TASK_COMPLETE"
_CONTEXT_STEP_WINDOW = 5


class TaskRunnerError(RuntimeError):
    """Raised when a task operation cannot be applied."""


class TaskLimitError(TaskRunnerError):
    """Raised when the configured task concurrency limit is reached."""


SubmitTurn = Callable[..., Awaitable[object]]
_CURRENT_TASK: contextvars.ContextVar = contextvars.ContextVar("zara_task_scope", default=None)
_TERMINAL = frozenset({TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED})


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
        max_queued: int = 32,
        max_subagents: int = 8,
        max_depth: int = 2,
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
        if type(max_queued) is not int or not 1 <= max_queued <= 1024:
            raise ValueError("max_queued must be an integer between 1 and 1024")
        for name, value, maximum in (("max_subagents", max_subagents, 64), ("max_depth", max_depth, 8)):
            if type(value) is not int or not 1 <= value <= maximum:
                raise ValueError(f"{name} must be an integer between 1 and {maximum}")
        self._max_subagents = max_subagents
        self._max_depth = max_depth
        self._cancelled_turns: set[str] = set()
        self._max_queued = max_queued
        self._queued: deque[str] = deque()
        self._dispatch_holds = 0
        self._changed = asyncio.Event()
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
        self._stopping = False
        recovered = self._store.recover_interrupted(principal_id=self._principal_id)
        if recovered:
            logger.info("[TaskRunner] recovered %d interrupted task(s)", recovered)

    async def stop(self) -> None:
        """Interrupt retained work and revoke active turns before shutdown."""
        self._stopping = True
        tasks = self.list_tasks(statuses=[
            TaskStatus.PENDING, TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL,
            TaskStatus.WAITING_INPUT, TaskStatus.BLOCKED,
        ])
        self._queued.clear()
        for task in tasks:
            self._interrupt(task.task_id)
        for task in tasks:
            await self._cancel_execution(task.task_id)
        self._changed.set()

    # ------------------------------------------------------------------
    # Task operations

    async def create_task(
        self,
        *,
        goal: str,
        max_task_steps: Optional[int] = None,
    ) -> AgentTask:
        if _CURRENT_TASK.get() is not None:
            return await self.enqueue_task(goal=goal, max_task_steps=max_task_steps)
        self._require_accepting()
        if self._running_count() >= self._max_concurrent:
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
            deadline_at=self._new_deadline(),
        )
        task = self._store.transition(
            task.task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING
        )
        logger.info("[TaskRunner] task=%s started", task.task_id)
        self._publish(events.TaskStarted(task_id=task.task_id, label="tasks"))
        self._spawn_run(task.task_id)
        return task

    async def resume_task(self, *, task_id: str) -> AgentTask:
        self._require_accepting()
        self._check_control_scope(task_id)
        task = self.get_task(task_id)
        if task is None:
            raise TaskRunnerError(f"task not found: {task_id!r}")
        if task.status not in {TaskStatus.PENDING, TaskStatus.INTERRUPTED}:
            raise TaskRunnerError(f"task {task_id!r} is not resumable (status={task.status.value})")
        if task.parent_task_id:
            parent = self.get_task(task.parent_task_id)
            if parent is None or parent.status in _TERMINAL or parent.status is TaskStatus.INTERRUPTED:
                raise TaskRunnerError("resume the parent task instead")
        targets = [child for child in self._descendants(task_id) if child.status is TaskStatus.INTERRUPTED] + [task]
        targets = [target for target in targets if target.task_id not in self._queued]
        if len(self._queued) + len(targets) > self._max_queued:
            raise TaskLimitError("task queue limit reached")
        for target in targets:
            run = self._runs.get(target.task_id)
            if run is not None and not run.done():
                raise TaskRunnerError("task cancellation has not completed")
        for target in targets:
            if target.status is TaskStatus.INTERRUPTED:
                self._store.transition(target.task_id, principal_id=self._principal_id, status=TaskStatus.PENDING)
            self._queued.append(target.task_id)
        self._pump_queue()
        return self.get_task(task_id)

    async def resume_all(self) -> list[AgentTask]:
        self._check_control_scope()
        roots = [task for task in self.list_tasks() if task.parent_task_id is None and task.status is TaskStatus.INTERRUPTED]
        return [await self.resume_task(task_id=task.task_id) for task in roots]

    def _new_deadline(self) -> Optional[float]:
        return None if self._wall_clock_seconds is None else time.time() + self._wall_clock_seconds

    def _require_accepting(self) -> None:
        if self._stopping or self._dispatch_holds:
            raise TaskRunnerError("task admission is stopped")

    def _running_count(self) -> int:
        count = 0
        for task_id, run in self._runs.items():
            task = self.get_task(task_id)
            waiting = task is not None and task.status is TaskStatus.BLOCKED and task.reason == "waiting_children"
            if not run.done() and not waiting:
                count += 1
        return count

    async def enqueue_task(
        self, *, goal: str, max_task_steps: Optional[int] = None,
        parent_task_id: Optional[str] = None,
        conversation_id: Optional[str] = None,
    ) -> AgentTask:
        """Admit a goal without blocking the conversation when the fleet is busy."""
        self._require_accepting()
        scope = _CURRENT_TASK.get()
        if scope is not None:
            owner, scoped_task_id = scope
            if owner is not self or parent_task_id not in {None, scoped_task_id}:
                raise TaskRunnerError("subagents cannot escape their parent task")
            parent_task_id = scoped_task_id
        if parent_task_id is not None:
            self._validate_parent(parent_task_id)
        if len(self._queued) >= self._max_queued:
            raise TaskLimitError(f"task queue limit reached ({self._max_queued})")
        budget = self._default_max_task_steps if max_task_steps is None else max_task_steps
        task = self._store.create_task(
            principal_id=self._principal_id, goal=goal, max_task_steps=budget,
            parent_task_id=parent_task_id, conversation_id=conversation_id,
            deadline_at=self._new_deadline(),
        )
        self._queued.append(task.task_id)
        self._pump_queue()
        return self.get_task(task.task_id)

    async def delegate_task(self, *, goal: str, max_task_steps: Optional[int] = None) -> AgentTask:
        scope = _CURRENT_TASK.get()
        if scope is None or scope[0] is not self:
            raise TaskRunnerError("delegation requires an active parent task")
        return await self.enqueue_task(goal=goal, max_task_steps=max_task_steps)

    def _validate_parent(self, task_id: str) -> None:
        parent = self.get_task(task_id)
        if parent is None or parent.status is not TaskStatus.RUNNING:
            raise TaskRunnerError("parent task is not running")
        family = [task for task in self.list_tasks() if task.root_task_id == parent.root_task_id]
        if len(family) - 1 >= self._max_subagents:
            raise TaskLimitError("root subagent limit reached")
        depth = 1
        ancestor = parent
        while ancestor.parent_task_id:
            depth += 1
            ancestor = self.get_task(ancestor.parent_task_id)
            if ancestor is None:
                raise TaskRunnerError("invalid task ancestry")
        if depth > self._max_depth:
            raise TaskLimitError("subagent depth limit reached")

    def _descendants(self, task_id: str) -> list[AgentTask]:
        family = self.list_tasks()
        children: dict[str, list[str]] = {}
        for task in family:
            children.setdefault(task.parent_task_id, []).append(task.task_id)
        selected = {task_id}
        pending = deque([task_id])
        while pending:
            for child_id in children.get(pending.popleft(), ()):
                if child_id not in selected:
                    selected.add(child_id)
                    pending.append(child_id)
        return [task for task in family if task.task_id in selected and task.task_id != task_id]

    def _check_control_scope(self, task_id: Optional[str] = None) -> None:
        scope = _CURRENT_TASK.get()
        if scope is None:
            return
        owner, current = scope
        if owner is not self or task_id is None:
            raise TaskRunnerError("task agents cannot control the whole fleet")
        allowed = {current} | {task.task_id for task in self._descendants(current)}
        if task_id not in allowed:
            raise TaskRunnerError("task control is outside the agent's subtree")

    def _pump_queue(self) -> None:
        if self._stopping or self._dispatch_holds:
            return
        while self._queued and self._running_count() < self._max_concurrent:
            task_id = self._queued.popleft()
            task = self.get_task(task_id)
            if task is None or task.status is not TaskStatus.PENDING:
                continue
            self._store.transition(
                task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING,
            )
            self._publish(events.TaskStarted(task_id=task_id, label="tasks"))
            self._spawn_run(task_id)
        self._changed.set()

    async def _cancel_execution(self, task_id: str) -> None:
        with self._lock:
            turn_id = self._task_turn.get(task_id)
            run = self._runs.get(task_id)
        if turn_id is not None:
            await self._revoke_turn(turn_id)
        if run is not None and not run.done():
            run.cancel()
            if run is not asyncio.current_task():
                _, pending = await asyncio.wait({run}, timeout=5.0)
                if pending:
                    logger.warning("[TaskRunner] task %s has not acknowledged cancellation", task_id)
        self._changed.set()

    async def _revoke_turn(self, turn_id: str) -> None:
        if turn_id in self._cancelled_turns:
            return
        self._cancelled_turns.add(turn_id)
        try:
            async with asyncio.timeout(5.0):
                await self._cancel_turn(turn_id)
        except Exception:
            logger.warning("[TaskRunner] turn cancellation failed for %s", turn_id, exc_info=True)

    async def cancel_task(self, *, task_id: str, reason: str = "cancelled") -> AgentTask:
        return await self._control_tree(task_id, pause=False, reason=reason)

    async def pause_task(self, *, task_id: str, reason: str = "user_pause") -> AgentTask:
        return await self._control_tree(task_id, pause=True, reason=reason)

    async def _control_tree(self, task_id: str, *, pause: bool, reason: str) -> AgentTask:
        self._check_control_scope(task_id)
        task = self.get_task(task_id)
        if task is None:
            raise TaskRunnerError(f"task not found: {task_id!r}")
        self._dispatch_holds += 1
        try:
            targets = [task] + self._descendants(task_id)
            ids = {target.task_id for target in targets}
            self._queued = deque(queued for queued in self._queued if queued not in ids)
            for target in targets:
                status = TaskStatus.INTERRUPTED if pause else TaskStatus.CANCELLED
                if target.status in _TERMINAL or target.status is status:
                    continue
                self._store.transition(target.task_id, principal_id=self._principal_id, status=status, reason=reason)
                if not pause:
                    self._publish(events.TaskCancelled(task_id=target.task_id, label="tasks", reason=reason))
            for target in targets:
                await self._cancel_execution(target.task_id)
            return self.get_task(task_id)
        finally:
            self._dispatch_holds -= 1
            self._pump_queue()

    async def cancel_all(self, *, reason: str = "user_stop_all") -> list[AgentTask]:
        return await self._control_all(pause=False, reason=reason)

    async def pause_all(self, *, reason: str = "user_pause") -> list[AgentTask]:
        return await self._control_all(pause=True, reason=reason)

    async def _control_all(self, *, pause: bool, reason: str) -> list[AgentTask]:
        self._check_control_scope()
        self._dispatch_holds += 1
        try:
            tasks = self.list_tasks()
            self._queued.clear()
            changed = []
            for task in tasks:
                if task.status in {TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED}:
                    continue
                target = TaskStatus.INTERRUPTED if pause else TaskStatus.CANCELLED
                if task.status is target:
                    continue
                changed.append(self._store.transition(
                    task.task_id, principal_id=self._principal_id,
                    status=target, reason=reason,
                ))
                if not pause:
                    self._publish(events.TaskCancelled(task_id=task.task_id, label="tasks", reason=reason))
            for task in changed:
                await self._cancel_execution(task.task_id)
            return changed
        finally:
            self._dispatch_holds -= 1
            self._changed.set()

    def get_task(self, task_id: str) -> Optional[AgentTask]:
        return self._store.get_task(task_id, principal_id=self._principal_id)

    def list_tasks(
        self, statuses: Optional[Iterable[TaskStatus]] = None
    ) -> list[AgentTask]:
        return self._store.list_tasks(
            principal_id=self._principal_id, statuses=statuses
        )

    async def wait_for_task(self, task_id: str, timeout: Optional[float] = None) -> None:
        async with asyncio.timeout(timeout):
            while True:
                self._changed.clear()
                task = self.get_task(task_id)
                run = self._runs.get(task_id)
                if task is None or (
                    task.status not in {TaskStatus.PENDING, TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL, TaskStatus.BLOCKED}
                    and (run is None or run.done())
                ):
                    return
                await self._changed.wait()

    # ------------------------------------------------------------------
    # Approval observation

    def observing_publisher(
        self, base: Callable[[events.RuntimeEvent], object]
    ) -> Callable[[events.RuntimeEvent], object]:
        """Wrap ``base`` so controller events update task waiting state.

        The host binds the backend's event publisher to this wrapper. Events
        for a task step's turn drive the persisted task state machine; the
        step coroutine itself stays parked inside the existing approval
        future, so there is no polling anywhere.
        """

        def publish(event: events.RuntimeEvent):
            try:
                self._observe(event)
            except Exception:
                logger.warning("[TaskRunner] event observation failed", exc_info=True)
            return base(event)

        return publish

    def _observe(self, event: events.RuntimeEvent) -> None:
        turn_id = getattr(event, "turn_id", None)
        if not turn_id:
            return
        with self._lock:
            active = self._active_steps.get(turn_id)
        if active is None:
            return
        task_id = active.task_id
        if isinstance(event, events.ToolWaitingForUser):
            self._on_waiting_for_approval(task_id, turn_id)
        elif isinstance(event, events.UserResponded):
            self._on_approval_resolved(task_id)
        elif isinstance(event, events.ToolCancelled):
            self._on_tool_cancelled(task_id, turn_id, str(event.reason))

    def _on_waiting_for_approval(self, task_id: str, turn_id: str) -> None:
        task = self._store.get_task(task_id, principal_id=self._principal_id)
        if task is None or task.status is not TaskStatus.RUNNING:
            return
        try:
            self._store.transition(
                task_id,
                principal_id=self._principal_id,
                status=TaskStatus.WAITING_APPROVAL,
            )
        except TaskStoreError:
            return
        self._publish(
            events.TaskWaitingApproval(
                task_id=task_id, turn_id=turn_id, label="tasks"
            )
        )

    def _on_approval_resolved(self, task_id: str) -> None:
        task = self._store.get_task(task_id, principal_id=self._principal_id)
        if task is None or task.status is not TaskStatus.WAITING_APPROVAL:
            return
        try:
            self._store.transition(
                task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING
            )
        except TaskStoreError:
            return

    def _on_tool_cancelled(self, task_id: str, turn_id: str, reason: str) -> None:
        if reason == "approval timeout":
            failure_reason = "approval_timeout"
        elif reason == "tool rejected":
            failure_reason = "approval_rejected"
        else:
            return
        self._finish(task_id, TaskStatus.FAILED, failure_reason)
        self._cancel_step(task_id, turn_id)

    def _cancel_step(self, task_id: str, turn_id: str) -> None:
        try:
            asyncio.get_running_loop().create_task(self._revoke_turn(turn_id))
        except RuntimeError:
            return
        run = self._runs.get(task_id)
        if run is not None and not run.done():
            run.cancel()

    # ------------------------------------------------------------------
    # Step loop

    def _spawn_run(self, task_id: str) -> None:
        context = contextvars.copy_context()
        context.run(_CURRENT_TASK.set, None)
        run = asyncio.create_task(
            self._run_task(task_id), name=f"zara-task-{task_id}", context=context,
        )
        self._runs[task_id] = run
        run.add_done_callback(lambda finished, tid=task_id: self._run_done(tid, finished))

    def _run_done(self, task_id: str, run: asyncio.Task) -> None:
        if self._runs.get(task_id) is run:
            self._runs.pop(task_id, None)
        self._changed.set()
        self._pump_queue()

    async def _run_task(self, task_id: str) -> None:
        try:
            task = self.get_task(task_id)
            remaining = self._wall_clock_seconds
            if task is not None and task.deadline_at is not None:
                remaining = task.deadline_at - time.time()
            if remaining is not None:
                if remaining <= 0:
                    raise TimeoutError("task deadline expired")
                async with asyncio.timeout(remaining):
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
        finally:
            current = self.get_task(task_id)
            if current is not None and current.status in {TaskStatus.FAILED, TaskStatus.CANCELLED, TaskStatus.INTERRUPTED}:
                for child in self._descendants(task_id):
                    if child.status not in _TERMINAL:
                        await self._control_tree(
                            child.task_id, pause=current.status is TaskStatus.INTERRUPTED,
                            reason="parent_" + current.status.value,
                        )

    async def _step_loop(self, task_id: str) -> None:
        while True:
            task = self._store.get_task(task_id, principal_id=self._principal_id)
            if task is None or task.status is not TaskStatus.RUNNING:
                return
            children = [child for child in self.list_tasks() if child.parent_task_id == task_id]
            if any(child.status not in _TERMINAL for child in children):
                if not await self._join_children(task_id):
                    return
                task = self.get_task(task_id)
            if not self._store.claim_step(task_id, principal_id=self._principal_id):
                self._finish(task_id, TaskStatus.FAILED, REASON_STEP_BUDGET)
                return
            step_index = task.steps_completed
            turn_id, completed, summary = await self._run_step(task, step_index)
            current = self._store.get_task(task_id, principal_id=self._principal_id)
            if current is None or current.status not in {
                TaskStatus.RUNNING,
                TaskStatus.WAITING_APPROVAL,
            }:
                return
            if current.status is TaskStatus.WAITING_APPROVAL:
                try:
                    self._store.transition(
                        task_id,
                        principal_id=self._principal_id,
                        status=TaskStatus.RUNNING,
                    )
                except TaskStoreError:
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
            children = [child for child in self.list_tasks() if child.parent_task_id == task_id]
            if any(child.status not in _TERMINAL for child in children):
                if not await self._join_children(task_id):
                    return
                continue
            if any(child.status is not TaskStatus.COMPLETED for child in children):
                self._finish(task_id, TaskStatus.FAILED, REASON_CHILD_FAILED)
                return
            if completed:
                self._finish(task_id, TaskStatus.COMPLETED, None)
                return
            steps = self._store.list_steps(task_id, principal_id=self._principal_id)
            if len(steps) >= 3 and len({step.summary for step in steps[-3:]}) == 1:
                self._finish(task_id, TaskStatus.FAILED, REASON_NO_PROGRESS)
                return

    async def _join_children(self, task_id: str) -> bool:
        self._store.transition(task_id, principal_id=self._principal_id, status=TaskStatus.BLOCKED, reason="waiting_children")
        self._pump_queue()
        while True:
            self._changed.clear()
            task = self.get_task(task_id)
            if task is None or task.status is not TaskStatus.BLOCKED:
                return False
            children = [child for child in self.list_tasks() if child.parent_task_id == task_id]
            if any(child.status in {TaskStatus.FAILED, TaskStatus.CANCELLED, TaskStatus.INTERRUPTED} for child in children):
                self._finish(task_id, TaskStatus.FAILED, REASON_CHILD_FAILED)
                return False
            if all(child.status is TaskStatus.COMPLETED for child in children) and self._running_count() < self._max_concurrent:
                self._store.transition(task_id, principal_id=self._principal_id, status=TaskStatus.RUNNING)
                return True
            await self._changed.wait()

    async def _run_step(self, task: AgentTask, step_index: int):
        turn_id = await self._allocate_turn_id()
        active = _ActiveStep(
            task_id=task.task_id, step_index=step_index, turn_id=turn_id
        )
        with self._lock:
            self._active_steps[turn_id] = active
            self._task_turn[task.task_id] = turn_id
        logger.info(
            "[TaskRunner] task=%s turn=%s step=%d starting",
            task.task_id,
            turn_id,
            step_index,
        )
        trace = LatencyTrace(trace_id=turn_id)
        token = _CURRENT_TASK.set((self, task.task_id))
        try:
            result = await self._submit_turn(
                self._step_prompt(task, step_index),
                turn_id=turn_id,
                conversation_id=None,
                system_context=self._task_context(task),
                latency_trace=trace,
            )
        except BaseException:
            await self._revoke_turn(turn_id)
            raise
        finally:
            _CURRENT_TASK.reset(token)
            self._cancelled_turns.discard(turn_id)
            with self._lock:
                self._active_steps.pop(turn_id, None)
                if self._task_turn.get(task.task_id) == turn_id:
                    self._task_turn.pop(task.task_id, None)
        response = str(getattr(result, "response", "") or "")
        completed = self._is_complete(response)
        logger.info(
            "[TaskRunner] task=%s turn=%s step=%d status=completed response_len=%d",
            task.task_id,
            turn_id,
            step_index,
            len(response),
        )
        return turn_id, completed, self._bounded_summary(response)

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
            logger.info("[TaskRunner] task=%s completed", task_id)
            self._publish(events.TaskCompleted(task_id=task_id, label="tasks"))
        elif status is TaskStatus.FAILED:
            logger.info(
                "[TaskRunner] task=%s failed reason=%s", task_id, reason or ""
            )
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
        logger.info("[TaskRunner] task=%s interrupted", task_id)

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
        children = [child for child in self.list_tasks() if child.parent_task_id == task.task_id]
        if children:
            lines.append("Subagent results (verify these before completing the parent):")
            for child in children:
                records = self._store.list_steps(child.task_id, principal_id=self._principal_id)
                summary = records[-1].summary if records else "no result yet"
                lines.append(f"- {child.task_id} [{child.status.value}]: {summary[:300]}")
        context = "\n".join(lines)
        if len(context) > self._step_log_chars:
            context = context[: self._step_log_chars]
        return context

    @staticmethod
    def _is_complete(response: str) -> bool:
        normalized = response.strip().upper()
        return normalized == COMPLETION_SENTINEL or normalized.startswith(COMPLETION_SENTINEL + ":")

    def _bounded_summary(self, response: str) -> str:
        stripped = response.strip()
        if self._is_complete(stripped):
            stripped = stripped[len(COMPLETION_SENTINEL):].lstrip(":").strip()
        if not stripped:
            stripped = "completed"
        return stripped[: self._step_log_chars]
