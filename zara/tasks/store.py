"""Persistent, principal-scoped storage for long-horizon agent tasks.

The store owns the AgentTask record and its bounded step log. State changes
flow exclusively through :meth:`TaskStore.transition`, which is guarded by a
legal-transition table so terminal states cannot be resurrected silently.
This is deliberately separate from ``TodoStore`` (human GTD todos) — machine
executed long-horizon tasks have a different lifecycle.
"""

from __future__ import annotations

import enum
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Iterable, Optional, Sequence, Union

from ..database import DatabaseManager, get_database

MAX_GOAL_CHARS = 2000
DEFAULT_STEP_LOG_CHARS = 2000
_MAX_TASK_ID_LENGTH = 64

STEP_STATUSES = frozenset({"completed", "failed", "cancelled"})


class TaskStatus(str, enum.Enum):
    PENDING = "pending"
    RUNNING = "running"
    WAITING_APPROVAL = "waiting_approval"
    WAITING_INPUT = "waiting_input"
    BLOCKED = "blocked"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"
    INTERRUPTED = "interrupted"


ACTIVE_STATES = frozenset(
    {
        TaskStatus.RUNNING,
        TaskStatus.WAITING_APPROVAL,
        TaskStatus.WAITING_INPUT,
        TaskStatus.BLOCKED,
    }
)

LEGAL_TRANSITIONS: dict[TaskStatus, frozenset[TaskStatus]] = {
    TaskStatus.PENDING: frozenset({TaskStatus.RUNNING, TaskStatus.CANCELLED}),
    TaskStatus.RUNNING: frozenset(
        {
            TaskStatus.WAITING_APPROVAL,
            TaskStatus.WAITING_INPUT,
            TaskStatus.BLOCKED,
            TaskStatus.COMPLETED,
            TaskStatus.FAILED,
            TaskStatus.CANCELLED,
            TaskStatus.INTERRUPTED,
        }
    ),
    TaskStatus.WAITING_APPROVAL: frozenset(
        {
            TaskStatus.RUNNING,
            TaskStatus.FAILED,
            TaskStatus.CANCELLED,
            TaskStatus.INTERRUPTED,
        }
    ),
    TaskStatus.WAITING_INPUT: frozenset(
        {
            TaskStatus.RUNNING,
            TaskStatus.FAILED,
            TaskStatus.CANCELLED,
            TaskStatus.INTERRUPTED,
        }
    ),
    TaskStatus.BLOCKED: frozenset(
        {
            TaskStatus.RUNNING,
            TaskStatus.FAILED,
            TaskStatus.CANCELLED,
            TaskStatus.INTERRUPTED,
        }
    ),
    TaskStatus.COMPLETED: frozenset(),
    TaskStatus.FAILED: frozenset(),
    TaskStatus.CANCELLED: frozenset(),
    TaskStatus.INTERRUPTED: frozenset(
        {TaskStatus.RUNNING, TaskStatus.CANCELLED}
    ),
}


class TaskStoreError(ValueError):
    """Raised when a task lookup or mutation cannot be applied."""


class TaskTransitionError(TaskStoreError):
    """Raised when a state change is not legal for the task's current state."""


@dataclass(frozen=True)
class AgentTask:
    task_id: str
    principal_id: str
    goal: str
    status: TaskStatus
    max_task_steps: int
    steps_completed: int
    created_at: str
    updated_at: str
    reason: Optional[str] = None


@dataclass(frozen=True)
class TaskStepRecord:
    task_id: str
    step_index: int
    status: str
    summary: str
    created_at: str


StatusLike = Union[TaskStatus, str]


def _coerce_status(value: StatusLike) -> TaskStatus:
    if isinstance(value, TaskStatus):
        return value
    if isinstance(value, str):
        try:
            return TaskStatus(value)
        except ValueError:
            pass
    raise ValueError(
        "status must be one of: " + ", ".join(item.value for item in TaskStatus)
    )


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None, microsecond=0).isoformat()


class TaskStore:
    """SQLite-backed store for :class:`AgentTask` records and step logs."""

    def __init__(
        self,
        db: Optional[DatabaseManager] = None,
        *,
        step_log_chars: int = DEFAULT_STEP_LOG_CHARS,
    ) -> None:
        if isinstance(step_log_chars, bool) or not isinstance(step_log_chars, int):
            raise ValueError("step_log_chars must be an integer")
        if step_log_chars < 1:
            raise ValueError("step_log_chars must be at least 1")
        self._db = db or get_database()
        self._step_log_chars = step_log_chars
        self._ensure_schema()

    def _ensure_schema(self) -> None:
        try:
            self._db.register_migration(
                2,
                [
                    """
                    CREATE TABLE IF NOT EXISTS agent_tasks (
                        task_id TEXT PRIMARY KEY,
                        principal_id TEXT NOT NULL,
                        goal TEXT NOT NULL,
                        status TEXT NOT NULL,
                        reason TEXT,
                        max_task_steps INTEGER NOT NULL,
                        steps_completed INTEGER NOT NULL DEFAULT 0,
                        created_at TEXT NOT NULL,
                        updated_at TEXT NOT NULL
                    )
                    """,
                    """
                    CREATE TABLE IF NOT EXISTS agent_task_steps (
                        task_id TEXT NOT NULL,
                        step_index INTEGER NOT NULL,
                        status TEXT NOT NULL,
                        summary TEXT NOT NULL,
                        created_at TEXT NOT NULL,
                        PRIMARY KEY (task_id, step_index),
                        FOREIGN KEY(task_id) REFERENCES agent_tasks(task_id)
                            ON DELETE CASCADE
                    )
                    """,
                    "CREATE INDEX IF NOT EXISTS idx_agent_tasks_status ON agent_tasks(status)",
                    (
                        "CREATE INDEX IF NOT EXISTS idx_agent_tasks_principal "
                        "ON agent_tasks(principal_id)"
                    ),
                ],
            )
        except ValueError:
            pass
        self._db.connect()

    # ------------------------------------------------------------------
    # Task CRUD

    def create_task(
        self,
        *,
        principal_id: str,
        goal: str,
        max_task_steps: int,
    ) -> AgentTask:
        principal = _validate_principal(principal_id)
        clean_goal = _validate_goal(goal)
        steps_budget = _validate_step_budget(max_task_steps)
        now = _now_iso()
        task_id = f"task-{uuid.uuid4().hex[:12]}"
        self._db.execute(
            """
            INSERT INTO agent_tasks (
                task_id, principal_id, goal, status, reason,
                max_task_steps, steps_completed, created_at, updated_at
            ) VALUES (?, ?, ?, ?, NULL, ?, 0, ?, ?)
            """,
            (
                task_id,
                principal,
                clean_goal,
                TaskStatus.PENDING.value,
                steps_budget,
                now,
                now,
            ),
        )
        return self.get_task(task_id, principal_id=principal)

    def get_task(self, task_id: str, *, principal_id: str) -> Optional[AgentTask]:
        row = self._db.fetch_one(
            "SELECT * FROM agent_tasks WHERE task_id = ? AND principal_id = ?",
            (task_id, _validate_principal(principal_id)),
        )
        return _row_to_task(row) if row is not None else None

    def list_tasks(
        self,
        *,
        principal_id: str,
        statuses: Optional[Iterable[StatusLike]] = None,
    ) -> list[AgentTask]:
        principal = _validate_principal(principal_id)
        params: list[object] = [principal]
        query = "SELECT * FROM agent_tasks WHERE principal_id = ?"
        if statuses is not None:
            values = [_coerce_status(status) for status in statuses]
            placeholders = ",".join("?" for _ in values)
            query += f" AND status IN ({placeholders})"
            params.extend(status.value for status in values)
        rows = self._db.fetch_all(query + " ORDER BY created_at, task_id", params)
        return [_row_to_task(row) for row in rows]

    def transition(
        self,
        task_id: str,
        *,
        principal_id: str,
        status: StatusLike,
        reason: Optional[str] = None,
    ) -> AgentTask:
        target = _coerce_status(status)
        clean_reason = _validate_reason(reason)
        current = self.get_task(task_id, principal_id=principal_id)
        if current is None:
            raise TaskStoreError(f"task not found: {task_id!r}")
        allowed = LEGAL_TRANSITIONS[current.status]
        if target not in allowed:
            raise TaskTransitionError(
                f"illegal task transition {current.status.value!r} -> {target.value!r} "
                f"for {task_id!r}"
            )
        self._db.execute(
            "UPDATE agent_tasks SET status = ?, reason = ?, updated_at = ? WHERE task_id = ?",
            (target.value, clean_reason, _now_iso(), task_id),
        )
        return self.get_task(task_id, principal_id=principal_id)

    # ------------------------------------------------------------------
    # Step log

    def record_step(
        self,
        task_id: str,
        *,
        principal_id: str,
        step_index: int,
        status: str,
        summary: str,
    ) -> TaskStepRecord:
        if self.get_task(task_id, principal_id=principal_id) is None:
            raise TaskStoreError(f"task not found: {task_id!r}")
        if status not in STEP_STATUSES:
            raise ValueError(
                "step status must be one of: " + ", ".join(sorted(STEP_STATUSES))
            )
        if isinstance(step_index, bool) or not isinstance(step_index, int):
            raise ValueError("step_index must be an integer")
        if step_index < 0:
            raise ValueError("step_index must not be negative")
        if not isinstance(summary, str):
            raise ValueError("summary must be a string")
        if len(summary) > self._step_log_chars:
            raise ValueError(
                f"summary exceeds the {self._step_log_chars} character step log bound"
            )
        created_at = _now_iso()
        try:
            self._db.execute(
                """
                INSERT INTO agent_task_steps (task_id, step_index, status, summary, created_at)
                VALUES (?, ?, ?, ?, ?)
                """,
                (task_id, step_index, status, summary, created_at),
            )
        except Exception as error:
            if "UNIQUE constraint failed" in str(error):
                raise TaskStoreError(
                    f"step {step_index} already recorded for {task_id!r}"
                ) from error
            raise
        if status == "completed":
            self._db.execute(
                "UPDATE agent_tasks SET steps_completed = steps_completed + 1, "
                "updated_at = ? WHERE task_id = ?",
                (_now_iso(), task_id),
            )
        return TaskStepRecord(
            task_id=task_id,
            step_index=step_index,
            status=status,
            summary=summary,
            created_at=created_at,
        )

    def list_steps(self, task_id: str, *, principal_id: str) -> list[TaskStepRecord]:
        if self.get_task(task_id, principal_id=principal_id) is None:
            raise TaskStoreError(f"task not found: {task_id!r}")
        rows = self._db.fetch_all(
            "SELECT * FROM agent_task_steps WHERE task_id = ? ORDER BY step_index",
            (task_id,),
        )
        return [
            TaskStepRecord(
                task_id=row["task_id"],
                step_index=int(row["step_index"]),
                status=row["status"],
                summary=row["summary"],
                created_at=row["created_at"],
            )
            for row in rows
        ]

    # ------------------------------------------------------------------
    # Recovery

    def recover_interrupted(self) -> int:
        """Move tasks left active by a dead runtime into ``interrupted``."""
        with self._db.transaction() as conn:
            cursor = conn.execute(
                """
                UPDATE agent_tasks
                SET status = ?, reason = ?, updated_at = ?
                WHERE status IN (?, ?, ?, ?)
                """,
                (
                    TaskStatus.INTERRUPTED.value,
                    "runtime_recovery",
                    _now_iso(),
                    TaskStatus.RUNNING.value,
                    TaskStatus.WAITING_APPROVAL.value,
                    TaskStatus.WAITING_INPUT.value,
                    TaskStatus.BLOCKED.value,
                ),
            )
            return int(cursor.rowcount or 0)


def _validate_principal(principal_id: object) -> str:
    if not isinstance(principal_id, str) or not principal_id.strip():
        raise ValueError("principal_id must be a non-empty string")
    return principal_id


def _validate_goal(goal: object) -> str:
    if not isinstance(goal, str):
        raise ValueError("goal must be a string")
    clean = goal.strip()
    if not clean:
        raise ValueError("goal must not be empty")
    if len(clean) > MAX_GOAL_CHARS:
        raise ValueError(f"goal must not exceed {MAX_GOAL_CHARS} characters")
    return clean


def _validate_step_budget(value: object) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise ValueError("max_task_steps must be a positive integer")
    return value


def _validate_reason(reason: Optional[str]) -> Optional[str]:
    if reason is None:
        return None
    if not isinstance(reason, str):
        raise ValueError("reason must be a string or None")
    clean = reason.strip()
    if len(clean) > 128:
        raise ValueError("reason must not exceed 128 characters")
    return clean or None


def _row_to_task(row) -> AgentTask:
    return AgentTask(
        task_id=row["task_id"],
        principal_id=row["principal_id"],
        goal=row["goal"],
        status=TaskStatus(row["status"]),
        max_task_steps=int(row["max_task_steps"]),
        steps_completed=int(row["steps_completed"]),
        created_at=row["created_at"],
        updated_at=row["updated_at"],
        reason=row["reason"],
    )


__all__ = [
    "ACTIVE_STATES",
    "AgentTask",
    "DEFAULT_STEP_LOG_CHARS",
    "LEGAL_TRANSITIONS",
    "MAX_GOAL_CHARS",
    "STEP_STATUSES",
    "TaskStatus",
    "TaskStepRecord",
    "TaskStore",
    "TaskStoreError",
    "TaskTransitionError",
]
