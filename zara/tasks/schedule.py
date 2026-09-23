"""Persistent recurring schedules with Prolog-first execution and LLM escalation."""

from __future__ import annotations

import asyncio
import enum
import logging
import re
import uuid
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Callable, Optional

from ..database import DatabaseManager, get_database

logger = logging.getLogger(__name__)

_MAX_CRON_SEARCH_MINUTES = 366 * 24 * 60 * 5
_MAX_INTERVAL_MINUTES = 366 * 24 * 60
_INTERVAL_RE = re.compile(r"^@every\s+([1-9][0-9]{0,5})([mhd])$", re.IGNORECASE)
_SCHEDULE_ID_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9:._-]{0,127}$")
_CRON_ALIASES = {
    "@hourly": "0 * * * *",
    "@daily": "0 0 * * *",
    "@midnight": "0 0 * * *",
    "@weekly": "0 0 * * 0",
    "@monthly": "0 0 1 * *",
    "@yearly": "0 0 1 1 *",
    "@annually": "0 0 1 1 *",
}


class ScheduleMode(str, enum.Enum):
    AUTO = "auto"
    PROLOG = "prolog"
    LLM = "llm"


class ScheduleState(str, enum.Enum):
    ACTIVE = "active"
    PAUSED = "paused"
    CANCELLED = "cancelled"


@dataclass(frozen=True)
class _CronField:
    values: frozenset[int]
    wildcard: bool


@dataclass(frozen=True)
class CronExpression:
    expression: str
    minute: _CronField
    hour: _CronField
    day: _CronField
    month: _CronField
    weekday: _CronField

    @classmethod
    def parse(cls, expression: str) -> "CronExpression":
        if not isinstance(expression, str) or not expression.strip():
            raise ValueError("cron expression must be a non-empty string")
        normalized = _CRON_ALIASES.get(expression.strip().lower(), expression.strip())
        parts = normalized.split()
        if len(parts) != 5:
            raise ValueError("cron expression must contain exactly five fields")
        try:
            fields = (
                _parse_cron_field(parts[0], 0, 59),
                _parse_cron_field(parts[1], 0, 23),
                _parse_cron_field(parts[2], 1, 31),
                _parse_cron_field(parts[3], 1, 12),
                _parse_cron_field(parts[4], 0, 7, weekday=True),
            )
        except ValueError as error:
            raise ValueError(f"invalid cron expression: {error}") from error
        return cls(normalized, *fields)

    def matches(self, value: datetime) -> bool:
        if value.minute not in self.minute.values:
            return False
        if value.hour not in self.hour.values:
            return False
        if value.month not in self.month.values:
            return False

        day_match = value.day in self.day.values
        cron_weekday = (value.weekday() + 1) % 7
        weekday_match = cron_weekday in self.weekday.values
        if self.day.wildcard and self.weekday.wildcard:
            return True
        if self.day.wildcard:
            return weekday_match
        if self.weekday.wildcard:
            return day_match
        return day_match or weekday_match

    def next_after(self, value: datetime) -> datetime:
        candidate = value.replace(second=0, microsecond=0) + timedelta(minutes=1)
        for _ in range(_MAX_CRON_SEARCH_MINUTES):
            if self.matches(candidate):
                return candidate
            candidate += timedelta(minutes=1)
        raise ValueError("cron expression has no match within five years")


@dataclass(frozen=True)
class IntervalExpression:
    expression: str
    interval: timedelta

    @classmethod
    def parse(cls, expression: str) -> "IntervalExpression":
        if not isinstance(expression, str):
            raise ValueError("interval expression must be a string")
        normalized = expression.strip().lower()
        match = _INTERVAL_RE.fullmatch(normalized)
        if match is None:
            raise ValueError(
                "interval expression must use '@every <positive integer><m|h|d>'"
            )
        amount = int(match.group(1))
        unit = match.group(2).lower()
        multiplier = {"m": 1, "h": 60, "d": 24 * 60}[unit]
        minutes = amount * multiplier
        if minutes < 1 or minutes > _MAX_INTERVAL_MINUTES:
            raise ValueError("interval must be between 1 minute and 366 days")
        return cls(f"@every {amount}{unit}", timedelta(minutes=minutes))

    def next_after(self, value: datetime) -> datetime:
        return _add_elapsed(value, self.interval)


ScheduleExpression = CronExpression | IntervalExpression


def parse_schedule_expression(expression: str) -> ScheduleExpression:
    if isinstance(expression, str) and expression.strip().lower().startswith("@every"):
        return IntervalExpression.parse(expression)
    return CronExpression.parse(expression)


def _timeline_value(value: datetime) -> datetime:
    if value.tzinfo is None or value.utcoffset() is None:
        return value
    return value.astimezone(timezone.utc)


def _add_elapsed(
    value: datetime,
    delta: timedelta,
    *,
    result_timezone=None,
) -> datetime:
    if value.tzinfo is None or value.utcoffset() is None:
        return value + delta
    target = value.astimezone(timezone.utc) + delta
    return target.astimezone(result_timezone or value.tzinfo)


def _next_future_run(
    expression: ScheduleExpression,
    *,
    due_at: datetime,
    now: datetime,
) -> datetime:
    if isinstance(expression, IntervalExpression):
        due_timeline = _timeline_value(due_at)
        now_timeline = _timeline_value(now)
        if now_timeline < due_timeline:
            if (
                due_at.tzinfo is not None
                and due_at.utcoffset() is not None
                and now.tzinfo is not None
                and now.utcoffset() is not None
            ):
                return due_at.astimezone(now.tzinfo)
            return due_at
        elapsed = now_timeline - due_timeline
        steps = int(elapsed // expression.interval) + 1
        result_timezone = (
            now.tzinfo
            if now.tzinfo is not None and now.utcoffset() is not None
            else None
        )
        return _add_elapsed(
            due_at,
            expression.interval * steps,
            result_timezone=result_timezone,
        )
    return expression.next_after(now)


@dataclass(frozen=True)
class ScheduledTask:
    schedule_id: str
    principal_id: str
    label: str
    cron: str
    goal: str
    mode: ScheduleMode
    state: ScheduleState
    next_run_at: Optional[str]
    last_run_at: Optional[str]
    last_status: Optional[str]
    last_route: Optional[str]
    last_task_id: Optional[str]
    created_at: str
    updated_at: str


class ScheduledTaskStore:
    def __init__(self, db: Optional[DatabaseManager] = None) -> None:
        self._db = db or get_database()
        self._ensure_schema()

    def _ensure_schema(self) -> None:
        try:
            self._db.register_migration(
                4,
                [
                    """
                    CREATE TABLE IF NOT EXISTS scheduled_tasks (
                        schedule_id TEXT PRIMARY KEY,
                        principal_id TEXT NOT NULL,
                        label TEXT NOT NULL,
                        cron TEXT NOT NULL,
                        goal TEXT NOT NULL,
                        mode TEXT NOT NULL,
                        state TEXT NOT NULL,
                        next_run_at TEXT,
                        last_run_at TEXT,
                        last_status TEXT,
                        last_route TEXT,
                        last_task_id TEXT,
                        created_at TEXT NOT NULL,
                        updated_at TEXT NOT NULL
                    )
                    """,
                    """
                    CREATE TABLE IF NOT EXISTS scheduled_task_runs (
                        schedule_id TEXT NOT NULL,
                        due_at TEXT NOT NULL,
                        started_at TEXT NOT NULL,
                        finished_at TEXT,
                        status TEXT NOT NULL,
                        route TEXT,
                        task_id TEXT,
                        error TEXT,
                        PRIMARY KEY (schedule_id, due_at),
                        FOREIGN KEY(schedule_id) REFERENCES scheduled_tasks(schedule_id)
                            ON DELETE CASCADE
                    )
                    """,
                    (
                        "CREATE INDEX IF NOT EXISTS idx_scheduled_tasks_principal_state "
                        "ON scheduled_tasks(principal_id, state)"
                    ),
                    (
                        "CREATE INDEX IF NOT EXISTS idx_scheduled_tasks_next_run "
                        "ON scheduled_tasks(next_run_at)"
                    ),
                ],
            )
        except ValueError:
            pass
        self._db.connect()

    def create_schedule(
        self,
        *,
        principal_id: str,
        cron: str,
        goal: str,
        mode: ScheduleMode | str = ScheduleMode.AUTO,
        label: Optional[str] = None,
        schedule_id: Optional[str] = None,
        now: Optional[datetime] = None,
    ) -> ScheduledTask:
        principal = _validate_text(principal_id, "principal")
        clean_goal = _validate_text(goal, "goal", max_chars=2000)
        parsed = parse_schedule_expression(cron)
        selected_mode = _coerce_mode(mode)
        identifier = _validate_schedule_id(
            schedule_id or f"schedule-{uuid.uuid4().hex[:12]}"
        )
        current = now or _local_now()
        timestamp = _iso(current)
        self._db.execute(
            """
            INSERT INTO scheduled_tasks (
                schedule_id, principal_id, label, cron, goal, mode, state,
                next_run_at, last_run_at, last_status, last_route, last_task_id,
                created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, 'active', ?, NULL, NULL, NULL, NULL, ?, ?)
            """,
            (
                identifier,
                principal,
                _validate_label(label, clean_goal),
                parsed.expression,
                clean_goal,
                selected_mode.value,
                _iso(parsed.next_after(current)),
                timestamp,
                timestamp,
            ),
        )
        return self._require(identifier, principal)

    def upsert_definition(
        self,
        *,
        principal_id: str,
        schedule_id: str,
        cron: str,
        goal: str,
        mode: ScheduleMode | str,
        label: Optional[str] = None,
        now: Optional[datetime] = None,
    ) -> ScheduledTask:
        parsed = parse_schedule_expression(cron)
        clean_goal = _validate_text(goal, "goal", max_chars=2000)
        selected_mode = _coerce_mode(mode)
        clean_label = _validate_label(label, clean_goal)
        existing = self.get_schedule(schedule_id, principal_id=principal_id)
        if existing is None:
            return self.create_schedule(
                principal_id=principal_id,
                schedule_id=schedule_id,
                cron=parsed.expression,
                goal=clean_goal,
                mode=selected_mode,
                label=clean_label,
                now=now,
            )

        if (
            existing.cron == parsed.expression
            and existing.goal == clean_goal
            and existing.mode == selected_mode
            and existing.label == clean_label
        ):
            return existing

        current = now or _local_now()
        next_run_at = existing.next_run_at
        if existing.state is ScheduleState.ACTIVE and (
            existing.cron != parsed.expression or next_run_at is None
        ):
            next_run_at = _iso(parsed.next_after(current))
        self._db.execute(
            """
            UPDATE scheduled_tasks
            SET label = ?, cron = ?, goal = ?, mode = ?, next_run_at = ?, updated_at = ?
            WHERE schedule_id = ? AND principal_id = ?
            """,
            (
                clean_label,
                parsed.expression,
                clean_goal,
                selected_mode.value,
                next_run_at,
                _iso(current),
                _validate_schedule_id(schedule_id),
                _validate_text(principal_id, "principal"),
            ),
        )
        return self._require(schedule_id, principal_id)

    def get_schedule(self, schedule_id: str, *, principal_id: str) -> Optional[ScheduledTask]:
        row = self._db.fetch_one(
            "SELECT * FROM scheduled_tasks WHERE schedule_id = ? AND principal_id = ?",
            (
                _validate_schedule_id(schedule_id),
                _validate_text(principal_id, "principal"),
            ),
        )
        return _row_to_schedule(row) if row is not None else None

    def list_schedules(self, *, principal_id: str) -> list[ScheduledTask]:
        rows = self._db.fetch_all(
            """
            SELECT * FROM scheduled_tasks
            WHERE principal_id = ?
            ORDER BY CASE state WHEN 'active' THEN 0 WHEN 'paused' THEN 1 ELSE 2 END,
                     next_run_at, created_at, schedule_id
            """,
            (_validate_text(principal_id, "principal"),),
        )
        return [_row_to_schedule(row) for row in rows]

    def due_schedules(self, *, principal_id: str, now: datetime) -> list[ScheduledTask]:
        return [
            row
            for row in self.list_schedules(principal_id=principal_id)
            if row.state is ScheduleState.ACTIVE
            and row.next_run_at is not None
            and _parse_iso(row.next_run_at) <= now
        ]

    def pause(self, schedule_id: str, *, principal_id: str) -> ScheduledTask:
        row = self._require(schedule_id, principal_id)
        if row.state is ScheduleState.CANCELLED:
            raise ValueError("cancelled schedules cannot be paused")
        return self._set_state(
            schedule_id,
            principal_id=principal_id,
            state=ScheduleState.PAUSED,
            next_run_at=None,
        )

    def resume(
        self,
        schedule_id: str,
        *,
        principal_id: str,
        now: Optional[datetime] = None,
    ) -> ScheduledTask:
        row = self._require(schedule_id, principal_id)
        if row.state is ScheduleState.CANCELLED:
            raise ValueError("cancelled schedules cannot be resumed")
        current = now or _local_now()
        return self._set_state(
            schedule_id,
            principal_id=principal_id,
            state=ScheduleState.ACTIVE,
            next_run_at=_iso(parse_schedule_expression(row.cron).next_after(current)),
            now=current,
        )

    def cancel(self, schedule_id: str, *, principal_id: str) -> ScheduledTask:
        return self._set_state(
            schedule_id,
            principal_id=principal_id,
            state=ScheduleState.CANCELLED,
            next_run_at=None,
        )

    def claim_run(
        self,
        schedule: ScheduledTask,
        *,
        due_at: datetime,
        next_run_at: datetime,
    ) -> bool:
        due = _iso(due_at)
        started = _iso(_local_now())
        with self._db.transaction(immediate=True) as connection:
            live = connection.execute(
                """
                SELECT state, next_run_at FROM scheduled_tasks
                WHERE schedule_id = ? AND principal_id = ?
                """,
                (schedule.schedule_id, schedule.principal_id),
            ).fetchone()
            if live is None:
                return False
            if live["state"] != ScheduleState.ACTIVE.value:
                return False
            if live["next_run_at"] != due:
                return False
            present = connection.execute(
                "SELECT 1 FROM scheduled_task_runs WHERE schedule_id = ? AND due_at = ?",
                (schedule.schedule_id, due),
            ).fetchone()
            if present is not None:
                return False
            connection.execute(
                """
                INSERT INTO scheduled_task_runs (schedule_id, due_at, started_at, status)
                VALUES (?, ?, ?, 'running')
                """,
                (schedule.schedule_id, due, started),
            )
            connection.execute(
                """
                UPDATE scheduled_tasks
                SET next_run_at = ?, last_run_at = ?, last_status = 'running', updated_at = ?
                WHERE schedule_id = ? AND principal_id = ?
                """,
                (
                    _iso(next_run_at),
                    due,
                    started,
                    schedule.schedule_id,
                    schedule.principal_id,
                ),
            )
        return True

    def finish_run(
        self,
        schedule_id: str,
        *,
        principal_id: str,
        due_at: datetime,
        status: str,
        route: str,
        task_id: Optional[str] = None,
        error: Optional[str] = None,
    ) -> ScheduledTask:
        finished = _iso(_local_now())
        due = _iso(due_at)
        self._db.execute(
            """
            UPDATE scheduled_task_runs
            SET finished_at = ?, status = ?, route = ?, task_id = ?, error = ?
            WHERE schedule_id = ? AND due_at = ?
            """,
            (finished, status, route, task_id, error, schedule_id, due),
        )
        self._db.execute(
            """
            UPDATE scheduled_tasks
            SET last_status = ?, last_route = ?, last_task_id = ?, updated_at = ?
            WHERE schedule_id = ? AND principal_id = ?
            """,
            (status, route, task_id, finished, schedule_id, principal_id),
        )
        return self._require(schedule_id, principal_id)

    def _set_state(
        self,
        schedule_id: str,
        *,
        principal_id: str,
        state: ScheduleState,
        next_run_at: Optional[str],
        now: Optional[datetime] = None,
    ) -> ScheduledTask:
        self._require(schedule_id, principal_id)
        self._db.execute(
            """
            UPDATE scheduled_tasks
            SET state = ?, next_run_at = ?, updated_at = ?
            WHERE schedule_id = ? AND principal_id = ?
            """,
            (
                state.value,
                next_run_at,
                _iso(now or _local_now()),
                schedule_id,
                principal_id,
            ),
        )
        return self._require(schedule_id, principal_id)

    def _require(self, schedule_id: str, principal_id: str) -> ScheduledTask:
        row = self.get_schedule(schedule_id, principal_id=principal_id)
        if row is None:
            raise ValueError(f"schedule not found: {schedule_id!r}")
        return row


class ScheduledTaskService:
    def __init__(
        self,
        *,
        store: ScheduledTaskStore,
        task_runner,
        prolog_engine,
        principal_id: str,
        poll_seconds: float = 15.0,
        now: Callable[[], datetime] = lambda: datetime.now().astimezone(),
    ) -> None:
        if poll_seconds <= 0:
            raise ValueError("poll_seconds must be positive")
        self._store = store
        self._task_runner = task_runner
        self._prolog_engine = prolog_engine
        self._principal_id = _validate_text(principal_id, "principal")
        self._poll_seconds = float(poll_seconds)
        self._now = now
        self._loop_task: Optional[asyncio.Task] = None

    async def start(self) -> None:
        await self.sync_prolog_definitions()
        if self._loop_task is None or self._loop_task.done():
            self._loop_task = asyncio.create_task(self._loop(), name="zara-scheduled-tasks")

    async def stop(self) -> None:
        task = self._loop_task
        self._loop_task = None
        if task is None:
            return
        task.cancel()
        await asyncio.gather(task, return_exceptions=True)

    async def create_schedule(
        self,
        *,
        cron: str,
        goal: str,
        mode: ScheduleMode | str = ScheduleMode.AUTO,
        label: Optional[str] = None,
    ) -> ScheduledTask:
        return self._store.create_schedule(
            principal_id=self._principal_id,
            cron=cron,
            goal=goal,
            mode=mode,
            label=label,
            now=self._now(),
        )

    def list_schedules(self) -> list[ScheduledTask]:
        return self._store.list_schedules(principal_id=self._principal_id)

    def get_schedule(self, schedule_id: str) -> Optional[ScheduledTask]:
        return self._store.get_schedule(schedule_id, principal_id=self._principal_id)

    async def pause_schedule(self, schedule_id: str) -> ScheduledTask:
        return self._store.pause(schedule_id, principal_id=self._principal_id)

    async def resume_schedule(self, schedule_id: str) -> ScheduledTask:
        return self._store.resume(
            schedule_id,
            principal_id=self._principal_id,
            now=self._now(),
        )

    async def cancel_schedule(self, schedule_id: str) -> ScheduledTask:
        return self._store.cancel(schedule_id, principal_id=self._principal_id)

    async def sync_prolog_definitions(self) -> None:
        if self._prolog_engine is None:
            return
        try:
            definitions = await asyncio.to_thread(
                self._prolog_engine.scheduled_task_definitions
            )
        except Exception:
            logger.warning("[ScheduledTasks] failed to read Prolog definitions", exc_info=True)
            return
        for definition in definitions:
            raw_id = str(definition["id"])
            self._store.upsert_definition(
                principal_id=self._principal_id,
                schedule_id=_validate_schedule_id(f"prolog:{raw_id}"),
                cron=str(definition["cron"]),
                goal=str(definition["goal"]),
                mode=str(definition["mode"]),
                label=raw_id,
                now=self._now(),
            )

    async def run_due(self, *, now: Optional[datetime] = None) -> int:
        await self.sync_prolog_definitions()
        current = now or self._now()
        rows = self._store.due_schedules(
            principal_id=self._principal_id,
            now=current,
        )
        started = 0
        for row in rows:
            due = _parse_iso(row.next_run_at)
            expression = parse_schedule_expression(row.cron)
            next_run = _next_future_run(expression, due_at=due, now=current)
            if not self._store.claim_run(row, due_at=due, next_run_at=next_run):
                continue
            started += 1
            await self._execute(row, due)
        return started

    async def _execute(self, row: ScheduledTask, due_at: datetime) -> None:
        route = await self._route(row)
        if route == "prolog" and self._prolog_engine is not None:
            try:
                handled = await asyncio.to_thread(
                    self._prolog_engine.execute_command,
                    row.goal,
                )
            except Exception:
                handled = False
                logger.warning(
                    "[ScheduledTasks] Prolog execution failed for %s",
                    row.schedule_id,
                    exc_info=True,
                )
            if handled:
                self._store.finish_run(
                    row.schedule_id,
                    principal_id=self._principal_id,
                    due_at=due_at,
                    status="completed",
                    route="prolog",
                )
                return
            route = "llm"

        if self._task_runner is None:
            self._store.finish_run(
                row.schedule_id,
                principal_id=self._principal_id,
                due_at=due_at,
                status="failed",
                route=route,
                error="long-horizon task runner unavailable",
            )
            return
        try:
            task = await self._task_runner.create_task(goal=row.goal)
        except Exception as error:
            self._store.finish_run(
                row.schedule_id,
                principal_id=self._principal_id,
                due_at=due_at,
                status="failed",
                route="llm",
                error=str(error)[:500],
            )
            return
        self._store.finish_run(
            row.schedule_id,
            principal_id=self._principal_id,
            due_at=due_at,
            status="delegated",
            route="llm",
            task_id=task.task_id,
        )

    async def _route(self, row: ScheduledTask) -> str:
        if row.mode is ScheduleMode.LLM:
            return "llm"
        if row.mode is ScheduleMode.PROLOG:
            return "prolog"
        if self._prolog_engine is None:
            return "llm"
        try:
            route = await asyncio.to_thread(
                self._prolog_engine.scheduled_execution_route,
                row.goal,
            )
        except Exception:
            logger.warning("[ScheduledTasks] Prolog route failed; escalating", exc_info=True)
            return "llm"
        return route if route in {"prolog", "llm"} else "llm"

    async def _loop(self) -> None:
        while True:
            try:
                await self.run_due()
            except asyncio.CancelledError:
                raise
            except Exception:
                logger.exception("[ScheduledTasks] scheduler tick failed")
            await asyncio.sleep(self._poll_seconds)


def _parse_cron_field(
    text: str,
    minimum: int,
    maximum: int,
    *,
    weekday: bool = False,
) -> _CronField:
    if not text:
        raise ValueError("empty cron field")
    wildcard = text == "*"
    values: set[int] = set()
    for item in text.split(","):
        if not item:
            raise ValueError("empty cron list item")
        base, separator, step_text = item.partition("/")
        step = 1
        if separator:
            if not step_text.isdigit() or int(step_text) < 1:
                raise ValueError("cron step must be a positive integer")
            step = int(step_text)
        if base == "*":
            start, end = minimum, maximum
        elif "-" in base:
            left, right = base.split("-", 1)
            if not left.isdigit() or not right.isdigit():
                raise ValueError("cron ranges must be numeric")
            start, end = int(left), int(right)
            if start > end:
                raise ValueError("cron range start exceeds end")
        else:
            if not base.isdigit():
                raise ValueError("cron values must be numeric")
            start = end = int(base)
        if start < minimum or end > maximum:
            raise ValueError(f"cron value must be between {minimum} and {maximum}")
        for value in range(start, end + 1, step):
            values.add(0 if weekday and value == 7 else value)
    if not values:
        raise ValueError("cron field produced no values")
    return _CronField(frozenset(values), wildcard)


def _coerce_mode(value: ScheduleMode | str) -> ScheduleMode:
    if isinstance(value, ScheduleMode):
        return value
    try:
        return ScheduleMode(str(value))
    except ValueError as error:
        raise ValueError("mode must be auto, prolog, or llm") from error


def _validate_text(value: str, name: str, *, max_chars: int = 256) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a non-empty string")
    clean = value.strip()
    if len(clean) > max_chars:
        raise ValueError(f"{name} exceeds {max_chars} characters")
    return clean


def _validate_label(label: Optional[str], goal: str) -> str:
    if label is None or not str(label).strip():
        return goal[:80]
    return _validate_text(str(label), "label", max_chars=120)


def _validate_schedule_id(value: str) -> str:
    if not isinstance(value, str) or not _SCHEDULE_ID_RE.fullmatch(value):
        raise ValueError("schedule id is invalid")
    return value


def _local_now() -> datetime:
    return datetime.now().astimezone()


def _iso(value: datetime) -> str:
    return value.isoformat(timespec="seconds")


def _parse_iso(value: Optional[str]) -> datetime:
    if value is None:
        raise ValueError("schedule has no next run")
    return datetime.fromisoformat(value)


def _row_to_schedule(row) -> ScheduledTask:
    return ScheduledTask(
        schedule_id=str(row["schedule_id"]),
        principal_id=str(row["principal_id"]),
        label=str(row["label"]),
        cron=str(row["cron"]),
        goal=str(row["goal"]),
        mode=ScheduleMode(str(row["mode"])),
        state=ScheduleState(str(row["state"])),
        next_run_at=row["next_run_at"],
        last_run_at=row["last_run_at"],
        last_status=row["last_status"],
        last_route=row["last_route"],
        last_task_id=row["last_task_id"],
        created_at=str(row["created_at"]),
        updated_at=str(row["updated_at"]),
    )


__all__ = [
    "CronExpression",
    "IntervalExpression",
    "ScheduleExpression",
    "parse_schedule_expression",
    "ScheduleMode",
    "ScheduleState",
    "ScheduledTask",
    "ScheduledTaskService",
    "ScheduledTaskStore",
]
