"""Small scheduled-task adapter over the canonical :class:`PrologEngine`."""

from __future__ import annotations

from typing import Any


class ScheduledTaskProlog:
    def __init__(self, engine) -> None:
        self._engine = engine

    def execute_command(self, goal: str) -> bool:
        return bool(self._engine.execute_command(goal))

    def scheduled_execution_route(self, goal: str) -> str:
        query = (
            "scheduled_tasks:execution_route("
            f"{_prolog_string(goal)}, Route)"
        )
        row = self._engine.query_once(query)
        if not row:
            return "llm"
        return _text(row.get("Route"))

    def scheduled_task_definitions(self) -> list[dict[str, str]]:
        rows = self._engine.query_all(
            "scheduled_tasks:definition(Id, Cron, Goal, Mode)",
            max_solutions=256,
        )
        return [
            {
                "id": _text(row.get("Id")),
                "cron": _text(row.get("Cron")),
                "goal": _text(row.get("Goal")),
                "mode": _text(row.get("Mode")),
            }
            for row in rows
        ]


def _prolog_string(value: str) -> str:
    import json

    return json.dumps(str(value), ensure_ascii=False)


def _text(value: Any) -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return str(value)


__all__ = ["ScheduledTaskProlog"]
