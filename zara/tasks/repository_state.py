"""Repository-local durable state for code tasks, backed by TaskStore."""

from __future__ import annotations

import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
from contextlib import contextmanager
from typing import Optional

from zara.database import DatabaseManager

from .store import ACTIVE_STATES, AgentTask, TaskStatus, TaskStore, TaskTransitionError

_REPOSITORY = re.compile(r"^[a-z][a-z0-9_-]{0,63}$")


class RepositoryTaskState:
    def __init__(self, repo_root: Path) -> None:
        root = Path(repo_root)
        if root.is_symlink() or not root.is_dir():
            raise ValueError("task state requires an existing Git worktree")
        result = subprocess.run(
            ["git", "-C", str(root), "rev-parse", "--show-toplevel"],
            capture_output=True, text=True, check=False,
        )
        if result.returncode or Path(result.stdout.strip()).resolve() != root.resolve():
            raise ValueError("task state requires an exact Git worktree root")
        state_dir = root / ".zara"
        if state_dir.is_symlink():
            raise ValueError("task state directory must not be a symlink")
        for name in ("task-state.sqlite3", "task-state.lock", "task-state.json"):
            if (state_dir / name).is_symlink():
                raise ValueError("task state files must not be symlinks")
        state_dir.mkdir(mode=0o700, exist_ok=True)
        os.chmod(state_dir, 0o700)
        ignore = state_dir / ".gitignore"
        if ignore.is_symlink():
            raise ValueError("task state ignore file must not be a symlink")
        ignore_rules = {"/task-state.sqlite3*", "/task-state.lock", "/code-worker.lock", "/tasks/", "/.gitignore"}
        existing = ignore.read_text(encoding="utf-8") if ignore.exists() else ""
        missing = sorted(ignore_rules - set(existing.splitlines()))
        if missing:
            with ignore.open("a", encoding="utf-8") as file:
                if existing and not existing.endswith("\n"):
                    file.write("\n")
                file.write("\n".join(missing) + "\n")
        self._projection_dir = state_dir / "tasks"
        if self._projection_dir.is_symlink():
            raise ValueError("task projection directory must not be a symlink")
        self._projection_dir.mkdir(mode=0o700, exist_ok=True)
        os.chmod(self._projection_dir, 0o700)
        self._lock_path = state_dir / "task-state.lock"
        self._db = DatabaseManager(state_dir / "task-state.sqlite3")
        self._store = TaskStore(self._db)
        self._db.register_migration(3, [
            "CREATE TABLE IF NOT EXISTS repository_task_aliases (task_id TEXT PRIMARY KEY, repository TEXT NOT NULL, FOREIGN KEY(task_id) REFERENCES agent_tasks(task_id) ON DELETE CASCADE)"
        ])

    def create(self, principal_id: str, repository: str, goal: str) -> AgentTask:
        if not isinstance(repository, str) or not _REPOSITORY.fullmatch(repository):
            raise ValueError("repository must be a bounded alias")
        with self._locked():
            task = self._store.create_task(principal_id=principal_id, goal=goal, max_task_steps=1)
            self._db.execute(
                "INSERT INTO repository_task_aliases (task_id, repository) VALUES (?, ?)",
                (task.task_id, repository),
            )
            self._project(principal_id)
            return task

    def start(self, principal_id: str, task_id: str) -> AgentTask:
        with self._locked():
            task = self._store.transition(task_id, principal_id=principal_id, status=TaskStatus.RUNNING)
            self._project(principal_id)
            return task

    def finish(self, principal_id: str, task_id: str, result: str, *, failed: bool = False) -> AgentTask:
        if not isinstance(result, str) or len(result) > 2000:
            raise ValueError("result must be a string of at most 2000 characters")
        with self._locked():
            current = self._store.get_task(task_id, principal_id=principal_id)
            if current is None or current.status is not TaskStatus.RUNNING:
                raise TaskTransitionError("only a running task can finish")
            self._store.record_step(task_id, principal_id=principal_id, step_index=0, status="failed" if failed else "completed", summary=result)
            task = self._store.transition(task_id, principal_id=principal_id, status=TaskStatus.FAILED if failed else TaskStatus.COMPLETED)
            self._project(principal_id)
            return task

    def cancel(self, principal_id: str, task_id: str, reason: str = "cancelled") -> AgentTask:
        with self._locked():
            task = self._store.transition(task_id, principal_id=principal_id, status=TaskStatus.CANCELLED, reason=reason)
            self._project(principal_id)
            return task

    def interrupt(self, principal_id: str, task_id: str, reason: str = "interrupted") -> AgentTask:
        with self._locked():
            task = self._store.transition(task_id, principal_id=principal_id, status=TaskStatus.INTERRUPTED, reason=reason)
            self._project(principal_id)
            return task

    def get(self, principal_id: str, task_id: str) -> Optional[AgentTask]:
        return self._store.get_task(task_id, principal_id=principal_id)

    def path_for(self, principal_id: str) -> Path:
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise ValueError("principal_id must be a non-empty string")
        digest = hashlib.sha256(principal_id.encode("utf-8")).hexdigest()
        return self._projection_dir / f"{digest}.json"

    def recover_interrupted(self, principal_id: str) -> int:
        with self._locked():
            active = self._store.list_tasks(principal_id=principal_id, statuses=ACTIVE_STATES)
            for task in active:
                self._store.transition(
                    task.task_id,
                    principal_id=principal_id,
                    status=TaskStatus.INTERRUPTED,
                    reason="runtime_recovery",
                )
            if active:
                self._project(principal_id)
            return len(active)

    def result(self, principal_id: str, task_id: str) -> Optional[str]:
        if self._store.get_task(task_id, principal_id=principal_id) is None:
            return None
        steps = self._store.list_steps(task_id, principal_id=principal_id)
        return steps[0].summary if steps else None

    def list(self, principal_id: str) -> list[AgentTask]:
        return self._store.list_tasks(principal_id=principal_id)

    @contextmanager
    def _locked(self):
        with self._lock_path.open("a+b") as file:
            fcntl.flock(file, fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(file, fcntl.LOCK_UN)

    def _project(self, principal_id: str) -> None:
        tasks = []
        for task in self._store.list_tasks(principal_id=principal_id):
            steps = self._store.list_steps(task.task_id, principal_id=principal_id)
            row = self._db.fetch_one(
                "SELECT repository FROM repository_task_aliases WHERE task_id = ?",
                (task.task_id,),
            )
            repository = row["repository"] if row is not None else ""
            result = steps[0].summary if steps else None
            tasks.append({
                "task_id": task.task_id,
                "principal_id": task.principal_id,
                "repository": repository,
                "goal": task.goal,
                "status": task.status.value,
                "result": result,
                "reason": task.reason,
                "created_at": task.created_at,
                "updated_at": task.updated_at,
            })
        path = self.path_for(principal_id)
        if path.is_symlink():
            raise ValueError("task projection must not be a symlink")
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=path.parent, prefix=".task-state-", delete=False) as file:
            temporary = Path(file.name)
            json.dump({"version": 1, "tasks": tasks}, file, sort_keys=True)
            file.write("\n")
            file.flush()
            os.fsync(file.fileno())
        os.chmod(temporary, 0o600)
        os.replace(temporary, path)
