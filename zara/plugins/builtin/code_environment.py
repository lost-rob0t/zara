"""Parallel coding workers for configured, separate repository worktrees."""

from __future__ import annotations

import fcntl
import json
import os
import pwd
import re
import shutil
import signal
import subprocess
import threading
from concurrent.futures import Future, ThreadPoolExecutor, TimeoutError as FutureTimeout
from contextlib import nullcontext
from pathlib import Path
from typing import Literal

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from zara.plugins import PluginMetadata, ServicePlugin, current_tool_cancellation


MAX_TASKS = 8
MAX_TASK_CHARS = 12_000
MAX_OUTPUT_CHARS = 24_000


class CodeTask(BaseModel):
    task: str = Field(..., min_length=1, max_length=MAX_TASK_CHARS)
    project: str = Field(..., min_length=1, max_length=128)
    mode: Literal["implement", "review", "plan"] = "implement"


class CodeTasksArgs(BaseModel):
    tasks: list[CodeTask] = Field(..., min_length=1, max_length=MAX_TASKS)


class CodeTaskLookupArgs(BaseModel):
    project: str = Field(..., min_length=1, max_length=128)
    task_id: str = Field(..., min_length=1, max_length=64)


class CodeTaskWaitArgs(CodeTaskLookupArgs):
    timeout_seconds: float = Field(10, ge=0, le=30)


class CodeEnvironmentPlugin(ServicePlugin):
    enabled_by_default = False
    metadata = PluginMetadata(
        name="code",
        version="0.1.0",
        description="Run bounded coding tasks in parallel repository worktrees.",
    )

    def __init__(self) -> None:
        self._binary = ""
        self._sandbox_binary = ""
        self._projects: dict[str, Path] = {}
        self._max_concurrent = 2
        self._timeout_seconds = 900.0
        self._allow_shell = False
        self._project_trust = False
        self._model_provider = ""
        self._model = ""
        self._thinking = ""
        self._credential_env_vars: tuple[str, ...] = ()
        self._principal_id = ""
        self._state_by_project = {}
        self._lock = threading.RLock()
        self._processes: set[subprocess.Popen] = set()
        self._busy_projects: set[str] = set()
        self._futures: dict[str, Future] = {}
        self._future_projects: dict[str, str] = {}
        self._task_states: dict[str, object] = {}
        self._process_by_task: dict[str, subprocess.Popen] = {}
        self._task_locks: dict[str, threading.RLock] = {}
        self._cancel_requested: set[str] = set()
        self._lease_fds: dict[str, int] = {}
        self._worker_context = threading.local()
        self._executor: ThreadPoolExecutor | None = None
        self._started = False

    def start(self, runtime) -> None:
        configuration = dict(runtime.configuration)
        binary = str(configuration.get("binary", "pi")).strip()
        resolved_binary = shutil.which(binary)
        if not resolved_binary:
            raise ValueError("code worker binary is unavailable")
        sandbox_binary = shutil.which("bwrap")
        if not sandbox_binary:
            raise ValueError("code worker filesystem sandbox is unavailable")
        raw_projects = configuration.get("projects", [])
        if not isinstance(raw_projects, list) or not raw_projects:
            raise ValueError("code requires Prolog-configured project aliases")
        projects = {}
        for name in raw_projects:
            if not isinstance(name, str) or not name or len(name) > 128:
                raise ValueError("code project names must be bounded strings")
            path = self._resolve_project(name)
            home = Path(pwd.getpwuid(os.getuid()).pw_dir).resolve()
            if path == home or path in home.parents:
                raise ValueError("code repository must not expose the home directory")
            projects[name] = path
        if len(set(projects.values())) != len(projects):
            raise ValueError("code project aliases must use separate worktrees")
        concurrency = configuration.get("max_concurrent", 2)
        if isinstance(concurrency, bool) or not isinstance(concurrency, int) or not 1 <= concurrency <= MAX_TASKS:
            raise ValueError(f"code max_concurrent must be between 1 and {MAX_TASKS}")
        timeout = configuration.get("timeout_seconds", 900)
        if isinstance(timeout, bool) or not isinstance(timeout, (int, float)) or not 1 <= timeout <= 3600:
            raise ValueError("code timeout_seconds must be between 1 and 3600")
        for key in ("allow_shell", "project_trust"):
            if not isinstance(configuration.get(key, False), bool):
                raise ValueError(f"code {key} must be true or false")
        settings = {}
        for key, maximum in (("model_provider", 128), ("model", 256), ("thinking", 32)):
            value = configuration.get(key, "")
            if not isinstance(value, str) or len(value) > maximum:
                raise ValueError(f"code {key} must be a bounded string")
            settings[key] = value.strip()
        credential_env_vars = configuration.get("credential_env_vars", [])
        if not isinstance(credential_env_vars, list) or len(credential_env_vars) > 16 or any(
            not isinstance(name, str) or not re.fullmatch(r"[A-Z][A-Z0-9_]{0,63}", name)
            for name in credential_env_vars
        ):
            raise ValueError("code credential_env_vars must list bounded uppercase names")
        principal_id = getattr(runtime, "principal_id", None)
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise ValueError("code requires a runtime principal")
        if not self._probe_sandbox(sandbox_binary):
            raise ValueError("code worker filesystem sandbox is unavailable")
        with self._lock:
            self._binary = resolved_binary
            self._sandbox_binary = sandbox_binary
            self._projects = projects
            self._max_concurrent = concurrency
            self._timeout_seconds = float(timeout)
            self._allow_shell = configuration.get("allow_shell", False)
            self._project_trust = configuration.get("project_trust", False)
            self._model_provider = settings["model_provider"]
            self._model = settings["model"]
            self._thinking = settings["thinking"]
            self._credential_env_vars = tuple(credential_env_vars)
            self._principal_id = principal_id.strip()
        for alias in projects:
            state = self._state_for_project(alias)
            state.recover_interrupted(self._principal_id)
        with self._lock:
            self._executor = ThreadPoolExecutor(max_workers=concurrency, thread_name_prefix="zara-code")
            self._started = True

    def stop(self) -> None:
        with self._lock:
            self._started = False
            futures = tuple(self._futures.items())
            states = dict(self._task_states)
            executor = self._executor
            self._executor = None
        for task_id, future in futures:
            with self._lock:
                task_lock = self._task_locks.setdefault(task_id, threading.RLock())
            with task_lock:
                self._cancel_requested.add(task_id)
                with self._lock:
                    process = self._process_by_task.get(task_id)
                if process is not None:
                    self._terminate(process)
                    process.wait(timeout=5)
                state = states[task_id]
                task = state.get(self._principal_id, task_id)
                if task is not None and task.status.value == "running":
                    state.interrupt(self._principal_id, task_id, "runtime_shutdown")
                elif task is not None and task.status.value == "pending":
                    state.cancel(self._principal_id, task_id, "runtime_shutdown")
                future.cancel()
        if executor is not None:
            executor.shutdown(wait=False, cancel_futures=True)

    def tools(self):
        def code_tasks(tasks: list[CodeTask]) -> str:
            return self._execute(tasks)

        def code_task_status(project: str, task_id: str) -> str:
            return json.dumps(self._status(project, task_id))

        def code_task_wait(project: str, task_id: str, timeout_seconds: float = 10) -> str:
            return json.dumps(self._wait(project, task_id, timeout_seconds))

        def code_task_cancel(project: str, task_id: str) -> str:
            return json.dumps(self._cancel(project, task_id))

        return (StructuredTool.from_function(
            code_tasks,
            name="code_tasks",
            description=(
                "Run up to eight independent coding tasks in configured repository worktrees. "
                "Each task needs a distinct project alias; returns ordered task IDs immediately. "
                "Use code_task_status or code_task_wait to retrieve results. Implement can edit files; review and plan "
                "are read-only."
            ),
            args_schema=CodeTasksArgs,
            metadata={"zara_requires_approval": True, "zara_supports_cancellation": True},
        ), StructuredTool.from_function(
            code_task_status,
            name="code_task_status",
            description="Read one persisted code task status and bounded result.",
            args_schema=CodeTaskLookupArgs,
            metadata={"zara_read_only": True},
        ), StructuredTool.from_function(
            code_task_wait,
            name="code_task_wait",
            description="Wait up to 30 seconds for one code task, then return its persisted status.",
            args_schema=CodeTaskWaitArgs,
        ), StructuredTool.from_function(
            code_task_cancel,
            name="code_task_cancel",
            description="Cancel a running code worker and persist its cancelled status.",
            args_schema=CodeTaskLookupArgs,
            metadata={"zara_requires_approval": True},
        ))

    def _execute(self, tasks: list[CodeTask]) -> str:
        try:
            cancellation = current_tool_cancellation()
        except RuntimeError:
            cancellation = None
        if cancellation is not None and cancellation.cancelled:
            raise RuntimeError("code task start was cancelled")
        parsed = [task if isinstance(task, CodeTask) else CodeTask.model_validate(task) for task in tasks]
        if not 1 <= len(parsed) <= MAX_TASKS:
            raise ValueError(f"code requires between 1 and {MAX_TASKS} tasks")
        projects = [task.project for task in parsed]
        if len(set(projects)) != len(projects):
            raise ValueError("concurrent code tasks need separate configured worktrees")
        for project in projects:
            if project not in self._projects:
                raise ValueError(f"unknown code project {project!r}")
            if self._resolve_project(project) != self._projects[project]:
                raise PermissionError("code project policy changed; restart the plugin")
        with self._lock:
            if not self._started:
                raise RuntimeError("code environment is stopped")
            if self._busy_projects.intersection(projects):
                raise RuntimeError("code project already has a running task")
            self._busy_projects.update(projects)
        try:
            for project in sorted(projects):
                self._acquire_lease(project)
            return self._execute_reserved(parsed, cancellation)
        except Exception:
            with self._lock:
                active = {
                    alias for task_id, alias in self._future_projects.items()
                    if task_id in self._futures and not self._futures[task_id].done()
                }
                for project in set(projects) - active:
                    self._release_project(project)
            raise

    def _acquire_lease(self, project: str) -> None:
        path = self._projects[project] / ".zara" / "code-worker.lock"
        if path.parent.is_symlink() or path.is_symlink():
            raise ValueError("code worker lease path must not be a symlink")
        flags = os.O_CREAT | os.O_RDWR | os.O_CLOEXEC | os.O_NOFOLLOW
        descriptor = os.open(path, flags, 0o600)
        try:
            fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            os.close(descriptor)
            raise RuntimeError("code worktree lease is held") from error
        except Exception:
            os.close(descriptor)
            raise
        with self._lock:
            self._lease_fds[project] = descriptor

    def _execute_reserved(self, parsed: list[CodeTask], cancellation) -> str:
        identities = []
        try:
            for task in parsed:
                state = self._state_for_project(task.project)
                created = state.create(self._principal_id, task.project, task.task)
                identities.append((state, created.task_id))
        except Exception:
            self._cancel_batch(parsed, identities)
            raise
        results = []
        try:
            for index, task in enumerate(parsed):
                if cancellation is not None and cancellation.cancelled:
                    raise RuntimeError("code task start was cancelled")
                state, task_id = identities[index]
                future = self._executor.submit(
                    self._run_persisted_worker, (state, task_id), task
                )
                with self._lock:
                    self._futures[task_id] = future
                    self._future_projects[task_id] = task.project
                    self._task_states[task_id] = state
                future.add_done_callback(
                    lambda _future, alias=task.project: self._release_project(alias)
                )
                results.append({
                    "index": index, "task_id": task_id,
                    "project": task.project, "status": "started",
                })
            if cancellation is not None and cancellation.cancelled:
                raise RuntimeError("code task start was cancelled")
        except Exception:
            self._cancel_batch(parsed, identities)
            raise
        return json.dumps({"results": results})

    def _cancel_batch(self, tasks: list[CodeTask], identities) -> None:
        for task, (_, task_id) in zip(tasks, identities):
            self._cancel(task.project, task_id)

    def _release_project(self, alias: str) -> None:
        with self._lock:
            self._busy_projects.discard(alias)
            descriptor = self._lease_fds.pop(alias, None)
        if descriptor is not None:
            fcntl.flock(descriptor, fcntl.LOCK_UN)
            os.close(descriptor)

    def _status(self, project: str, task_id: str) -> dict:
        if project not in self._projects or self._resolve_project(project) != self._projects[project]:
            raise ValueError("unknown or changed code project")
        state = self._state_for_project(project)
        task = state.get(self._principal_id, task_id)
        if task is None:
            raise ValueError("code task not found")
        result = state.result(self._principal_id, task_id)
        return {"task_id": task_id, "project": project, "status": task.status.value,
                "result": result[:2000] if result is not None else None, "reason": task.reason}

    def _wait(self, project: str, task_id: str, timeout_seconds: float) -> dict:
        if not 0 <= timeout_seconds <= 30:
            raise ValueError("code wait timeout must be between 0 and 30 seconds")
        with self._lock:
            future = self._futures.get(task_id)
        if future is not None:
            try:
                future.result(timeout=timeout_seconds)
            except FutureTimeout:
                pass
            except Exception:
                pass
        return self._status(project, task_id)

    def _cancel(self, project: str, task_id: str) -> dict:
        self._status(project, task_id)
        with self._lock:
            task_lock = self._task_locks.setdefault(task_id, threading.RLock())
        with task_lock:
            current = self._status(project, task_id)
            if current["status"] in {"completed", "failed", "cancelled", "interrupted"}:
                return current
            self._cancel_requested.add(task_id)
            with self._lock:
                future = self._futures.get(task_id)
                process = self._process_by_task.get(task_id)
            if future is not None:
                future.cancel()
            if process is not None:
                self._terminate(process)
                process.wait(timeout=5)
                if process.poll() is None:
                    raise RuntimeError("code worker did not stop")
            self._state_for_project(project).cancel(self._principal_id, task_id)
            return self._status(project, task_id)

    def _run_persisted_worker(self, identity, task: CodeTask) -> str:
        state, task_id = identity
        with self._lock:
            task_lock = self._task_locks.setdefault(task_id, threading.RLock())
        with task_lock:
            if task_id in self._cancel_requested:
                return ""
            state.start(self._principal_id, task_id)
        self._worker_context.task_id = task_id
        try:
            result = self._run_worker(task.task, task.project, task.mode)
        except Exception as error:
            with task_lock:
                if state.get(self._principal_id, task_id).status.value == "running":
                    state.finish(self._principal_id, task_id, str(error)[:2000], failed=True)
            raise
        finally:
            self._worker_context.task_id = None
        with task_lock:
            if state.get(self._principal_id, task_id).status.value == "running":
                state.finish(self._principal_id, task_id, result[:2000])
        return result

    @staticmethod
    def _resolve_project(alias: str) -> Path:
        from zara.agent.tools.file_tools import resolve_writable_repo
        from zara.prolog_engine import PrologEngine, locate_main_pl

        return resolve_writable_repo(PrologEngine(locate_main_pl()), alias)

    def _state_for_project(self, alias: str):
        from zara.tasks.repository_state import RepositoryTaskState

        with self._lock:
            state = self._state_by_project.get(alias)
            if state is None:
                state = RepositoryTaskState(self._projects[alias])
                self._state_by_project[alias] = state
            return state

    def _run_worker(self, task: str, project: str, mode: str) -> str:
        tools = ["read", "grep", "find", "ls"]
        if mode == "implement":
            tools.extend(("edit", "write"))
            if self._allow_shell:
                tools.append("bash")
        command = [
            self._binary, "--print", "--no-session", "--tools", ",".join(tools),
            "--approve" if self._project_trust else "--no-approve",
        ]
        if not self._project_trust:
            command.append("--no-context-files")
        if self._model_provider:
            command.extend(("--provider", self._model_provider))
        if self._model:
            command.extend(("--model", self._model))
        if self._thinking:
            command.extend(("--thinking", self._thinking))
        instruction = {
            "implement": "Implement the requested change and report edits and verification.",
            "review": "Review only. Do not change files.",
            "plan": "Plan only. Do not change files.",
        }[mode]
        task_id = getattr(self._worker_context, "task_id", None)
        with self._lock:
            task_lock = self._task_locks.setdefault(task_id, threading.RLock()) if task_id is not None else nullcontext()
        with task_lock:
            if task_id is not None and task_id in self._cancel_requested:
                raise RuntimeError("code task was cancelled before launch")
            with self._lock:
                if not self._started:
                    raise RuntimeError("code environment stopped")
            process = subprocess.Popen(
                self._sandbox_command(project, command),
                cwd=self._projects[project],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env=self._worker_environment(),
                start_new_session=os.name != "nt",
            )
            with self._lock:
                self._processes.add(process)
                if task_id is not None:
                    self._process_by_task[task_id] = process
        try:
            try:
                stdout, stderr = process.communicate(
                    input=f"{instruction}\n\nTask:\n{task}\n",
                    timeout=self._timeout_seconds,
                )
            except subprocess.TimeoutExpired as error:
                self._terminate(process)
                process.communicate()
                raise RuntimeError("code worker timed out") from error
            if process.returncode != 0:
                detail = (stderr or stdout).strip()[:MAX_OUTPUT_CHARS]
                raise RuntimeError(f"code worker exited {process.returncode}: {detail}")
            return stdout.strip()[:MAX_OUTPUT_CHARS]
        finally:
            with self._lock:
                self._processes.discard(process)
                if task_id is not None:
                    self._process_by_task.pop(task_id, None)

    def _worker_environment(self) -> dict[str, str]:
        allowed = {"PATH", "LANG", "LC_ALL", "SSL_CERT_FILE", "NIX_SSL_CERT_FILE"}
        allowed.update(self._credential_env_vars)
        return {name: value for name, value in os.environ.items() if name in allowed}

    @staticmethod
    def _probe_sandbox(binary: str) -> bool:
        try:
            result = subprocess.run(
                [binary, "--ro-bind", "/", "/", "--", "/bin/true"],
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                check=False, timeout=5,
            )
        except (OSError, subprocess.TimeoutExpired):
            return False
        return result.returncode == 0

    def _sandbox_command(self, project: str, command: list[str]) -> list[str]:
        root = str(self._projects[project])
        home = pwd.getpwuid(os.getuid()).pw_dir
        return [
            self._sandbox_binary,
            "--die-with-parent", "--share-net",
            "--ro-bind", "/", "/",
            "--tmpfs", home,
            "--tmpfs", "/tmp",
            "--dev-bind", "/dev", "/dev",
            "--proc", "/proc",
            "--bind", root, root,
            *self._git_metadata_mounts(self._projects[project]),
            "--tmpfs", str(self._projects[project] / ".zara"),
            "--setenv", "HOME", "/tmp",
            "--setenv", "XDG_CONFIG_HOME", "/tmp/.config",
            "--setenv", "TMPDIR", "/tmp",
            "--chdir", root,
            "--",
            *command,
        ]

    @staticmethod
    def _git_metadata_mounts(root: Path) -> list[str]:
        git_entry = root / ".git"
        if not git_entry.is_file() and not git_entry.is_dir():
            return []
        result = subprocess.run(
            ["git", "-C", str(root), "rev-parse", "--path-format=absolute", "--git-dir", "--git-common-dir"],
            capture_output=True, text=True, check=False,
        )
        if result.returncode != 0:
            raise ValueError("code worktree Git metadata is unavailable")
        paths = sorted({Path(line).resolve() for line in result.stdout.splitlines()}, key=lambda path: len(path.parts))
        mounts: list[str] = []
        covered = [root]
        for path in paths:
            if not path.is_dir():
                raise ValueError("code worktree Git metadata is unavailable")
            if any(path == parent or parent in path.parents for parent in covered):
                continue
            mounts.extend(("--ro-bind", str(path), str(path)))
            covered.append(path)
        return mounts

    @staticmethod
    def _terminate(process: subprocess.Popen) -> None:
        if process.poll() is not None:
            return
        if os.name != "nt":
            try:
                os.killpg(process.pid, signal.SIGKILL)
                return
            except OSError:
                pass
        process.kill()


def create_plugin() -> CodeEnvironmentPlugin:
    return CodeEnvironmentPlugin()
