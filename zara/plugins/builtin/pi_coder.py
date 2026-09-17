"""Generic Zara coder plugin backed by Pi."""

from __future__ import annotations

import os
import shutil
import signal
import subprocess
import threading
from pathlib import Path
from typing import Literal

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from zara.plugins import PluginMetadata, ServicePlugin


MAX_TASK_CHARS = 12_000
DEFAULT_TIMEOUT_SECONDS = 900.0
MAX_TIMEOUT_SECONDS = 3_600.0
DEFAULT_MAX_OUTPUT_CHARS = 24_000
MAX_OUTPUT_CHARS = 200_000
READ_ONLY_TOOLS = ("read", "grep", "find", "ls")
WRITE_TOOLS = ("read", "grep", "find", "ls", "edit", "write")


class CoderArgs(BaseModel):
    task: str = Field(
        ...,
        min_length=1,
        max_length=MAX_TASK_CHARS,
        description="Coding task for the configured coder engine.",
    )
    project: str = Field(
        "",
        max_length=128,
        description="Configured Zara project name. Empty selects the configured default project.",
    )
    mode: Literal["implement", "review", "plan"] = Field(
        "implement",
        description="implement may write files; review and plan are forced read-only.",
    )


class PiCoderPlugin(ServicePlugin):
    """Generic approval-gated coder surface; Pi is the first execution engine."""

    enabled_by_default = False
    metadata = PluginMetadata(
        name="coder",
        version="0.1.0",
        api_version="1",
        description="Generic approval-gated coding delegation, currently backed by Pi.",
    )

    def __init__(self) -> None:
        self._engine = "pi"
        self._binary = ""
        self._projects: dict[str, Path] = {}
        self._default_project = ""
        self._model_provider = ""
        self._model = ""
        self._thinking = ""
        self._project_trust = False
        self._allow_shell = False
        self._timeout_seconds = DEFAULT_TIMEOUT_SECONDS
        self._max_output_chars = DEFAULT_MAX_OUTPUT_CHARS
        self._started = False
        self._state_lock = threading.RLock()
        self._execution_lock = threading.Lock()
        self._processes: set[subprocess.Popen] = set()

    def start(self, runtime) -> None:
        configuration = dict(runtime.configuration)
        engine = str(configuration.get("engine", "pi") or "pi").strip().lower()
        if engine != "pi":
            raise ValueError("coder engine must currently be 'pi'")

        binary = self._resolve_binary(configuration.get("binary", "pi"))
        projects = self._resolve_projects(configuration.get("projects", {}))

        self._projects = projects
        default_project = self._resolve_default_project(
            configuration.get("default_project", "")
        )
        model_provider = self._bounded_text(
            configuration.get("model_provider", ""),
            128,
        )
        model = self._bounded_text(configuration.get("model", ""), 256)
        thinking = self._resolve_thinking(configuration.get("thinking", ""))
        project_trust = self._require_bool(
            configuration.get("project_trust", False),
            "project_trust",
        )
        allow_shell = self._require_bool(
            configuration.get("allow_shell", False),
            "allow_shell",
        )
        timeout_seconds = self._bounded_float(
            configuration.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS),
            minimum=1.0,
            maximum=MAX_TIMEOUT_SECONDS,
            label="timeout_seconds",
        )
        max_output_chars = self._bounded_int(
            configuration.get("max_output_chars", DEFAULT_MAX_OUTPUT_CHARS),
            minimum=1_000,
            maximum=MAX_OUTPUT_CHARS,
            label="max_output_chars",
        )

        with self._state_lock:
            self._engine = engine
            self._binary = binary
            self._default_project = default_project
            self._model_provider = model_provider
            self._model = model
            self._thinking = thinking
            self._project_trust = project_trust
            self._allow_shell = allow_shell
            self._timeout_seconds = timeout_seconds
            self._max_output_chars = max_output_chars
            self._started = True

    def stop(self) -> None:
        with self._state_lock:
            self._started = False
            processes = tuple(self._processes)
        for process in processes:
            self._terminate(process)

    def tools(self):
        def coder(
            task: str,
            project: str = "",
            mode: Literal["implement", "review", "plan"] = "implement",
        ) -> str:
            return self._run(task=task, project=project, mode=mode)

        def coder_projects() -> str:
            self._require_started()
            names = sorted(self._projects)
            if not names:
                return "No coder projects are configured."
            rendered = []
            for name in names:
                suffix = " (default)" if name == self._default_project else ""
                rendered.append(f"- {name}{suffix}")
            return "\n".join(rendered)

        def coder_status() -> str:
            self._require_started()
            return (
                f"Coder engine: {self._engine}. "
                f"Projects: {len(self._projects)}. "
                f"Shell: {'enabled' if self._allow_shell else 'disabled'}. "
                f"Project resources: {'trusted' if self._project_trust else 'ignored'}. "
                f"Worker: {'busy' if self._execution_lock.locked() else 'idle'}."
            )

        return (
            StructuredTool.from_function(
                coder,
                name="coder",
                description=(
                    "Delegate a bounded coding task to Zara's configured coder engine inside a "
                    "configured local project. implement may edit/write files and may run shell "
                    "commands only when the operator enabled allow_shell; review and plan are "
                    "always read-only."
                ),
                args_schema=CoderArgs,
                metadata={"zara_requires_approval": True},
            ),
            StructuredTool.from_function(
                coder_projects,
                name="coder_projects",
                description="List configured coder project names without exposing host paths.",
            ),
            StructuredTool.from_function(
                coder_status,
                name="coder_status",
                description="Show coder engine and effective policy without exposing credentials or paths.",
            ),
        )

    def _run(
        self,
        *,
        task: str,
        project: str,
        mode: Literal["implement", "review", "plan"],
    ) -> str:
        self._require_started()
        if not self._execution_lock.acquire(blocking=False):
            raise RuntimeError("coder is already running a task")
        try:
            return self._run_exclusive(task=task, project=project, mode=mode)
        finally:
            self._execution_lock.release()

    def _run_exclusive(
        self,
        *,
        task: str,
        project: str,
        mode: Literal["implement", "review", "plan"],
    ) -> str:
        task = str(task).strip()
        if not task:
            raise ValueError("coder task must not be empty")
        if len(task) > MAX_TASK_CHARS:
            raise ValueError(f"coder task must not exceed {MAX_TASK_CHARS} characters")

        project_name, project_path = self._select_project(project)
        command = self._command(mode)
        prompt = self._prompt(task=task, project_name=project_name, mode=mode)

        process = subprocess.Popen(
            command,
            cwd=str(project_path),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace",
            start_new_session=os.name != "nt",
        )
        with self._state_lock:
            if not self._started:
                self._terminate(process)
                raise RuntimeError("coder plugin stopped before task execution")
            self._processes.add(process)
        try:
            try:
                stdout, stderr = process.communicate(
                    input=prompt,
                    timeout=self._timeout_seconds,
                )
            except subprocess.TimeoutExpired as error:
                self._terminate(process)
                process.communicate()
                raise RuntimeError("Pi coder timed out") from error

            if process.returncode != 0:
                detail = self._bounded_output(stderr.strip() or stdout.strip())
                if detail:
                    raise RuntimeError(
                        f"Pi coder failed with exit code {process.returncode}: {detail}"
                    )
                raise RuntimeError(f"Pi coder failed with exit code {process.returncode}")

            output = self._bounded_output(stdout.strip())
            return output or "Pi coder completed without text output."
        finally:
            with self._state_lock:
                self._processes.discard(process)

    def _command(self, mode: Literal["implement", "review", "plan"]) -> list[str]:
        tools = list(READ_ONLY_TOOLS)
        if mode == "implement":
            tools = list(WRITE_TOOLS)
            if self._allow_shell:
                tools.append("bash")

        command = [
            self._binary,
            "--print",
            "--no-session",
            "--tools",
            ",".join(tools),
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
        return command

    @staticmethod
    def _prompt(
        *,
        task: str,
        project_name: str,
        mode: Literal["implement", "review", "plan"],
    ) -> str:
        mode_instruction = {
            "implement": (
                "Implement the requested change in the current project. Keep edits scoped to the "
                "task and report what changed and any verification performed."
            ),
            "review": (
                "Review only. Do not modify files. Identify concrete defects, risks, and useful "
                "fixes with file references where possible."
            ),
            "plan": (
                "Plan only. Do not modify files. Produce an implementation plan grounded in the "
                "current project."
            ),
        }[mode]
        return (
            f"Zara delegated a coding task for configured project {project_name!r}.\n"
            f"Mode: {mode}. {mode_instruction}\n\n"
            f"Task:\n{task}\n"
        )

    def _select_project(self, requested: str) -> tuple[str, Path]:
        name = str(requested or "").strip() or self._default_project
        if not name:
            raise ValueError("no coder project selected and no default_project is configured")
        path = self._projects.get(name)
        if path is None:
            raise ValueError(f"unknown coder project {name!r}")
        if not path.is_dir():
            raise RuntimeError(f"configured coder project {name!r} is unavailable")
        return name, path

    @staticmethod
    def _resolve_binary(value: object) -> str:
        binary = str(value or "pi").strip()
        if not binary:
            raise ValueError("coder Pi binary must not be empty")
        expanded = Path(binary).expanduser()
        has_path = expanded.is_absolute() or os.sep in binary or (
            os.altsep is not None and os.altsep in binary
        )
        if has_path:
            resolved = expanded.resolve()
            if not resolved.is_file():
                raise RuntimeError("configured Pi executable does not exist")
            return str(resolved)
        found = shutil.which(binary)
        if found is None:
            raise RuntimeError(
                "Pi executable was not found; install Pi or configure plugins.coder.binary"
            )
        return found

    @staticmethod
    def _resolve_projects(value: object) -> dict[str, Path]:
        if not isinstance(value, dict):
            raise TypeError("coder projects must be a table mapping names to local folders")
        projects: dict[str, Path] = {}
        for raw_name, raw_path in value.items():
            name = str(raw_name).strip()
            if not name or len(name) > 128:
                raise ValueError("coder project names must contain 1 to 128 characters")
            if not isinstance(raw_path, str) or not raw_path.strip():
                raise TypeError(f"coder project {name!r} path must be a non-empty string")
            path = Path(raw_path).expanduser().resolve()
            if not path.is_dir():
                raise RuntimeError(f"coder project {name!r} is not an existing directory")
            projects[name] = path
        if not projects:
            raise ValueError("coder requires at least one configured project")
        return projects

    def _resolve_default_project(self, value: object) -> str:
        name = str(value or "").strip()
        if not name and len(self._projects) == 1:
            return next(iter(self._projects))
        if name and name not in self._projects:
            raise ValueError("coder default_project must name a configured project")
        return name

    @staticmethod
    def _resolve_thinking(value: object) -> str:
        thinking = str(value or "").strip().lower()
        allowed = {"", "off", "minimal", "low", "medium", "high", "xhigh", "max"}
        if thinking not in allowed:
            raise ValueError("coder thinking must be off/minimal/low/medium/high/xhigh/max")
        return thinking

    @staticmethod
    def _bounded_text(value: object, maximum: int) -> str:
        text = str(value or "").strip()
        if len(text) > maximum:
            raise ValueError(f"coder setting must not exceed {maximum} characters")
        return text

    @staticmethod
    def _require_bool(value: object, label: str) -> bool:
        if not isinstance(value, bool):
            raise TypeError(f"coder {label} must be true or false")
        return value

    @staticmethod
    def _bounded_float(value: object, *, minimum: float, maximum: float, label: str) -> float:
        if isinstance(value, bool):
            raise TypeError(f"coder {label} must be numeric")
        try:
            number = float(value)
        except (TypeError, ValueError) as error:
            raise TypeError(f"coder {label} must be numeric") from error
        if not minimum <= number <= maximum:
            raise ValueError(f"coder {label} must be between {minimum:g} and {maximum:g}")
        return number

    @staticmethod
    def _bounded_int(value: object, *, minimum: int, maximum: int, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"coder {label} must be an integer")
        if not minimum <= value <= maximum:
            raise ValueError(f"coder {label} must be between {minimum} and {maximum}")
        return value

    def _bounded_output(self, text: str) -> str:
        if len(text) <= self._max_output_chars:
            return text
        omitted = len(text) - self._max_output_chars
        return f"{text[:self._max_output_chars]}\n...[{omitted} characters omitted]"

    @staticmethod
    def _terminate(process: subprocess.Popen) -> None:
        if process.poll() is not None:
            return
        if os.name != "nt":
            try:
                os.killpg(process.pid, signal.SIGKILL)
                return
            except ProcessLookupError:
                return
            except OSError:
                pass
        process.kill()

    def _require_started(self) -> None:
        with self._state_lock:
            if not self._started:
                raise RuntimeError("coder plugin is not started")


def create_plugin() -> PiCoderPlugin:
    return PiCoderPlugin()


__all__ = ["CoderArgs", "PiCoderPlugin", "create_plugin"]
