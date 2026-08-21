"""Autonomous metric-driven research loop for Zara.

Each experiment is made in a disposable repository copy. The worker may inspect
the copy, but only explicitly allowlisted files can be promoted back after the
fixed evaluator reports a strictly better metric. Evaluator output is parsed as
data and is never replayed into the worker prompt.
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal, Optional

from .actors import BoundedActor
from .agent import AgentManager
from .agent.tools.file_tools import build_file_tools
from .config import ZaraConfig, get_config


Direction = Literal["minimize", "maximize"]
_FILE_TOOL_NAMES = ("read_file", "write_file", "diff_file", "list_dir")
_IGNORED_NAMES = {
    ".git",
    ".zara",
    "__pycache__",
    ".pytest_cache",
    ".mypy_cache",
    ".tox",
    "build",
    "dist",
}


@dataclass(frozen=True)
class ResearchContract:
    goal: str
    command: tuple[str, ...]
    metric_pattern: str
    direction: Direction
    files: tuple[str, ...]
    timeout_seconds: float = 300.0
    max_iterations: int = 0
    state_dir: str = ".zara/autoresearch"

    def validate(self, repo_root: Path) -> tuple[Path, ...]:
        if not self.goal.strip():
            raise ValueError("research goal cannot be empty")
        if not self.command:
            raise ValueError("research command cannot be empty")
        if self.direction not in {"minimize", "maximize"}:
            raise ValueError("direction must be 'minimize' or 'maximize'")
        if self.timeout_seconds <= 0:
            raise ValueError("timeout_seconds must be positive")
        if self.max_iterations < 0:
            raise ValueError("max_iterations cannot be negative")
        if not self.files:
            raise ValueError("at least one research file is required")
        if Path(self.state_dir).is_absolute():
            raise ValueError("state_dir must be relative to the repo root")

        try:
            metric_regex = re.compile(self.metric_pattern, re.MULTILINE)
        except re.error as error:
            raise ValueError(f"invalid metric pattern: {error}") from error
        if metric_regex.groups < 1:
            raise ValueError("metric_pattern must contain a capture group")

        root = repo_root.resolve(strict=True)
        state_root = (root / self.state_dir).resolve(strict=False)
        if state_root != root and root not in state_root.parents:
            raise ValueError("state_dir escapes repo root")

        resolved: list[Path] = []
        for value in self.files:
            source_path = root / value
            if source_path.is_symlink():
                raise ValueError(f"research file cannot be a symlink: {value}")
            candidate = source_path.resolve(strict=True)
            if root not in candidate.parents:
                raise ValueError(f"research file escapes repo root: {value}")
            if not candidate.is_file():
                raise ValueError(f"research file must be a regular file: {value}")
            resolved.append(candidate)
        return tuple(resolved)


@dataclass(frozen=True)
class StartResearch:
    contract: ResearchContract


@dataclass(frozen=True)
class StopResearch:
    pass


@dataclass(frozen=True)
class GetResearchStatus:
    pass


@dataclass(frozen=True)
class _RunIteration:
    pass


@dataclass(frozen=True)
class ResearchStatus:
    running: bool
    iteration: int
    best_metric: Optional[float]
    last_metric: Optional[float]
    accepted: Optional[bool]
    error: Optional[str] = None


@dataclass(frozen=True)
class Evaluation:
    metric: float
    returncode: int
    elapsed_seconds: float


class ResearchLedger:
    """Append-only JSONL experiment ledger."""

    def __init__(self, path: Path):
        self.path = path
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def append(self, record: dict[str, Any]) -> None:
        payload = json.dumps(record, sort_keys=True, separators=(",", ":"))
        with self.path.open("a", encoding="utf-8") as handle:
            handle.write(payload + "\n")
            handle.flush()
            os.fsync(handle.fileno())

    def numeric_history(self, limit: int = 12) -> list[dict[str, Any]]:
        if not self.path.exists():
            return []

        records: list[dict[str, Any]] = []
        with self.path.open("r", encoding="utf-8") as handle:
            for line in handle:
                try:
                    record = json.loads(line)
                except json.JSONDecodeError:
                    continue
                records.append(
                    {
                        "iteration": record.get("iteration"),
                        "metric": record.get("metric"),
                        "accepted": record.get("accepted"),
                        "status": record.get("status"),
                    }
                )
        return records[-limit:]


def parse_metric(output: str, pattern: str) -> float:
    matches = list(re.finditer(pattern, output, re.MULTILINE))
    if not matches:
        raise ValueError("metric pattern did not match evaluator output")

    raw = matches[-1].group(1)
    try:
        return float(raw)
    except ValueError as error:
        raise ValueError(f"captured metric is not numeric: {raw!r}") from error


def metric_improved(candidate: float, current: float, direction: Direction) -> bool:
    if direction == "minimize":
        return candidate < current
    if direction == "maximize":
        return candidate > current
    raise ValueError(f"unsupported direction: {direction}")


def run_evaluator(repo_root: Path, contract: ResearchContract) -> Evaluation:
    started = time.monotonic()
    completed = subprocess.run(
        list(contract.command),
        cwd=repo_root,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        timeout=contract.timeout_seconds,
        check=False,
        env=os.environ.copy(),
    )
    elapsed = time.monotonic() - started

    if completed.returncode != 0:
        raise RuntimeError(f"evaluator exited with status {completed.returncode}")

    return Evaluation(
        metric=parse_metric(completed.stdout, contract.metric_pattern),
        returncode=completed.returncode,
        elapsed_seconds=elapsed,
    )


def _file_hash(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _tree_hashes(root: Path) -> dict[str, str]:
    hashes: dict[str, str] = {}
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root)
        if any(part in _IGNORED_NAMES for part in relative.parts):
            continue
        if not path.is_file() or path.is_symlink():
            continue
        hashes[relative.as_posix()] = _file_hash(path)
    return hashes


def _changed_paths(before: dict[str, str], after: dict[str, str]) -> set[str]:
    paths = set(before) | set(after)
    return {path for path in paths if before.get(path) != after.get(path)}


def _copy_repo(source: Path, destination: Path) -> None:
    shutil.copytree(
        source,
        destination,
        dirs_exist_ok=True,
        ignore=shutil.ignore_patterns(*_IGNORED_NAMES),
    )


def _replace_file(source: Path, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=".zara-autoresearch-",
        dir=destination.parent,
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as target, source.open("rb") as origin:
            shutil.copyfileobj(origin, target)
            target.flush()
            os.fsync(target.fileno())
        shutil.copymode(source, temporary)
        os.replace(temporary, destination)
    finally:
        if temporary.exists():
            temporary.unlink()


class AutoResearchActor(BoundedActor):
    """Own one research session and execute one experiment per actor message."""

    mailbox_size = 16
    mailbox_overflow = "drop_newest"

    def __init__(
        self,
        repo_root: Path | str = ".",
        config: Optional[ZaraConfig] = None,
    ):
        super().__init__()
        self.repo_root = Path(repo_root).resolve(strict=True)
        self.config = config or get_config()
        self.contract: Optional[ResearchContract] = None
        self.ledger: Optional[ResearchLedger] = None
        self.iteration = 0
        self.best_metric: Optional[float] = None
        self.last_metric: Optional[float] = None
        self.last_accepted: Optional[bool] = None
        self.error: Optional[str] = None
        self.running = False

    def on_receive(self, message: Any) -> Any:
        if isinstance(message, StartResearch):
            return self._start(message.contract)
        if isinstance(message, StopResearch):
            self.running = False
            return self._status()
        if isinstance(message, GetResearchStatus):
            return self._status()
        if isinstance(message, _RunIteration):
            self._run_iteration()
            return self._status()
        return super().on_receive(message)

    def _start(self, contract: ResearchContract) -> ResearchStatus:
        if self.running:
            raise RuntimeError("autoresearch session is already running")

        contract.validate(self.repo_root)
        self.contract = contract
        state_root = self.repo_root / contract.state_dir
        state_root.mkdir(parents=True, exist_ok=True)
        self.ledger = ResearchLedger(state_root / "results.jsonl")
        self.iteration = 0
        self.error = None
        self.last_metric = None
        self.last_accepted = None

        baseline = run_evaluator(self.repo_root, contract)
        self.best_metric = baseline.metric
        self.last_metric = baseline.metric
        self.last_accepted = True
        self.ledger.append(
            {
                "iteration": 0,
                "status": "baseline",
                "metric": baseline.metric,
                "accepted": True,
                "elapsed_seconds": baseline.elapsed_seconds,
                "contract": {
                    "goal": contract.goal,
                    "command": list(contract.command),
                    "metric_pattern": contract.metric_pattern,
                    "direction": contract.direction,
                    "files": list(contract.files),
                    "timeout_seconds": contract.timeout_seconds,
                    "max_iterations": contract.max_iterations,
                },
            }
        )

        self.running = True
        self.actor_ref.tell(_RunIteration())
        return self._status()

    def _run_iteration(self) -> None:
        if not self.running or self.contract is None or self.ledger is None:
            return

        stop_file = self.repo_root / self.contract.state_dir / "STOP"
        if stop_file.exists():
            self.running = False
            return
        if self.contract.max_iterations and self.iteration >= self.contract.max_iterations:
            self.running = False
            return

        next_iteration = self.iteration + 1
        try:
            result = self._experiment(next_iteration)
            self.last_metric = result.get("metric")
            self.last_accepted = bool(result.get("accepted"))
            self.ledger.append(result)
            self.iteration = next_iteration
            self.error = None
        except Exception as error:
            self.iteration = next_iteration
            self.last_metric = None
            self.last_accepted = False
            self.error = str(error)
            self.ledger.append(
                {
                    "iteration": next_iteration,
                    "status": "error",
                    "metric": None,
                    "accepted": False,
                    "error_type": type(error).__name__,
                }
            )

        if self.contract.max_iterations and self.iteration >= self.contract.max_iterations:
            self.running = False
            return
        if self.running:
            self.actor_ref.tell(_RunIteration())

    def _experiment(self, iteration: int) -> dict[str, Any]:
        assert self.contract is not None
        assert self.ledger is not None
        assert self.best_metric is not None

        with tempfile.TemporaryDirectory(prefix="zara-autoresearch-") as temporary:
            workspace = Path(temporary) / "repo"
            _copy_repo(self.repo_root, workspace)
            before = _tree_hashes(workspace)

            agent = AgentManager(config=self.config)
            agent.tool_registry.unregister_tools(list(_FILE_TOOL_NAMES))
            agent.tool_registry.register_tools(
                build_file_tools(
                    base_dir=workspace,
                    readable_roots=(workspace,),
                    writable_roots=(workspace,),
                    max_bytes=200000,
                )
            )

            history = self.ledger.numeric_history()
            allowed = "\n".join(f"- {path}" for path in self.contract.files)
            prompt = (
                "You are the worker for an autonomous Zara research experiment. "
                "The human explicitly authorizes file inspection and edits inside this disposable "
                "workspace. Make exactly one coherent experimental change.\n\n"
                f"Goal: {self.contract.goal}\n"
                f"Metric direction: {self.contract.direction}\n"
                f"Current best metric: {self.best_metric}\n"
                f"Allowed files:\n{allowed}\n\n"
                f"Numeric experiment history: {json.dumps(history, separators=(',', ':'))}\n\n"
                "Only modify allowed files. Do not run the evaluator. Do not create helper files. "
                "Do not edit tests unless a test file is explicitly allowed. Read before writing. "
                "Make one hypothesis-driven change and briefly state the hypothesis when finished."
            )
            response = asyncio.run(agent.process_async(prompt))

            after = _tree_hashes(workspace)
            changed = _changed_paths(before, after)
            allowed_set = set(self.contract.files)
            unauthorized = sorted(changed - allowed_set)
            if unauthorized:
                return {
                    "iteration": iteration,
                    "status": "rejected_scope",
                    "metric": None,
                    "accepted": False,
                    "changed_files": sorted(changed),
                    "unauthorized_files": unauthorized,
                }
            if not changed:
                return {
                    "iteration": iteration,
                    "status": "no_change",
                    "metric": None,
                    "accepted": False,
                    "changed_files": [],
                }

            evaluation = run_evaluator(workspace, self.contract)
            accepted = metric_improved(
                evaluation.metric,
                self.best_metric,
                self.contract.direction,
            )
            if accepted:
                for relative in sorted(changed):
                    _replace_file(workspace / relative, self.repo_root / relative)
                self.best_metric = evaluation.metric

            text = response.get("response", "") if isinstance(response, dict) else ""
            return {
                "iteration": iteration,
                "status": "accepted" if accepted else "rejected_metric",
                "metric": evaluation.metric,
                "accepted": accepted,
                "best_metric": self.best_metric,
                "elapsed_seconds": evaluation.elapsed_seconds,
                "changed_files": sorted(changed),
                "hypothesis": str(text)[:1000],
            }

    def _status(self) -> ResearchStatus:
        return ResearchStatus(
            running=self.running,
            iteration=self.iteration,
            best_metric=self.best_metric,
            last_metric=self.last_metric,
            accepted=self.last_accepted,
            error=self.error,
        )


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run Zara autonomous research")
    parser.add_argument("--goal", required=True)
    parser.add_argument("--command", required=True, help="Evaluator command without shell syntax")
    parser.add_argument("--metric", required=True, help="Regex with one numeric capture group")
    parser.add_argument("--direction", choices=("minimize", "maximize"), required=True)
    parser.add_argument("--files", nargs="+", required=True)
    parser.add_argument("--timeout", type=float, default=300.0)
    parser.add_argument("--iterations", type=int, default=0, help="0 means run until stopped")
    parser.add_argument("--repo", default=".")
    parser.add_argument("--state-dir", default=".zara/autoresearch")
    return parser


def main(argv: Optional[list[str]] = None) -> int:
    args = _build_parser().parse_args(argv)
    contract = ResearchContract(
        goal=args.goal,
        command=tuple(shlex.split(args.command)),
        metric_pattern=args.metric,
        direction=args.direction,
        files=tuple(args.files),
        timeout_seconds=args.timeout,
        max_iterations=args.iterations,
        state_dir=args.state_dir,
    )
    actor = AutoResearchActor.start(repo_root=Path(args.repo))
    actor_timeout = max(600.0, contract.timeout_seconds + 720.0)

    try:
        status = actor.ask(StartResearch(contract), timeout=actor_timeout)
        previous_iteration = -1
        while status.running:
            if status.iteration != previous_iteration:
                print(
                    f"iteration={status.iteration} best={status.best_metric} "
                    f"last={status.last_metric} accepted={status.accepted}"
                )
                previous_iteration = status.iteration
            time.sleep(1.0)
            status = actor.ask(GetResearchStatus(), timeout=actor_timeout)

        print(
            f"stopped iteration={status.iteration} best={status.best_metric} "
            f"error={status.error or '-'}"
        )
        return 0 if status.error is None else 1
    except KeyboardInterrupt:
        actor.tell(StopResearch())
        return 130
    finally:
        actor.stop(block=True, timeout=actor_timeout)


if __name__ == "__main__":
    raise SystemExit(main())
