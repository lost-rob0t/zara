#!/usr/bin/env python3
"""Stateful, serialized access to SWI-Prolog through PySWIP."""

from __future__ import annotations

import json
import logging
import math
import threading
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional

try:
    from pyswip import Prolog
except ImportError as error:
    raise ImportError(
        "PySWIP not installed. Install with: pip install pyswip"
    ) from error


class PrologEngineError(RuntimeError):
    """Base exception for failures at the Python/Prolog boundary."""


class PrologStartupError(PrologEngineError):
    """Raised when the main Prolog program cannot be loaded."""


class PrologQueryError(PrologEngineError):
    """Raised when SWI-Prolog cannot execute a query."""

    def __init__(self, goal: str, cause: Exception):
        super().__init__(f"Prolog query failed: {goal}: {cause}")
        self.goal = goal
        self.cause = cause


class PrologSerializationError(PrologEngineError):
    """Raised when a Python value cannot be represented as a Prolog term."""


@dataclass(frozen=True)
class IntentResult:
    kind: str
    name: str
    args: List[Any]


def _compound_parts(value: Any) -> Optional[tuple[str, List[Any]]]:
    if isinstance(value, str) and value.endswith(")"):
        separator = value.find("(")
        if separator > 0:
            return value[:separator], [value[separator + 1:-1]]

    name = getattr(value, "name", None)
    args = getattr(value, "args", None)
    if isinstance(name, str) and args is not None:
        return name, list(args)
    return None


def adapt_intent_result(result: Dict[str, Any]) -> IntentResult:
    value = result.get("Intent")
    args = list(result.get("Args", []))
    compound = _compound_parts(value)
    if compound is None:
        return IntentResult("prolog", str(value), args)

    functor, values = compound
    inner = values[0] if values else ""
    if functor == "python":
        return IntentResult("python", str(inner), args)
    if functor == "pending":
        return IntentResult("pending", str(inner), args)
    return IntentResult("prolog", str(value), args)


def _prolog_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def _prolog_atom(value: str) -> str:
    escaped = (
        value.replace("\\", "\\\\")
        .replace("'", "\\'")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
    )
    return f"'{escaped}'"


def _prolog_term(value: Any) -> str:
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        if not math.isfinite(value):
            raise PrologSerializationError("Non-finite numbers are not Prolog terms")
        return repr(value)
    if isinstance(value, str):
        return _prolog_atom(value)
    if isinstance(value, (list, tuple)):
        return f"[{', '.join(_prolog_term(item) for item in value)}]"
    raise PrologSerializationError(
        f"Unsupported Prolog value type: {type(value).__name__}"
    )


def _normalize_value(value: Any) -> Any:
    if isinstance(value, bytes):
        return value.decode("utf-8")
    if isinstance(value, list):
        return [_normalize_value(item) for item in value]
    if isinstance(value, tuple):
        return tuple(_normalize_value(item) for item in value)
    if isinstance(value, dict):
        return {key: _normalize_value(item) for key, item in value.items()}
    return value


class PrologEngine:
    """Serialized access to the process-wide PySWIP runtime.

    PySWIP exposes one process-wide SWI-Prolog runtime. All engine instances
    therefore share a lock. A lazy query owns that lock until it is exhausted
    or closed, so callers must close partially-consumed iterators promptly.
    """

    _runtime_lock = threading.Lock()
    _load_hook_installed = False

    def __init__(self, main_file: Optional[Path] = None):
        self.prolog = Prolog()
        self.loaded_files: set[Path] = set()
        self.logger = logging.getLogger(__name__)

        if main_file is not None:
            self.consult(main_file)

    def consult(self, filepath: Path) -> None:
        """Load a Prolog file or raise a typed startup error."""
        resolved_path = Path(filepath).resolve()
        if not resolved_path.is_file():
            raise PrologStartupError(f"Prolog file not found: {resolved_path}")

        try:
            with self._runtime_lock:
                self._ensure_load_error_hook()
                self._run_internal_query(
                    "nb_setval(zara_engine_loading, true), "
                    "nb_setval(zara_engine_load_failed, false)"
                )
                try:
                    self.prolog.consult(str(resolved_path))
                    load_state = self._run_internal_query(
                        "nb_getval(zara_engine_load_failed, Failed)"
                    )
                finally:
                    self._run_internal_query(
                        "nb_delete(zara_engine_loading), "
                        "nb_delete(zara_engine_load_failed)"
                    )
        except Exception as error:
            raise PrologStartupError(
                f"Failed to consult Prolog file {resolved_path}: {error}"
            ) from error

        if not load_state or load_state[0].get("Failed") != "false":
            raise PrologStartupError(
                f"Failed to consult Prolog file {resolved_path}: load errors reported"
            )

        self.loaded_files.add(resolved_path)
        self.logger.info("Loaded Prolog program: %s", resolved_path)

    def _ensure_load_error_hook(self) -> None:
        if type(self)._load_hook_installed:
            return
        self._run_internal_query(
            "assertz((user:message_hook("
            "load_file_errors(_, Errors, _), silent, _) :- "
            "nb_current(zara_engine_loading, true), Errors > 0, "
            "nb_setval(zara_engine_load_failed, true), fail))"
        )
        type(self)._load_hook_installed = True

    def _run_internal_query(self, goal: str) -> List[Dict[str, Any]]:
        query = self.prolog.query(goal)
        try:
            return [dict(result) for result in query]
        finally:
            query.close()

    def query_once(self, goal: str) -> Optional[Dict[str, Any]]:
        """Return the first solution, ``None`` for failure, or raise on error."""
        results = self.query_iter(goal, max_solutions=1)
        try:
            return next(results, None)
        finally:
            results.close()

    def query_all(
        self,
        goal: str,
        max_solutions: int = 100,
    ) -> List[Dict[str, Any]]:
        """Return up to ``max_solutions`` solutions or raise on engine error."""
        if max_solutions < 1:
            raise ValueError("max_solutions must be at least 1")
        return list(self.query_iter(goal, max_solutions=max_solutions))

    def query_iter(
        self,
        goal: str,
        max_solutions: int = -1,
    ) -> Iterator[Dict[str, Any]]:
        """Yield solutions while owning the process-wide Prolog runtime lock."""
        query = None
        with self._runtime_lock:
            try:
                query = self.prolog.query(goal, maxresult=max_solutions)
                for solution in query:
                    yield _normalize_value(dict(solution))
            except PrologQueryError:
                raise
            except Exception as error:
                self.logger.error("Prolog query failed: %s", goal)
                raise PrologQueryError(goal, error) from error
            finally:
                if query is not None:
                    close = getattr(query, "close", None)
                    if close is not None:
                        try:
                            close()
                        except Exception as error:
                            raise PrologQueryError(goal, error) from error

    def execute_command(self, input_text: str) -> bool:
        """Execute command_loop:handle_command/1."""
        goal = f"command_loop:handle_command({_prolog_string(input_text)})"
        return self.query_once(goal) is not None

    def get_app_mapping(self, app_name: str) -> Optional[str]:
        """Query app_mapping/2 from config."""
        goal = f"kb_config:app_mapping({_prolog_atom(app_name)}, Cmd)"
        result = self.query_once(goal)
        return result.get("Cmd") if result else None

    def resolve_intent(
        self,
        text: str,
        state: str = "passive",
    ) -> Optional[IntentResult]:
        """Resolve natural language to an intent and arguments."""
        if state not in {"passive", "conversation", "dictation"}:
            raise ValueError(f"Unsupported intent state: {state}")
        goal = (
            f"intent_resolver:resolve({_prolog_string(text)}, {state}, "
            "Intent, Args)"
        )
        self.logger.info("Resolving intent in state %s for input %r", state, text)
        result = self.query_once(goal)
        self.logger.info("Intent result: %r", result)
        return adapt_intent_result(result) if result else None

    def execute_intent(self, intent: str, args: List[Any]) -> bool:
        """Execute a resolved intent and report logical success."""
        goal = f"commands:execute({_prolog_atom(intent)}, {_prolog_term(args)})"
        return self.query_once(goal) is not None

    def schedule_has_no_overlap(
        self,
        schedule_iso: str,
        existing_times: List[str],
    ) -> bool:
        """Check a proposed schedule through the Prolog overlap policy."""
        goal = (
            f"todo_schedule:no_overlap({_prolog_atom(schedule_iso)}, "
            f"{_prolog_term(existing_times)})"
        )
        return self.query_once(goal) is not None

    def is_conversation_stop(
        self,
        text: str,
        state: str = "conversation",
    ) -> bool:
        """Check whether text matches a conversation stop intent."""
        result = self.resolve_intent(text, state=state)
        if result is None:
            return False
        return result.kind == "prolog" and result.name == "end_conversation"

    def dictation_active(self) -> bool:
        """Check whether dictation mode is currently active."""
        return self.query_once("dictation:dictation_active") is not None

    def reload_config(self) -> bool:
        """Reload user configuration and report logical success."""
        return self.query_once("config_loader:reload_user_config") is not None


def test_engine() -> None:
    logging.basicConfig(level=logging.INFO)
    main_file = Path(__file__).parent.parent / "main.pl"
    engine = PrologEngine(main_file)
    print(engine.resolve_intent("open firefox"))
    print(engine.get_app_mapping("terminal"))
    print(engine.execute_command("hello"))


if __name__ == "__main__":
    test_engine()
