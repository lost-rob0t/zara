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

from zara.runtime.frames import FrameStatus, IntentFrame

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


def _functor_name(value: Any) -> Optional[str]:
    name = getattr(value, "name", None)
    if isinstance(name, str):
        return name
    return None


def _functor_args(value: Any) -> List[Any]:
    args = getattr(value, "args", None)
    if args is None:
        return []
    return list(args)


def _decode_status(row: Dict[str, Any]) -> tuple[Any, tuple[str, ...], tuple[str, ...], Optional[str], Optional[str]]:
    from zara.runtime.frames import FrameStatus

    kind = str(row["StatusKind"])
    missing = tuple(str(item) for item in (row.get("Missing") or []))
    alternatives = tuple(str(item) for item in (row.get("Alternatives") or []))
    invalid_slot = row.get("InvalidSlot")
    invalid_reason = row.get("InvalidReason")
    invalid_slot = None if invalid_slot in (None, "none") else str(invalid_slot)
    invalid_reason = None if invalid_reason in (None, "none") else str(invalid_reason)
    return (
        FrameStatus(kind),
        missing,
        alternatives,
        invalid_slot,
        invalid_reason,
    )


def _decode_slot_value(row: Dict[str, Any]) -> Any:
    from zara.runtime.frames import (
        BoolValue,
        DateTimeValue,
        DurationValue,
        NumberValue,
        RefValue,
        TextValue,
    )

    kind = str(row["ValueKind"])
    first = row["A1"]
    second = row["A2"]
    if kind == "text":
        return TextValue(text=str(first))
    if kind == "duration":
        return DurationValue(seconds=int(first))
    if kind == "number":
        return NumberValue(value=first)
    if kind == "boolean":
        return BoolValue(value=bool(first))
    if kind == "ref":
        return RefValue(kind=str(first), id=str(second))
    if kind == "datetime":
        year, month, day, hour, minute, second = (int(item) for item in first)
        return DateTimeValue(year, month, day, hour, minute, second)
    raise ValueError(f"Unsupported slot value kind: {kind!r}")


def _grouped_frames(
    head_rows: List[Dict[str, Any]],
    slot_rows: List[Dict[str, Any]],
) -> List[IntentFrame]:
    from zara.runtime.frames import FilledSlot, SlotOrigin

    frames: List[IntentFrame] = []
    by_index: Dict[int, Dict[str, Any]] = {}
    for row in head_rows:
        index = int(row["Idx"])
        status, missing, alternatives, invalid_slot, invalid_reason = _decode_status(
            row
        )
        by_index[index] = {
            "intent_ns": str(row["NS"]),
            "intent_name": str(row["Name"]),
            "status": status,
            "missing": missing,
            "alternatives": alternatives,
            "invalid_slot": invalid_slot,
            "invalid_reason": invalid_reason,
            "slots": [],
        }
    for row in slot_rows:
        index = int(row["Idx"])
        entry = by_index.get(index)
        if entry is None:
            raise ValueError(f"Slot row references unknown frame index: {row!r}")
        entry["slots"].append(
            (
                int(row["SlotIdx"]),
                FilledSlot(
                    name=str(row["SlotName"]),
                    value=_decode_slot_value(row),
                    origin=SlotOrigin(str(row["Origin"])),
                ),
            )
        )
    for index in sorted(by_index):
        entry = by_index[index]
        slots = tuple(
            slot for _, slot in sorted(entry["slots"], key=lambda pair: pair[0])
        )
        frames.append(
            IntentFrame(
                intent_ns=entry["intent_ns"],
                intent_name=entry["intent_name"],
                slots=slots,
                status=entry["status"],
                missing=entry["missing"],
                alternatives=entry["alternatives"],
                invalid_slot=entry["invalid_slot"],
                invalid_reason=entry["invalid_reason"],
            )
        )
    return frames


def encode_frame_term(frame: Any) -> str:
    """Encode a Python IntentFrame mirror as a portable frame/3 term string."""
    slots = []
    for slot in frame.slots:
        value = slot.value
        value_type = type(value).__name__
        if value_type == "TextValue":
            value_term = f"text({_prolog_atom(value.text)})"
        elif value_type == "DurationValue":
            value_term = f"duration({int(value.seconds)})"
        elif value_type == "NumberValue":
            value_term = f"number({_prolog_term(value.value)})"
        elif value_type == "BoolValue":
            value_term = f"boolean({'true' if value.value else 'false'})"
        elif value_type == "RefValue":
            value_term = (
                f"ref(kind({_prolog_atom(value.kind)}), "
                f"id({_prolog_atom(value.id)}))"
            )
        elif value_type == "DateTimeValue":
            value_term = (
                f"datetime({int(value.year)}, {int(value.month)}, "
                f"{int(value.day)}, {int(value.hour)}, "
                f"{int(value.minute)}, {int(value.second)})"
            )
        else:
            raise ValueError(f"Unsupported slot value type: {value_type}")
        slots.append(
            f"slot(name({_prolog_atom(slot.name)}), value({value_term}), "
            f"origin({_prolog_atom(slot.origin.value)}))"
        )
    status = frame.status
    status_term = _prolog_atom(status.value)
    if status is FrameStatus.MISSING:
        status_term = "missing([{}])".format(
            ", ".join(_prolog_atom(name) for name in frame.missing)
        )
    elif status is FrameStatus.AMBIGUOUS:
        status_term = "ambiguous([{}])".format(
            ", ".join(_prolog_atom(alt) for alt in frame.alternatives)
        )
    elif status is FrameStatus.INVALID:
        status_term = (
            f"invalid(value({_prolog_atom(frame.invalid_slot)}), "
            f"{_prolog_atom(frame.invalid_reason)})"
        )
    return (
        f"frame(intent(ns({_prolog_atom(frame.intent_ns)}), "
        f"name({_prolog_atom(frame.intent_name)})), "
        f"[{', '.join(slots)}], {status_term})"
    )


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

    def get_wake_words(self) -> List[str]:
        """Query wake words from ``kb_config:wake_word/1``."""
        results = self.query_all("kb_config:wake_word(W)", max_solutions=64)
        words: List[str] = []
        for result in results:
            value = result.get("W")
            if isinstance(value, bytes):
                value = value.decode("utf-8")
            if not isinstance(value, str):
                value = getattr(value, "value", None) or str(value)
            if isinstance(value, str) and value.strip():
                words.append(value.strip())
        return words

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

    def resolve_frames(
        self,
        text: str,
        state: str = "passive",
        context_frame: Optional[Any] = None,
        missing: tuple[str, ...] = (),
    ) -> List[IntentFrame]:
        """Resolve an utterance to IntentFrame values (contract v1).

        ``context_frame`` plus ``missing`` encode an open clarification
        session frame as ``partial_frame(Frame, Missing)`` for follow-up
        resolution. The resolver is pure; frames carry no envelope metadata.
        """
        if state not in {"passive", "conversation", "dictation"}:
            raise ValueError(f"Unsupported intent state: {state}")
        if context_frame is not None:
            context_term = (
                "partial_frame("
                f"{encode_frame_term(context_frame)}, "
                f"[{', '.join(_prolog_atom(str(name)) for name in missing)}]"
                ")"
            )
        else:
            context_term = "[]"
        goal = (
            f"intent_frames:resolve_frames({_prolog_string(text)}, {state}, "
            f"{context_term}, Frames)"
        )
        self.logger.info("Resolving frames in state %s for input %r", state, text)
        return self.frames_from_goal(goal)

    def frames_from_goal(self, goal: str) -> List[IntentFrame]:
        """Project the ``Frames`` binding of ``goal`` into IntentFrame values.

        Nested compounds stringify under pyswip marshalling, so frames are
        decoded through the flat ``frame_head_row``/``frame_slot_row``
        projections (atoms, integers and lists of atoms only).
        """
        head_rows = self.query_all(
            f"{goal}, "
            "intent_frames:frame_head_row(Frames, Idx, NS, Name, StatusKind, "
            "Missing, Alternatives, InvalidSlot, InvalidReason)",
            max_solutions=64,
        )
        if not head_rows:
            return []
        slot_rows = self.query_all(
            f"{goal}, "
            "intent_frames:frame_slot_row(Frames, Idx, SlotIdx, SlotName, "
            "Origin, ValueKind, A1, A2)",
            max_solutions=256,
        )
        return _grouped_frames(head_rows, slot_rows)

    def plan_for_frame(self, frame: Any, environment: Any) -> Any:
        """Derive the typed ExecutionPlan for a complete frame (issue #157).

        The selection is pure Prolog; environments are encoded from the
        typed PlanEnvironment mirror. Non-complete frames are refused here:
        open frames belong to the clarification session, not the plan layer.
        A ``None`` result means Prolog produced no plan structure (bounds
        violation); typed unavailability otherwise rides the plan status.
        """
        from zara.runtime.frames import FrameStatus
        from zara.runtime.plans import PlanEnvironment

        if not isinstance(environment, PlanEnvironment):
            raise TypeError("plan_for_frame requires a PlanEnvironment")
        if frame.status is not FrameStatus.COMPLETE:
            raise ValueError(
                "plan_for_frame requires a complete frame; "
                "clarification owns open frames"
            )
        goal = (
            f"capability_plans:plan_for_frame({encode_frame_term(frame)}, "
            f"{encode_environment_term(environment)}, Plan), "
            "Plans = [Plan], "
            "capability_plans:plan_head_row(Plans, Idx, NS, Name, StatusKind, "
            "Reason, ProviderId, Location, DeviceRef, SideEffect, "
            "RequiresAuth, Evidence, Alternatives)"
        )
        self.logger.info("Deriving plan for frame %s/%s", frame.intent_ns, frame.intent_name)
        head_rows = self.query_all(goal, max_solutions=2)
        if not head_rows:
            return None
        if len(head_rows) > 1:
            raise ValueError("plan_for_frame produced multiple plans for one frame")
        arg_rows = self.query_all(
            "capability_plans:plan_for_frame("
            f"{encode_frame_term(frame)}, {encode_environment_term(environment)}, Plan), "
            "Plans = [Plan], "
            "capability_plans:plan_arg_row(Plans, Idx, ArgIdx, ArgName, "
            "ValueKind, A1, A2)",
            max_solutions=64,
        )
        plans = _grouped_plans(head_rows, arg_rows)
        return plans[0]

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


def encode_environment_term(environment: Any) -> str:
    """Encode a PlanEnvironment mirror as a portable environment/6 term string."""
    from zara.runtime.plans import PreferDevice, PreferLocation

    auths = ", ".join(_prolog_atom(str(auth)) for auth in environment.auths)
    devices = ", ".join(
        "device({}, {}, [{}])".format(
            _prolog_atom(str(device.device_id)),
            _prolog_atom(str(device.owner)),
            ", ".join(_prolog_atom(str(cap)) for cap in device.capabilities),
        )
        for device in environment.devices
    )
    providers = ", ".join(
        _prolog_atom(str(provider)) for provider in environment.providers
    )
    aliases = ", ".join(
        "alias({}, {})".format(
            _prolog_atom(str(provider)), _prolog_atom(str(alias))
        )
        for provider, alias in environment.aliases
    )
    policies = ", ".join(
        (
            "prefer(location({}))".format(_prolog_atom(policy.location.value))
            if isinstance(policy, PreferLocation)
            else "prefer(device({}))".format(_prolog_atom(policy.device_id))
        )
        for policy in environment.policies
    )
    return (
        "environment("
        f"principal({_prolog_atom(str(environment.principal))}), "
        f"auths([{auths}]), "
        f"devices([{devices}]), "
        f"providers([{providers}]), "
        f"aliases([{aliases}]), "
        f"policies([{policies}]))"
    )


def _sentinel(value: Any) -> Optional[str]:
    return None if value in (None, "none") else str(value)


def _grouped_plans(
    head_rows: List[Dict[str, Any]],
    arg_rows: List[Dict[str, Any]],
) -> List[Any]:
    from zara.runtime.plans import (
        ExecutionPlan,
        PlanArgument,
        PlanLocation,
        PlanSideEffect,
        PlanStatus,
    )

    plans: List[Any] = []
    by_index: Dict[int, Dict[str, Any]] = {}
    for row in head_rows:
        index = int(row["Idx"])
        location = _sentinel(row.get("Location"))
        by_index[index] = {
            "intent_ns": str(row["NS"]),
            "intent_name": str(row["Name"]),
            "status": PlanStatus(str(row["StatusKind"])),
            "reason": _sentinel(row.get("Reason")),
            "provider": _sentinel(row.get("ProviderId")),
            "location": PlanLocation(location) if location is not None else None,
            "device": _sentinel(row.get("DeviceRef")),
            "side_effect": PlanSideEffect(str(row["SideEffect"])),
            "requires_auth": _sentinel(row.get("RequiresAuth")),
            "evidence": tuple(str(item) for item in (row.get("Evidence") or [])),
            "alternatives": tuple(
                str(item) for item in (row.get("Alternatives") or [])
            ),
            "arguments": [],
        }
    for row in arg_rows:
        index = int(row["Idx"])
        entry = by_index.get(index)
        if entry is None:
            raise ValueError(f"Plan argument row references unknown index: {row!r}")
        entry["arguments"].append(
            (
                int(row["ArgIdx"]),
                PlanArgument(
                    name=str(row["ArgName"]),
                    value=_decode_slot_value(row),
                ),
            )
        )
    for index in sorted(by_index):
        entry = by_index[index]
        arguments = tuple(
            argument
            for _, argument in sorted(entry["arguments"], key=lambda pair: pair[0])
        )
        plans.append(
            ExecutionPlan(
                intent_ns=entry["intent_ns"],
                intent_name=entry["intent_name"],
                provider=entry["provider"],
                location=entry["location"],
                device=entry["device"],
                side_effect=entry["side_effect"],
                requires_auth=entry["requires_auth"],
                status=entry["status"],
                reason=entry["reason"],
                alternatives=entry["alternatives"],
                arguments=arguments,
                evidence=entry["evidence"],
            )
        )
    return plans


def test_engine() -> None:
    logging.basicConfig(level=logging.INFO)
    main_file = Path(__file__).parent.parent / "main.pl"
    engine = PrologEngine(main_file)
    print(engine.resolve_intent("open firefox"))
    print(engine.get_app_mapping("terminal"))
    print(engine.execute_command("hello"))


if __name__ == "__main__":
    test_engine()
