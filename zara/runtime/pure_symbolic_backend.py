"""Provider-free symbolic conversation backend for RuntimeHost.

This is a thin application adapter over Zara's canonical Prolog dialogue
contract. It owns no history, planner, expert registry, permission system,
tool executor, or provider client. RuntimeHost remains the lifecycle,
cancellation, stale-generation, capability, and effect boundary.
"""

from __future__ import annotations

import asyncio
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Optional, Protocol

from zara.latency import LatencyTrace
from zara.prolog_engine import PrologEngine, locate_main_pl

from .backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand

PURE_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
PURE_SYMBOLIC_RENDER_ERROR = (
    "I couldn't render that symbolic response. No model or provider was used."
)
_MAX_CONTEXT_TERM_CHARS = 8192
_MAX_EXPERT_EVIDENCE_CHARS = 128
_DIALOGUE_ACT_RE = re.compile(r"^([a-z][a-z0-9_.-]{0,127})(?:\(|$)")
_EXPERT_ANSWER_RE = re.compile(r"^answer\(expert,")


class SymbolicProjectionPort(Protocol):
    """Runtime-neutral persistence port supplied by a product composition root."""

    def load_dialogue_context(self, conversation_id: str) -> tuple[str, int]: ...

    def commit_turn(
        self,
        *,
        conversation_id: str,
        expected_generation: int,
        turn_id: str,
        response: str,
        dialogue_act: str,
        response_act_term: str,
        context_term: str,
        renderer_provenance: str,
        expert_evidence_ref: Optional[str] = None,
    ) -> None: ...


@dataclass(frozen=True)
class PureSymbolicTurn:
    """Portable result projected from ``ZARA-SYMBOLIC-DIALOGUE/1`` semantics."""

    response: str
    act_term: str
    context_term: str
    expert_evidence_ref: Optional[str] = None
    renderer: str = PURE_SYMBOLIC_RENDERER
    provider_calls: int = 0
    model_calls: int = 0


def _as_text(value: Any) -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return str(value)


def _module_path() -> Path:
    return locate_main_pl().parent / "modules" / "symbolic_dialogue_turn.pl"


def _engine_factory() -> PrologEngine:
    return PrologEngine(main_file=locate_main_pl())


def _bounded_context_term(value: object) -> str:
    if not isinstance(value, str):
        raise TypeError("symbolic dialogue context must be text")
    if not value or len(value) > _MAX_CONTEXT_TERM_CHARS:
        raise ValueError(
            f"symbolic dialogue context must be 1..{_MAX_CONTEXT_TERM_CHARS} characters"
        )
    if "\x00" in value:
        raise ValueError("symbolic dialogue context must not contain NUL")
    return value


def _dialogue_act_token(value: object) -> str:
    if not isinstance(value, str):
        raise TypeError("symbolic response act must be text")
    match = _DIALOGUE_ACT_RE.match(value)
    if match is None:
        raise ValueError("symbolic response act is not a normalized term")
    return match.group(1)


def _require_expert_evidence_ref(value: object) -> str:
    if not isinstance(value, str):
        raise TypeError("expert evidence reference must be text")
    if not value or len(value) > _MAX_EXPERT_EVIDENCE_CHARS:
        raise ValueError(
            f"expert evidence reference must be 1..{_MAX_EXPERT_EVIDENCE_CHARS} characters"
        )
    if any(ord(character) < 0x20 or 0x7F <= ord(character) <= 0x9F for character in value):
        raise ValueError("expert evidence reference contains control characters")
    return value


def _dialogue_envelope(
    act_term: object,
    expert_evidence_ref: object,
) -> tuple[str, Optional[str]]:
    dialogue_act = _dialogue_act_token(act_term)
    is_expert_answer = bool(
        isinstance(act_term, str) and _EXPERT_ANSWER_RE.match(act_term)
    )
    if is_expert_answer:
        if expert_evidence_ref is None:
            raise RuntimeError("canonical expert answer requires expert evidence")
        return "expert_answer", _require_expert_evidence_ref(expert_evidence_ref)
    if expert_evidence_ref is not None:
        raise RuntimeError("non-expert symbolic dialogue returned expert evidence")
    return dialogue_act, None


def _resolve_turn(
    engine: PrologEngine,
    text: str,
    context_term: str = "[]",
) -> PureSymbolicTurn:
    """Run one turn through the canonical Prolog dialogue adapter."""
    text_term = json.dumps(text, ensure_ascii=False)
    context_text = json.dumps(_bounded_context_term(context_term), ensure_ascii=False)
    goal = (
        f"term_string(Context0, {context_text}, [syntax_errors(error)]), "
        "ground(Context0), "
        "(Context0 = [] ; "
        " Context0 = partial_frame(frame(_, _, _), Open), is_list(Open) ; "
        " Context0 = completed_frame(frame(_, _, _))), "
        "symbolic_dialogue_turn:dialogue_turn("
        f"{text_term}, conversation, Context0, turn(_Frames, Act, Context)), "
        "symbolic_dialogue:render_response(Act, Response), "
        "term_string(Act, ActTerm, [quoted(true)]), "
        "term_string(Context, ContextTerm, [quoted(true)]), "
        "(Act = answer(expert, _, evidence(EvidenceValue)) -> "
        " (string(EvidenceValue) -> EvidenceRef = EvidenceValue ; "
        "  atom(EvidenceValue) -> atom_string(EvidenceValue, EvidenceRef) ; fail) ; "
        " EvidenceRef = \"\")"
    )
    row = engine.query_once(goal)
    if row is None:
        return PureSymbolicTurn(
            response=PURE_SYMBOLIC_RENDER_ERROR,
            act_term="error(renderer_unavailable)",
            context_term="[]",
        )
    raw_evidence = _as_text(row.get("EvidenceRef", ""))
    return PureSymbolicTurn(
        response=_as_text(row["Response"]),
        act_term=_as_text(row["ActTerm"]),
        context_term=_bounded_context_term(_as_text(row["ContextTerm"])),
        expert_evidence_ref=raw_evidence or None,
    )


def _is_exact_zero(value: Any) -> bool:
    return type(value) is int and value == 0


class PureSymbolicRuntimeBackend(RuntimeBackend):
    """Hard-zero model/provider backend under Zara's existing RuntimeHost."""

    def __init__(
        self,
        engine_factory: Optional[Callable[[], Any]] = None,
        *,
        module_path: Optional[Path] = None,
        turn_resolver: Optional[Callable[[Any, str], PureSymbolicTurn]] = None,
        projection_adapter: Optional[SymbolicProjectionPort] = None,
    ) -> None:
        self._engine_factory = engine_factory or _engine_factory
        self._module_path = module_path
        self._turn_resolver = turn_resolver or _resolve_turn
        self._uses_default_turn_resolver = turn_resolver is None
        self._projection_adapter = projection_adapter
        self._engine: Any = None

    async def start(self) -> None:
        if self._engine is not None:
            return
        engine = self._engine_factory()
        path = self._module_path or _module_path()
        await asyncio.to_thread(engine.consult, path)
        self._engine = engine

    def _load_dialogue_context(
        self,
        conversation_id: Optional[str],
    ) -> tuple[str, int]:
        adapter = self._projection_adapter
        if adapter is None or conversation_id is None:
            return "[]", 0
        context, generation = adapter.load_dialogue_context(conversation_id)
        if type(generation) is not int or generation < 0:
            raise RuntimeError("symbolic projection returned an invalid generation")
        return _bounded_context_term(context), generation

    async def submit_turn(
        self,
        text: str,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
        context_ids: tuple[str, ...] = (),
        system_context: Optional[str] = None,
        conversation_history: Optional[list] = None,
        latency_trace: Optional[LatencyTrace] = None,
    ) -> RuntimeTurnResult:
        del turn_id, latency_trace
        if self._engine is None:
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not available in pure symbolic mode"
            )
        if system_context is not None or conversation_history not in (None, []):
            raise UnsupportedRuntimeCommand(
                "task prompt context is not available in pure symbolic mode"
            )

        context_term, base_generation = self._load_dialogue_context(conversation_id)
        if self._uses_default_turn_resolver:
            symbolic = await asyncio.to_thread(
                self._turn_resolver,
                self._engine,
                text,
                context_term,
            )
        else:
            symbolic = await asyncio.to_thread(self._turn_resolver, self._engine, text)
        if not _is_exact_zero(symbolic.provider_calls) or not _is_exact_zero(
            symbolic.model_calls
        ):
            raise RuntimeError("pure symbolic resolver violated the zero-call contract")
        if symbolic.renderer != PURE_SYMBOLIC_RENDERER:
            raise RuntimeError(
                "pure symbolic resolver returned a non-canonical renderer"
            )
        dialogue_act, expert_evidence_ref = _dialogue_envelope(
            symbolic.act_term,
            symbolic.expert_evidence_ref,
        )
        metadata = {
            "route": "pure_symbolic",
            "response_act": symbolic.act_term,
            "dialogue_act": dialogue_act,
            "dialogue_context": symbolic.context_term,
            "renderer": symbolic.renderer,
            "providers_enabled": False,
            "max_provider_calls": 0,
            "max_model_calls": 0,
            "provider_calls": 0,
            "model_calls": 0,
        }
        if expert_evidence_ref is not None:
            metadata["expert_evidence_ref"] = expert_evidence_ref
        if self._projection_adapter is not None and conversation_id is not None:
            metadata["projection_base_generation"] = base_generation
        return RuntimeTurnResult(response=symbolic.response, metadata=metadata)

    def commit_turn_result(
        self,
        result: RuntimeTurnResult,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
    ) -> None:
        """Commit one accepted turn through the injected canonical projection port."""
        adapter = self._projection_adapter
        if adapter is None or conversation_id is None:
            return
        metadata = result.metadata
        if metadata.get("route") != "pure_symbolic":
            raise RuntimeError("pure symbolic commit received a foreign route")
        base_generation = metadata.get("projection_base_generation")
        if type(base_generation) is not int or base_generation < 0:
            raise RuntimeError("pure symbolic commit has invalid projection generation")
        if metadata.get("renderer") != PURE_SYMBOLIC_RENDERER:
            raise RuntimeError("pure symbolic commit has invalid renderer")
        if metadata.get("providers_enabled") is not False:
            raise RuntimeError("pure symbolic commit has providers enabled")
        for key in (
            "max_provider_calls",
            "max_model_calls",
            "provider_calls",
            "model_calls",
        ):
            if not _is_exact_zero(metadata.get(key)):
                raise RuntimeError(f"pure symbolic commit has nonzero {key}")

        response_act = metadata.get("response_act")
        dialogue_act, expert_evidence_ref = _dialogue_envelope(
            response_act,
            metadata.get("expert_evidence_ref"),
        )
        if metadata.get("dialogue_act") != dialogue_act:
            raise RuntimeError("pure symbolic commit has mismatched dialogue act")
        context_term = _bounded_context_term(metadata.get("dialogue_context"))
        adapter.commit_turn(
            conversation_id=conversation_id,
            expected_generation=base_generation,
            turn_id=turn_id,
            response=result.response,
            dialogue_act=dialogue_act,
            response_act_term=response_act,
            context_term=context_term,
            renderer_provenance=PURE_SYMBOLIC_RENDERER,
            expert_evidence_ref=expert_evidence_ref,
        )

    async def cancel_turn(self, turn_id: str) -> None:
        # The canonical Prolog turn has no effects. RuntimeHost owns the task
        # cancellation and generation fence, so late results are discarded.
        del turn_id

    async def stop(self) -> None:
        self._engine = None


__all__ = [
    "PURE_SYMBOLIC_RENDERER",
    "PureSymbolicRuntimeBackend",
    "PureSymbolicTurn",
    "SymbolicProjectionPort",
]
