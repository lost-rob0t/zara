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
from typing import Any, Callable, Optional

from zara.latency import LatencyTrace
from zara.prolog_engine import PrologEngine, locate_main_pl

from .backend import RuntimeBackend, RuntimeTurnResult, UnsupportedRuntimeCommand

PURE_SYMBOLIC_RENDERER = "symbolic-dcg/v1"
PURE_SYMBOLIC_RENDER_ERROR = (
    "I couldn't render that symbolic response. No model or provider was used."
)
_MAX_CONTEXT_TERM_CHARS = 8192
_DIALOGUE_ACT_RE = re.compile(r"^([a-z][a-z0-9_.-]{0,127})(?:\\(|$)")


@dataclass(frozen=True)
class PureSymbolicTurn:
    """Portable result projected from ``ZARA-SYMBOLIC-DIALOGUE/1`` semantics."""

    response: str
    act_term: str
    context_term: str
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
    if "\\x00" in value:
        raise ValueError("symbolic dialogue context must not contain NUL")
    return value


def _dialogue_act_token(value: object) -> str:
    if not isinstance(value, str):
        raise TypeError("symbolic response act must be text")
    match = _DIALOGUE_ACT_RE.match(value)
    if match is None:
        raise ValueError("symbolic response act is not a normalized term")
    return match.group(1)

def _resolve_turn(engine: PrologEngine, text: str, context_term: str = "[]") -> PureSymbolicTurn:
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
        "term_string(Context, ContextTerm, [quoted(true)])"
    )
    row = engine.query_once(goal)
    if row is None:
        return PureSymbolicTurn(
            response=PURE_SYMBOLIC_RENDER_ERROR,
            act_term="error(renderer_unavailable)",
            context_term="[]",
        )
    return PureSymbolicTurn(
        response=_as_text(row["Response"]),
        act_term=_as_text(row["ActTerm"]),
        context_term=_bounded_context_term(_as_text(row["ContextTerm"])),
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
        projection_store: Any = None,
    ) -> None:
        self._engine_factory = engine_factory or _engine_factory
        self._module_path = module_path
        self._turn_resolver = turn_resolver or _resolve_turn
        self._uses_default_turn_resolver = turn_resolver is None
        self._projection_store = projection_store
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
        store = self._projection_store
        if store is None or conversation_id is None:
            return "[]", 0
        projection = store.load_symbolic_projection(conversation_id)
        if projection is None:
            return "[]", 0
        projection.assert_pure_symbolic()
        context = projection.dialogue_state.get("prolog_context_term", "[]")
        return _bounded_context_term(context), projection.projection_generation

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
                self._turn_resolver, self._engine, text, context_term
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
        metadata = {
            "route": "pure_symbolic",
            "response_act": symbolic.act_term,
            "dialogue_context": symbolic.context_term,
            "renderer": symbolic.renderer,
            "providers_enabled": False,
            "max_provider_calls": 0,
            "max_model_calls": 0,
            "provider_calls": 0,
            "model_calls": 0,
        }
        if self._projection_store is not None and conversation_id is not None:
            metadata["projection_base_generation"] = base_generation
        return RuntimeTurnResult(response=symbolic.response, metadata=metadata)

    def commit_turn_result(
        self,
        result: RuntimeTurnResult,
        *,
        turn_id: str,
        conversation_id: Optional[str] = None,
    ) -> None:
        """CAS one accepted symbolic turn into Zara\'s canonical projection."""
        store = self._projection_store
        if store is None or conversation_id is None:
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
        for key in ("max_model_calls", "provider_calls", "model_calls"):
            if not _is_exact_zero(metadata.get(key)):
                raise RuntimeError(f"pure symbolic commit has nonzero {key}")

        response_act = metadata.get("response_act")
        dialogue_act = _dialogue_act_token(response_act)
        context_term = _bounded_context_term(metadata.get("dialogue_context"))
        current = store.load_symbolic_projection(conversation_id)
        current_generation = current.projection_generation if current is not None else 0
        if current_generation != base_generation:
            raise RuntimeError(
                "stale symbolic projection result: "
                f"read generation {base_generation}, current {current_generation}"
            )
        if current is not None:
            current.assert_pure_symbolic()

        from zara.desktop.conversation import SymbolicConversationProjection

        dialogue_state = dict(current.dialogue_state) if current is not None else {}
        dialogue_state["prolog_context_term"] = context_term
        dialogue_state["response_act_term"] = response_act
        prior_questions = list(current.unresolved_questions) if current is not None else []
        unresolved_questions = [
            item for item in prior_questions
            if item.get("source") != "symbolic_dialogue"
        ]
        if dialogue_act == "clarify":
            unresolved_questions.append({
                "id": f"turn:{turn_id}:clarification",
                "text": result.response,
                "source": "symbolic_dialogue",
            })

        projection = SymbolicConversationProjection(
            conversation_id=conversation_id,
            projection_generation=base_generation + 1,
            runtime_generation=(current.runtime_generation if current else 0) + 1,
            turn_id=turn_id,
            outcome="success",
            project_id=current.project_id if current else None,
            project_generation=current.project_generation if current else 0,
            dialogue_act=dialogue_act,
            dialogue_state=dialogue_state,
            discourse_entities=list(current.discourse_entities) if current else [],
            unresolved_questions=unresolved_questions,
            expert_evidence=list(current.expert_evidence) if current else [],
            verified_facts=list(current.verified_facts) if current else [],
            verified_outcome_refs=list(current.verified_outcome_refs) if current else [],
            renderer_provenance=PURE_SYMBOLIC_RENDERER,
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        )
        stored = store.save_symbolic_projection(
            projection, expected_generation=base_generation
        )
        stored.assert_pure_symbolic()

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
]
