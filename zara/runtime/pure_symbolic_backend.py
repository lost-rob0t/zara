"""Provider-free symbolic conversation backend for RuntimeHost.

This is a thin application adapter over Zara's canonical Prolog dialogue
contract. It owns no history, planner, expert registry, permission system,
tool executor, or provider client. RuntimeHost remains the lifecycle,
cancellation, stale-generation, capability, and effect boundary.
"""

from __future__ import annotations

import asyncio
import json
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


def _resolve_turn(engine: PrologEngine, text: str) -> PureSymbolicTurn:
    """Run one stateless turn through the canonical Prolog dialogue adapter."""
    text_term = json.dumps(text, ensure_ascii=False)
    goal = (
        "symbolic_dialogue_turn:dialogue_turn("
        f"{text_term}, conversation, [], turn(_Frames, Act, Context)), "
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
        context_term=_as_text(row["ContextTerm"]),
    )


class PureSymbolicRuntimeBackend(RuntimeBackend):
    """Hard-zero model/provider backend under Zara's existing RuntimeHost."""

    def __init__(
        self,
        engine_factory: Optional[Callable[[], Any]] = None,
        *,
        module_path: Optional[Path] = None,
        turn_resolver: Optional[Callable[[Any, str], PureSymbolicTurn]] = None,
    ) -> None:
        self._engine_factory = engine_factory or _engine_factory
        self._module_path = module_path
        self._turn_resolver = turn_resolver or _resolve_turn
        self._engine: Any = None

    async def start(self) -> None:
        if self._engine is not None:
            return
        engine = self._engine_factory()
        path = self._module_path or _module_path()
        await asyncio.to_thread(engine.consult, path)
        self._engine = engine

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
        del turn_id, conversation_id, latency_trace
        if self._engine is None:
            raise RuntimeError("runtime backend is not started")
        if context_ids:
            raise UnsupportedRuntimeCommand(
                "context attachments are not available in pure symbolic mode"
            )
        if system_context is not None or conversation_history is not None:
            raise UnsupportedRuntimeCommand(
                "task prompt context is not available in pure symbolic mode"
            )

        symbolic = await asyncio.to_thread(self._turn_resolver, self._engine, text)
        if symbolic.provider_calls != 0 or symbolic.model_calls != 0:
            raise RuntimeError("pure symbolic resolver violated the zero-call contract")
        if symbolic.renderer != PURE_SYMBOLIC_RENDERER:
            raise RuntimeError(
                "pure symbolic resolver returned a non-canonical renderer"
            )
        return RuntimeTurnResult(
            response=symbolic.response,
            metadata={
                "route": "pure_symbolic",
                "response_act": symbolic.act_term,
                "dialogue_context": symbolic.context_term,
                "renderer": symbolic.renderer,
                "providers_enabled": False,
                "max_provider_calls": 0,
                "max_model_calls": 0,
                "provider_calls": 0,
                "model_calls": 0,
            },
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
]
