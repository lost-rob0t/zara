from __future__ import annotations

from pathlib import Path

import pytest

from zara.prolog_engine import SymbolicDialogueTurn
from zara.runtime.backend import (
    PureSymbolicRuntimeBackend,
    UnsupportedRuntimeCommand,
)


class FakeEngine:
    def __init__(self) -> None:
        self.consulted: list[Path] = []
        self.calls: list[tuple[str, str, str]] = []

    def consult(self, path: Path) -> None:
        self.consulted.append(path)

    def symbolic_dialogue_turn(
        self,
        text: str,
        *,
        state: str,
        context_term: str,
    ) -> SymbolicDialogueTurn:
        self.calls.append((text, state, context_term))
        return SymbolicDialogueTurn(
            response="Hello from symbols.",
            act_term="greeting",
            context_term="[]",
            renderer="symbolic-dcg/v1",
            provider_calls=0,
            model_calls=0,
        )


@pytest.mark.asyncio
async def test_pure_symbolic_backend_is_provider_free_and_exact_zero() -> None:
    engine = FakeEngine()
    backend = PureSymbolicRuntimeBackend(engine_factory=lambda: engine)

    await backend.start()
    result = await backend.submit_turn(
        "hello",
        turn_id="turn-1",
        conversation_id="conversation-1",
    )

    assert engine.consulted
    assert engine.consulted[-1].name == "symbolic_dialogue_turn.pl"
    assert engine.calls == [("hello", "conversation", "[]")]
    assert result.response == "Hello from symbols."
    assert result.tool_results == ()
    assert result.metadata == {
        "route": "pure_symbolic",
        "response_act": "greeting",
        "dialogue_context": "[]",
        "renderer": "symbolic-dcg/v1",
        "providers_enabled": False,
        "max_provider_calls": 0,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }


@pytest.mark.asyncio
async def test_pure_symbolic_backend_fails_closed_on_noncanonical_context_inputs() -> None:
    engine = FakeEngine()
    backend = PureSymbolicRuntimeBackend(engine_factory=lambda: engine)
    await backend.start()

    with pytest.raises(
        UnsupportedRuntimeCommand,
        match="context attachments are not available in pure symbolic mode",
    ):
        await backend.submit_turn(
            "hello",
            turn_id="turn-2",
            context_ids=("attachment-1",),
        )

    with pytest.raises(
        UnsupportedRuntimeCommand,
        match="task prompt context is not available in pure symbolic mode",
    ):
        await backend.submit_turn(
            "hello",
            turn_id="turn-3",
            system_context="pretend this came from a model",
        )

    assert engine.calls == []
