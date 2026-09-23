from __future__ import annotations

from pathlib import Path

import pytest

from zara.runtime.backend import UnsupportedRuntimeCommand
from zara.runtime.pure_symbolic_backend import (
    PureSymbolicRuntimeBackend,
    PureSymbolicTurn,
)


class FakeEngine:
    def __init__(self) -> None:
        self.consulted: list[Path] = []

    def consult(self, path: Path) -> None:
        self.consulted.append(path)


@pytest.mark.asyncio
async def test_pure_symbolic_backend_is_provider_free_and_exact_zero() -> None:
    engine = FakeEngine()
    calls: list[str] = []

    def resolve_turn(_engine: FakeEngine, text: str) -> PureSymbolicTurn:
        assert _engine is engine
        calls.append(text)
        return PureSymbolicTurn(
            response="Hello from symbols.",
            act_term="greeting",
            context_term="[]",
        )

    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        turn_resolver=resolve_turn,
    )

    await backend.start()
    result = await backend.submit_turn(
        "hello",
        turn_id="turn-1",
        conversation_id="conversation-1",
    )

    assert engine.consulted
    assert engine.consulted[-1].name == "symbolic_dialogue_turn.pl"
    assert calls == ["hello"]
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
    calls: list[str] = []

    def resolve_turn(_engine: FakeEngine, text: str) -> PureSymbolicTurn:
        calls.append(text)
        return PureSymbolicTurn("unused", "unsupported", "[]")

    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        turn_resolver=resolve_turn,
    )
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

    assert calls == []


@pytest.mark.asyncio
async def test_pure_symbolic_backend_rejects_nonzero_usage_evidence() -> None:
    engine = FakeEngine()
    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        turn_resolver=lambda _engine, _text: PureSymbolicTurn(
            response="bad",
            act_term="unsupported",
            context_term="[]",
            provider_calls=1,
        ),
    )
    await backend.start()

    with pytest.raises(RuntimeError, match="zero-call contract"):
        await backend.submit_turn("hello", turn_id="turn-4")


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("provider_calls", False),
        ("provider_calls", 0.0),
        ("provider_calls", "0"),
        ("model_calls", False),
        ("model_calls", 0.0),
        ("model_calls", "0"),
    ],
)
@pytest.mark.asyncio
async def test_pure_symbolic_backend_requires_builtin_integer_zero_usage(
    field: str,
    value,
) -> None:
    engine = FakeEngine()

    def resolve_turn(_engine, _text):
        kwargs = {field: value}
        return PureSymbolicTurn(
            response="bad",
            act_term="unsupported",
            context_term="[]",
            **kwargs,
        )

    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        turn_resolver=resolve_turn,
    )
    await backend.start()

    with pytest.raises(RuntimeError, match="zero-call contract"):
        await backend.submit_turn("hello", turn_id="turn-5")
