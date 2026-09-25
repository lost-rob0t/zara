from __future__ import annotations

from pathlib import Path

import pytest

from zara.runtime import pure_symbolic_backend as symbolic
from zara.runtime.backend import RuntimeTurnResult, UnsupportedRuntimeCommand
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


class LegacyProjection:
    def __init__(self, generation=7):
        self.generation = generation
        self.commits = []

    def load_dialogue_context(self, conversation_id):
        assert conversation_id == "conversation-1"
        return "[]", self.generation

    def commit_turn(self, **kwargs):
        self.commits.append(kwargs)


def canonical_commit_result(**updates):
    metadata = {
        "route": "pure_symbolic",
        "projection_base_generation": 7,
        "response_act": "greeting",
        "dialogue_context": "[]",
        "renderer": symbolic.PURE_SYMBOLIC_RENDERER,
        "providers_enabled": False,
        "max_provider_calls": 0,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }
    metadata.update(updates)
    return RuntimeTurnResult(response="hello", metadata=metadata)


def test_symbolic_persisted_wire_boundaries_fail_closed():
    for value in (1, "", "x" * 8193, "[]\x00"):
        with pytest.raises((TypeError, ValueError)):
            symbolic._bounded_context_term(value)
    for value in (1, "", "x" * 4097, "greeting\x00"):
        with pytest.raises((TypeError, ValueError)):
            symbolic._bounded_response_act_term(value)
    with pytest.raises(TypeError):
        symbolic._dialogue_act_token(1)
    with pytest.raises(ValueError):
        symbolic._dialogue_act_token("Greeting")
    for value in (1, "", "x" * 129, "evidence:bad\x1f"):
        with pytest.raises((TypeError, ValueError)):
            symbolic._require_expert_evidence_ref(value)
    assert symbolic._as_text(b"hello") == "hello"


def test_legacy_projection_without_response_act_loader_preserves_generation():
    backend = PureSymbolicRuntimeBackend(projection_adapter=LegacyProjection())
    assert backend._load_dialogue_state("conversation-1") == ("[]", None, 7)

    invalid = PureSymbolicRuntimeBackend(
        projection_adapter=LegacyProjection(generation=True)
    )
    with pytest.raises(RuntimeError, match="invalid generation"):
        invalid._load_dialogue_state("conversation-1")


@pytest.mark.asyncio
async def test_symbolic_start_is_idempotent_and_cancel_is_effect_free():
    engine = FakeEngine()
    module_path = Path("/fixture/symbolic_dialogue_turn.pl")
    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        module_path=module_path,
        turn_resolver=lambda _engine, _text: PureSymbolicTurn(
            "ok", "greeting", "[]"
        ),
    )
    await backend.start()
    await backend.start()
    await backend.cancel_turn("turn-edge")
    assert engine.consulted == [module_path]


@pytest.mark.asyncio
async def test_symbolic_submit_requires_start_and_canonical_renderer():
    calls = []
    backend = PureSymbolicRuntimeBackend(
        turn_resolver=lambda _engine, text: calls.append(text)
    )
    with pytest.raises(RuntimeError, match="not started"):
        await backend.submit_turn("hello", turn_id="turn-unstarted")
    assert calls == []

    engine = FakeEngine()
    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        module_path=Path("/fixture/symbolic_dialogue_turn.pl"),
        turn_resolver=lambda _engine, _text: PureSymbolicTurn(
            response="bad",
            act_term="greeting",
            context_term="[]",
            renderer="provider-renderer/v1",
        ),
    )
    await backend.start()
    with pytest.raises(RuntimeError, match="non-canonical renderer"):
        await backend.submit_turn("hello", turn_id="turn-renderer")


@pytest.mark.parametrize(
    ("updates", "message"),
    [
        ({"route": "standard"}, "foreign route"),
        ({"projection_base_generation": True}, "invalid projection generation"),
        ({"renderer": "provider-renderer/v1"}, "invalid renderer"),
        ({"providers_enabled": True}, "providers enabled"),
        ({"max_provider_calls": 1}, "nonzero max_provider_calls"),
        ({"max_model_calls": 1}, "nonzero max_model_calls"),
        ({"provider_calls": 1}, "nonzero provider_calls"),
        ({"model_calls": 1}, "nonzero model_calls"),
        ({"dialogue_act": "expert_answer"}, "mismatched dialogue act"),
    ],
)
def test_symbolic_commit_rejects_forged_projection_metadata(updates, message):
    adapter = LegacyProjection()
    backend = PureSymbolicRuntimeBackend(projection_adapter=adapter)
    with pytest.raises(RuntimeError, match=message):
        backend.commit_turn_result(
            canonical_commit_result(**updates),
            turn_id="turn-commit",
            conversation_id="conversation-1",
        )
    assert adapter.commits == []


def test_symbolic_commit_without_projection_owner_is_noop():
    PureSymbolicRuntimeBackend().commit_turn_result(
        canonical_commit_result(),
        turn_id="turn-no-owner",
        conversation_id="conversation-1",
    )
