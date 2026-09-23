from __future__ import annotations

from pathlib import Path

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend, PureSymbolicTurn


EVIDENCE_REF = "expert:timer/invocation-7"
EXPERT_ACT = (
    'answer(expert,"Timer is already running.",'
    f"evidence('{EVIDENCE_REF}'))"
)


class _ExpertRowEngine:
    def __init__(self) -> None:
        self.consulted: list[Path] = []
        self.goals: list[str] = []

    def consult(self, path: Path) -> None:
        self.consulted.append(path)

    def query_once(self, goal: str):
        self.goals.append(goal)
        return {
            "Response": "Timer is already running.",
            "ActTerm": EXPERT_ACT,
            "ContextTerm": "[]",
            "EvidenceRef": EVIDENCE_REF,
        }


@pytest.mark.asyncio
async def test_default_desktop_resolver_exports_same_expert_evidence_as_android() -> None:
    engine = _ExpertRowEngine()
    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: engine,
        module_path=Path("symbolic_dialogue_turn.pl"),
    )
    await backend.start()
    try:
        result = await backend.submit_turn(
            "timer status",
            turn_id="turn-expert-default",
            conversation_id="conversation-expert-default",
        )
    finally:
        await backend.stop()

    assert result.response == "Timer is already running."
    assert result.metadata["response_act"] == EXPERT_ACT
    assert result.metadata["dialogue_act"] == "expert_answer"
    assert result.metadata["expert_evidence_ref"] == EVIDENCE_REF
    assert result.metadata["providers_enabled"] is False
    assert result.metadata["max_model_calls"] == 0
    assert result.metadata["provider_calls"] == 0
    assert result.metadata["model_calls"] == 0
    assert engine.goals
    assert "answer(expert" in engine.goals[-1]
    assert "EvidenceRef" in engine.goals[-1]


@pytest.mark.asyncio
async def test_desktop_expert_answer_persists_evidence_across_restart(tmp_path) -> None:
    path = tmp_path / "desktop-expert-evidence.db"
    database = DatabaseManager(path)
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Expert evidence parity",
        conversation_id="conversation-expert-parity",
    )

    backend = PureSymbolicRuntimeBackend(
        engine_factory=lambda: _ExpertRowEngine(),
        module_path=Path("symbolic_dialogue_turn.pl"),
        turn_resolver=lambda _engine, _text: PureSymbolicTurn(
            response="Timer is already running.",
            act_term=EXPERT_ACT,
            context_term="[]",
            expert_evidence_ref=EVIDENCE_REF,
        ),
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await backend.start()
    try:
        result = await backend.submit_turn(
            "timer status",
            turn_id="turn-expert-1",
            conversation_id=conversation.id,
        )
        backend.commit_turn_result(
            result,
            turn_id="turn-expert-1",
            conversation_id=conversation.id,
        )
    finally:
        await backend.stop()

    projection = store.load_symbolic_projection(conversation.id)
    assert projection is not None
    projection.assert_pure_symbolic()
    assert projection.dialogue_act == "expert_answer"
    assert projection.expert_evidence == [{"ref": EVIDENCE_REF}]
    assert projection.providers_enabled is False
    assert projection.max_model_calls == 0
    assert projection.provider_calls == 0
    assert projection.model_calls == 0
    database.close()

    reopened_database = DatabaseManager(path)
    reopened = ConversationStore(reopened_database)
    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.dialogue_act == "expert_answer"
    assert recovered.expert_evidence == [{"ref": EVIDENCE_REF}]
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0
    reopened_database.close()


@pytest.mark.asyncio
async def test_desktop_expert_evidence_envelope_fails_closed_on_mismatch() -> None:
    engine = _ExpertRowEngine()

    async def submit(symbolic: PureSymbolicTurn):
        backend = PureSymbolicRuntimeBackend(
            engine_factory=lambda: engine,
            module_path=Path("symbolic_dialogue_turn.pl"),
            turn_resolver=lambda _engine, _text: symbolic,
        )
        await backend.start()
        try:
            return await backend.submit_turn("status", turn_id="turn-envelope")
        finally:
            await backend.stop()

    with pytest.raises(RuntimeError, match="expert evidence"):
        await submit(
            PureSymbolicTurn(
                response="missing",
                act_term=EXPERT_ACT,
                context_term="[]",
            )
        )

    with pytest.raises(RuntimeError, match="non-expert.*expert evidence"):
        await submit(
            PureSymbolicTurn(
                response="hello",
                act_term="greeting",
                context_term="[]",
                expert_evidence_ref=EVIDENCE_REF,
            )
        )

    with pytest.raises((TypeError, ValueError), match="expert evidence"):
        await submit(
            PureSymbolicTurn(
                response="bad",
                act_term=EXPERT_ACT,
                context_term="[]",
                expert_evidence_ref="expert:bad\ncontrol",
            )
        )
