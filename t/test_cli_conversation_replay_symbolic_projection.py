from __future__ import annotations

from types import SimpleNamespace

import pytest

from zara.desktop.conversation.replay_status import conversation_symbolic_status
from zara.desktop.conversation.symbolic_projection import SymbolicConversationProjection


class _ReplayStore:
    def __init__(self, projection) -> None:
        self.projection = projection
        self.requested: list[tuple[str, str]] = []

    def get_conversation(self, conversation_id: str):
        self.requested.append(("conversation", conversation_id))
        if conversation_id == "missing":
            return None
        return SimpleNamespace(id=conversation_id)

    def load_symbolic_projection(self, conversation_id: str):
        self.requested.append(("projection", conversation_id))
        return self.projection


def _projection() -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id="emacs-main",
        projection_generation=9,
        runtime_generation=7,
        turn_id="turn-7",
        outcome="success",
        project_id="dotfiles",
        project_generation=3,
        dialogue_act="inform",
        dialogue_state={"topic": "nix-shell"},
        discourse_entities=[{"id": "file:flake.nix", "kind": "file"}],
        unresolved_questions=[{"id": "q:1", "text": "which profile?"}],
        expert_evidence=[{"expert": "DotfilesExpert", "ref": "evidence:42"}],
        verified_facts=[{"fact": "project uses flakes", "ref": "fact:9"}],
        verified_outcome_refs=["zara.verified-outcome/v1:outcome:turn-7"],
        renderer_provenance="symbolic-dcg/v1",
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
        updated_at="2026-09-20T18:05:00",
    )


def test_status_reads_canonical_symbolic_projection_for_restart_continuity():
    store = _ReplayStore(_projection())

    payload = conversation_symbolic_status(store, " emacs-main ")

    assert payload == {
        "version": "ZARA-SYMBOLIC-REPLAY/1",
        "conversation_id": "emacs-main",
        "symbolic_projection": {
            "projection_generation": 9,
            "runtime_generation": 7,
            "turn_id": "turn-7",
            "outcome": "success",
            "project_id": "dotfiles",
            "project_generation": 3,
            "dialogue_act": "inform",
            "dialogue_state": {"topic": "nix-shell"},
            "discourse_entities": [{"id": "file:flake.nix", "kind": "file"}],
            "unresolved_questions": [{"id": "q:1", "text": "which profile?"}],
            "expert_evidence": [{"expert": "DotfilesExpert", "ref": "evidence:42"}],
            "verified_facts": [{"fact": "project uses flakes", "ref": "fact:9"}],
            "verified_outcome_refs": ["zara.verified-outcome/v1:outcome:turn-7"],
            "renderer_provenance": "symbolic-dcg/v1",
            "providers_enabled": False,
            "max_model_calls": 0,
            "provider_calls": 0,
            "model_calls": 0,
            "updated_at": "2026-09-20T18:05:00",
        },
    }
    assert store.requested == [
        ("conversation", "emacs-main"),
        ("projection", "emacs-main"),
    ]


def test_status_preserves_explicit_absence_of_symbolic_projection():
    store = _ReplayStore(None)

    payload = conversation_symbolic_status(store, "emacs-main")

    assert payload["symbolic_projection"] is None


def test_status_unknown_conversation_fails_closed():
    store = _ReplayStore(None)

    with pytest.raises(ValueError, match="unknown conversation"):
        conversation_symbolic_status(store, "missing")

    assert store.requested == [("conversation", "missing")]


def test_status_rejects_non_symbolic_success_renderer():
    projection = _projection()
    bad = SymbolicConversationProjection(
        **{
            **projection.__dict__,
            "renderer_provenance": "model-fallback/openrouter",
        }
    )
    store = _ReplayStore(bad)

    with pytest.raises(ValueError, match="renderer_provenance"):
        conversation_symbolic_status(store, "emacs-main")
