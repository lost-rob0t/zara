from __future__ import annotations

from types import SimpleNamespace

import zara.__main__ as cli
from zara.desktop.conversation.models import MessageRole, MessageStatus


class _ReplayStore:
    def __init__(self, projection) -> None:
        self.projection = projection
        self.requested: list[tuple[str, str]] = []

    def get_conversation(self, conversation_id: str):
        self.requested.append(("conversation", conversation_id))
        return SimpleNamespace(
            id=conversation_id,
            title="Emacs main",
            created_at="2026-09-20T18:00:00",
            updated_at="2026-09-20T18:05:00",
        )

    def load_messages(self, conversation_id: str):
        self.requested.append(("messages", conversation_id))
        return [
            SimpleNamespace(
                sequence=1,
                role=MessageRole.USER,
                content="continue there",
                status=MessageStatus.COMPLETE,
                turn_id="turn-7",
                error="",
                tool_run_id=None,
            )
        ]

    def load_symbolic_projection(self, conversation_id: str):
        self.requested.append(("projection", conversation_id))
        return self.projection


def _projection():
    return SimpleNamespace(
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


def test_replay_includes_canonical_symbolic_projection_for_restart_continuity():
    store = _ReplayStore(_projection())

    payload = cli._conversation_replay_payload(store, "emacs-main")

    assert payload["symbolic_projection"] == {
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
    }
    assert store.requested == [
        ("conversation", "emacs-main"),
        ("messages", "emacs-main"),
        ("projection", "emacs-main"),
    ]


def test_replay_preserves_explicit_absence_of_symbolic_projection():
    store = _ReplayStore(None)

    payload = cli._conversation_replay_payload(store, "emacs-main")

    assert "symbolic_projection" in payload
    assert payload["symbolic_projection"] is None
    assert store.requested[-1] == ("projection", "emacs-main")
