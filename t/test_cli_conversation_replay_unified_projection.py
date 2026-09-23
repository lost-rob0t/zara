from __future__ import annotations

from types import SimpleNamespace

import zara.__main__ as cli


class _Projection(SimpleNamespace):
    def validate(self) -> None:
        return None


class _ReplayStore:
    def __init__(self) -> None:
        self.calls: list[str] = []

    def get_conversation(self, conversation_id: str):
        self.calls.append("conversation")
        return SimpleNamespace(
            id=conversation_id,
            title="Emacs main",
            created_at="2026-09-20T18:00:00",
            updated_at="2026-09-20T18:05:00",
        )

    def load_messages(self, conversation_id: str):
        self.calls.append("messages")
        return []

    def load_symbolic_projection(self, conversation_id: str):
        self.calls.append("symbolic_projection")
        return _Projection(
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


def test_conversation_replay_includes_canonical_symbolic_projection():
    store = _ReplayStore()

    payload = cli._conversation_replay_payload(store, " emacs-main ")

    assert payload["conversation"]["id"] == "emacs-main"
    assert payload["messages"] == []
    assert payload["symbolic_projection"]["project_id"] == "dotfiles"
    assert payload["symbolic_projection"]["dialogue_act"] == "inform"
    assert payload["symbolic_projection"]["providers_enabled"] is False
    assert payload["symbolic_projection"]["max_model_calls"] == 0
    assert payload["symbolic_projection"]["provider_calls"] == 0
    assert payload["symbolic_projection"]["model_calls"] == 0
    assert store.calls == ["conversation", "messages", "symbolic_projection"]
