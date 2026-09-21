"""Desktop-owned adapter between pure-symbolic turns and canonical projection state.

The runtime package stays UI/desktop-neutral. This adapter is injected by the
Desktop composition root and maps bounded pure-symbolic turn metadata onto the
existing ConversationStore projection; it creates no second history/state owner.
"""

from __future__ import annotations

from .symbolic_projection import SymbolicConversationProjection


_CONTEXT_PROJECT_ID = "prolog_context_project_id"
_CONTEXT_PROJECT_GENERATION = "prolog_context_project_generation"
_MAX_EXPERT_EVIDENCE_CHARS = 128


def _dialogue_context_matches_project(projection: SymbolicConversationProjection) -> bool:
    dialogue_state = projection.dialogue_state
    context_project_id = dialogue_state.get(_CONTEXT_PROJECT_ID)
    context_project_generation = dialogue_state.get(_CONTEXT_PROJECT_GENERATION, 0)
    if context_project_id is not None and not isinstance(context_project_id, str):
        return False
    if type(context_project_generation) is not int:
        return False
    return (
        context_project_id == projection.project_id
        and context_project_generation == projection.project_generation
    )


def _bounded_expert_evidence_ref(value: object) -> str:
    if not isinstance(value, str):
        raise TypeError("expert evidence reference must be text")
    if not value or len(value) > _MAX_EXPERT_EVIDENCE_CHARS:
        raise ValueError(
            f"expert evidence reference must be 1..{_MAX_EXPERT_EVIDENCE_CHARS} characters"
        )
    if any(ord(character) < 0x20 or 0x7F <= ord(character) <= 0x9F for character in value):
        raise ValueError("expert evidence reference contains control characters")
    return value


class PureSymbolicProjectionAdapter:
    """Persist pure-symbolic dialogue context in the canonical conversation store."""

    def __init__(self, store) -> None:
        self.store = store

    def load_dialogue_context(self, conversation_id: str) -> tuple[str, int]:
        projection = self.store.load_symbolic_projection(conversation_id)
        if projection is None:
            return "[]", 0
        projection.assert_pure_symbolic()
        if not _dialogue_context_matches_project(projection):
            return "[]", projection.projection_generation
        context = projection.dialogue_state.get("prolog_context_term", "[]")
        if not isinstance(context, str):
            raise TypeError("persisted symbolic dialogue context must be text")
        return context, projection.projection_generation

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
        expert_evidence_ref: str | None = None,
    ) -> None:
        current = self.store.load_symbolic_projection(conversation_id)
        current_generation = current.projection_generation if current is not None else 0
        if current_generation != expected_generation:
            raise RuntimeError(
                "stale symbolic projection result: "
                f"read generation {expected_generation}, current {current_generation}"
            )
        if current is not None:
            current.assert_pure_symbolic()

        project_context_is_current = (
            current is None or _dialogue_context_matches_project(current)
        )
        if project_context_is_current:
            dialogue_state = dict(current.dialogue_state) if current is not None else {}
            prior_questions = list(current.unresolved_questions) if current is not None else []
            discourse_entities = list(current.discourse_entities) if current is not None else []
            expert_evidence = list(current.expert_evidence) if current is not None else []
            verified_facts = list(current.verified_facts) if current is not None else []
        else:
            dialogue_state = {}
            prior_questions = []
            discourse_entities = []
            expert_evidence = []
            verified_facts = []

        if dialogue_act == "expert_answer":
            expert_evidence = [
                {"ref": _bounded_expert_evidence_ref(expert_evidence_ref)}
            ]
        elif expert_evidence_ref is not None:
            raise RuntimeError("non-expert symbolic dialogue returned expert evidence")

        dialogue_state["prolog_context_term"] = context_term
        dialogue_state["response_act_term"] = response_act_term
        dialogue_state[_CONTEXT_PROJECT_ID] = current.project_id if current else None
        dialogue_state[_CONTEXT_PROJECT_GENERATION] = (
            current.project_generation if current else 0
        )

        unresolved_questions = [
            item
            for item in prior_questions
            if item.get("source") != "symbolic_dialogue"
        ]
        if dialogue_act == "clarify":
            unresolved_questions.append(
                {
                    "id": f"turn:{turn_id}:clarification",
                    "text": response,
                    "source": "symbolic_dialogue",
                }
            )

        projection = SymbolicConversationProjection(
            conversation_id=conversation_id,
            projection_generation=expected_generation + 1,
            runtime_generation=(current.runtime_generation if current else 0) + 1,
            turn_id=turn_id,
            outcome="success",
            project_id=current.project_id if current else None,
            project_generation=current.project_generation if current else 0,
            dialogue_act=dialogue_act,
            dialogue_state=dialogue_state,
            discourse_entities=discourse_entities,
            unresolved_questions=unresolved_questions,
            expert_evidence=expert_evidence,
            verified_facts=verified_facts,
            verified_outcome_refs=list(current.verified_outcome_refs) if current else [],
            renderer_provenance=renderer_provenance,
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        )
        stored = self.store.save_symbolic_projection(
            projection,
            expected_generation=expected_generation,
        )
        stored.assert_pure_symbolic()


__all__ = ["PureSymbolicProjectionAdapter"]
