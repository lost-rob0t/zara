"""Read-only canonical symbolic conversation replay/status surface.

This module consumes ``ConversationStore`` and its durable symbolic projection.
It owns no history, runtime, provider, expert registry, scheduler, permission
system, or effect path.  It is intentionally safe for Desktop/Emacs restart
inspection because it performs no provider/model/network fallback.
"""

from __future__ import annotations

import argparse
import json
import sys
from typing import Any

from .store import ConversationStore


SYMBOLIC_REPLAY_STATUS_VERSION = "ZARA-SYMBOLIC-REPLAY/1"
_IDENTIFIER_LIMIT = 128


def _normalize_conversation_id(value: str) -> str:
    if not isinstance(value, str):
        raise TypeError("conversation id must be a string")
    normalized = value.strip()
    if not normalized:
        raise ValueError("conversation id must not be empty")
    if "\x00" in normalized:
        raise ValueError("conversation id must not contain NUL")
    if len(normalized) > _IDENTIFIER_LIMIT:
        raise ValueError(f"conversation id exceeds {_IDENTIFIER_LIMIT} characters")
    return normalized


def symbolic_projection_payload(projection) -> dict[str, Any] | None:
    """Return a strict JSON-ready view of one canonical persisted projection."""
    if projection is None:
        return None
    projection.validate()
    return {
        "projection_generation": projection.projection_generation,
        "runtime_generation": projection.runtime_generation,
        "turn_id": projection.turn_id,
        "outcome": projection.outcome,
        "project_id": projection.project_id,
        "project_generation": projection.project_generation,
        "dialogue_act": projection.dialogue_act,
        "dialogue_state": projection.dialogue_state,
        "discourse_entities": projection.discourse_entities,
        "unresolved_questions": projection.unresolved_questions,
        "expert_evidence": projection.expert_evidence,
        "verified_facts": projection.verified_facts,
        "verified_outcome_refs": projection.verified_outcome_refs,
        "renderer_provenance": projection.renderer_provenance,
        "providers_enabled": projection.providers_enabled,
        "max_model_calls": projection.max_model_calls,
        "provider_calls": projection.provider_calls,
        "model_calls": projection.model_calls,
        "updated_at": projection.updated_at,
    }


def conversation_symbolic_status(store, conversation_id: str) -> dict[str, Any]:
    """Read canonical durable symbolic state for CONVERSATION_ID."""
    normalized = _normalize_conversation_id(conversation_id)
    conversation = store.get_conversation(normalized)
    if conversation is None:
        raise ValueError(f"unknown conversation {normalized!r}")
    projection = store.load_symbolic_projection(normalized)
    return {
        "version": SYMBOLIC_REPLAY_STATUS_VERSION,
        "conversation_id": normalized,
        "symbolic_projection": symbolic_projection_payload(projection),
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="python -m zara.desktop.conversation.replay_status",
        description="Read Zara's canonical durable symbolic conversation status",
    )
    parser.add_argument("--conversation-id", required=True)
    return parser


def main(argv: list[str] | None = None) -> int:
    args = _parser().parse_args(argv)
    try:
        payload = conversation_symbolic_status(
            ConversationStore(),
            args.conversation_id,
        )
        print(json.dumps(payload, sort_keys=True, separators=(",", ":")))
        return 0
    except Exception as error:
        print(f"Error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
