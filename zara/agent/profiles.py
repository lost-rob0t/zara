"""Addressable agent profiles backed by Zara's canonical Prolog runtime."""

from __future__ import annotations

import json
import re
from dataclasses import dataclass
from typing import Optional


_LEADING_MENTION = re.compile(r"^\s*@([A-Za-z0-9_-]+)(?=\s|$)")


@dataclass(frozen=True)
class AgentProfile:
    profile_id: str
    display_name: str
    prompt: str
    tools: Optional[tuple[str, ...]]
    kbs: tuple[str, ...]
    memory_scope: str

    def system_context(self) -> str:
        lines = [
            f"Active agent profile: {self.display_name} ({self.profile_id}).",
        ]
        if self.prompt:
            lines.append(self.prompt)
        if self.kbs:
            lines.append("Enabled knowledge bases: " + ", ".join(self.kbs) + ".")
        if self.tools is not None:
            lines.append("Allowed tools: " + ", ".join(self.tools) + ".")
        lines.append(f"Memory scope: {self.memory_scope}.")
        return "\n".join(lines)


@dataclass(frozen=True)
class SelectedAgentProfile:
    profile: AgentProfile
    request: str


class AgentProfileResolver:
    def __init__(self, prolog_engine):
        self.prolog_engine = prolog_engine

    def resolve(self, user_input: str) -> Optional[SelectedAgentProfile]:
        match = _LEADING_MENTION.match(user_input)
        if match is None:
            return None

        remainder = user_input[match.end():].lstrip()
        if _LEADING_MENTION.match(remainder):
            return None

        mention = f"@{match.group(1)}"
        row = self.prolog_engine.query_once(
            "agent_profiles:resolve_mention("
            f"{json.dumps(mention, ensure_ascii=False)}, "
            "Id, Display, Prompt, Tools, KBs, Scope)"
        )
        if not row:
            return None

        tools = _tools(row.get("Tools"))
        profile = AgentProfile(
            profile_id=_text(row.get("Id")),
            display_name=_text(row.get("Display")),
            prompt=_text(row.get("Prompt")),
            tools=tools,
            kbs=_text_tuple(row.get("KBs")),
            memory_scope=_text(row.get("Scope")) or "shared",
        )
        return SelectedAgentProfile(profile=profile, request=remainder)


def _tools(value) -> Optional[tuple[str, ...]]:
    if _text(value) == "all":
        return None
    return _text_tuple(value)


def _text_tuple(value) -> tuple[str, ...]:
    if value is None:
        return ()
    if isinstance(value, (list, tuple)):
        return tuple(_text(item) for item in value)
    text = _text(value)
    return (text,) if text else ()


def _text(value) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return str(value)
