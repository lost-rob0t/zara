"""Provider-neutral typed events for streaming LLM/agent output (ZARA-027).

Every producer (LangGraph agent node, ``zara.llm.LLMClient``) maps its
provider-specific stream into these frozen event shapes. Nothing
provider-specific — SSE frames, raw tool protocol payloads, hidden reasoning,
or provider metadata — may escape into the runtime/UI contract through these
events.

Each stream ends in exactly one terminal event: ``Completed``, ``Cancelled``,
or ``Failed``.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, kw_only=True)
class LLMStreamEvent:
    """Base type for all streaming events."""


@dataclass(frozen=True, kw_only=True)
class TextDelta(LLMStreamEvent):
    text: str = ""


@dataclass(frozen=True, kw_only=True)
class SentenceReady(LLMStreamEvent):
    text: str = ""
    is_final: bool = False


@dataclass(frozen=True, kw_only=True)
class ToolCallStarted(LLMStreamEvent):
    name: str = ""
    id: str = ""


@dataclass(frozen=True, kw_only=True)
class ToolResult(LLMStreamEvent):
    name: str = ""
    id: str = ""


@dataclass(frozen=True, kw_only=True)
class Completed(LLMStreamEvent):
    full_text: str = ""


@dataclass(frozen=True, kw_only=True)
class Cancelled(LLMStreamEvent):
    pass


@dataclass(frozen=True, kw_only=True)
class Failed(LLMStreamEvent):
    error_type: str = ""
