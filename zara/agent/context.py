from __future__ import annotations

import inspect
import re
from dataclasses import dataclass
from typing import Any, Callable, Iterable, Sequence

from langchain_core.messages import BaseMessage, HumanMessage, SystemMessage


_CONTEXT_BASE_ID = "context:base"
_CONTEXT_SUMMARY_ID = "context:summary"
_GENERATED_PREFIXES = (
    _CONTEXT_BASE_ID,
    "context:transient:",
    "context:skills:",
)
_KIND_RE = re.compile(r"[^a-zA-Z0-9_-]+")


class ContextError(RuntimeError):
    pass


class ContextBudgetError(ContextError):
    pass


class StaleContextTurn(ContextError):
    pass


@dataclass(frozen=True)
class ContextConfig:
    strategy: str = "truncate"
    max_tokens: int = 32000
    preserve_recent_turns: int = 8
    summary_max_tokens: int = 2000
    skill_max_tokens: int = 6000

    def __post_init__(self) -> None:
        if self.strategy not in {"truncate", "compress"}:
            raise ValueError("context strategy must be 'truncate' or 'compress'")
        if isinstance(self.max_tokens, bool) or not isinstance(self.max_tokens, int):
            raise ValueError("context max_tokens must be an integer")
        if self.max_tokens < 1:
            raise ValueError("context max_tokens must be positive")
        if (
            isinstance(self.preserve_recent_turns, bool)
            or not isinstance(self.preserve_recent_turns, int)
            or self.preserve_recent_turns < 0
        ):
            raise ValueError("context preserve_recent_turns must be a non-negative integer")
        if (
            isinstance(self.summary_max_tokens, bool)
            or not isinstance(self.summary_max_tokens, int)
            or not 1 <= self.summary_max_tokens <= self.max_tokens
        ):
            raise ValueError("context summary_max_tokens must fit max_tokens")
        if (
            isinstance(self.skill_max_tokens, bool)
            or not isinstance(self.skill_max_tokens, int)
            or not 1 <= self.skill_max_tokens <= self.max_tokens
        ):
            raise ValueError("context skill_max_tokens must fit max_tokens")


@dataclass(frozen=True)
class ContextLease:
    turn_id: str
    generation: int


@dataclass(frozen=True)
class TransientContext:
    kind: str
    text: str


@dataclass(frozen=True)
class ContextAudit:
    token_count: int = 0
    removed_groups: tuple[tuple[str, ...], ...] = ()
    compressed_groups: tuple[tuple[str, ...], ...] = ()
    transient_kinds: tuple[str, ...] = ()
    skill_context_included: bool = False


@dataclass(frozen=True)
class ContextBuild:
    messages: tuple[BaseMessage, ...]
    token_count: int
    audit: ContextAudit


class ContextManager:
    def __init__(
        self,
        *,
        system_prompt: str | Callable[[], str],
        config: ContextConfig | None = None,
        token_counter: Callable[[Sequence[BaseMessage]], int] | None = None,
        summarizer: Callable[[Sequence[BaseMessage], int], Any] | None = None,
    ):
        self._system_prompt = system_prompt
        self.config = config or ContextConfig()
        self._token_counter = token_counter or _default_token_counter
        self._summarizer = summarizer
        self._history: list[BaseMessage] = []
        self._generation = 0
        self._active_lease: ContextLease | None = None
        self._audit = ContextAudit()

    @property
    def history(self) -> tuple[BaseMessage, ...]:
        return tuple(self._history)

    @property
    def audit(self) -> ContextAudit:
        return self._audit

    def begin_turn(self, turn_id: str) -> ContextLease:
        if not isinstance(turn_id, str) or not turn_id:
            raise ValueError("turn_id must be a non-empty string")
        self._generation += 1
        lease = ContextLease(turn_id=turn_id, generation=self._generation)
        self._active_lease = lease
        return lease

    async def build_messages(
        self,
        lease: ContextLease,
        user_input: str,
        *,
        transients: Iterable[TransientContext] = (),
        skill_context: str | None = None,
    ) -> ContextBuild:
        self._require_active(lease)
        transient_items = tuple(
            item for item in transients if isinstance(item.text, str) and item.text.strip()
        )
        if skill_context is not None and not skill_context.strip():
            skill_context = None

        removed: list[tuple[str, ...]] = []
        compressed: list[tuple[str, ...]] = []
        if self.config.strategy == "truncate":
            self._truncate_to_budget(
                lease,
                user_input,
                transient_items,
                skill_context,
                removed,
            )
        else:
            await self._compress_to_budget(
                lease,
                user_input,
                transient_items,
                skill_context,
                compressed,
            )

        self._require_active(lease)
        messages = self._assemble(lease, user_input, transient_items, skill_context)
        token_count = self._count(messages)
        if token_count > self.config.max_tokens:
            raise ContextBudgetError(
                f"context uses {token_count} tokens above max {self.config.max_tokens}"
            )
        audit = ContextAudit(
            token_count=token_count,
            removed_groups=tuple(removed),
            compressed_groups=tuple(compressed),
            transient_kinds=tuple(item.kind for item in transient_items),
            skill_context_included=skill_context is not None,
        )
        self._audit = audit
        return ContextBuild(tuple(messages), token_count, audit)

    def commit_result(
        self,
        lease: ContextLease,
        result_messages: Sequence[BaseMessage],
    ) -> None:
        self._require_active(lease)
        from .graph import validate_and_clean_messages

        cleaned = validate_and_clean_messages(list(result_messages))
        self._history = [
            message for message in cleaned if not _is_generated_transient(message)
        ]
        self._active_lease = None

    def cancel_turn(self, turn_id: str) -> None:
        active = self._active_lease
        if active is not None and active.turn_id == turn_id:
            self._active_lease = None

    def clear(self) -> None:
        self._history.clear()
        self._active_lease = None
        self._generation += 1
        self._audit = ContextAudit()

    def _require_active(self, lease: ContextLease) -> None:
        if self._active_lease != lease:
            raise StaleContextTurn(
                f"turn {lease.turn_id!r} generation {lease.generation} is stale"
            )

    def _assemble(
        self,
        lease: ContextLease,
        user_input: str,
        transients: Sequence[TransientContext],
        skill_context: str | None,
    ) -> list[BaseMessage]:
        summary, persistent = _split_summary(self._history)
        messages: list[BaseMessage] = [
            SystemMessage(content=self._render_system_prompt(), id=_CONTEXT_BASE_ID)
        ]
        if summary is not None:
            messages.append(summary)
        for index, item in enumerate(transients):
            kind = _normalize_kind(item.kind)
            messages.append(
                SystemMessage(
                    content=item.text,
                    id=f"context:transient:{kind}:{lease.generation}:{index}",
                )
            )
        if skill_context is not None:
            messages.append(
                SystemMessage(
                    content=skill_context,
                    id=f"context:skills:{lease.generation}",
                )
            )
        messages.extend(persistent)
        messages.append(
            HumanMessage(
                content=user_input,
                id=f"context:user:{lease.generation}",
            )
        )
        return messages

    def _render_system_prompt(self) -> str:
        value = self._system_prompt() if callable(self._system_prompt) else self._system_prompt
        if not isinstance(value, str) or not value:
            raise ContextError("canonical system prompt must be a non-empty string")
        return value

    def _truncate_to_budget(
        self,
        lease: ContextLease,
        user_input: str,
        transients: Sequence[TransientContext],
        skill_context: str | None,
        removed: list[tuple[str, ...]],
    ) -> None:
        while self._count(self._assemble(lease, user_input, transients, skill_context)) > self.config.max_tokens:
            summary, persistent = _split_summary(self._history)
            groups = _turn_groups(persistent)
            removable_count = max(0, len(groups) - self.config.preserve_recent_turns)
            if removable_count == 0:
                raise ContextBudgetError("protected context cannot fit configured token budget")
            group = groups[0]
            removed.append(_group_identity(group))
            remaining = [message for current in groups[1:] for message in current]
            self._history = ([summary] if summary is not None else []) + remaining

    async def _compress_to_budget(
        self,
        lease: ContextLease,
        user_input: str,
        transients: Sequence[TransientContext],
        skill_context: str | None,
        compressed: list[tuple[str, ...]],
    ) -> None:
        if self._summarizer is None:
            if self._count(self._assemble(lease, user_input, transients, skill_context)) > self.config.max_tokens:
                raise ContextBudgetError("compress strategy requires a summarizer when context exceeds budget")
            return

        while self._count(self._assemble(lease, user_input, transients, skill_context)) > self.config.max_tokens:
            summary, persistent = _split_summary(self._history)
            groups = _turn_groups(persistent)
            removable_count = max(0, len(groups) - self.config.preserve_recent_turns)
            if removable_count == 0:
                raise ContextBudgetError("protected context cannot fit configured token budget")

            group = groups[0]
            source: list[BaseMessage] = []
            if summary is not None:
                source.append(summary)
            source.extend(group)
            candidate = self._summarizer(tuple(source), self.config.summary_max_tokens)
            if inspect.isawaitable(candidate):
                candidate = await candidate
            self._require_active(lease)
            if not isinstance(candidate, str) or not candidate.strip():
                raise ContextBudgetError("summarizer returned an empty context summary")
            summary_message = SystemMessage(
                content=candidate.strip(),
                id=_CONTEXT_SUMMARY_ID,
            )
            if self._count([summary_message]) > self.config.summary_max_tokens:
                raise ContextBudgetError("summarizer exceeded context summary token budget")

            remaining = [message for current in groups[1:] for message in current]
            self._history = [summary_message, *remaining]
            compressed.append(_group_identity(source))

    def _count(self, messages: Sequence[BaseMessage]) -> int:
        value = self._token_counter(messages)
        if isinstance(value, bool) or not isinstance(value, int) or value < 0:
            raise ContextError("token counter must return a non-negative integer")
        return value


def _split_summary(
    history: Sequence[BaseMessage],
) -> tuple[SystemMessage | None, list[BaseMessage]]:
    summary: SystemMessage | None = None
    persistent: list[BaseMessage] = []
    for message in history:
        if getattr(message, "id", None) == _CONTEXT_SUMMARY_ID:
            if summary is not None:
                raise ContextError("active history contains multiple context summaries")
            if not isinstance(message, SystemMessage):
                raise ContextError("context summary must be a system message")
            summary = message
        else:
            persistent.append(message)
    return summary, persistent


def _turn_groups(messages: Sequence[BaseMessage]) -> list[list[BaseMessage]]:
    groups: list[list[BaseMessage]] = []
    current: list[BaseMessage] = []
    for message in messages:
        if isinstance(message, HumanMessage) and current:
            groups.append(current)
            current = []
        current.append(message)
    if current:
        groups.append(current)
    return groups


def _group_identity(messages: Sequence[BaseMessage]) -> tuple[str, ...]:
    return tuple(
        str(getattr(message, "id", None) or f"{type(message).__name__}:{index}")
        for index, message in enumerate(messages)
    )


def _is_generated_transient(message: BaseMessage) -> bool:
    message_id = str(getattr(message, "id", "") or "")
    return any(message_id == prefix or message_id.startswith(prefix) for prefix in _GENERATED_PREFIXES)


def _normalize_kind(kind: str) -> str:
    normalized = _KIND_RE.sub("-", str(kind)).strip("-").lower()
    return normalized or "runtime"


def _default_token_counter(messages: Sequence[BaseMessage]) -> int:
    total = 0
    for message in messages:
        content = getattr(message, "content", "")
        rendered = content if isinstance(content, str) else repr(content)
        total += 4 + max(1, (len(rendered) + 3) // 4)
    return total
