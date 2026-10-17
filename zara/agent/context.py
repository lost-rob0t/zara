from __future__ import annotations

import inspect
import copy
import json
import asyncio
import math
import threading
import time
from contextlib import contextmanager
from contextvars import ContextVar, copy_context
import re
from dataclasses import dataclass, replace
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
_PROVIDER_CONTEXT = ContextVar("zara_provider_context", default=None)


@contextmanager
def provider_context_scope(manager, lease):
    token = _PROVIDER_CONTEXT.set((manager, lease))
    try:
        yield
    finally:
        _PROVIDER_CONTEXT.reset(token)


def check_provider_context(messages):
    active = _PROVIDER_CONTEXT.get()
    if active is not None:
        manager, lease = active
        manager._require_active(lease)
        if manager._count(messages) > manager.config.max_tokens:
            raise ContextBudgetError("provider context exceeds configured budget")
        if (not messages or not isinstance(messages[0], SystemMessage)
                or messages[0].id != _CONTEXT_BASE_ID
                or sum(message.id == _CONTEXT_BASE_ID for message in messages) != 1):
            raise ContextError("provider context requires one canonical leading base prompt")
        cleaned = manager._clean_history(messages)
        from langchain_core.messages import ToolMessage
        original_results = [message.tool_call_id for message in messages if isinstance(message, ToolMessage)]
        cleaned_results = [message.tool_call_id for message in cleaned if isinstance(message, ToolMessage)]
        if original_results != cleaned_results:
            raise ContextError("invalid tool-call/result ordering")


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
    reason: str = "build"
    summary_revision: int = 0


@dataclass(frozen=True)
class ContextBuild:
    messages: tuple[BaseMessage, ...]
    token_count: int
    audit: ContextAudit


@dataclass(frozen=True)
class CompactResult:
    changed: bool
    reason: str
    before_tokens: int
    after_tokens: int
    removed_groups: tuple[tuple[str, ...], ...] = ()
    summary_revision: int = 0


class SummaryWork:
    def __init__(self):
        self._lock = threading.Lock()
        self._task = None
        self._sync_pending = False
        self._sync_submitted = False
        self._abandoned = False

    def _busy(self):
        return self._sync_pending or (self._task is not None and not self._task.done())

    @property
    def state(self):
        with self._lock:
            return ("draining" if self._abandoned else "running") if self._busy() else "idle"

    def _call_sync(self, callback, messages, budget, deadline):
        try:
            with self._lock:
                abandoned = self._abandoned
            if abandoned or time.monotonic() >= deadline:
                raise TimeoutError("summary expired before invocation")
            return callback(messages, budget)
        finally:
            with self._lock:
                self._sync_pending = False
                if self._task is not None and self._task.done():
                    self._task = None

    def _completed(self, task):
        if not task.cancelled():
            task.exception()
        with self._lock:
            if self._task is task and task.cancelled():
                self._abandoned = True
                if not self._sync_submitted:
                    self._sync_pending = False
            if self._task is task and not self._sync_pending:
                self._task = None

    def _abandon(self, task, asynchronous):
        with self._lock:
            if self._task is not task:
                return
            already_abandoned = self._abandoned
            self._abandoned = True
        if asynchronous and not already_abandoned and not task.done():
            task.cancel()

    async def run(self, callback, messages, budget, timeout):
        asynchronous = inspect.iscoroutinefunction(callback)
        loop = asyncio.get_running_loop()
        deadline = time.monotonic() + timeout

        async def invoke():
            if asynchronous:
                return await callback(messages, budget)
            context = copy_context()
            try:
                with self._lock:
                    self._sync_submitted = True
                future = loop.run_in_executor(
                    None, context.run, self._call_sync, callback, messages, budget, deadline
                )
            except BaseException:
                with self._lock:
                    self._sync_pending = False
                raise
            # Host cancellation must not cancel an executor job before its
            # finally block can release the retained work slot.
            future.add_done_callback(lambda done: None if done.cancelled() else done.exception())
            result = await asyncio.shield(future)
            return await result if inspect.isawaitable(result) else result

        with self._lock:
            if self._busy():
                raise ContextError("summary backend is busy draining prior work")
            self._sync_pending = not asynchronous
            self._sync_submitted = False
            self._abandoned = False
            task = loop.create_task(invoke(), name="zara-context-summary")
            self._task = task
        task.add_done_callback(self._completed)
        try:
            done, _ = await asyncio.wait({task}, timeout=max(0, deadline - time.monotonic()))
            if task not in done or time.monotonic() >= deadline:
                self._abandon(task, asynchronous)
                raise TimeoutError("summary deadline exceeded")
            return task.result()
        except asyncio.CancelledError:
            self._abandon(task, asynchronous)
            raise


class ContextManager:
    def __init__(
        self,
        *,
        system_prompt: str | Callable[[], str],
        config: ContextConfig | None = None,
        token_counter: Callable[[Sequence[BaseMessage]], int] | None = None,
        summarizer: Callable[[Sequence[BaseMessage], int], Any] | None = None,
        summary_timeout: float = 60.0,
        summary_work: SummaryWork | None = None,
    ):
        if (isinstance(summary_timeout, bool) or not isinstance(summary_timeout, (int, float))
                or not math.isfinite(summary_timeout) or not 0 < summary_timeout <= 300):
            raise ValueError("summary timeout must be positive and bounded")
        self._system_prompt = system_prompt
        self.config = config or ContextConfig()
        self._token_counter = token_counter or _default_token_counter
        self._summarizer = summarizer
        self._summary_timeout = summary_timeout
        self._summary_work = summary_work if summary_work is not None else SummaryWork()
        self._history: list[BaseMessage] = []
        self._generation = 0
        self._active_lease: ContextLease | None = None
        self._audit = ContextAudit()
        self._revision = 0
        self._summary_revision = 0
        self._lease_validator = None
        self._built_lease = None

    @property
    def history(self) -> tuple[BaseMessage, ...]:
        return tuple(copy.deepcopy(self._history))

    @property
    def audit(self) -> ContextAudit:
        return self._audit

    @property
    def busy(self) -> bool:
        return self._active_lease is not None

    @property
    def summary_work_state(self) -> str:
        return self._summary_work.state

    def bind_summary_work(self, work: SummaryWork) -> None:
        if not isinstance(work, SummaryWork):
            raise TypeError("summary work must be a SummaryWork")
        if self._summary_work is not work and self._summary_work.state != "idle":
            raise ContextError("cannot replace busy summary work")
        self._summary_work = work

    def begin_turn(self, turn_id: str) -> ContextLease:
        if not isinstance(turn_id, str) or not turn_id:
            raise ValueError("turn_id must be a non-empty string")
        self._generation += 1
        lease = ContextLease(turn_id=turn_id, generation=self._generation)
        self._active_lease = lease
        self._built_lease = None
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
        revision = self._revision
        staged = self._stage()
        build = await staged._build_messages(
            lease, user_input, transients=transients, skill_context=skill_context,
        )
        self._require_revision(lease, revision)
        self._audit = build.audit
        self._built_lease = lease
        return build

    def _stage(self):
        staged = copy.copy(self)
        staged._history = copy.deepcopy(self._history)
        staged._lease_validator = self._require_active
        return staged

    def _require_revision(self, lease, revision):
        self._require_active(lease)
        if self._revision != revision:
            raise StaleContextTurn("context changed during operation")

    async def compact(self, lease: ContextLease, *, reason: str = "manual") -> CompactResult:
        self._require_active(lease)
        if not isinstance(reason, str) or not reason.strip() or len(reason) > 64:
            raise ValueError("compaction reason must be bounded non-empty text")
        revision = self._revision
        staged = self._stage()
        before = self._count(self._history)
        summary, persistent = _split_summary(staged._history)
        groups = _turn_groups(persistent)
        count = max(0, len(groups) - self.config.preserve_recent_turns)
        removed = tuple(_group_identity(group) for group in groups[:count])
        if count:
            source = ([summary] if summary is not None else []) + [
                message for group in groups[:count] for message in group
            ]
            replacement = await staged._summarize_prefix(lease, source)
            tail = [message for group in groups[count:] for message in group]
            staged._history = [replacement, *tail]
            if staged._count(staged._history) > self.config.max_tokens:
                raise ContextBudgetError("compacted history exceeds context budget")
        self._require_revision(lease, revision)
        after = staged._count(staged._history)
        if count:
            self._history = staged._history
            self._summary_revision += 1
            self._revision += 1
            self._audit = ContextAudit(token_count=after, compressed_groups=removed,
                                       reason=reason, summary_revision=self._summary_revision)
        self._active_lease = None
        return CompactResult(bool(count), reason, before, after, removed, self._summary_revision)

    def replace_history(self, messages: Sequence[BaseMessage]) -> None:
        self._history = self._clean_history(messages)
        self._active_lease = None
        self._generation += 1
        self._revision += 1

    @staticmethod
    def _clean_history(messages):
        from .graph import validate_and_clean_messages
        from langchain_core.messages import AIMessage, ToolMessage

        history = [
            message for message in copy.deepcopy(list(messages))
            if not _is_generated_transient(message)
            and (not isinstance(message, SystemMessage) or message.id == _CONTEXT_SUMMARY_ID)
        ]
        cleaned = validate_and_clean_messages(history)
        for index, message in enumerate(cleaned):
            if isinstance(message, AIMessage) and message.tool_calls:
                expected = [call["id"] for call in message.tool_calls]
                if len(expected) != len(set(expected)) or any(not value for value in expected):
                    raise ContextError("invalid tool-call identifiers")
                actual = []
                for result in cleaned[index + 1:]:
                    if not isinstance(result, ToolMessage):
                        break
                    actual.append(result.tool_call_id)
                if expected != actual:
                    raise ContextError("incomplete tool-call/result group")
        _split_summary(cleaned)
        return cleaned

    async def _build_messages(
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
        if skill_context is not None and self._count([SystemMessage(content=skill_context)]) > self.config.skill_max_tokens:
            raise ContextBudgetError("selected skill context exceeds skill budget")

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
            reason=self.config.strategy,
            summary_revision=self._summary_revision + bool(compressed),
        )
        self._audit = audit
        return ContextBuild(tuple(messages), token_count, audit)

    def commit_result(
        self,
        lease: ContextLease,
        result_messages: Sequence[BaseMessage],
    ) -> None:
        self._require_active(lease)
        history = self._clean_history(result_messages)
        if self._count(history) > 2 * self.config.max_tokens:
            raise ContextBudgetError("completed turn exceeds active history limit")
        old_summary, _ = _split_summary(self._history)
        new_summary, _ = _split_summary(history)
        compressed = self._built_lease == lease and bool(self._audit.compressed_groups)
        if new_summary is not None and (new_summary != old_summary or compressed):
            self._summary_revision += 1
        self._audit = replace(self._audit, summary_revision=self._summary_revision)
        self._history = history
        self._built_lease = None
        self._revision += 1
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
        self._revision += 1

    def _require_active(self, lease: ContextLease) -> None:
        if self._lease_validator is not None:
            self._lease_validator(lease)
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
            summary_message = await self._summarize_prefix(lease, source)

            remaining = [message for current in groups[1:] for message in current]
            self._history = [summary_message, *remaining]
            compressed.append(_group_identity(source))

    async def _summarize_prefix(self, lease, source):
        if self._summarizer is None:
            raise ContextBudgetError("compression requires a summarizer")
        candidate = await self._summary_work.run(
            self._summarizer, tuple(source), self.config.summary_max_tokens, self._summary_timeout,
        )
        self._require_active(lease)
        if not isinstance(candidate, str) or not candidate.strip():
            raise ContextBudgetError("summarizer returned an empty context summary")
        message = SystemMessage(content=candidate.strip(), id=_CONTEXT_SUMMARY_ID)
        if self._count([message]) > self.config.summary_max_tokens:
            raise ContextBudgetError("summarizer exceeded context summary token budget")
        return message

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
        payload = message.model_dump(exclude={"response_metadata", "usage_metadata"})
        rendered = json.dumps(payload, ensure_ascii=False, default=str)
        total += 64 + len(rendered.encode("utf-8"))
    return total
