"""Conversation-mode lifecycle state.

Model-context assembly and retention belong to ContextManager. This class owns
only whether conversational mode is active, its inactivity timeout, and a small
read-only compatibility view for callers that still inspect or clear history.
"""

from __future__ import annotations

import time
from collections.abc import Callable, Iterator, Sequence
from typing import Any, Optional


class _ConversationHistoryView(Sequence[Any]):
    """Read-only snapshot-style view with an explicit compatibility clear()."""

    def __init__(self, provider: Callable[[], Sequence[Any]], clear: Callable[[], None]):
        self._provider = provider
        self._clear = clear

    def _snapshot(self) -> tuple[Any, ...]:
        return tuple(self._provider())

    def __len__(self) -> int:
        return len(self._snapshot())

    def __getitem__(self, index):
        return self._snapshot()[index]

    def __iter__(self) -> Iterator[Any]:
        return iter(self._snapshot())

    def clear(self) -> None:
        """Compatibility shim: route destructive clear through ContextManager."""
        self._clear()


class ConversationManager:
    """Manage conversational-mode activity and timeout state."""

    def __init__(
        self,
        timeout_seconds: int = 60,
        *,
        principal=None,
        history_provider: Callable[[], Sequence[Any]] | None = None,
        history_clear: Callable[[], None] | None = None,
    ):
        if principal is not None:
            # Import lazily to avoid a module cycle while still rejecting
            # request-shaped owner objects on the daemon path.
            from zara.server import PrincipalContext

            if not isinstance(principal, PrincipalContext):
                raise TypeError("principal must be a PrincipalContext")

        self.principal = principal
        self.in_conversation: bool = False
        self.last_activity: Optional[float] = None
        self.timeout_seconds: int = timeout_seconds
        self.bind_history(
            history_provider or (lambda: ()),
            history_clear or (lambda: None),
        )

    def bind_history(
        self,
        provider: Callable[[], Sequence[Any]],
        clear: Callable[[], None],
    ) -> None:
        """Bind the read-only compatibility projection to the context owner."""
        self._history_provider = provider
        self._history_clear = clear
        self._history_view = _ConversationHistoryView(provider, clear)

    @property
    def conversation_history(self) -> Sequence[Any]:
        """Compatibility view; callers cannot replace the owned history list."""
        return self._history_view

    def should_enter_conversation(self, prolog_failed: bool, user_input: str) -> bool:
        """Return whether the assistant should enter or remain in conversation mode."""
        if self.in_conversation:
            return True

        if prolog_failed:
            return True

        question_words = [
            "what",
            "why",
            "how",
            "when",
            "who",
            "explain",
            "tell me",
            "can you",
        ]
        user_lower = user_input.lower().strip()
        return any(user_lower.startswith(question) for question in question_words)

    def should_exit_conversation(self) -> bool:
        """Return whether inactivity has exceeded the configured timeout."""
        if not self.in_conversation or self.last_activity is None:
            return False
        return time.time() - self.last_activity > self.timeout_seconds

    def enter_conversation(self) -> None:
        """Enter conversation mode and reset activity timing."""
        self.in_conversation = True
        self.last_activity = time.time()

    def exit_conversation(self) -> None:
        """Exit conversation mode and clear model context through its owner."""
        self.in_conversation = False
        self.last_activity = None
        self._history_clear()

    def update_activity(self, grace_seconds: float = 0.0) -> None:
        """Update the inactivity deadline for the current user interaction."""
        grace = max(0.0, grace_seconds)
        self.last_activity = time.time() + grace
