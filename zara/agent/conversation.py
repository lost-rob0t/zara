"""
Conversation mode state management.

Tracks when to enter/exit conversation mode and manages conversation history.
"""

import time
from collections.abc import Sequence
from typing import List, Optional
from langchain_core.messages import BaseMessage


class _HistoryView(Sequence):
    def __init__(self, provider, clear):
        self._provider = provider
        self._clear = clear

    def __len__(self):
        return len(self._provider())

    def __getitem__(self, index):
        return self._provider()[index]

    def __iter__(self):
        return iter(self._provider())

    def __eq__(self, other):
        return list(self) == list(other)

    def copy(self):
        return list(self)

    def clear(self):
        self._clear()


class ConversationManager:
    """
    Manages conversation mode state and transitions.

    Handles:
    - Entering/exiting conversation mode
    - Timeout detection for auto-exit
    - Conversation history management
    - Activity tracking
    """

    def __init__(self, timeout_seconds: int = 60, *, principal=None,
                 history_provider=None, history_clear=None, history_replace=None):
        """
        Initialize conversation manager.

        Args:
            timeout_seconds: Seconds of inactivity before auto-exiting conversation mode
            principal: Optional canonical server-side owner for daemon conversation state
        """
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
        self._history: List[BaseMessage] = []
        self._history_provider = None
        self._history_replace = None
        if history_provider is not None:
            self.bind_history(history_provider, history_clear, history_replace)

    def bind_history(self, provider, clear, replace=None):
        self._history_provider = provider
        self._history_replace = replace
        self._history_view = _HistoryView(provider, clear)
        self._history = []

    @property
    def conversation_history(self):
        return self._history_view if self._history_provider is not None else self._history

    @conversation_history.setter
    def conversation_history(self, messages):
        if self._history_provider is not None:
            if self._history_replace is None:
                raise AttributeError("replace history through ContextManager")
            self._history_replace(messages)
        else:
            self._history = list(messages)

    def should_enter_conversation(self, prolog_failed: bool, user_input: str) -> bool:
        """
        Decide if we should enter conversation mode.

        Enter conversation mode if:
        1. Already in conversation (stay in it)
        2. Prolog resolution failed
        3. User input looks conversational (starts with question words)

        Args:
            prolog_failed: Whether Prolog command resolution failed
            user_input: User's input text

        Returns:
            True if should enter/continue conversation mode
        """
        # Already in conversation
        if self.in_conversation:
            return True

        # Prolog failed
        if prolog_failed:
            return True

        # User asks conversational question (heuristic)
        question_words = ["what", "why", "how", "when", "who", "explain", "tell me", "can you"]
        user_lower = user_input.lower().strip()
        if any(user_lower.startswith(q) for q in question_words):
            return True

        return False

    def should_exit_conversation(self) -> bool:
        """
        Check if conversation should exit due to timeout.

        Returns:
            True if conversation has been inactive too long
        """
        if not self.in_conversation:
            return False

        if self.last_activity is None:
            return False

        elapsed = time.time() - self.last_activity
        return elapsed > self.timeout_seconds

    def enter_conversation(self):
        """Enter conversation mode and reset activity timer."""
        self.in_conversation = True
        self.last_activity = time.time()

    def exit_conversation(self):
        """Exit conversation mode and clear history."""
        self.in_conversation = False
        self.last_activity = None
        self.conversation_history.clear()

    def update_activity(self, grace_seconds: float = 0.0):
        """Update last activity timestamp (call on each user interaction)."""
        grace = max(0.0, grace_seconds)
        self.last_activity = time.time() + grace
