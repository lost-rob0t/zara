"""
Conversation mode state management.

Tracks when to enter/exit conversation mode and manages conversation history.
"""

import time
from typing import List, Optional
from langchain_core.messages import BaseMessage


class ConversationManager:
    """
    Manages conversation mode state and transitions.

    Handles:
    - Entering/exiting conversation mode
    - Timeout detection for auto-exit
    - Conversation history management
    - Activity tracking
    """

    def __init__(self, timeout_seconds: int = 60):
        """
        Initialize conversation manager.

        Args:
            timeout_seconds: Seconds of inactivity before auto-exiting conversation mode
        """
        self.in_conversation: bool = False
        self.last_activity: Optional[float] = None
        self.timeout_seconds: int = timeout_seconds
        self.conversation_history: List[BaseMessage] = []

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

    def update_activity(self):
        """Update last activity timestamp (call on each user interaction)."""
        self.last_activity = time.time()
