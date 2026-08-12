"""Shared conversation state and durable storage for Zara Desktop."""

from .models import (
    ConversationRecord,
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from .service import ConversationService, ConversationUpdate
from .store import ConversationStore

__all__ = [
    "ConversationRecord",
    "ConversationService",
    "ConversationState",
    "ConversationStore",
    "ConversationUpdate",
    "MessageRecord",
    "MessageRole",
    "MessageStatus",
]
