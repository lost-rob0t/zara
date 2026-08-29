"""Shared conversation state and durable storage for Zara Desktop."""

from typing import Optional

from zara.database import DatabaseManager
from zara.server import PrincipalContext

from .migrations import repair_conversation_schema
from .models import (
    ConversationRecord,
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from .store import ConversationStore as _ConversationStore


class ConversationStore(_ConversationStore):
    """Conversation store with compatibility repair for legacy v2 databases."""

    def __init__(
        self,
        db: Optional[DatabaseManager] = None,
        *,
        principal: Optional[PrincipalContext] = None,
    ) -> None:
        repair_conversation_schema(db)
        super().__init__(db, principal=principal)


from .service import ConversationService, ConversationUpdate  # noqa: E402

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
