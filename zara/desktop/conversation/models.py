"""Qt-neutral conversation records shared by desktop surfaces."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


class MessageRole(str, Enum):
    USER = "user"
    ASSISTANT = "assistant"
    SYSTEM = "system"
    TOOL = "tool"


class MessageStatus(str, Enum):
    PENDING = "pending"
    STREAMING = "streaming"
    COMPLETE = "complete"
    ERROR = "error"
    CANCELLED = "cancelled"


@dataclass
class ConversationRecord:
    id: str
    title: str
    created_at: str
    updated_at: str
    provider: str = ""
    model: str = ""


@dataclass
class MessageRecord:
    id: str
    conversation_id: str
    sequence: int
    role: MessageRole
    content: str
    status: MessageStatus
    created_at: str
    updated_at: str
    turn_id: Optional[str] = None
    error: str = ""
    tool_run_id: Optional[str] = None


@dataclass
class ConversationState:
    conversation: ConversationRecord
    messages: list[MessageRecord] = field(default_factory=list)
    active_turn_id: Optional[str] = None

    @property
    def provider(self) -> str:
        return self.conversation.provider

    @property
    def model(self) -> str:
        return self.conversation.model

    def message_by_id(self, message_id: str) -> Optional[MessageRecord]:
        for message in self.messages:
            if message.id == message_id:
                return message
        return None

    def latest_message(
        self,
        *,
        role: Optional[MessageRole] = None,
        turn_id: Optional[str] = None,
    ) -> Optional[MessageRecord]:
        for message in reversed(self.messages):
            if role is not None and message.role is not role:
                continue
            if turn_id is not None and message.turn_id != turn_id:
                continue
            return message
        return None


__all__ = [
    "ConversationRecord",
    "ConversationState",
    "MessageRecord",
    "MessageRole",
    "MessageStatus",
]
