"""Conversation orchestration and runtime-event reduction for desktop surfaces."""

from __future__ import annotations

import threading
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Optional

from zara.runtime import events
from zara.runtime.commands import CommandReceipt

from .models import (
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)
from .store import ConversationStore


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")


@dataclass(frozen=True)
class ConversationUpdate:
    conversation_id: str
    message_ids: tuple[str, ...] = ()
    metadata_changed: bool = False
    active_turn_changed: bool = False
    full_reload: bool = False


class ConversationService:
    """Own durable conversation state without owning any Qt widgets or LLM client."""

    def __init__(self, store: ConversationStore) -> None:
        self.store = store
        self._lock = threading.RLock()
        self._states: dict[str, ConversationState] = {}
        self._pending_requests: dict[str, tuple[str, str]] = {}
        self._last_active_conversation_id: Optional[str] = None

    def create_conversation(self, title: str = "New chat") -> ConversationState:
        with self._lock:
            record = self.store.create_conversation(title)
            state = ConversationState(conversation=record)
            self._states[record.id] = state
            self._last_active_conversation_id = record.id
            return state

    def get_state(self, conversation_id: str) -> ConversationState:
        with self._lock:
            state = self._states.get(conversation_id)
            if state is None:
                state = self.store.load_state(conversation_id)
                self._states[conversation_id] = state
            return state

    def list_conversations(self, query: str = "", *, limit: int = 100):
        return self.store.list_conversations(query, limit=limit)

    def rename_conversation(self, conversation_id: str, title: str) -> ConversationUpdate:
        with self._lock:
            state = self.get_state(conversation_id)
            record = self.store.rename_conversation(conversation_id, title)
            state.conversation.title = record.title
            state.conversation.updated_at = record.updated_at
            return ConversationUpdate(conversation_id=conversation_id, metadata_changed=True)

    def add_user_message(
        self,
        conversation_id: str,
        text: str,
        *,
        request_id: str,
    ) -> tuple[MessageRecord, ConversationUpdate]:
        clean_text = text.strip()
        if not clean_text:
            raise ValueError("message text must not be empty")
        with self._lock:
            state = self.get_state(conversation_id)
            now = _now_iso()
            message = MessageRecord(
                id=uuid.uuid4().hex,
                conversation_id=conversation_id,
                sequence=self.store.next_sequence(conversation_id),
                role=MessageRole.USER,
                content=clean_text,
                status=MessageStatus.COMPLETE,
                created_at=now,
                updated_at=now,
            )
            state.messages.append(message)
            self.store.save_message(message)
            self._pending_requests[request_id] = (conversation_id, message.id)
            self._last_active_conversation_id = conversation_id

            metadata_changed = False
            if state.conversation.title == "New chat" and self._user_message_count(state) == 1:
                state.conversation.title = self._derive_title(clean_text)
                self.store.save_conversation(state.conversation)
                metadata_changed = True

            return message, ConversationUpdate(
                conversation_id=conversation_id,
                message_ids=(message.id,),
                metadata_changed=metadata_changed,
            )

    def owns_request(self, request_id: str) -> bool:
        with self._lock:
            return request_id in self._pending_requests

    def has_pending_request(self, conversation_id: str) -> bool:
        with self._lock:
            return any(cid == conversation_id for cid, _ in self._pending_requests.values())

    def bind_receipt(self, receipt: CommandReceipt) -> Optional[ConversationUpdate]:
        with self._lock:
            pending = self._pending_requests.pop(receipt.request_id, None)
            if pending is None:
                return None
            conversation_id, message_id = pending
            state = self.get_state(conversation_id)
            message = state.message_by_id(message_id)
            changed_ids: list[str] = []
            active_changed = False
            if message is not None and receipt.turn_id:
                message.turn_id = receipt.turn_id
                self.store.save_message(message)
                changed_ids.append(message.id)
                state.active_turn_id = receipt.turn_id
                active_changed = True
            return ConversationUpdate(
                conversation_id=conversation_id,
                message_ids=tuple(changed_ids),
                active_turn_changed=active_changed,
            )

    def mark_command_failed(self, request_id: str, reason: str) -> Optional[ConversationUpdate]:
        with self._lock:
            pending = self._pending_requests.pop(request_id, None)
            if pending is None:
                return None
            conversation_id, user_message_id = pending
            state = self.get_state(conversation_id)
            user_message = state.message_by_id(user_message_id)
            turn_id = user_message.turn_id if user_message else None
            error_message = self._new_message(
                state,
                role=MessageRole.SYSTEM,
                turn_id=turn_id,
                content="Message could not be sent.",
                status=MessageStatus.ERROR,
                error=reason or "runtime rejected the turn",
            )
            state.active_turn_id = None
            return ConversationUpdate(
                conversation_id=conversation_id,
                message_ids=(error_message.id,),
                active_turn_changed=True,
            )

    def apply_event(self, event: events.RuntimeEvent) -> Optional[ConversationUpdate]:
        with self._lock:
            conversation_id = event.conversation_id or self._last_active_conversation_id

            if isinstance(event, events.ProviderChanged):
                if conversation_id is None:
                    return None
                state = self.get_state(conversation_id)
                state.conversation.provider = event.provider
                state.conversation.model = event.model
                self.store.save_conversation(state.conversation)
                return ConversationUpdate(conversation_id=conversation_id, metadata_changed=True)

            if conversation_id is None:
                return None

            try:
                state = self.get_state(conversation_id)
            except KeyError:
                # Runtime events must not invent durable conversations. The UI
                # creates the conversation before SubmitTurn crosses RuntimeHost.
                return None

            self._last_active_conversation_id = conversation_id

            if isinstance(event, events.TurnStarted):
                changed: list[str] = []
                if event.turn_id:
                    state.active_turn_id = event.turn_id
                    pending_user = self._latest_unbound_user(state)
                    if pending_user is not None:
                        pending_user.turn_id = event.turn_id
                        self.store.save_message(pending_user)
                        changed.append(pending_user.id)
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=tuple(changed),
                    active_turn_changed=bool(event.turn_id),
                )

            if isinstance(event, events.AssistantStarted):
                message = self._ensure_assistant(state, event.turn_id)
                message.status = MessageStatus.STREAMING
                message.error = ""
                self.store.save_message(message)
                if event.turn_id:
                    state.active_turn_id = event.turn_id
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                    active_turn_changed=bool(event.turn_id),
                )

            if isinstance(event, events.AssistantDelta):
                message = self._ensure_assistant(state, event.turn_id)
                if event.text:
                    message.content += event.text
                message.status = MessageStatus.STREAMING
                self.store.save_message(message)
                if event.turn_id:
                    state.active_turn_id = event.turn_id
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                    active_turn_changed=bool(event.turn_id),
                )

            if isinstance(event, events.AssistantComplete):
                message = self._ensure_assistant(state, event.turn_id)
                if event.text:
                    if not message.content or event.text.startswith(message.content):
                        message.content = event.text
                    elif event.text != message.content:
                        # Some providers return only a final tail. Preserve the
                        # deltas already shown and append only the missing tail.
                        message.content += event.text
                message.status = MessageStatus.COMPLETE if event.success else MessageStatus.ERROR
                if not event.success and not message.error:
                    message.error = "assistant generation failed"
                self.store.save_message(message)
                if state.active_turn_id == event.turn_id:
                    state.active_turn_id = None
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                    active_turn_changed=True,
                )

            if isinstance(event, events.ResponseText):
                # RuntimeHost's current buffered fallback emits one complete
                # response. Represent it as one durable content update; never
                # synthesize fake token deltas.
                message = self._ensure_assistant(state, event.turn_id)
                if not message.content or event.text.startswith(message.content):
                    message.content = event.text
                elif event.text != message.content:
                    message.content += event.text
                message.status = MessageStatus.COMPLETE
                self.store.save_message(message)
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                )

            if isinstance(event, (events.AssistantFailed, events.AgentFailed)):
                message = self._ensure_assistant(state, event.turn_id)
                reason = event.reason or "assistant failed"
                message.status = MessageStatus.ERROR
                message.error = reason
                if not message.content:
                    message.content = "Zara could not complete this response."
                self.store.save_message(message)
                if state.active_turn_id == event.turn_id:
                    state.active_turn_id = None
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                    active_turn_changed=True,
                )

            if isinstance(event, events.TurnCancelled):
                message = state.latest_message(role=MessageRole.ASSISTANT, turn_id=event.turn_id)
                changed: tuple[str, ...] = ()
                if message is not None:
                    message.status = MessageStatus.CANCELLED
                    if event.reason:
                        message.error = event.reason
                    self.store.save_message(message)
                    changed = (message.id,)
                if state.active_turn_id == event.turn_id:
                    state.active_turn_id = None
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=changed,
                    active_turn_changed=True,
                )

            if isinstance(event, (events.AgentCompleted, events.OutputReady)):
                if event.turn_id and state.active_turn_id == event.turn_id:
                    state.active_turn_id = None
                    return ConversationUpdate(
                        conversation_id=conversation_id,
                        active_turn_changed=True,
                    )
                return None

            if isinstance(event, (
                events.ToolQueued,
                events.ToolStarted,
                events.ToolProgress,
                events.ToolWaitingForUser,
                events.ToolCompleted,
                events.ToolFailed,
                events.ToolCancelled,
            )):
                message = self._reduce_tool_event(state, event)
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                )

            if isinstance(event, events.RuntimeError) and event.turn_id:
                message = self._new_message(
                    state,
                    role=MessageRole.SYSTEM,
                    turn_id=event.turn_id,
                    content="Runtime error",
                    status=MessageStatus.ERROR,
                    error=event.reason or "runtime error",
                )
                return ConversationUpdate(
                    conversation_id=conversation_id,
                    message_ids=(message.id,),
                )

            return None

    def _ensure_assistant(self, state: ConversationState, turn_id: Optional[str]) -> MessageRecord:
        if turn_id:
            existing = state.latest_message(role=MessageRole.ASSISTANT, turn_id=turn_id)
            if existing is not None:
                return existing
        else:
            latest = state.latest_message(role=MessageRole.ASSISTANT)
            if latest is not None and latest.status in {MessageStatus.PENDING, MessageStatus.STREAMING}:
                return latest
        return self._new_message(
            state,
            role=MessageRole.ASSISTANT,
            turn_id=turn_id,
            content="",
            status=MessageStatus.PENDING,
        )

    def _reduce_tool_event(self, state: ConversationState, event: events.RuntimeEvent) -> MessageRecord:
        tool_run_id = getattr(event, "tool_run_id", None)
        message = None
        if tool_run_id:
            for candidate in reversed(state.messages):
                if candidate.role is MessageRole.TOOL and candidate.tool_run_id == tool_run_id:
                    message = candidate
                    break
        if message is None:
            message = self._new_message(
                state,
                role=MessageRole.TOOL,
                turn_id=event.turn_id,
                content="",
                status=MessageStatus.PENDING,
                tool_run_id=tool_run_id,
            )

        name = getattr(event, "tool_name", None) or "tool"
        if isinstance(event, events.ToolQueued):
            message.content = f"{name}: queued"
            message.status = MessageStatus.PENDING
        elif isinstance(event, events.ToolStarted):
            message.content = f"{name}: running"
            message.status = MessageStatus.STREAMING
        elif isinstance(event, events.ToolProgress):
            detail = event.message.strip()
            message.content = f"{name}: {detail}" if detail else f"{name}: running"
            message.status = MessageStatus.STREAMING
        elif isinstance(event, events.ToolWaitingForUser):
            message.content = f"{name}: waiting for approval"
            message.status = MessageStatus.PENDING
        elif isinstance(event, events.ToolCompleted):
            message.content = f"{name}: completed"
            message.status = MessageStatus.COMPLETE if event.success else MessageStatus.ERROR
        elif isinstance(event, events.ToolFailed):
            message.content = f"{name}: failed"
            message.status = MessageStatus.ERROR
            message.error = event.reason
        elif isinstance(event, events.ToolCancelled):
            message.content = f"{name}: cancelled"
            message.status = MessageStatus.CANCELLED
            message.error = event.reason
        self.store.save_message(message)
        return message

    def _new_message(
        self,
        state: ConversationState,
        *,
        role: MessageRole,
        turn_id: Optional[str],
        content: str,
        status: MessageStatus,
        error: str = "",
        tool_run_id: Optional[str] = None,
    ) -> MessageRecord:
        now = _now_iso()
        message = MessageRecord(
            id=uuid.uuid4().hex,
            conversation_id=state.conversation.id,
            sequence=self.store.next_sequence(state.conversation.id),
            turn_id=turn_id,
            role=role,
            content=content,
            status=status,
            error=error,
            tool_run_id=tool_run_id,
            created_at=now,
            updated_at=now,
        )
        state.messages.append(message)
        self.store.save_message(message)
        return message

    @staticmethod
    def _derive_title(text: str) -> str:
        single_line = " ".join(text.split())
        return single_line if len(single_line) <= 60 else single_line[:57].rstrip() + "…"

    @staticmethod
    def _user_message_count(state: ConversationState) -> int:
        return sum(1 for message in state.messages if message.role is MessageRole.USER)

    @staticmethod
    def _latest_unbound_user(state: ConversationState) -> Optional[MessageRecord]:
        for message in reversed(state.messages):
            if message.role is MessageRole.USER and message.turn_id is None:
                return message
        return None


__all__ = ["ConversationService", "ConversationUpdate"]
