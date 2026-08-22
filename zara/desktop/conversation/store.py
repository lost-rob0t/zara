"""SQLite persistence for principal-scoped desktop conversations."""

from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Optional

from zara.database import DatabaseManager, get_database

from .models import (
    ConversationRecord,
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)

_CONVERSATION_MIGRATION_VERSION = 2
_INTERRUPTED_ERROR = "Interrupted when Zara stopped."
LOCAL_PRINCIPAL_ID = "local-owner"


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")


def _require_principal_id(principal_id: str) -> str:
    if not isinstance(principal_id, str) or not principal_id.strip():
        raise ValueError("principal_id must be a non-empty string")
    if principal_id != principal_id.strip():
        raise ValueError("principal_id must not contain leading or trailing whitespace")
    return principal_id


class ConversationStore:
    """Durable conversation repository sealed to one principal."""

    def __init__(
        self,
        db: Optional[DatabaseManager] = None,
        *,
        principal_id: str = LOCAL_PRINCIPAL_ID,
    ) -> None:
        self._db = db or get_database()
        self.principal_id = _require_principal_id(principal_id)
        self._ensure_schema()

    @property
    def database(self) -> DatabaseManager:
        return self._db

    def _ensure_schema(self) -> None:
        statements = [
            """
            CREATE TABLE IF NOT EXISTS desktop_conversations (
                id TEXT PRIMARY KEY,
                principal_id TEXT NOT NULL DEFAULT 'local-owner',
                title TEXT NOT NULL,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                provider TEXT NOT NULL DEFAULT '',
                model TEXT NOT NULL DEFAULT ''
            )
            """,
            """
            CREATE TABLE IF NOT EXISTS desktop_messages (
                id TEXT PRIMARY KEY,
                conversation_id TEXT NOT NULL,
                sequence INTEGER NOT NULL,
                turn_id TEXT,
                role TEXT NOT NULL,
                content TEXT NOT NULL,
                status TEXT NOT NULL,
                error TEXT NOT NULL DEFAULT '',
                tool_run_id TEXT,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                FOREIGN KEY(conversation_id)
                    REFERENCES desktop_conversations(id) ON DELETE CASCADE,
                UNIQUE(conversation_id, sequence)
            )
            """,
            "CREATE INDEX IF NOT EXISTS idx_desktop_conversations_updated ON desktop_conversations(updated_at DESC)",
            "CREATE INDEX IF NOT EXISTS idx_desktop_messages_conversation ON desktop_messages(conversation_id, sequence)",
            "CREATE INDEX IF NOT EXISTS idx_desktop_messages_turn ON desktop_messages(conversation_id, turn_id)",
            "CREATE INDEX IF NOT EXISTS idx_desktop_messages_tool_run ON desktop_messages(conversation_id, tool_run_id)",
        ]
        try:
            self._db.register_migration(_CONVERSATION_MIGRATION_VERSION, statements)
        except ValueError:
            pass
        self._db.connect()
        self._repair_principal_schema()

    def _repair_principal_schema(self) -> None:
        """Repair pre-#131 databases without consuming another global version."""
        columns = {
            row["name"] for row in self._db.fetch_all("PRAGMA table_info(desktop_conversations)")
        }
        if "principal_id" not in columns:
            self._db.execute(
                "ALTER TABLE desktop_conversations "
                "ADD COLUMN principal_id TEXT NOT NULL DEFAULT 'local-owner'"
            )
        self._db.execute(
            "CREATE INDEX IF NOT EXISTS idx_desktop_conversations_principal_updated "
            "ON desktop_conversations(principal_id, updated_at DESC)"
        )

    def create_conversation(
        self,
        title: str = "New chat",
        *,
        conversation_id: Optional[str] = None,
    ) -> ConversationRecord:
        clean_title = title.strip() or "New chat"
        now = _now_iso()
        record = ConversationRecord(
            id=conversation_id or uuid.uuid4().hex,
            title=clean_title,
            created_at=now,
            updated_at=now,
        )
        self._db.execute(
            """
            INSERT INTO desktop_conversations
                (id, principal_id, title, created_at, updated_at, provider, model)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                record.id,
                self.principal_id,
                record.title,
                record.created_at,
                record.updated_at,
                record.provider,
                record.model,
            ),
        )
        return record

    def save_conversation(self, record: ConversationRecord) -> None:
        if self.get_conversation(record.id) is None:
            raise KeyError(record.id)
        record.updated_at = _now_iso()
        self._db.execute(
            """
            UPDATE desktop_conversations
            SET title = ?, updated_at = ?, provider = ?, model = ?
            WHERE id = ? AND principal_id = ?
            """,
            (
                record.title,
                record.updated_at,
                record.provider,
                record.model,
                record.id,
                self.principal_id,
            ),
        )

    def rename_conversation(self, conversation_id: str, title: str) -> ConversationRecord:
        clean_title = title.strip()
        if not clean_title:
            raise ValueError("conversation title must not be empty")
        record = self.get_conversation(conversation_id)
        if record is None:
            raise KeyError(conversation_id)
        record.title = clean_title
        self.save_conversation(record)
        return record

    def get_conversation(self, conversation_id: str) -> Optional[ConversationRecord]:
        row = self._db.fetch_one(
            "SELECT * FROM desktop_conversations WHERE id = ? AND principal_id = ?",
            (conversation_id, self.principal_id),
        )
        if row is None:
            return None
        return ConversationRecord(
            id=row["id"],
            title=row["title"],
            created_at=row["created_at"],
            updated_at=row["updated_at"],
            provider=row["provider"],
            model=row["model"],
        )

    def list_conversations(self, query: str = "", *, limit: int = 100) -> list[ConversationRecord]:
        if limit < 1:
            raise ValueError("limit must be >= 1")
        clean_query = query.strip().lower()
        if clean_query:
            pattern = f"%{clean_query}%"
            rows = self._db.fetch_all(
                """
                SELECT DISTINCT c.*
                FROM desktop_conversations AS c
                LEFT JOIN desktop_messages AS m ON m.conversation_id = c.id
                WHERE c.principal_id = ?
                  AND (lower(c.title) LIKE ? OR lower(m.content) LIKE ?)
                ORDER BY c.updated_at DESC
                LIMIT ?
                """,
                (self.principal_id, pattern, pattern, limit),
            )
        else:
            rows = self._db.fetch_all(
                """
                SELECT * FROM desktop_conversations
                WHERE principal_id = ?
                ORDER BY updated_at DESC
                LIMIT ?
                """,
                (self.principal_id, limit),
            )
        return [
            ConversationRecord(
                id=row["id"],
                title=row["title"],
                created_at=row["created_at"],
                updated_at=row["updated_at"],
                provider=row["provider"],
                model=row["model"],
            )
            for row in rows
        ]

    def next_sequence(self, conversation_id: str) -> int:
        if self.get_conversation(conversation_id) is None:
            raise KeyError(conversation_id)
        row = self._db.fetch_one(
            """
            SELECT COALESCE(MAX(sequence), 0) AS max_sequence
            FROM desktop_messages WHERE conversation_id = ?
            """,
            (conversation_id,),
        )
        return int(row["max_sequence"] if row is not None else 0) + 1

    def save_message(self, message: MessageRecord) -> None:
        if self.get_conversation(message.conversation_id) is None:
            raise KeyError(message.conversation_id)
        existing = self._db.fetch_one(
            """
            SELECT c.principal_id
            FROM desktop_messages AS m
            JOIN desktop_conversations AS c ON c.id = m.conversation_id
            WHERE m.id = ?
            """,
            (message.id,),
        )
        if existing is not None and existing["principal_id"] != self.principal_id:
            raise KeyError(message.id)

        message.updated_at = _now_iso()
        self._db.execute(
            """
            INSERT INTO desktop_messages (
                id, conversation_id, sequence, turn_id, role, content, status,
                error, tool_run_id, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(id) DO UPDATE SET
                turn_id = excluded.turn_id,
                role = excluded.role,
                content = excluded.content,
                status = excluded.status,
                error = excluded.error,
                tool_run_id = excluded.tool_run_id,
                updated_at = excluded.updated_at
            """,
            (
                message.id,
                message.conversation_id,
                message.sequence,
                message.turn_id,
                message.role.value,
                message.content,
                message.status.value,
                message.error,
                message.tool_run_id,
                message.created_at,
                message.updated_at,
            ),
        )
        self._db.execute(
            """
            UPDATE desktop_conversations SET updated_at = ?
            WHERE id = ? AND principal_id = ?
            """,
            (message.updated_at, message.conversation_id, self.principal_id),
        )

    def load_messages(self, conversation_id: str) -> list[MessageRecord]:
        if self.get_conversation(conversation_id) is None:
            return []
        rows = self._db.fetch_all(
            """
            SELECT * FROM desktop_messages
            WHERE conversation_id = ?
            ORDER BY sequence ASC
            """,
            (conversation_id,),
        )
        return [
            MessageRecord(
                id=row["id"],
                conversation_id=row["conversation_id"],
                sequence=int(row["sequence"]),
                turn_id=row["turn_id"],
                role=MessageRole(row["role"]),
                content=row["content"],
                status=MessageStatus(row["status"]),
                error=row["error"],
                tool_run_id=row["tool_run_id"],
                created_at=row["created_at"],
                updated_at=row["updated_at"],
            )
            for row in rows
        ]

    def load_state(self, conversation_id: str) -> ConversationState:
        conversation = self.get_conversation(conversation_id)
        if conversation is None:
            raise KeyError(conversation_id)
        messages = self.load_messages(conversation_id)
        for message in messages:
            if message.status in {MessageStatus.PENDING, MessageStatus.STREAMING}:
                message.status = MessageStatus.CANCELLED
                if not message.error:
                    message.error = _INTERRUPTED_ERROR
                self.save_message(message)
        return ConversationState(
            conversation=conversation,
            messages=messages,
            active_turn_id=None,
        )


__all__ = ["ConversationStore", "LOCAL_PRINCIPAL_ID"]
