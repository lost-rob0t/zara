"""SQLite persistence for desktop conversations using Zara's shared database."""

from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Optional

from zara.database import DatabaseManager, get_database
from zara.server import PrincipalContext

from .models import (
    ConversationRecord,
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)

_CONVERSATION_MIGRATION_VERSION = 2
_INTERRUPTED_ERROR = "Interrupted when Zara stopped."
_LEGACY_PRINCIPAL_ID = "__zara_legacy_local_owner__"


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")


class ConversationStore:
    """Durable conversation repository bound to exactly one principal."""

    def __init__(
        self,
        db: Optional[DatabaseManager] = None,
        *,
        principal: Optional[PrincipalContext] = None,
    ) -> None:
        self._db = db or get_database()
        self._principal = principal or PrincipalContext.local_owner()
        if not isinstance(self._principal, PrincipalContext):
            raise TypeError("principal must be a PrincipalContext")
        self._ensure_schema()
        self._ensure_principal_schema()
        self._claim_legacy_rows_for_local_owner()

    @property
    def database(self) -> DatabaseManager:
        return self._db

    @property
    def principal(self) -> PrincipalContext:
        return self._principal

    def _ensure_schema(self) -> None:
        statements = [
            """
            CREATE TABLE IF NOT EXISTS desktop_conversations (
                id TEXT PRIMARY KEY,
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
            # Multiple desktop surfaces may share one DatabaseManager instance.
            # Registration is process-local; the migration itself is idempotent.
            pass
        self._db.connect()

    def _ensure_principal_schema(self) -> None:
        """Upgrade conversation tables without consuming the shared integer migration space."""
        conversation_columns = {
            row["name"] for row in self._db.fetch_all("PRAGMA table_info(desktop_conversations)")
        }
        message_columns = {
            row["name"] for row in self._db.fetch_all("PRAGMA table_info(desktop_messages)")
        }
        with self._db.transaction() as conn:
            if "principal_id" not in conversation_columns:
                conn.execute(
                    "ALTER TABLE desktop_conversations "
                    f"ADD COLUMN principal_id TEXT NOT NULL DEFAULT '{_LEGACY_PRINCIPAL_ID}'"
                )
            if "principal_id" not in message_columns:
                conn.execute(
                    "ALTER TABLE desktop_messages "
                    f"ADD COLUMN principal_id TEXT NOT NULL DEFAULT '{_LEGACY_PRINCIPAL_ID}'"
                )
            conn.execute(
                "CREATE INDEX IF NOT EXISTS idx_desktop_conversations_principal_updated "
                "ON desktop_conversations(principal_id, updated_at DESC)"
            )
            conn.execute(
                "CREATE INDEX IF NOT EXISTS idx_desktop_messages_principal_conversation "
                "ON desktop_messages(principal_id, conversation_id, sequence)"
            )

    def _claim_legacy_rows_for_local_owner(self) -> None:
        if self._principal.kind != "local-owner":
            return
        owner = self._principal.principal_id
        with self._db.transaction() as conn:
            conn.execute(
                "UPDATE desktop_conversations SET principal_id = ? WHERE principal_id = ?",
                (owner, _LEGACY_PRINCIPAL_ID),
            )
            conn.execute(
                "UPDATE desktop_messages SET principal_id = ? WHERE principal_id = ?",
                (owner, _LEGACY_PRINCIPAL_ID),
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
                (id, title, created_at, updated_at, provider, model, principal_id)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                record.id,
                record.title,
                record.created_at,
                record.updated_at,
                record.provider,
                record.model,
                self._principal.principal_id,
            ),
        )
        return record

    def save_conversation(self, record: ConversationRecord) -> None:
        record.updated_at = _now_iso()
        cursor = self._db.execute(
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
                self._principal.principal_id,
            ),
        )
        if cursor.rowcount != 1:
            raise KeyError(record.id)

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
            (conversation_id, self._principal.principal_id),
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
        owner = self._principal.principal_id
        if clean_query:
            pattern = f"%{clean_query}%"
            rows = self._db.fetch_all(
                """
                SELECT DISTINCT c.*
                FROM desktop_conversations AS c
                LEFT JOIN desktop_messages AS m
                    ON m.conversation_id = c.id AND m.principal_id = c.principal_id
                WHERE c.principal_id = ?
                  AND (lower(c.title) LIKE ? OR lower(m.content) LIKE ?)
                ORDER BY c.updated_at DESC
                LIMIT ?
                """,
                (owner, pattern, pattern, limit),
            )
        else:
            rows = self._db.fetch_all(
                """
                SELECT * FROM desktop_conversations
                WHERE principal_id = ?
                ORDER BY updated_at DESC
                LIMIT ?
                """,
                (owner, limit),
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
            FROM desktop_messages
            WHERE conversation_id = ? AND principal_id = ?
            """,
            (conversation_id, self._principal.principal_id),
        )
        return int(row["max_sequence"] if row is not None else 0) + 1

    def save_message(self, message: MessageRecord) -> None:
        if self.get_conversation(message.conversation_id) is None:
            raise KeyError(message.conversation_id)
        message.updated_at = _now_iso()
        owner = self._principal.principal_id
        cursor = self._db.execute(
            """
            INSERT INTO desktop_messages (
                id, conversation_id, sequence, turn_id, role, content, status,
                error, tool_run_id, created_at, updated_at, principal_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(id) DO UPDATE SET
                turn_id = excluded.turn_id,
                role = excluded.role,
                content = excluded.content,
                status = excluded.status,
                error = excluded.error,
                tool_run_id = excluded.tool_run_id,
                updated_at = excluded.updated_at
            WHERE desktop_messages.principal_id = excluded.principal_id
              AND desktop_messages.conversation_id = excluded.conversation_id
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
                owner,
            ),
        )
        if cursor.rowcount != 1:
            raise KeyError(message.id)
        self._db.execute(
            """
            UPDATE desktop_conversations
            SET updated_at = ?
            WHERE id = ? AND principal_id = ?
            """,
            (message.updated_at, message.conversation_id, owner),
        )

    def load_messages(self, conversation_id: str) -> list[MessageRecord]:
        if self.get_conversation(conversation_id) is None:
            return []
        rows = self._db.fetch_all(
            """
            SELECT * FROM desktop_messages
            WHERE conversation_id = ? AND principal_id = ?
            ORDER BY sequence ASC
            """,
            (conversation_id, self._principal.principal_id),
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
        """Load durable history and recover work that cannot still be live.

        Runtime turn ownership is process-local to ``RuntimeHost``. If Zara is
        constructing a fresh ``ConversationState`` from SQLite, a previously
        persisted pending/streaming row cannot represent a live turn in this
        service instance. Mark it interrupted instead of restoring a phantom
        ``active_turn_id`` that would leave Send disabled forever after a crash
        or restart.
        """
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


__all__ = ["ConversationStore"]
