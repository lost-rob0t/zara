"""SQLite persistence for conversations using Zara's portable database ABI."""

from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Optional

from zara.conversation_schema import (
    LEGACY_LOCAL_PRINCIPAL_ID,
    PORTABLE_LOCAL_PRINCIPAL_ID,
    conversation_schema_statements,
)
from zara.database import DatabaseManager, get_database
from zara.server import PrincipalContext

from .models import (
    ConversationRecord,
    ConversationState,
    MessageRecord,
    MessageRole,
    MessageStatus,
)

_INTERRUPTED_ERROR = "Interrupted when Zara stopped."


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")


class ConversationStore:
    """Durable conversation repository bound to exactly one principal.

    Local-owner history uses a platform-neutral storage principal so the same
    SQLite file can move between Linux/macOS/Windows and Android without the
    host OS UID changing ownership of the rows. Authenticated/non-local
    principals remain isolated under their real principal IDs.
    """

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
        self._storage_principal_id = (
            PORTABLE_LOCAL_PRINCIPAL_ID
            if self._principal.kind == "local-owner"
            else self._principal.principal_id
        )
        self._ensure_schema()
        self._claim_legacy_rows_for_local_owner()

    @property
    def database(self) -> DatabaseManager:
        return self._db

    @property
    def principal(self) -> PrincipalContext:
        return self._principal

    @property
    def storage_principal_id(self) -> str:
        """Principal key persisted in the portable conversation tables."""

        return self._storage_principal_id

    def _ensure_schema(self) -> None:
        """Install the conversation ABI without consuming a global migration slot.

        ``DatabaseManager.schema_migrations`` is shared by unrelated Zara
        subsystems, several of which historically used the same integer
        versions. Android also uses SQLite ``user_version`` for its app-local
        database lifecycle. Conversation history therefore owns an idempotent
        table/index ABI instead of claiming a process-global migration number.
        This is what lets a database produced by either platform be opened by
        the other regardless of which unrelated stores were initialized first.
        """

        self._db.connect()
        with self._db.transaction(immediate=True) as conn:
            self._repair_principal_column(conn, "desktop_conversations")
            self._repair_principal_column(conn, "desktop_messages")
            for statement in conversation_schema_statements():
                conn.execute(statement)

    @staticmethod
    def _repair_principal_column(conn, table: str) -> None:
        columns = {
            row["name"] for row in conn.execute(f"PRAGMA table_info({table})")
        }
        if columns and "principal_id" not in columns:
            conn.execute(
                f"ALTER TABLE {table} "
                f"ADD COLUMN principal_id TEXT NOT NULL DEFAULT '{PORTABLE_LOCAL_PRINCIPAL_ID}'"
            )

    def _claim_legacy_rows_for_local_owner(self) -> None:
        if self._principal.kind != "local-owner":
            return
        # Before the portable local-owner key existed, Desktop persisted the
        # host's numeric Unix UID (``uid:<digits>``). A database restored under
        # another local account must keep that local history visible, while
        # authenticated principals such as ``user:alice`` remain untouched.
        legacy_local_predicate = """
            principal_id IN (?, ?)
            OR (
                substr(principal_id, 1, 4) = 'uid:'
                AND length(substr(principal_id, 5)) > 0
                AND substr(principal_id, 5) NOT GLOB '*[^0-9]*'
            )
        """
        parameters = (
            PORTABLE_LOCAL_PRINCIPAL_ID,
            LEGACY_LOCAL_PRINCIPAL_ID,
            self._principal.principal_id,
        )
        with self._db.transaction() as conn:
            conn.execute(
                f"""
                UPDATE desktop_conversations
                SET principal_id = ?
                WHERE {legacy_local_predicate}
                """,
                parameters,
            )
            conn.execute(
                f"""
                UPDATE desktop_messages
                SET principal_id = ?
                WHERE {legacy_local_predicate}
                """,
                parameters,
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
                self._storage_principal_id,
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
                self._storage_principal_id,
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
            (conversation_id, self._storage_principal_id),
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
        owner = self._storage_principal_id
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
            (conversation_id, self._storage_principal_id),
        )
        return int(row["max_sequence"] if row is not None else 0) + 1

    def save_message(self, message: MessageRecord) -> None:
        if self.get_conversation(message.conversation_id) is None:
            raise KeyError(message.conversation_id)
        message.updated_at = _now_iso()
        owner = self._storage_principal_id
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
            (conversation_id, self._storage_principal_id),
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
