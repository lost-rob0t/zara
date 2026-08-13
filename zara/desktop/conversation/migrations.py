"""Compatibility repairs for desktop conversation persistence."""

from __future__ import annotations

from typing import Optional

from zara.database import DatabaseManager, get_database

_CONVERSATION_MIGRATION_VERSION = 2
_CONVERSATION_REPAIR_MIGRATION_VERSION = 3
_CONVERSATION_TABLES = ("desktop_conversations", "desktop_messages")

_CONVERSATION_SCHEMA_STATEMENTS = [
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


def repair_conversation_schema(db: Optional[DatabaseManager] = None) -> None:
    """Repair databases that recorded migration v2 without its desktop tables.

    Early desktop builds used the shared integer-only migration registry. A
    database can therefore contain a historical ``schema_migrations`` row for
    version 2 while lacking Zara Desktop's conversation tables. In that state
    ``ConversationStore`` correctly sees v2 as applied and SQLite later fails
    with ``no such table: desktop_conversations``.

    Only that inconsistent state triggers the repair. Clean databases still
    receive the canonical v2 migration from ``ConversationStore`` and healthy
    v2 databases are left untouched. The v3 statements are idempotent so a
    partially-created desktop schema is repaired as well.
    """

    database = db or get_database()
    database.connect()

    applied_v2 = database.fetch_one(
        "SELECT 1 FROM schema_migrations WHERE version = ?",
        (_CONVERSATION_MIGRATION_VERSION,),
    )
    if applied_v2 is None:
        return

    missing_table = any(
        database.fetch_one(
            "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?",
            (table_name,),
        )
        is None
        for table_name in _CONVERSATION_TABLES
    )
    if not missing_table:
        return

    try:
        database.register_migration(
            _CONVERSATION_REPAIR_MIGRATION_VERSION,
            _CONVERSATION_SCHEMA_STATEMENTS,
        )
    except ValueError:
        # Another desktop surface may already have registered the repair on
        # this process-global DatabaseManager. Registration is idempotent at
        # the schema level; reconnecting below applies any pending migration.
        pass
    database.connect()


__all__ = ["repair_conversation_schema"]
