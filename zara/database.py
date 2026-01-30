"""
Shared SQLite database layer for Zarathushtra.
"""

from __future__ import annotations

import logging
import os
import sqlite3
import threading
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional, Sequence

from .config import get_config

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Migration:
    version: int
    statements: Sequence[str]


class DatabaseManager:
    def __init__(self, db_path: Path) -> None:
        self._db_path = db_path
        self._lock = threading.Lock()
        self._connection: Optional[sqlite3.Connection] = None
        self._migrations: dict[int, Migration] = {}

    @property
    def path(self) -> Path:
        return self._db_path

    def register_migration(self, version: int, statements: Sequence[str]) -> None:
        if version in self._migrations:
            raise ValueError(f"Migration {version} already registered")
        self._migrations[version] = Migration(version=version, statements=list(statements))
        if self._connection is not None:
            self._initialize_schema()

    def connect(self) -> sqlite3.Connection:
        if self._connection is None:
            self._db_path.parent.mkdir(parents=True, exist_ok=True)
            self._connection = sqlite3.connect(self._db_path, check_same_thread=False)
            self._connection.row_factory = sqlite3.Row
            self._connection.execute("PRAGMA foreign_keys = ON")
            self._initialize_schema()
        return self._connection

    def close(self) -> None:
        if self._connection is not None:
            self._connection.close()
            self._connection = None

    def execute(self, statement: str, params: Sequence[object] | None = None) -> sqlite3.Cursor:
        if params is None:
            params = []
        with self._lock:
            conn = self.connect()
            cursor = conn.execute(statement, params)
            conn.commit()
            return cursor

    def fetch_all(self, statement: str, params: Sequence[object] | None = None) -> list[sqlite3.Row]:
        cursor = self.execute(statement, params)
        return cursor.fetchall()

    def fetch_one(self, statement: str, params: Sequence[object] | None = None) -> Optional[sqlite3.Row]:
        cursor = self.execute(statement, params)
        return cursor.fetchone()

    def _initialize_schema(self) -> None:
        conn = self._connection
        if conn is None:
            return
        with self._lock:
            conn.execute(
                """
                CREATE TABLE IF NOT EXISTS schema_migrations (
                    version INTEGER PRIMARY KEY,
                    applied_at INTEGER NOT NULL
                )
                """
            )
            conn.commit()
            applied = {
                row["version"]
                for row in conn.execute("SELECT version FROM schema_migrations")
            }
            for version in sorted(self._migrations):
                if version in applied:
                    continue
                migration = self._migrations[version]
                for statement in migration.statements:
                    conn.execute(statement)
                conn.execute(
                    "INSERT INTO schema_migrations (version, applied_at) VALUES (?, strftime('%s','now'))",
                    (version,),
                )
                conn.commit()


def resolve_db_path() -> Path:
    config = get_config()
    db_config = config.get_section("database")
    raw_path = db_config.get("path", "~/.local/share/zarathushtra/zara.db")
    expanded = os.path.expanduser(os.path.expandvars(str(raw_path)))
    return Path(expanded)


_global_db: Optional[DatabaseManager] = None


def get_database() -> DatabaseManager:
    global _global_db
    if _global_db is None:
        _global_db = DatabaseManager(resolve_db_path())
    return _global_db


def initialize_database() -> DatabaseManager:
    db = get_database()
    db.connect()
    return db


initialize_database()


__all__ = [
    "DatabaseManager",
    "Migration",
    "get_database",
    "initialize_database",
    "resolve_db_path",
]
