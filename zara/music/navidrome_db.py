"""Read-only live Navidrome catalog access for Zara Music.

Bulk imports use SQLite's online backup API to obtain a consistent snapshot
without copying a live WAL database by hand. A persistent read-only connection
can watch PRAGMA data_version for external commits and trigger bounded
reconciliation work.
"""

from __future__ import annotations

import sqlite3
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator


MAX_MEDIA_BATCH = 4096

_REQUIRED_MEDIA_COLUMNS = frozenset(
    {
        "id",
        "library_id",
        "path",
        "title",
        "album",
        "artist",
        "duration",
        "size",
    }
)

_KNOWN_MEDIA_COLUMNS = (
    "id",
    "library_id",
    "path",
    "title",
    "album",
    "artist",
    "duration",
    "size",
    "updated_at",
    "created_at",
    "album_id",
    "artist_id",
    "genre",
    "year",
    "suffix",
    "bit_rate",
    "bit_depth",
    "sample_rate",
    "channels",
    "mbz_recording_id",
    "mbz_album_id",
    "mbz_artist_id",
    "mbz_album_artist_id",
)


class NavidromeSchemaError(RuntimeError):
    """The live Navidrome database does not satisfy the reviewed source shape."""


@dataclass(frozen=True)
class NavidromeSchemaInfo:
    migration_version: int
    journal_mode: str
    media_columns: frozenset[str]


@dataclass(frozen=True)
class NavidromeSnapshot:
    path: Path
    migration_version: int


class NavidromeLiveReader:
    """Bounded, read-only adapter over a Navidrome SQLite database."""

    def __init__(self, database: str | Path) -> None:
        self._path = Path(database).expanduser().resolve()
        if not self._path.is_file():
            raise FileNotFoundError(self._path)
        self._connection = self._open_readonly(self._path)
        try:
            self._schema = self._read_schema_info()
        except Exception:
            self._connection.close()
            raise

    def __enter__(self) -> "NavidromeLiveReader":
        return self

    def __exit__(self, exc_type, exc, traceback) -> None:
        self.close()

    @property
    def path(self) -> Path:
        return self._path

    @property
    def connection(self) -> sqlite3.Connection:
        return self._connection

    def close(self) -> None:
        self._connection.close()

    def schema_info(self) -> NavidromeSchemaInfo:
        return self._schema

    def data_version(self) -> int:
        row = self._connection.execute("PRAGMA data_version").fetchone()
        if row is None:
            raise NavidromeSchemaError("Navidrome database did not return data_version")
        return int(row[0])

    def create_snapshot(self, destination: str | Path) -> NavidromeSnapshot:
        """Create a consistent SQLite snapshot from the live read connection."""

        target = Path(destination).expanduser().resolve()
        if target.exists():
            raise FileExistsError(target)
        target.parent.mkdir(parents=True, exist_ok=True)

        output = sqlite3.connect(target)
        try:
            self._connection.backup(output, pages=2048, sleep=0.01)
            output.commit()
        except Exception:
            output.close()
            target.unlink(missing_ok=True)
            raise
        else:
            output.close()

        return NavidromeSnapshot(
            path=target,
            migration_version=self._schema.migration_version,
        )

    def iter_media_batches(
        self,
        *,
        batch_size: int = 1000,
        after_id: str | None = None,
    ) -> Iterator[tuple[dict[str, object], ...]]:
        """Yield bounded media rows using primary-key keyset pagination."""

        if isinstance(batch_size, bool) or not isinstance(batch_size, int):
            raise ValueError("batch_size must be an integer")
        if batch_size < 1 or batch_size > MAX_MEDIA_BATCH:
            raise ValueError(
                f"batch_size must be between 1 and {MAX_MEDIA_BATCH}"
            )
        if after_id is not None and (not isinstance(after_id, str) or not after_id):
            raise ValueError("after_id must be a non-empty string or None")

        columns = tuple(
            column
            for column in _KNOWN_MEDIA_COLUMNS
            if column in self._schema.media_columns
        )
        projection = ", ".join(f'"{column}"' for column in columns)
        cursor = after_id

        while True:
            if cursor is None:
                rows = self._connection.execute(
                    f'SELECT {projection} FROM media_file ORDER BY id LIMIT ?',
                    (batch_size,),
                ).fetchall()
            else:
                rows = self._connection.execute(
                    f'SELECT {projection} FROM media_file '
                    'WHERE id > ? ORDER BY id LIMIT ?',
                    (cursor, batch_size),
                ).fetchall()

            if not rows:
                return

            batch = tuple({column: row[column] for column in columns} for row in rows)
            yield batch
            cursor = str(rows[-1]["id"])

    @staticmethod
    def _open_readonly(path: Path) -> sqlite3.Connection:
        uri = f"{path.as_uri()}?mode=ro"
        connection = sqlite3.connect(
            uri,
            uri=True,
            check_same_thread=False,
            timeout=15.0,
        )
        connection.row_factory = sqlite3.Row
        connection.execute("PRAGMA query_only = ON")
        connection.execute("PRAGMA busy_timeout = 15000")
        return connection

    def _read_schema_info(self) -> NavidromeSchemaInfo:
        migration_row = self._connection.execute(
            """
            SELECT MAX(version_id)
            FROM goose_db_version
            WHERE is_applied = 1
            """
        ).fetchone()
        if migration_row is None or migration_row[0] is None:
            raise NavidromeSchemaError(
                "Navidrome goose migration version is unavailable"
            )

        rows = self._connection.execute("PRAGMA table_info(media_file)").fetchall()
        columns = frozenset(str(row["name"]) for row in rows)
        missing = sorted(_REQUIRED_MEDIA_COLUMNS - columns)
        if missing:
            raise NavidromeSchemaError(
                "Navidrome media_file schema is missing required columns: "
                + ", ".join(missing)
            )

        journal_row = self._connection.execute("PRAGMA journal_mode").fetchone()
        if journal_row is None:
            raise NavidromeSchemaError("Navidrome journal mode is unavailable")

        return NavidromeSchemaInfo(
            migration_version=int(migration_row[0]),
            journal_mode=str(journal_row[0]),
            media_columns=columns,
        )


__all__ = [
    "MAX_MEDIA_BATCH",
    "NavidromeLiveReader",
    "NavidromeSchemaError",
    "NavidromeSchemaInfo",
    "NavidromeSnapshot",
]
