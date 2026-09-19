from __future__ import annotations

import sqlite3
from pathlib import Path

import pytest

from zara.music.navidrome_db import (
    MAX_MEDIA_BATCH,
    NavidromeLiveReader,
    NavidromeSchemaError,
)


def build_navidrome_db(path: Path, *, omit: str | None = None) -> None:
    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA journal_mode=WAL")
        connection.execute(
            """
            CREATE TABLE goose_db_version (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                version_id INTEGER NOT NULL,
                is_applied INTEGER NOT NULL,
                tstamp TEXT
            )
            """
        )
        connection.execute(
            "INSERT INTO goose_db_version(version_id, is_applied) VALUES (?, 1)",
            (20260703013908,),
        )
        columns = [
            ("id", "TEXT PRIMARY KEY"),
            ("library_id", "TEXT NOT NULL"),
            ("path", "TEXT NOT NULL"),
            ("title", "TEXT NOT NULL"),
            ("album", "TEXT NOT NULL"),
            ("artist", "TEXT NOT NULL"),
            ("duration", "REAL NOT NULL"),
            ("size", "INTEGER NOT NULL"),
            ("updated_at", "TEXT"),
            ("mbz_recording_id", "TEXT"),
        ]
        selected = [item for item in columns if item[0] != omit]
        connection.execute(
            "CREATE TABLE media_file ("
            + ", ".join(f"{name} {definition}" for name, definition in selected)
            + ")"
        )
        connection.executemany(
            """
            INSERT INTO media_file(
                id, library_id, path, title, album, artist, duration, size,
                updated_at, mbz_recording_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
                (
                    "a-track",
                    "library-1",
                    "Artist/Album/01.flac",
                    "One",
                    "Album",
                    "Artist",
                    180.5,
                    1000,
                    "2026-09-18T20:00:00Z",
                    "mbid-1",
                ),
                (
                    "b-track",
                    "library-1",
                    "Artist/Album/02.flac",
                    "Two",
                    "Album",
                    "Artist",
                    181.0,
                    1001,
                    "2026-09-18T20:01:00Z",
                    None,
                ),
            ],
        )
        connection.commit()
    finally:
        connection.close()


def test_live_reader_is_read_only_and_exposes_schema_version(tmp_path):
    database = tmp_path / "navidrome.db"
    build_navidrome_db(database)

    with NavidromeLiveReader(database) as reader:
        schema = reader.schema_info()

        assert schema.migration_version == 20260703013908
        assert schema.journal_mode.lower() == "wal"
        assert "mbz_recording_id" in schema.media_columns
        with pytest.raises(sqlite3.OperationalError):
            reader.connection.execute(
                "INSERT INTO media_file(id) VALUES ('must-not-write')"
            )


def test_live_data_version_changes_after_external_commit(tmp_path):
    database = tmp_path / "navidrome.db"
    build_navidrome_db(database)

    with NavidromeLiveReader(database) as reader:
        before = reader.data_version()

        writer = sqlite3.connect(database)
        try:
            writer.execute(
                "UPDATE media_file SET title = ? WHERE id = ?",
                ("Changed", "a-track"),
            )
            writer.commit()
        finally:
            writer.close()

        assert reader.data_version() != before


def test_online_backup_produces_consistent_scan_source(tmp_path):
    database = tmp_path / "navidrome.db"
    snapshot = tmp_path / "snapshot.db"
    build_navidrome_db(database)

    with NavidromeLiveReader(database) as reader:
        result = reader.create_snapshot(snapshot)

    assert result.path == snapshot
    assert result.migration_version == 20260703013908

    writer = sqlite3.connect(database)
    try:
        writer.execute(
            "UPDATE media_file SET title = ? WHERE id = ?",
            ("After snapshot", "a-track"),
        )
        writer.commit()
    finally:
        writer.close()

    with NavidromeLiveReader(snapshot) as reader:
        rows = [row for batch in reader.iter_media_batches(batch_size=1) for row in batch]

    assert [row["id"] for row in rows] == ["a-track", "b-track"]
    assert rows[0]["title"] == "One"


def test_media_batches_are_bounded_and_keyset_ordered(tmp_path):
    database = tmp_path / "navidrome.db"
    build_navidrome_db(database)

    with NavidromeLiveReader(database) as reader:
        batches = list(reader.iter_media_batches(batch_size=1))

    assert [[row["id"] for row in batch] for batch in batches] == [
        ["a-track"],
        ["b-track"],
    ]

    with NavidromeLiveReader(database) as reader:
        with pytest.raises(ValueError, match="batch_size"):
            list(reader.iter_media_batches(batch_size=MAX_MEDIA_BATCH + 1))


def test_schema_guard_fails_closed_when_required_column_is_missing(tmp_path):
    database = tmp_path / "navidrome.db"

    connection = sqlite3.connect(database)
    try:
        connection.execute("PRAGMA journal_mode=WAL")
        connection.execute(
            "CREATE TABLE goose_db_version(version_id INTEGER, is_applied INTEGER)"
        )
        connection.execute(
            "INSERT INTO goose_db_version(version_id, is_applied) VALUES (1, 1)"
        )
        connection.execute(
            """
            CREATE TABLE media_file (
                id TEXT PRIMARY KEY,
                library_id TEXT,
                path TEXT,
                title TEXT,
                album TEXT,
                duration REAL,
                size INTEGER
            )
            """
        )
        connection.commit()
    finally:
        connection.close()

    with pytest.raises(NavidromeSchemaError, match="artist"):
        NavidromeLiveReader(database)
