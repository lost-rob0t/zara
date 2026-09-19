from __future__ import annotations

import json
import sqlite3
from pathlib import Path

import pytest

from zara.music.mirror import NavidromeTek9Importer
from zara.music.navidrome_db import NavidromeLiveReader
from zara.music.tek9_worker import Tek9WorkerUnavailable


def build_db(path: Path) -> None:
    connection = sqlite3.connect(path)
    try:
        connection.execute("PRAGMA journal_mode=WAL")
        connection.execute(
            "CREATE TABLE goose_db_version(version_id INTEGER, is_applied INTEGER)"
        )
        connection.execute(
            "INSERT INTO goose_db_version(version_id, is_applied) VALUES (7, 1)"
        )
        connection.execute(
            """
            CREATE TABLE media_file (
                id TEXT PRIMARY KEY,
                library_id TEXT NOT NULL,
                path TEXT NOT NULL,
                title TEXT NOT NULL,
                album TEXT NOT NULL,
                artist TEXT NOT NULL,
                duration REAL NOT NULL,
                size INTEGER NOT NULL,
                updated_at TEXT,
                album_id TEXT,
                artist_id TEXT,
                mbz_recording_id TEXT
            )
            """
        )
        connection.executemany(
            """
            INSERT INTO media_file(
                id, library_id, path, title, album, artist, duration, size,
                updated_at, album_id, artist_id, mbz_recording_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
                (
                    "a",
                    "lib",
                    "A/Album/a.flac",
                    "A",
                    "Album",
                    "Artist",
                    180.0,
                    100,
                    "2026-09-18T20:00:00Z",
                    "album",
                    "artist",
                    "mbid-a",
                ),
                (
                    "b",
                    "lib",
                    "A/Album/b.flac",
                    "B",
                    "Album",
                    "Artist",
                    181.0,
                    101,
                    "2026-09-18T20:01:00Z",
                    "album",
                    "artist",
                    "mbid-b",
                ),
                (
                    "c",
                    "lib",
                    "A/Album/c.flac",
                    "C",
                    "Album",
                    "Artist",
                    182.0,
                    102,
                    "2026-09-18T20:02:00Z",
                    "album",
                    "artist",
                    "mbid-c",
                ),
            ],
        )
        connection.commit()
    finally:
        connection.close()


class FakeTek9:
    def __init__(self, *, generation=0, watermark=None):
        self.generation = generation
        self.watermark = watermark
        self.applied = []

    def status(self, source_id):
        assert source_id == "navidrome"
        return {
            "ok": True,
            "generation": self.generation,
            "watermark": self.watermark,
        }

    def apply_batch(self, **kwargs):
        assert kwargs["expected_generation"] == self.generation
        self.generation = kwargs["generation"]
        self.watermark = kwargs.get("watermark")
        self.applied.append(kwargs)
        return {
            "ok": True,
            "generation": self.generation,
            "watermark": self.watermark,
        }


def test_whole_library_import_is_bounded_and_finishes_live(tmp_path):
    database = tmp_path / "navidrome.db"
    state = tmp_path / "state"
    build_db(database)
    tek9 = FakeTek9()

    with NavidromeLiveReader(database) as reader:
        result = NavidromeTek9Importer(
            reader=reader,
            tek9=tek9,
            state_dir=state,
            snapshot_name_factory=lambda: "snapshot-one.db",
        ).import_all(batch_size=2)

    assert result.rows == 3
    assert result.batches == 2
    assert result.generation == 3
    assert [len(call["documents"]) for call in tek9.applied] == [2, 1, 0]
    final = json.loads(tek9.watermark)
    assert final["phase"] == "live"
    assert final["schema_version"] == 7
    assert not (state / "snapshot-one.db").exists()


def test_import_resumes_existing_snapshot_from_checkpoint(tmp_path):
    database = tmp_path / "navidrome.db"
    state = tmp_path / "state"
    state.mkdir()
    build_db(database)

    with NavidromeLiveReader(database) as live:
        live.create_snapshot(state / "resume.db")

    watermark = json.dumps(
        {
            "v": 1,
            "phase": "snapshot",
            "snapshot": "resume.db",
            "last_id": "a",
            "schema_version": 7,
        },
        separators=(",", ":"),
        sort_keys=True,
    )
    tek9 = FakeTek9(generation=4, watermark=watermark)

    with NavidromeLiveReader(database) as reader:
        result = NavidromeTek9Importer(
            reader=reader,
            tek9=tek9,
            state_dir=state,
            snapshot_name_factory=lambda: "must-not-be-used.db",
        ).import_all(batch_size=1)

    assert result.rows == 2
    assert [call["documents"][0]["value"]["source_id"] for call in tek9.applied[:-1]] == [
        "b",
        "c",
    ]
    assert tek9.generation == 7


def test_missing_resume_snapshot_restarts_full_scan_safely(tmp_path):
    database = tmp_path / "navidrome.db"
    state = tmp_path / "state"
    build_db(database)
    watermark = json.dumps(
        {
            "v": 1,
            "phase": "snapshot",
            "snapshot": "gone.db",
            "last_id": "b",
            "schema_version": 7,
        },
        separators=(",", ":"),
        sort_keys=True,
    )
    tek9 = FakeTek9(generation=8, watermark=watermark)

    with NavidromeLiveReader(database) as reader:
        result = NavidromeTek9Importer(
            reader=reader,
            tek9=tek9,
            state_dir=state,
            snapshot_name_factory=lambda: "replacement.db",
        ).import_all(batch_size=3)

    assert result.rows == 3
    assert [doc["value"]["source_id"] for doc in tek9.applied[0]["documents"]] == [
        "a",
        "b",
        "c",
    ]


def test_snapshot_checkpoint_cannot_escape_state_directory(tmp_path):
    database = tmp_path / "navidrome.db"
    build_db(database)
    watermark = json.dumps(
        {
            "v": 1,
            "phase": "snapshot",
            "snapshot": "../escape.db",
            "last_id": "b",
            "schema_version": 7,
        }
    )
    tek9 = FakeTek9(generation=1, watermark=watermark)

    with NavidromeLiveReader(database) as reader:
        with pytest.raises(ValueError, match="snapshot"):
            NavidromeTek9Importer(
                reader=reader,
                tek9=tek9,
                state_dir=tmp_path / "state",
            ).import_all()


def test_worker_death_after_commit_is_reconciled_by_status(tmp_path):
    database = tmp_path / "navidrome.db"
    build_db(database)

    class CommitThenDie(FakeTek9):
        def __init__(self):
            super().__init__()
            self.once = True

        def apply_batch(self, **kwargs):
            result = super().apply_batch(**kwargs)
            if self.once:
                self.once = False
                raise Tek9WorkerUnavailable("response pipe died")
            return result

    tek9 = CommitThenDie()
    with NavidromeLiveReader(database) as reader:
        result = NavidromeTek9Importer(
            reader=reader,
            tek9=tek9,
            state_dir=tmp_path / "state",
            snapshot_name_factory=lambda: "snapshot.db",
        ).import_all(batch_size=3)

    assert result.rows == 3
    assert tek9.generation == 2
