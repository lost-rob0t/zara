import threading
import time

from zara.database import DatabaseManager


class FakeCursor:
    def __init__(self, connection) -> None:
        self._connection = connection
        self._commits_at_execute = connection.commit_count

    def _invalidated(self) -> bool:
        return self._connection.commit_count != self._commits_at_execute + 1

    def fetchone(self):
        if self._invalidated():
            return None
        return {"status": "pending", "id": 1}

    def fetchall(self):
        if self._invalidated():
            return []
        return [{"status": "pending", "id": 1}]


class FakeConnection:
    def __init__(self) -> None:
        self.commit_count = 0

    def execute(self, _statement, _params=None):
        return FakeCursor(self)

    def commit(self):
        self.commit_count += 1


def build_fake_db():
    db = DatabaseManager.__new__(DatabaseManager)
    db._db_path = None
    db._lock = threading.RLock()
    db._connection = FakeConnection()
    db._migrations = {}
    return db


def run_reader_race(db, fetch):
    results = []
    reader_exposed_gap = threading.Event()

    original_execute = db.execute

    def gated_execute(statement, params=None):
        cursor = original_execute(statement, params)
        if statement.startswith("SELECT"):
            reader_exposed_gap.set()
            time.sleep(0.3)
        return cursor

    def run_fetch():
        db.execute = gated_execute
        results.append(fetch())

    reader_thread = threading.Thread(target=run_fetch)
    reader_thread.start()
    assert reader_exposed_gap.wait(timeout=5)

    writer = threading.Thread(
        target=lambda: db.execute("INSERT INTO items VALUES (2, 'x')")
    )
    writer.start()

    reader_thread.join(timeout=10)
    writer.join(timeout=10)

    assert db._connection.commit_count >= 2, "both reader and writer must have committed"
    return results


def test_fetch_one_completes_before_a_concurrent_writer_commits():
    db = build_fake_db()
    results = run_reader_race(db, lambda: db.fetch_one("SELECT status FROM items WHERE id = 1"))
    assert results[0] == {"status": "pending", "id": 1}


def test_fetch_all_completes_before_a_concurrent_writer_commits():
    db = build_fake_db()
    results = run_reader_race(db, lambda: db.fetch_all("SELECT status FROM items"))
    assert results[0] == [{"status": "pending", "id": 1}]
