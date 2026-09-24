from __future__ import annotations

import threading
from pathlib import Path

import pytest

from zara.database import DatabaseManager
from zara.notification_routing import (
    NotificationDecision,
    NotificationEvent,
    NotificationRouterStore,
    NotificationStale,
)


NOW = 1_790_135_000_000


def _event(generation: int) -> NotificationEvent:
    return NotificationEvent(
        notification_id="notification:generation-race",
        principal_id="user:alice",
        workspace_id="ws:main",
        source_peer="phone",
        owner_peer="phone",
        platform="android",
        source_key="android:pkg:42",
        generation=generation,
        app="com.example.chat",
        created_at_ms=NOW - 1000,
        expires_at_ms=NOW + 60_000,
        title="hello",
        body=f"generation-{generation}",
        category="message",
        importance="normal",
        action_handles=("dismiss",),
    )


def _decision(event: NotificationEvent, sink: str) -> NotificationDecision:
    return NotificationDecision(
        notification_id=event.notification_id,
        decision="allow",
        sinks=(sink,),
        presentation={"generation": event.generation},
        evidence=(f"generation:{event.generation}",),
    )


def test_concurrent_generation_admission_cannot_regress_durable_high_water(
    tmp_path: Path,
) -> None:
    path = tmp_path / "zara.db"
    db1 = DatabaseManager(path)
    db2 = DatabaseManager(path)
    store1 = NotificationRouterStore(db1)
    store2 = NotificationRouterStore(db2)
    first = _event(1)
    second = _event(2)

    read_barrier = threading.Barrier(2)
    second_committed = threading.Event()
    fetch1 = db1.fetch_one
    fetch2 = db2.fetch_one
    execute1 = db1.execute
    execute2 = db2.execute

    def synchronized_fetch(original):
        def wrapped(statement, params=None):
            row = original(statement, params)
            if "FROM notification_router_seen" in statement and "content_digest" in statement:
                read_barrier.wait(timeout=2)
            return row

        return wrapped

    def delayed_first_write(statement, params=None):
        if "INSERT INTO notification_router_seen" in statement:
            assert second_committed.wait(2), "generation 2 never committed"
        return execute1(statement, params)

    def observed_second_write(statement, params=None):
        cursor = execute2(statement, params)
        if "INSERT INTO notification_router_seen" in statement:
            second_committed.set()
        return cursor

    db1.fetch_one = synchronized_fetch(fetch1)
    db2.fetch_one = synchronized_fetch(fetch2)
    db1.execute = delayed_first_write
    db2.execute = observed_second_write

    failures: list[BaseException] = []

    def observe(store, event, digest):
        try:
            store.observe(event, digest, NOW + event.generation)
        except NotificationStale:
            return
        except BaseException as error:
            failures.append(error)

    threads = [
        threading.Thread(target=observe, args=(store1, first, "digest:one")),
        threading.Thread(target=observe, args=(store2, second, "digest:two")),
    ]
    try:
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=5)
            assert not thread.is_alive()
    finally:
        db1.fetch_one = fetch1
        db2.fetch_one = fetch2
        db1.execute = execute1
        db2.execute = execute2

    assert failures == []
    row = db1.fetch_one(
        """
        SELECT generation FROM notification_router_seen
         WHERE principal_id=? AND workspace_id=? AND notification_id=?
        """,
        (first.principal_id, first.workspace_id, first.notification_id),
    )
    assert row is not None
    assert int(row["generation"]) == 2


def test_stale_route_state_commit_cannot_replace_newer_presentations(
    tmp_path: Path,
) -> None:
    path = tmp_path / "zara.db"
    db1 = DatabaseManager(path)
    db2 = DatabaseManager(path)
    store1 = NotificationRouterStore(db1)
    store2 = NotificationRouterStore(db2)
    first = _event(1)
    second = _event(2)

    store1.observe(first, "digest:one", NOW + 1)
    store2.observe(second, "digest:two", NOW + 2)
    store2.record_route_state(second, _decision(second, "desktop"))

    with pytest.raises(NotificationStale):
        store1.record_route_state(first, _decision(first, "wear"))

    rows = db1.fetch_all(
        """
        SELECT generation, sink_peer
          FROM notification_router_presentations
         WHERE principal_id=? AND workspace_id=? AND notification_id=?
         ORDER BY generation, sink_peer
        """,
        (first.principal_id, first.workspace_id, first.notification_id),
    )
    assert [(int(row["generation"]), str(row["sink_peer"])) for row in rows] == [
        (2, "desktop"),
    ]
