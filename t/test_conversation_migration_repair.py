from __future__ import annotations

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore


def test_conversation_store_repairs_v2_marker_without_desktop_tables(tmp_path):
    db = DatabaseManager(tmp_path / "broken-desktop.db")
    db.connect()
    db.execute(
        "INSERT INTO schema_migrations (version, applied_at) VALUES (?, strftime('%s','now'))",
        (2,),
    )

    assert db.fetch_one(
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'desktop_conversations'"
    ) is None
    assert db.fetch_one(
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'desktop_messages'"
    ) is None

    store = ConversationStore(db)
    conversation = store.create_conversation("Recovered desktop")

    assert store.get_conversation(conversation.id) is not None
    assert db.fetch_one(
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'desktop_conversations'"
    ) is not None
    assert db.fetch_one(
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'desktop_messages'"
    ) is not None

    # The repair is schema-driven rather than another integer migration. That
    # avoids repeating the same version-collision failure with version 3.
    versions = {row["version"] for row in db.fetch_all("SELECT version FROM schema_migrations")}
    assert versions == {2}

    # Re-opening through the exported store must remain a no-op repair.
    ConversationStore(db)
    assert {row["version"] for row in db.fetch_all("SELECT version FROM schema_migrations")} == {2}
